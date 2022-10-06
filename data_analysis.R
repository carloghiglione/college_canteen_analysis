remove(list = ls())
library(pheatmap)
library(MASS)
library(igraph)

#importo il dataset nella variabile dataset
load("dataset.Rdata")

#numero totale di residenti
n_res <- length(unique(dataset$Id))

#creo una struttura dati che per ogni residente contenga le informazioni necessarie
setClass(
  "DataId",
  slots = list(
    id = "numeric",              #ID del residente
    insiemeP = "numeric",        #array del numero di giorni in cui il residente ? stato a pranzo con ogni altro residente (senza considerare il turno)
    insiemeC = "numeric",        #array del numero di giorni in cui il residente ? stato a cena con ogni altro residente (senza considerare il turno)
    turni_insiemeP = "numeric",  #array del numero di giorni in cui il residente ? stato a pranzo e allo stesso turno con ogni altro residente
    turni_insiemeC = "numeric",  #array del numero di giorni in cui il residente ? stato a cena e allo stesso turno con ogni altro residente
    tot_t1P = "numeric",         #numero totale di pranzi al primo turno
    tot_t2P = "numeric",         #numero totale di pranzi al secondo turno
    tot_t1C = "numeric",         #numero totale di cene al primo turno
    tot_t2C = "numeric"          #numero totale di cene al secondo turno
  )
)

#array dove salver? le strutture dati per ogni residente
all_res <- c()

for(res in 1:n_res){
  #inizializzo a zero la struttura dati del residente corrente
  res_curr <- new("DataId", id=res,
                  insiemeP = rep(0,n_res),
                  insiemeC = rep(0,n_res),
                  turni_insiemeP = rep(0,n_res),
                  turni_insiemeC = rep(0,n_res),
                  tot_t1P = 0,
                  tot_t2P = 0,
                  tot_t1C = 0,
                  tot_t2C = 0)
  #aggiungo all'array la struttura dati
  all_res <- c(all_res, res_curr)
}

#lista delle date dei giorni del dataset
dates <- unique(dataset$Data)

for(day in 1:length(dates)){
  #estraggo dal dataset separatatamente per pranzo e cena le righe del giorno corrente
  curr_df_P <- dataset[which(dataset$Data == dates[day] & dataset$Tipo == "Pranzo"),]
  curr_df_C <- dataset[which(dataset$Data == dates[day] & dataset$Tipo == "Cena"),]
  t1P = curr_df_P[which(curr_df_P$Prenotazione == "Turno 1"),]$Id   #lista residenti al turno 1 del pranzo 
  t2P = curr_df_P[which(curr_df_P$Prenotazione == "Turno 2"),]$Id   #lista residenti al turno 2 del pranzo 
  t1C = curr_df_P[which(curr_df_C$Prenotazione == "Turno 1"),]$Id   #lista residenti al turno 1 della cena
  t2C = curr_df_P[which(curr_df_C$Prenotazione == "Turno 2"),]$Id   #lista residenti al turno 1 della cena 
  for(res in 1:n_res){
    if(res %in% t1P)
      all_res[[res]]@turni_insiemeP[t1P] = all_res[[res]]@turni_insiemeP[t1P] + 1  #se il residente corrente ? nella lista dei turno 1 pranzo, aggiungi + 1 turni_insiemeP per tutti i residenti con lui quel giorno
    if(res %in% t2P)
      all_res[[res]]@turni_insiemeP[t2P] = all_res[[res]]@turni_insiemeP[t2P] + 1  #idem turno 2 pranzo
    if(res %in% t1C)
      all_res[[res]]@turni_insiemeC[t1C] = all_res[[res]]@turni_insiemeC[t1C] + 1  #idem turno 1 cena
    if(res %in% t2C)
      all_res[[res]]@turni_insiemeC[t2C] = all_res[[res]]@turni_insiemeC[t2C] + 1  #idem turno 2 cena
    
    if(res %in% t1P || res %in% t2P)
      all_res[[res]]@insiemeP[unique(c(t1P,t2P))] = all_res[[res]]@insiemeP[unique(c(t1P,t2P))] + 1  #se il residente corrente era a pranzo nel giorno corrente, aggiungi +1 a insieme_P per tutti i residendi con lui quel giorn
    if(res %in% t1C || res %in% t2C)
      all_res[[res]]@insiemeC[unique(c(t1C,t2C))] = all_res[[res]]@insiemeC[unique(c(t1C,t2C))] + 1  #idem per le cene
  }
}

for(res in 1:n_res){
  all_res[[res]]@tot_t1P <- length(which(dataset$Id == res & dataset$Tipo == "Pranzo" & dataset$Prenotazione == "Turno 1"))   #tot numero giorni al turno 1 pranzo
  all_res[[res]]@tot_t2P <- length(which(dataset$Id == res & dataset$Tipo == "Pranzo" & dataset$Prenotazione == "Turno 2"))   #tot numero giorni al turno 2 pranzo
  all_res[[res]]@tot_t1C <- length(which(dataset$Id == res & dataset$Tipo == "Cena" & dataset$Prenotazione == "Turno 1"))     #tot numero giorni al turno 1 cena
  all_res[[res]]@tot_t2C <- length(which(dataset$Id == res & dataset$Tipo == "Cena" & dataset$Prenotazione == "Turno 2"))     #tot numero giorni al turno 2 cena
}

all_res_perc <- all_res    #trasformo in percentuale i vari dati
for(res in 1:n_res){
  all_res_perc[[res]]@turni_insiemeP <- (all_res[[res]]@turni_insiemeP)/(all_res[[res]]@insiemeP)
  all_res_perc[[res]]@turni_insiemeC <- (all_res[[res]]@turni_insiemeC)/(all_res[[res]]@insiemeC)
}

#strutture dati
mat_P <- matrix(0,n_res,n_res)   #matrice incontri reciproci pranzo
mat_C <- matrix(0,n_res,n_res)   #matrice incontri reciproci pranzo
perc_t1P <- rep(0,n_res)         #array percentuali turno 1 pranzo
perc_t2P <- rep(0,n_res)         #array percentuali turno 2 pranzo
perc_t1C <- rep(0,n_res)         #array percentuali turno 1 cena
perc_t2C <- rep(0,n_res)         #array percentuali turno 2 cena

#assegno i valori alle strutture dati
for(res in 1:n_res){
  mat_P[res,] <- all_res_perc[[res]]@turni_insiemeP
  mat_C[res,] <- all_res_perc[[res]]@turni_insiemeC
  perc_t1P[res] <- (all_res[[res]]@tot_t1P)/(all_res[[res]]@tot_t1P + all_res[[res]]@tot_t2P)
  perc_t2P[res] <- (all_res[[res]]@tot_t2P)/(all_res[[res]]@tot_t1P + all_res[[res]]@tot_t2P)
  perc_t1C[res] <- (all_res[[res]]@tot_t1C)/(all_res[[res]]@tot_t1C + all_res[[res]]@tot_t2C)
  perc_t2C[res] <- (all_res[[res]]@tot_t2C)/(all_res[[res]]@tot_t1C + all_res[[res]]@tot_t2C)
}

#visualizzo le matrici degli incontri reciproci con una heatmap
pheatmap(mat_P, color = colorRampPalette(c("black", "white"))(5), cluster_rows = TRUE, cluster_cols = TRUE, treeheight_row = 0, treeheight_col = 0, main="Heatmap Pranzo")
pheatmap(mat_C, color = colorRampPalette(c("black", "white"))(5), cluster_rows = TRUE, cluster_cols = TRUE, treeheight_row = 0, treeheight_col = 0, main="Heatmap Cena")

#array degli score di incontro per ogni residente (pranzo e cena separatamente)
score_P <- rep(0,n_res)
score_C <- rep(0,n_res)
alp <- 0.25  #parametro di sufficienza minima di incontro

for(res in 1:n_res){
  quali_suff_P <- which(mat_P[res,] > alp)  #lista dei residenti con cui il residente corrente supera la soglia di sufficienza minima a pranzo
  quali_suff_C <- which(mat_C[res,] > alp)  #idem cena
  score_P[res] <- length(quali_suff_P)      #numero di residenti con cui il residente corrente supera la soglia di sufficienza minima a pranzo
  score_C[res] <- length(quali_suff_C)      #idem cena
}
score_P <- 10*score_P/n_res      #calcolo score
mean_score_P <- mean(score_P)    #media score
score_C <- 10*score_C/n_res
mean_score_C <- mean(score_C)

#visualizzo gli istogrammi degli score di incontro
hist(score_P,breaks=seq(1,10,0.5), xlim=c(1,10), ylim=c(1,30), main="Score Pranzo",col="cornflowerblue")
abline(v=mean_score_P, col="red")
axis(side=1, at=seq(1,10,1))

hist(score_C,breaks=seq(1,10,0.5), xlim=c(1,10), ylim=c(1,30), main="Score Cena",col="cornflowerblue")
abline(v=mean_score_C, col="red")
axis(side=1, at=seq(1,10,1))


#modelli di regressione lineare

#modello punteggi pranzi vs percentuali turno 1 pranzi
mod_t1P <- lm(score_P ~ perc_t1P + I(perc_t1P^2))   #costruzione modello y = ax^2 + b^x + c + N(0,sigma^2)
coefs_t1P <- mod_t1P$coef                           #coefficienti a, b, c
print(summary(mod_t1P))                             #summary
print(shapiro.test(mod_t1P$residuals))              #shapiro test sui residui

#modello punteggi cene vs percentuali turno 1 cene  
mod_t1C <- lm(score_C ~ perc_t1C + I(perc_t1C^2))   #come sopra
coefs_t1C <- mod_t1C$coef
print(summary(mod_t1C))
print(shapiro.test(mod_t1C$residuals))  

#plot modello punteggi pranzi vs percentuali turno 1 pranzi
plot(perc_t1P,score_P, pch = 16)
s_perc_t1P <- sort(perc_t1P)
lines(s_perc_t1P, coefs_t1P[1] + coefs_t1P[2] * s_perc_t1P + coefs_t1P[3] * s_perc_t1P^2, pch = 16, col='red')

#plot modello punteggi pranzi vs percentuali turno 1 cene
plot(perc_t1C,score_C, pch = 16)
s_perc_t1C <- sort(perc_t1C)
lines(s_perc_t1C, coefs_t1C[1] + coefs_t1C[2] * s_perc_t1C + coefs_t1C[3] * s_perc_t1C^2, pch = 16, col='red')

#analisi normalit? Pranzo
par(mfrow = c(2, 2))                                                                          #grid 2x2
boxplot(mod_t1P$residuals)                                                                    #boxplot residui
hist(rstandard(mod_t1P), breaks=seq(min(rstandard(mod_t1P)),max(rstandard(mod_t1P)),l=10+1))  #istogramma residui standardizzati
plot(mod_t1P$fitted.values,rstandard(mod_t1P), pch = 16)                                      #plot residui vs fitted values
abline(h = 0, col='red')
abline(h = 2, col='blue')
abline(h = -2, col='blue')
qqnorm(mod_t1P$residuals, pch = 16)                                                           #qqplot residui
qqline(mod_t1P$residuals, col='red')

#analisi normalit? Cena
par(mfrow = c(2, 2))
boxplot(mod_t1C$residuals)
hist(rstandard(mod_t1C), breaks=seq(min(rstandard(mod_t1C)),max(rstandard(mod_t1C)),l=10+1))
plot(mod_t1C$fitted.values,rstandard(mod_t1C), pch = 16)
points(mod_t1C$fitted.values[10],rstandard(mod_t1C)[10], col="red", pch=16)
abline(h = 0, col='red')
abline(h = 2, col='blue')
abline(h = -2, col='blue')
qqnorm(mod_t1C$residuals, pch = 16)
points(mod_t1C$residuals[10], pch=16, col='red')
qqline(mod_t1C$residuals, col='red')




drop_param <- 0.80

diag(mat_P) <- 0
mat_P[which(mat_P < drop_param)] <- 0
graph_P <- graph_from_adjacency_matrix(mat_P, weighted = TRUE, mode = "undirected")
edges_P <- E(graph_P)
edges_P_weigth <- edge_attr(graph_P)[[1]]

diag(mat_C) <- 0
mat_C[which(mat_C < drop_param)] <- 0
graph_C <- graph_from_adjacency_matrix(mat_C, weighted = TRUE, mode = "undirected")
edges_C <- E(graph_C)
edges_C_weigth <- edge_attr(graph_C)[[1]]

n_colors <- 5
colors <- colorRampPalette(c("blue", "red"))(n_colors) 
percs <- seq(0,1,0.2)
for(k in 1:n_colors){
  V(graph_P)[which(perc_t1P >= percs[k] & perc_t1P < percs[k+1])]$color <- colors[k]
  V(graph_C)[which(perc_t1C >= percs[k] & perc_t1C < percs[k+1])]$color <- colors[k]
}
V(graph_P)[which(perc_t1P == 1)]$color <- colors[n_colors]
V(graph_C)[which(perc_t1C == 1)]$color <- colors[n_colors]

#visualizzazione grafo
rescale = function(x){(x^3)/100}
sizeCut <- c(5,6,7,8,9,10)
sizeCutScale <- rescale(sizeCut)
x11()
#par(mfrow = c(1, 2))  #to add labels, vertex.labels=surnames
plot.igraph(graph_P, main = "Pranzo", vertex.size = rescale(score_P),
            edge.color = "black", vertex.label = NA, edge.width = edges_P_weigth)
legend('topleft',legend=unique(sizeCut),pt.cex= sizeCutScale,col='black')
a <- legend('topleft',legend=unique(sizeCut),pt.cex=sizeCutScale/200,col='white',
            pch=21, pt.bg='white')#, title = "Score")
x <- (a$text$x + a$rect$left) / 2
y <- a$text$y
symbols(x,y,circles=sizeCutScale/200,inches=FALSE,add=TRUE)
legend("bottomleft",legend=c("0.0 - 0.2","0.2 - 0.4","0.4 - 0.6","0.6 - 0.8","0.8 - 1.0"), fill=colors, title = "Turno 1")

x11()
plot.igraph(graph_C, main = "Cena", vertex.size = rescale(score_C),
            edge.color = "black", vertex.label = NA, edge.width = edges_P_weigth)
legend('topleft',legend=unique(sizeCut),pt.cex= sizeCutScale,col='black')
a <- legend('topleft',legend=unique(sizeCut),pt.cex=sizeCutScale/200,col='white',
            pch=21, pt.bg='white')#, title = "Score")
x <- (a$text$x + a$rect$left) / 2
y <- a$text$y
symbols(x,y,circles=sizeCutScale/200,inches=FALSE,add=TRUE)
legend("bottomleft",legend=c("0.0 - 0.2","0.2 - 0.4","0.4 - 0.6","0.6 - 0.8","0.8 - 1.0"), fill=colors, title = "Turno 1")
