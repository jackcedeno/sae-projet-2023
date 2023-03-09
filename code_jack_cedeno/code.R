library(dplyr)
library(ggplot2)
library(reshape2)
library(scales)

setwd("C:/Users/jackc/Desktop/school/homework/datavizprojet")
epi <- read.table("Epi_Clin.txt", sep = "\t", header = TRUE)
epidf <- as.data.frame.matrix(epi)

categories <- c('CAT1', 'CAT2', 'RESP', 'CARD', 'NEURO', 'GASTR', 'RENAL', 'META', 'HEMA', 'SEPS', 'TRAUMA', 'ORTHO', 'DEATH', 'CA')
diagnostics <- c('RESP', 'CARD', 'NEURO', 'GASTR', 'RENAL', 'META', 'HEMA', 'SEPS', 'TRAUMA', 'ORTHO', 'CA')
good_variables <- c('PTID','CAT1', 'CAT2', 'RESP', 'CARD', 'NEURO', 'GASTR', 'RENAL', 'META', 'HEMA', 'SEPS', 'TRAUMA', 'ORTHO', 'DEATH', 'SWANG1', 'CA')
categories_in_cat1 <- c('Respiratoire', 'MOSF', 'CHF', 'Autre')
our_color = rgb(0.1,0.4,0.5,0.7)

#Traitement des donnees

#Enlever les doublons
epidf[!duplicated(epidf), ]

# Retirer les lignes où il y a des valeurs aberrantes
epidf <- subset(epidf,DAS2D3PC <= 33)
#epidf <- subset(epidf,ADLD3P <= 7)
epidf <- subset(epidf,APS1 <= 299)
epidf <- subset(epidf,SCOMA1 <= 100)

#Classer les categories
df <- epidf

saver <- function(to_save, name, w, h){
  ggsave(filename = paste(name, '.png', sep=''), device='png', width = w, height = h, plot = to_save, path='./plots')
}

replacer <- function(df, colonne, dict){ #on definit une fonction "replacer"
  for(diagn in names(dict)){ #pour chaque diagnostique dans la dictionaire
    df[,colonne][df[,colonne] == diagn] <- dict[diagn] #on prend la colonne, si le valeur dans la colonne est egale a la diagnostique, on remplace avec le nom du classe definit dans la dictionnaire
    #chez nous, colonne = CAT2, diagn = ARF, COPD, MOSF w/Malignancy..., qui vont etre remplacer par respiratoire, mosf etc
  }
  return(df)
}
dict <- c( #on definit la relation, ce qu'on a, et son classe (donc ARF est remplace par respiratoire)
  'ARF' = 'Respiratoire',
  'COPD' = 'Respiratoire',
  'MOSF w/Malignancy' = 'MOSF',
  'MOSF w/Sepsis' = 'MOSF',
  'Cirrhosis' = 'Autre',
  'Colon Cancer' = 'Autre',
  'Coma' = 'Autre',
  'Lung Cancer' = 'Autre'
)

df <- replacer(df, 'CAT1', dict) #application de la fonction sur CAT1
df <- replacer(df, 'CAT2', dict) #meme chose mais pour cat2

df <- df[good_variables]

#chi2

# On peut prendre 0.05 comme truc pour p-valeur (donc p-value)
for(i in diagnostics){
  print(i)
  print(chisq.test(table(df[,i], df$SWANG1), simulate.p.value = TRUE))
}

#graphiques

#CAT1
percents <- c()
for(i in categories_in_cat1){
  percents[i] = sum(df$CAT1==i)/nrow(df)
}
percents <- stack(percents)

p <- ggplot(data = percents, aes(x = ind, y = values)) + geom_bar(stat='identity', fill = our_color) + ylab('Pourcentage') + 
  xlab('Catégorie de diagnostique') + 
  geom_text(aes(label = paste(round(values*100,1), '%'), vjust = 2), color = 'white') + scale_y_continuous(labels = scales::percent)
p
saver(p, 'unicat1', 5, 5)

#CAT2
percents <- c()
for(i in categories_in_cat1){
  percents[i] = sum(df$CAT2==i)/nrow(df)
}
percents <- stack(percents)

p <- ggplot(data = percents, aes(x = ind, y = values)) + geom_bar(stat='identity', fill = our_color) + ylab('Pourcentage') + 
  xlab('Catégorie de diagnostique') + 
  geom_text(aes(label = paste(round(values*100,1), '%'), vjust = 2), color = 'white') + 
  scale_y_continuous(labels = scales::percent)
p
saver(p, 'unicat2', 5, 5)

#Les diagnostiques
percents <- c()
for(i in diagnostics){
  percents[i] <- sum(ifelse(df[,i]=='Yes',TRUE,FALSE)) / nrow(df)
}
percents <- stack(percents)
percents <- melt(percents, id.vars = 'ind', variable.name = 'series')

p <- ggplot(percents, aes(ind, value)) + geom_bar(stat='identity', fill = our_color) + ylab('Pourcentage') + 
  xlab('Catégorie de maladie') +
  geom_text(aes(label = paste(round(value*100,1), '%'), vjust = 2), color = 'white') + 
  scale_y_continuous(labels = scales::percent)
p
saver(p, 'unicats', 7, 7)
#Les morts

percents <- c(
  'Vivant' = sum(df$DEATH == 'No')/nrow(df),
  'Mort' = sum(df$DEATH == 'Yes')/nrow(df)
)
percents <- stack(percents)

p <- ggplot(data = percents, aes(x = ind, y = values)) + geom_bar(stat='identity', fill = our_color) + ylab('Pourcentage') + 
  xlab('Vivant ou mort') + 
  geom_text(aes(label = paste(round(values*100,1), '%'), vjust = 2), color = 'white') + 
  scale_y_continuous(labels = scales::percent)
p
saver(p, 'death', 5, 5)

#Death ~ SWANG1
percents <- data.frame(matrix(ncol = 3, nrow = 0))
for(i in c('Yes', 'No')){
  for(j in c('RHC', 'No RHC')){
    percents <- rbind(percents, data.frame(i,j,length(df$DEATH[df$DEATH == i & df$SWANG1 == j])/nrow(df)))
  }
}
colnames(percents) <- c('DEATH', 'SWANG1', 'Values')

p <- ggplot(percents, aes(x=DEATH, y=Values, fill=SWANG1)) + geom_bar(stat='identity', position='dodge') +
  geom_text(aes(label = paste(round(Values*100,1), '%')),position=position_dodge(width = 0.9), color = 'White', vjust=2) +
  scale_y_continuous(labels = scales::percent) + xlab('Vivant') + ylab('Pourcentage de la population') + labs(title = 'Population de la population morte ayant reçu un RHC ', fill = 'Traitement')
p
saver(p, 'deathswang1', 6,5)

#Diagnostique ~ RHC
percents <- data.frame(matrix(ncol = 3, nrow = 0))
for(i in categories_in_cat1){
  for(j in c('RHC', 'No RHC')){
    percents <- rbind(percents, data.frame(i,j,length(df$CAT1[df$CAT1 == i & df$SWANG1 == j])/nrow(df)))
  }
}
colnames(percents) <- c('CAT1', 'SWANG1', 'Values')

p <- ggplot(percents, aes(x=CAT1, y=Values, fill=SWANG1)) + geom_bar(stat='identity', position='dodge') +
  geom_text(aes(label = paste(round(Values*100,1), '%')),position=position_dodge(width = 0.9), color = 'black') +
  scale_y_continuous(labels = scales::percent) + xlab('Catégorie de diagnostique') + ylab('Pourcentage de la population') + coord_flip() + labs(title = 'Pourcentage de la population ayant reçu un RHC', fill = 'Traitement')
p
saver(p, 'RHCparrapportauCAT1', 7,5)

summarizer <- function(v){
  print(glue('Moyenne = {mean(v)}'))
}

