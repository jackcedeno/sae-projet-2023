library(dplyr)
library(ggplot2)
library(reshape2)
library(scales)
setwd("C:/Users/Laurent/Desktop/Projet")
epi <- read.table("Epi_Clin.txt", sep = "\t", header = TRUE)
epidf <- as.data.frame.matrix(epi)

categories <- c('CAT1', 'CAT2', 'RESP', 'CARD', 'NEURO', 'GASTR', 'RENAL', 'META', 'HEMA', 'SEPS', 'TRAUMA', 'ORTHO', 'DEATH')
diagnostics <- c('RESP', 'CARD', 'NEURO', 'GASTR', 'RENAL', 'META', 'HEMA', 'SEPS', 'TRAUMA', 'ORTHO')
good_variables <- c('PTID','CAT1', 'CAT2', 'RESP', 'CARD', 'NEURO', 'GASTR', 'RENAL', 'META', 'HEMA', 'SEPS', 'TRAUMA', 'ORTHO', 'DEATH', 'SWANG1','SURV2MD1')
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



prop <-c()
Respiratoire_filter <- filter(df, CAT1 == "Respiratoire")
Respiratoire_filter <- filter(Respiratoire_filter,SWANG1 == "No RHC")
prop<-append(prop,mean(Respiratoire_filter$SURV2MD1))
Respiratoire_filter <- filter(df,CAT1 == "Respiratoire")
Respiratoire_filter <- filter(Respiratoire_filter,SWANG1 == "RHC")
prop<-append(prop,mean(Respiratoire_filter$SURV2MD1))

CHF_filter <- filter(df, CAT1 == "CHF")
CHF_filter <- filter(CHF_filter,SWANG1 == "No RHC")
prop<-append(prop,mean(CHF_filter$SURV2MD1))
CHF_filter <- filter(df,CAT1 == "CHF")
CHF_filter <- filter(CHF_filter,SWANG1 == "RHC")
prop<-append(prop,mean(CHF_filter$SURV2MD1))

MOSF_filter <- filter(df, CAT1 == "MOSF")
MOSF_filter <- filter(MOSF_filter,SWANG1 == "No RHC")
prop<-append(prop,mean(MOSF_filter$SURV2MD1))
MOSF_filter <- filter(df,CAT1 == "MOSF")
MOSF_filter <- filter(MOSF_filter,SWANG1 == "RHC")
prop<-append(prop,mean(MOSF_filter$SURV2MD1))

Autre_filter <- filter(df, CAT1 == "Autre")
Autre_filter <- filter(Autre_filter,SWANG1 == "No RHC")
prop<-append(prop,mean(Autre_filter$SURV2MD1))
Autre_filter <- filter(df,CAT1 == "Autre")
Autre_filter <- filter(Autre_filter,SWANG1 == "RHC")
prop<-append(prop,mean(Autre_filter$SURV2MD1))
prop

#Create data frame
Surv_df <- data.frame( 
  
  SWANG1 = rep(c("No RHC", "RHC"), 4), 
  
  CAT1 = c(rep("Respiratoire",2),rep("CHF",2), rep("MOSF",2),rep("Autre",2)), 
  
  proportion = prop)


# Grouped
p<- ggplot(Surv_df, aes(fill=SWANG1, y=proportion, x=CAT1)) + 
  geom_bar(position="dodge", stat="identity")+
  geom_text(aes(label = paste(round(proportion*100),"%"), group =proportion),position=position_dodge(-0.9),color = "black")+
  labs(title="Moyenne des probabilités de survie à 2 mois selon le RHC pour les diagnostics de CAT1",fill="Traitement")+
  theme(plot.title=element_text(hjust=0.5))+
  scale_y_continuous(labels = percent)+
  coord_flip()
p
saver(p,'CAT1_SURV',8,5)

prop <-c()

MOSF_filter <- filter(df, CAT2 == "MOSF")
MOSF_filter <- filter(MOSF_filter,SWANG1 == "No RHC")
prop<-append(prop,mean(MOSF_filter$SURV2MD1))
MOSF_filter <- filter(df,CAT2 == "MOSF")
MOSF_filter <- filter(MOSF_filter,SWANG1 == "RHC")
prop<-append(prop,mean(MOSF_filter$SURV2MD1))

Autre_filter <- filter(df, CAT2 == "Autre")
Autre_filter <- filter(Autre_filter,SWANG1 == "No RHC")
prop<-append(prop,mean(Autre_filter$SURV2MD1))
Autre_filter <- filter(df,CAT2 == "Autre")
Autre_filter <- filter(Autre_filter,SWANG1 == "RHC")
prop<-append(prop,mean(Autre_filter$SURV2MD1))
prop

#Create data frame
Surv_df <- data.frame( 
  
  SWANG1 = rep(c("No RHC", "RHC"), 2), 
  
  CAT2 = c(rep("MOSF",2),rep("Autre",2)), 
  
  proportion = prop)


# Grouped
p<- ggplot(Surv_df, aes(fill=SWANG1, y=proportion, x=CAT2)) + 
  geom_bar(position="dodge", stat="identity")+
  geom_text(aes(label = paste(round(proportion*100),"%")),position=position_dodge(0.9),color = "black")+
  labs(title="Moyenne des probabilités de survie à 2 mois selon le RHC pour les diagnostics de CAT2",fill="Traitement")+
  theme(plot.title=element_text(hjust=0.5))+
  scale_y_continuous(labels = percent)+
  coord_flip()
p
saver(p,'CAT2_SURV',8,5)
