setwd("C:/Users/Laurent/Desktop/Projet")
epi <- read.table("Epi_Clin.txt",header=T,na.strings=" ", sep="\t")
epidf <- as.data.frame.matrix(epi)

epidf <- epidf[,-52] #Supprime la colonne 52, variable ADLD3P
epidf <- subset(epidf,DAS2D3PC <= 33)
epidf <- subset(epidf,APS1 <= 299)
epidf <- subset(epidf,SCOMA1 <= 100)

#Enlever les doublons potentiels
epidf[!duplicated(epidf), ]

library(ggplot2) #Pour faire des graphiques
library(dplyr)   #Pour filtrer
library(tidyverse)#Pour mettre les % en ordonnées
library(scales)

saver <- function(to_save, name, w, h){
  ggsave(filename = paste(name, '.png', sep=''), device='png', width = w, height = h, plot = to_save, path='./plots')
}

#CA/RHC
CA_filter <- filter(epidf,CA == "Yes")

a <- nrow(CA_filter[CA_filter$SWANG1 == "No RHC", ]) 

b <- nrow(CA_filter[CA_filter$SWANG1 == "RHC", ])

c <- nrow(CA_filter[CA_filter$CA == "Yes", ]) 

res_CA_NoRHC <- (a/c)

res_CA_RHC <- (b/c)



#Resp/RHC
RESP_filter <- filter(epidf,RESP == "Yes")

a <- nrow(RESP_filter[RESP_filter$SWANG1 == "No RHC", ]) 

b <- nrow(RESP_filter[RESP_filter$SWANG1 == "RHC", ])

c <- nrow(RESP_filter[RESP_filter$RESP == "Yes", ]) 

res_RESP_NoRHC <- (a/c) 

res_RESP_RHC <- (b/c) 




#Card/RHC
CARD_filter <- filter(epidf,CARD == "Yes")

a <- nrow(CARD_filter[CARD_filter$SWANG1 == "No RHC", ]) 

b <- nrow(CARD_filter[CARD_filter$SWANG1 == "RHC", ])

c <- nrow(CARD_filter[CARD_filter$CARD == "Yes", ]) 

res_CARD_NoRHC <- (a/c)

res_CARD_RHC <- (b/c)



#Neuro/RHC
NEURO_filter <- filter(epidf,NEURO == "Yes")

a <- nrow(NEURO_filter[NEURO_filter$SWANG1 == "No RHC", ]) 

b <- nrow(NEURO_filter[NEURO_filter$SWANG1 == "RHC", ])

c <- nrow(NEURO_filter[NEURO_filter$NEURO == "Yes", ]) 

res_NEURO_NoRHC <- (a/c)

res_NEURO_RHC<- (b/c)



#Gastr/RHC
GASTR_filter <- filter(epidf,GASTR == "Yes")

a <- nrow(GASTR_filter[GASTR_filter$SWANG1 == "No RHC", ]) 

b <- nrow(GASTR_filter[GASTR_filter$SWANG1 == "RHC", ])

c <- nrow(GASTR_filter[GASTR_filter$GASTR == "Yes", ]) 

res_GASTR_NoRHC <- (a/c)

res_GASTR_RHC <- (b/c)



#Renal/RHC
RENAL_filter <- filter(epidf,RENAL == "Yes")

a <- nrow(RENAL_filter[RENAL_filter$SWANG1 == "No RHC", ]) 

b <- nrow(RENAL_filter[RENAL_filter$SWANG1 == "RHC", ])

c <- nrow(RENAL_filter[RENAL_filter$RENAL == "Yes", ]) 

res_RENAL_NoRHC <- (a/c)

res_RENAL_RHC <- (b/c)


df1 <- data.frame(
  SWANG1 = rep(c("No RHC", "RHC"), each = 6),
  diagnostic = c("CA", "RESP", "CARD","NEURO","GASTR","RENAL"),
  proportion = c(res_CA_NoRHC,res_RESP_NoRHC,res_CARD_NoRHC,res_NEURO_NoRHC,res_GASTR_NoRHC,res_RENAL_NoRHC,res_CA_RHC,res_RESP_RHC,res_CARD_RHC,res_NEURO_RHC,res_GASTR_RHC,res_RENAL_RHC)
)


df2 <- df1 %>%
  group_by(diagnostic) %>%
  arrange(diagnostic, desc(SWANG1)) %>%
  mutate(lab_ypos = cumsum(proportion) - 0.5 * proportion) 
df2

p <- ggplot(df2, aes(x = diagnostic, y = proportion))+
  geom_col(aes(fill = SWANG1), width = 0.7)+
  geom_text(aes(y = lab_ypos, label = paste(round(proportion*100), "%"), group =proportion), color = "white")+
  scale_y_continuous(labels = percent)+
  labs(title="Proportion de patients ayant reçu le RHC en fonction des diagnostics",fill="Traitement")+
  theme(plot.title=element_text(hjust=0.5))
p
saver(p, 'RHC_DIAG', 7, 5)

prop <-c()
CA_filter <- filter(epidf,CA == "Yes")
CA_filter <- filter(CA_filter,SWANG1 == "No RHC")
prop<-append(prop,mean(CA_filter$SURV2MD1))
CA_filter <- filter(epidf,CA == "Yes")
CA_filter <- filter(CA_filter,SWANG1 == "RHC")
prop<-append(prop,mean(CA_filter$SURV2MD1))
RESP_filter <- filter(epidf,RESP == "Yes")
RESP_filter <- filter(RESP_filter,SWANG1 == "No RHC")
prop<-append(prop,mean(RESP_filter$SURV2MD1))
RESP_filter <- filter(epidf,RESP == "Yes")
RESP_filter <- filter(RESP_filter,SWANG1 == "RHC")
prop<-append(prop,mean(RESP_filter$SURV2MD1))
CARD_filter <- filter(epidf,CARD == "Yes")
CARD_filter <- filter(CARD_filter,SWANG1 == "No RHC")
prop<-append(prop,mean(CARD_filter$SURV2MD1))
CARD_filter <- filter(epidf,CARD == "Yes")
CARD_filter <- filter(CARD_filter,SWANG1 == "RHC")
prop<-append(prop,mean(CARD_filter$SURV2MD1))
NEURO_filter <- filter(epidf,NEURO == "Yes")
NEURO_filter <- filter(NEURO_filter,SWANG1 == "No RHC")
prop<-append(prop,mean(NEURO_filter$SURV2MD1))
NEURO_filter <- filter(epidf,NEURO == "Yes")
NEURO_filter <- filter(NEURO_filter,SWANG1 == "RHC")
prop<-append(prop,mean(NEURO_filter$SURV2MD1))
GASTR_filter <- filter(epidf,GASTR == "Yes")
GASTR_filter <- filter(GASTR_filter,SWANG1 == "No RHC")
prop<-append(prop,mean(GASTR_filter$SURV2MD1))
GASTR_filter <- filter(epidf,GASTR == "Yes")
GASTR_filter <- filter(GASTR_filter,SWANG1 == "RHC")
prop<-append(prop,mean(GASTR_filter$SURV2MD1))
RENAL_filter <- filter(epidf,RENAL == "Yes")
RENAL_filter <- filter(RENAL_filter,SWANG1 == "No RHC")
prop<-append(prop,mean(RENAL_filter$SURV2MD1))
RENAL_filter <- filter(epidf,RENAL == "Yes")
RENAL_filter <- filter(RENAL_filter,SWANG1 == "RHC")
prop<-append(prop,mean(RENAL_filter$SURV2MD1))
prop

#Create data frame
Surv_df <- data.frame( 
  
  SWANG1 = rep(c("No RHC", "RHC"), 6), 
  
  diagnostic = c(rep("CA",2),rep("RESP",2), rep("CARD",2),rep("NEURO",2),rep("GASTR",2),rep("RENAL",2)), 
  
  proportion = prop)


# Grouped
p<- ggplot(Surv_df, aes(fill=SWANG1, y=proportion, x=diagnostic)) + 
  geom_bar(position="dodge", stat="identity")+
  geom_text(aes(label = paste(round(proportion*100),"%")),position=position_dodge(0.9),color = "black")+
  labs(title="Moyenne des probabilités de survie à 2 mois selon le RHC pour chaque diagnostic",fill="Traitement")+
  theme(plot.title=element_text(hjust=0.5))+
  scale_y_continuous(labels = percent)
p
saver(p, 'RHC_Survie', 8, 5)

prop <-c()
Meta_filter <- filter(epidf,META == "Yes")
Meta_filter <- filter(Meta_filter,SWANG1 == "No RHC")
prop<-append(prop,mean(Meta_filter$SURV2MD1))
Meta_filter <- filter(epidf,META == "Yes")
Meta_filter <- filter(Meta_filter,SWANG1 == "RHC")
prop<-append(prop,mean(Meta_filter$SURV2MD1))
Hema_filter <- filter(epidf,HEMA == "Yes")
Hema_filter <- filter(Hema_filter,SWANG1 == "No RHC")
prop<-append(prop,mean(Hema_filter$SURV2MD1))
Hema_filter <- filter(epidf,HEMA == "Yes")
Hema_filter <- filter(Hema_filter,SWANG1 == "RHC")
prop<-append(prop,mean(Hema_filter$SURV2MD1))
Seps_filter <- filter(epidf,SEPS == "Yes")
Seps_filter <- filter(Seps_filter,SWANG1 == "No RHC")
prop<-append(prop,mean(Seps_filter$SURV2MD1))
Seps_filter <- filter(epidf,SEPS == "Yes")
Seps_filter <- filter(Seps_filter,SWANG1 == "RHC")
prop<-append(prop,mean(Seps_filter$SURV2MD1))
Trauma_filter <- filter(epidf,TRAUMA == "Yes")
Trauma_filter <- filter(Trauma_filter,SWANG1 == "No RHC")
prop<-append(prop,mean(Trauma_filter$SURV2MD1))
Trauma_filter <- filter(epidf,TRAUMA == "Yes")
Trauma_filter <- filter(Trauma_filter,SWANG1 == "RHC")
prop<-append(prop,mean(Trauma_filter$SURV2MD1))
Ortho_filter <- filter(epidf,ORTHO == "Yes")
Ortho_filter <- filter(Ortho_filter,SWANG1 == "No RHC")
prop<-append(prop,mean(Ortho_filter$SURV2MD1))
Ortho_filter <- filter(epidf,ORTHO == "Yes")
Ortho_filter <- filter(Ortho_filter,SWANG1 == "RHC")
prop<-append(prop,mean(Ortho_filter$SURV2MD1))
prop

#Create data frame
Surv_df <- data.frame( 
  
  SWANG1 = rep(c("No RHC", "RHC"), 5), 
  
  diagnostic = c(rep("META",2),rep("HEMA",2), rep("SEPS",2),rep("TRAUMA",2),rep("ORTHO",2)), 
  
  proportion = prop)


# Grouped
p<- ggplot(Surv_df, aes(fill=SWANG1, y=proportion, x=diagnostic)) + 
  geom_bar(position="dodge", stat="identity")+
  geom_text(aes(label = paste(round(proportion*100),"%")),position=position_dodge(0.9),color = "black")+
  labs(title="Moyenne des probabilités de survie à 2 mois selon le RHC pour chaque diagnostic",fill="Traitement")+
  theme(plot.title=element_text(hjust=0.5))+
  scale_y_continuous(labels = percent)
p
saver(p, 'RHC_Survie2', 8, 5)