setwd("C:/Users/.../Bureau/Travail/SAE/SAE_DataViz")
epi <- read.table("Epi_Clin.txt",header=T,na.strings=" ", sep="\t")

epidf <- as.data.frame.matrix(epi)

epidf <- epidf[,-52] #Supprime la colonne 52, variable ADLD3P car 90% sont des NA

#Retire les lignes où il y a des valeurs aberrantes

epidf <- subset(epidf,SCOMA1 <= 100)

epidf <- subset(epidf,DAS2D3PC <= 33)

epidf <- subset(epidf,APS1 <= 299)

library(scales)
library(dplyr)
library(ggplot2)

#Meta
Meta_filter <- filter(epidf, META == "Yes") # FIltrer la base de donnée epidf, ici on veut ceux qui sont positifs au test Meta
without <- nrow(Meta_filter[Meta_filter$SWANG1 == "No RHC", ]) # Ici on compte le nombre de lignes avec mention "No RHC"
with <- nrow(Meta_filter[Meta_filter$SWANG1 == "RHC", ])# Ici on compte les lignes avec "RHC"
popu <- nrow(Meta_filter[Meta_filter$META == "Yes", ]) # Ici, on compte le nombre de personnes total (la population)
res_Meta_NoRHC <- (without/popu)
res_Meta_RHC <- (with/popu) #Calcule en proportion le nombre de gens qui on fait le RHC parmi les gens diagnostiqué avec Met


#Hema
Hema_filter <- filter(epidf, HEMA == "Yes")
without <- nrow(Hema_filter[Hema_filter$SWANG1 == "No RHC", ])
with <- nrow(Hema_filter[Hema_filter$SWANG1 == "RHC", ])
popu <- nrow(Hema_filter[Hema_filter$HEMA == "Yes", ])
res_Hema_NoRHC <- (without/popu)
res_Hema_RHC <- (with/popu)

#SEPS
Seps_filter <- filter(epidf, SEPS == "Yes")
without <- nrow(Seps_filter[Seps_filter$SWANG1 == "No RHC", ])
with <- nrow(Seps_filter[Seps_filter$SWANG1 == "RHC", ])
popu <- nrow(Seps_filter[Seps_filter$SEPS == "Yes", ])
res_Seps_NoRHC <- (without/popu)
res_Seps_RHC <- (with/popu)

#TRAUMA
Trauma_filter <- filter(epidf, TRAUMA == "Yes")
without <- nrow(Trauma_filter[Trauma_filter$SWANG1 == "No RHC", ])
with <- nrow(Trauma_filter[Trauma_filter$SWANG1 == "RHC", ])
popu <- nrow(Trauma_filter[Trauma_filter$TRAUMA == "Yes", ])
res_Trauma_NoRHC <- (without/popu)
res_Trauma_RHC <- (with/popu)


#ORTHO
Ortho_filter <- filter(epidf, ORTHO == "Yes")
without <- nrow(Ortho_filter[Ortho_filter$SWANG1 == "No RHC", ])
with <- nrow(Ortho_filter[Ortho_filter$SWANG1 == "RHC", ])
popu <- nrow(Ortho_filter[Ortho_filter$ORTHO == "Yes", ])
res_Ortho_NoRHC <- (without/popu)
res_Ortho_RHC <- (with/popu)

df1 <- data.frame(
    
    SWANG1 = rep(c("No RHC", "RHC"), each = 5),
    
    diagnostic = c("META", "HEMA", "SEPS","TRAUMA","ORTHO"),
    
    proportion = c(res_Meta_NoRHC,res_Hema_NoRHC,res_Seps_NoRHC,res_Trauma_NoRHC,res_Ortho_NoRHC,res_Meta_RHC,res_Hema_RHC,res_Seps_RHC,res_Trauma_RHC,res_Ortho_RHC)
    
)



#Pour mettre les étiquettes de données  

df2 <- df1 %>%
 
    group_by(diagnostic) %>%
    
    arrange(diagnostic, desc(SWANG1)) %>%
    
    mutate(lab_ypos = cumsum(proportion) - 0.5 * proportion)  

#df2

p <- ggplot(df2, aes(x = diagnostic, y = proportion))+
 
    geom_col(aes(fill = SWANG1), width = 0.7)+
    
    geom_text(aes(y = lab_ypos, label = paste(round(proportion*100,1), "%"), group =proportion), color = "white")+
    
    labs(title = "Proportion de patients ayant reçu le RHC en fonction des diagnostics", fill = "Traitement")+
    
    theme(plot.title=element_text(hjust=0.5)) + scale_y_continuous(labels = percent)

p
saver(p, 'Prop_RHC_diag', 7,5)

#Proportion de patient en fonction du diagnostique
diagnostics <- 0
diag <- c("CA", "RESP", "CARD","NEURO","GASTR","RENAL","META", "HEMA", "SEPS","TRAUMA","ORTHO")
abscisse <- c()
ordonnees <- c()
for (i in diag) {
    diag_yes <- (nrow(epidf[epidf[,i] == "Yes",])/nrow(epidf))
    abscisse <- append(abscisse, i)
    ordonnees <- append(ordonnees, diag_yes)
}

diagnostics <- data.frame(abscisse, ordonnees)
sum(ordonnees)
p <- ggplot(diagnostics, aes(x = abscisse, ordonnees)) + geom_bar(stat = "identity", fill = "steelblue") +
    geom_text(aes(label = paste(round(ordonnees*100,1), "%")), color = "black", vjust = -0.1) + labs(title = "Proportion des hospitalisés") +
    theme(plot.title = element_text(hjust = 0.5)) + scale_y_continuous(labels = percent)
p
saver(p, 'Prop_patient_diag', 7,5)

#Proportion de patient qui ont fait un RHC
result_RHC <- c((nrow(epidf[epidf$SWANG1 == "RHC",])/nrow(epidf)), (nrow(epidf[epidf$SWANG1 == "No RHC",])/nrow(epidf)))
rsp_RHC <- c("Yes", "No")
survey <- data.frame(rsp_RHC, result_RHC)
p <-  ggplot(survey, aes(x = rsp_RHC, y = result_RHC, fill = rsp_RHC)) + geom_bar(stat = "identity") +
    geom_text(aes(label = paste(round(result_RHC*100,1), "%")), color = "black", vjust = -0.1) + labs(title = "Proportion des hospitalisés selon s'ils ont pris le traitement RHC ou non.", fill = "Traitement") +
    theme(plot.title = element_text(hjust = 0.5)) + scale_y_continuous(labels = percent)
    p
saver(p, 'Prop_patient_RHC', 7,5)

#death_date

dead_people <- filter(epidf, y_DTHDTE != "NA")
concatenate_hospital_exit <- paste(dead_people$y_DSCHDTE, "-",dead_people$m_DSCHDTE, "-",dead_people$d_DSCHDTE)
exit_date <- c(gsub(" ", "", concatenate_hospital_exit))
concatenate_dead_date <- paste(dead_people$y_DTHDTE, "-",dead_people$m_DTHDTE, "-",dead_people$d_DTHDTE)
dead_date <- c(gsub(" ", "", concatenate_dead_date))
dead_df <- data.frame(dead_people$SWANG1, exit_date, dead_date)
No_RHC <- filter(dead_df, dead_people.SWANG1 == "No RHC")
months_No_RHC <- c(as.Date(No_RHC$dead_date) - as.Date(No_RHC$exit_date))
RHC <- filter(dead_df, dead_people.SWANG1 == "RHC")
months_RHC <- c(as.Date(RHC$dead_date) - as.Date(RHC$exit_date))

total_No_RHC <- nrow(No_RHC)
total_RHC <- nrow(RHC)

survival_duration <- c(length(months_No_RHC[months_No_RHC > 60])/total_No_RHC, length(months_No_RHC[months_No_RHC < 60])/total_No_RHC, length(months_RHC[months_RHC > 60])/total_RHC, length(months_RHC[months_RHC < 60])/total_RHC)

df_survival <- data.frame(rhc = c(rep("RHC",2),rep("No RHC",2)), survival_duration, condi = rep(c("Plus de 2 mois", "Moins de 2 mois"),2))

p <- ggplot(df_survival, aes(fill = condi, x=rhc, y=survival_duration)) +
    geom_bar(position="dodge", stat = "identity") + geom_text(aes(label = paste(round(survival_duration*100,1), "%")), position = position_dodge(0.9), color = "black", vjust = -0.1) + labs(title = "Proportion de gens morts en moins/plus de 2 mois.", fill = "Durée de vie", ylab = "proportion") +
    theme(plot.title = element_text(hjust = 0.5)) + scale_y_continuous(labels = percent)
p
saver(p, 'Prop_mort-vivant_apres_2mois', 8,5)

saver <- function(to_save, name, w, h){
    ggsave(filename = paste(name, '.png', sep=''), device='png', width = w, height = h, plot = to_save, path='C:/Users/chen patrick/OneDrive/Bureau/Travail/SAE/SAE_DataViz')
}

#code chi2 du RHC et de la mort
event <- c('SWANG1', 'DEATH')
for (i in event){
  print(i)
  print(chisq.test(table(epidf[,i], epidf$DEATH),
simulate.p.value = TRUE))
}