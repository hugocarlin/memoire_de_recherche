######################################################
###################   CHARGEMENT   ###################
######################################################

# effacer les objets de l'environnement
rm(list = ls())

# bibliotheques
library(openxlsx)
library("RColorBrewer")
library(stringr)
library(ggplot2)
library(weights)
library(matrixStats)
library(reshape2)
library(dplyr)

# chargement emplacement
setwd("/Users/hugocarlin/Documents/Université/Master SEP/Mémoire/Base de données")

# chargement xlsx
database <- read.xlsx('database.xlsx',startRow = 2)





################################################################
##################   PREPARATION DES DONNEES  ##################
################################################################

# suppression variables inutiles
database <- database[,-c(1,4)]

# renommer les variables
noms_var <- c("Sexe","Age","CSP","Freq_avant","Facon_avant",
              "Modif_tps_trav_1","Cont_paris_1","Freq_1","Autre_1","Facon_1",
              "Modif_tps_trav_2","Freq_2","Facon_2")
colnames(database) <- noms_var

# Age : enlever "ans"
database$Age <- strtoi(substr(database$Age,1,2))

# CSP : raccourcir les str
#database$CSP <- str_replace_all(database$CSP, pattern = "Artisan, commerçant, chef d'entreprise", replacement = "1")
#database$CSP <- str_replace_all(database$CSP, pattern = "Cadre et profession intellectuelle supérieure", replacement = "2")
#database$CSP <- str_replace_all(database$CSP, pattern = "Profession intermédiaire", replacement = "3")
#database$CSP <- str_replace_all(database$CSP, pattern = "Employé", replacement = "4")
#database$CSP <- str_replace_all(database$CSP, pattern = "Ouvrier", replacement = "5")
#database$CSP <- str_replace_all(database$CSP, pattern = "Sans activité professionnelle, chômage", replacement = "6")
#database$CSP <- str_replace_all(database$CSP, pattern = "Etudiant", replacement = "6")

database$CSP <- str_replace_all(database$CSP, pattern = "Agriculteur", replacement = "1")
database$CSP <- str_replace_all(database$CSP, pattern = "Artisan, commerçant, chef d'entreprise", replacement = "1")
database$CSP <- str_replace_all(database$CSP, pattern = "Cadre et profession intellectuelle supérieure", replacement = "2")
database$CSP <- str_replace_all(database$CSP, pattern = "Profession intermédiaire", replacement = "1")
database$CSP <- str_replace_all(database$CSP, pattern = "Employé", replacement = "3")
database$CSP <- str_replace_all(database$CSP, pattern = "Ouvrier", replacement = "1")
database$CSP <- str_replace_all(database$CSP, pattern = "Sans activité professionnelle, chômage", replacement = "4")
database$CSP <- str_replace_all(database$CSP, pattern = "Etudiant", replacement = "4")
database$CSP <- str_replace_all(database$CSP, pattern = "Retraité", replacement = "4")


# Avez vous continué à parié ?
database$Cont_paris_1 <- str_replace_all(database$Cont_paris_1, pattern = "Non je n'ai plus parié", replacement = "Non")
database$Cont_paris_1 <- str_replace_all(database$Cont_paris_1, pattern = "Oui j'ai parié sur le championnat biélorusse de football", replacement = "Oui")

# Poker
database$poker <- database$Autre_1
database$poker <- str_replace_all(database$poker, pattern = "Casino", replacement = "Non")
database$poker <- str_replace_all(database$poker, pattern = "Oui mais j'y jouais déjà", replacement = "Oui")
database$poker <- str_replace_all(database$poker, pattern = "Poker", replacement = "Oui")
database$poker <- str_replace_all(database$poker, pattern = "Poker/Casino", replacement = "Oui")
database$poker <- str_replace_all(database$poker, pattern = "Oui/Non", replacement = "Oui")






##############################################################
##################   PRESENTATION COLLECTE   ##################
##############################################################

head(database)
summary(database)


# Sexe
table_sexe <- table(database$Sexe)
prop_sexe <- prop.table(table_sexe)*100
pie(table_sexe, labels = paste(names(table_sexe), "\n",round(prop_sexe,1),"%", sep = ""),
    main="Répartition des sexes", col = c("skyblue2","skyblue4"))


# Age
summary(database$Age)
hist(database$Age, col = c("skyblue4"),
     main = paste("Histogramme de l'âge des individus"),
     ylab = "Effectifs", xlab = "Âge")
moins35 <- which(database[,2]<36)
plus35 <- which(database[,2]>35)
database[moins35,14] <- "Moins de 35 ans"
database[plus35,14] <- "Plus de 35 ans"
names(database)[14]<-"Tranche_age"
table_age <- table(database$Tranche_age)
table_age
prop_age <- prop.table(table_age)*100
prop_age


# CSP
table_csp <- table(database$CSP)
table_csp
prop_csp <- prop.table(table_csp) 
prop_csp*100
csp <- data.frame(csp_names = c("Employés","Cadres\nProfessions\nsupérieures","Inactifs\nautres","Artisans\nComerçants\nChefs\nd'entreprises","Ouvriers","Professions\nintermediaires","Agriculteurs\nexploitants","Inactifs\nretraités"),
                  csp_val = c(30.5,14.6,42.7,4.9,4.9,2.4,0,0))
ggplot(data=csp, aes(x=csp_names, y=csp_val)) +
  geom_bar(stat="identity", fill="skyblue4")+
  geom_text(aes(label=paste0(csp_val,"%")), vjust=-0.3, size=3.5)+
  theme_minimal() +
  xlab("\nCatégories socioprofessionnelles") +
  ylab("Proportion\n") +
  theme(axis.text.x = element_text(size=11),
        axis.title.x = element_text(size = 14),
        axis.title.y = element_text(size = 14))


# CSP x tranche d'age
table_csp_age <- table(database$CSP,database$Tranche_age)
table_csp_age


# Redressement
library(icarus)

database$poids_init <- 1
mar1 <- c("Sexe",2,7,75,0,0)
mar2 <- c("Tranche_age",2,59,23,0,0)
mar3 <- c("CSP",4,16,26,23,17)
marges <- rbind(mar1, mar2, mar3)

summary(database[,c(1,3,14,15)])
database$Sexe <- as.factor(database$Sexe)
database$CSP <- as.factor(database$CSP)
database$Tranche_age <- as.factor(database$Tranche_age)

costs <- c(1,1,1)
calage <- calibration(data=database, marginMatrix=marges, colWeights="poids_init"
                      , costs=costs, gap=10, description=TRUE, popTotal=82)

database <-cbind(database,poids_final = calage)











###############################################
##################   AVANT   ##################
###############################################

# Frequence avant confinements
table_freq_avant <- wtd.table(database$Freq_avant,weights = database$poids_final)
table_freq_avant
prop.table(table_freq_avant$sum.of.weights)*100
freq_a <- data.frame(freq_a_val = c(0,1,2,3,4,5,6,7,8,9,10),
                     freq_a_freq = c(1.87,3.18,4.13,7.49,6.87,21.84,6.83,17.48,19.74,10.56,0))
ggplot(data=freq_a, aes(x=freq_a_val, y=freq_a_freq)) +
  geom_bar(stat="identity", fill="skyblue4") +
  geom_text(aes(label=paste0(freq_a_freq,"%")), vjust=-0.3, size=3.5) +
  theme_minimal() +
  xlab("\nFréquence") +
  ylab("Proportion\n") + 
  scale_x_continuous(breaks=seq(0,10,by = 1)) +
  theme(axis.text.x = element_text(size=11),
        axis.title.x = element_text(size = 14),
        axis.title.y = element_text(size = 14))


weighted.mean(database$Freq_avant,database$poids_final)
weightedMedian(database$Freq_avant,database$poids_final)

weighted.mean(database$Freq_avant[database$Freq_avant!=0],database$poids_final[database$Freq_avant!=0])
weightedMedian(database$Freq_avant[database$Freq_avant!=0],database$poids_final[database$Freq_avant!=0])

weighted.mean(database$Freq_avant[database$Freq_1!=0],database$poids_final[database$Freq_1!=0])



# Frequence avant x age
wtd.cor(database$Freq_avant,database$Age,weight=database$poids_final)


# Frequence avant x csp
fit <- aov(Freq_avant ~ CSP, data = database, weight = database$poids_final)
summary(fit)


# Type de joueur avant confinements
table_prof_avant <- wtd.table(database$Facon_avant,weights = database$poids_final)
table_prof_avant
prop.table(table_prof_avant$sum.of.weights)*100
prof_a <- data.frame(prof_a_val = c(1,2,3,4,5,6,7,8,9,10),
                     prof_a_freq = c(0.94,0.44,4.64,0,16.88,14.21,13.54,30.20,12.99,6.15))

weighted.mean(database$Facon_avant,database$poids_final,na.rm = TRUE)
weightedMedian(database$Facon_avant,database$poids_final, na.rm = TRUE)

ggplot(data=prof_a, aes(x=prof_a_val, y=prof_a_freq)) +
  geom_bar(stat="identity", fill="skyblue4") +
  geom_text(aes(label=paste0(prof_a_freq,"%")), vjust=-0.3, size=3.5) +
  theme_minimal() +
  xlab("\nAnalyse du parieur") +
  ylab("Proportion\n") + 
  scale_x_continuous(breaks=seq(0,10,by = 1)) +
  theme(axis.text.x = element_text(size=11),
        axis.title.x = element_text(size = 14),
        axis.title.y = element_text(size = 14))


# Type de joueur x age
wtd.cor(database$Facon_avant,database$Age,weight=database$poids_final)


# Type de joueur x csp
fit <- aov(Facon_avant ~ CSP, data = database, weight = database$poids_final)
summary(fit)


# Frequence x profil
wtd.cor(database$Facon_avant,database$Freq_avant,weight=database$poids_final)
plot(database$Facon_avant,database$Freq_avant)




#############################################################
##################   PREMIER CONFINEMENT   ##################
#############################################################


table_cont1 <- wtd.table(database$Cont_paris_1,weights = database$poids_final)
table_cont1
prop.table(table_cont1$sum.of.weights)*100


# Continué à parier x frequence avant
fit <- aov(Freq_avant ~ Cont_paris_1, data=database, weight=database$poids_final)
summary(fit)
boxplot(Freq_avant ~ Cont_paris_1, data=database, weight=database$poids_final,
        xlab = "Avez vous parié sur le championnat biélorusse ?",
        ylab = "Fréquence en temps normal")

weighted.mean(database$Freq_avant,database$poids_final)
weightedMedian(database$Freq_avant,database$poids_final)





fit <- aov(Freq_avant ~ Cont_paris_1, data=database,weight=database$poids_final)
summary(fit)
boxplot(Freq_avant ~ Cont_paris_1, data=database,weight=database$poids_final,
        xlab = "Avez vous parié sur le championnat biélorusse ?",
        ylab = "Fréquence en temps normal")


# Continué à parier x analyse avant
fit <- aov(Facon_avant ~ Cont_paris_1, data=database,weight=database$poids_final)
summary(fit)
boxplot(Facon_avant ~ Cont_paris_1, data=database,weight=database$poids_final,
        xlab = "Avez vous parié sur le championnat biélorusse ?",
        ylab = "Analyse du parieur en temps normal")



# Frequence 1 et analyse 1
table_freq_1 <- wtd.table(database$Freq_1,weights = database$poids_final)
table_freq_1
prop.table(table_freq_1$sum.of.weights)*100
freq_1 <- data.frame(freq_1_val = c(0,1,2,3,4,5,6,7,8,9,10),
                     freq_1_freq = c(53.37,0,0.43,1.78,1.94,14.89,5.48,4.57,9.77,4.08,3.70))

weighted.mean(database$Freq_1[database$Freq_1!=0],database$poids_final[database$Freq_1!=0])
weightedMedian(database$Freq_1[database$Freq_1!=0],database$poids_final[database$Freq_1!=0])

weighted.mean(database$Freq_1,database$poids_final,na.rm = TRUE)
weightedMedian(database$Freq_1,database$poids_final,na.rm = TRUE)

weighted.mean(database$Freq_avant[database$Freq_1!=0],database$poids_final[database$Freq_1!=0])

ggplot(data=freq_1, aes(x=freq_1_val, y=freq_1_freq)) +
  geom_bar(stat="identity", fill="skyblue4") +
  geom_text(aes(label=paste0(freq_1_freq,"%")), vjust=-0.3, size=3.5) +
  theme_minimal() +
  xlab("\nFréquence") +
  ylab("Proportion\n") + 
  scale_x_continuous(breaks=seq(0,10,by = 1)) +
  theme(axis.text.x = element_text(size=11),
        axis.title.x = element_text(size = 14),
        axis.title.y = element_text(size = 14))


table_facon_1 <- wtd.table(database$Facon_1,weights = database$poids_final)
table_facon_1
prop.table(table_facon_1$sum.of.weights)*100
facon_1 <- data.frame(facon_1_val = c(1,2,3,4,5,6,7,8,9,10),
                     facon_1_freq = c(0.92,0,4.83,0.92,16.83,23.30,6.78,15.66,13.71,17.04))
weighted.mean(database$Facon_1,database$poids_final,na.rm = TRUE)
weightedMedian(database$Facon_1,database$poids_final,na.rm = TRUE)
ggplot(data=facon_1, aes(x=facon_1_val, y=facon_1_freq)) +
  geom_bar(stat="identity", fill="skyblue4") +
  geom_text(aes(label=paste0(facon_1_freq,"%")), vjust=-0.3, size=3.5) +
  theme_minimal() +
  xlab("\nAnalyse du parieur") +
  ylab("Proportion\n") + 
  scale_x_continuous(breaks=seq(0,10,by = 1)) +
  theme(axis.text.x = element_text(size=11),
        axis.title.x = element_text(size = 14),
        axis.title.y = element_text(size = 14))


# Frequence 1 x analyse 1
wtd.cor(database$Facon_1,database$Freq_1,weight=database$poids_final)

attach(database)
a <- lm(Facon_avant[Cont_paris_1=="Oui"] ~ Freq_avant[Cont_paris_1=="Oui"],weights = poids_final[Cont_paris_1=="Oui"])
summary(a)
plot(a)

b <- lm(Facon_1[Cont_paris_1=="Oui"] ~ Freq_1[Cont_paris_1=="Oui"],weights = poids_final[Cont_paris_1=="Oui"])
summary(b)
plot(b)

plot(Facon_avant[Cont_paris_1=="Oui"], Freq_avant[Cont_paris_1=="Oui"])



# Frequence avant x poker
table_poker <- wtd.table(database$Autre_1,weights = database$poids_final)
table_poker
prop.table(table_poker$sum.of.weights)*100
fit <- aov(Freq_avant ~ poker, data=database,weight=database$poids_final)
summary(fit)
boxplot(Freq_avant ~ poker, data=database,weight=database$poids_final,
        xlab = "Avez vous pratiqué le poker pendant le premier confinement ?",
        ylab = "Fréquence de jeu en temps normal")





#############################################################
##################   SECOND CONFINEMENT   ###################
#############################################################

# Temps de travail
table_travail <- wtd.table(database$Modif_tps_trav_2,weights = database$poids_final)
table_travail
prop.table(table_travail$sum.of.weights)*100

# Frequence 2 et analyse 2
table_freq_2 <- wtd.table(database$Freq_2[database$Freq_2!=0],weights = database$poids_final[database$Freq_2!=0]) 
table_freq_2
prop.table(table_freq_2$sum.of.weights)*100
freq_2 <- data.frame(freq_2_val = c(0,1,2,3,4,5,6,7,8,9,10),
                     freq_2_freq = c(0.92,2.25,0.43,1.35,1.82,5.48,16.21,20.88,28.11,12.43,10.11))

weighted.mean(database$Freq_2,database$poids_final)
weightedMedian(database$Freq_2,database$poids_final)

weighted.mean(database$Freq_2[database$Freq_2!=0],database$poids_final[database$Freq_2!=0])
weightedMedian(database$Freq_2[database$Freq_2!=0],database$poids_final[database$Freq_2!=0])

ggplot(data=freq_2, aes(x=freq_2_val, y=freq_2_freq)) +
  geom_bar(stat="identity", fill="skyblue4") +
  geom_text(aes(label=paste0(freq_2_freq,"%")), vjust=-0.3, size=3.5) +
  theme_minimal() +
  xlab("\nFréquence") +
  ylab("Proportion\n") + 
  scale_x_continuous(breaks=seq(0,10,by = 1)) +
  theme(axis.text.x = element_text(size=11),
        axis.title.x = element_text(size = 14),
        axis.title.y = element_text(size = 14))


table_facon_2 <- wtd.table(database$Facon_2,weights = database$poids_final)
table_facon_2
prop.table(table_facon_2$sum.of.weights)*100
facon_2 <- data.frame(facon_2_val = c(1,2,3,4,5,6,7,8,9,10),
                      facon_2_freq = c(2.27,0,0,0.43,5.43,13.40,13.19,31.83,15.73,17.71))
weighted.mean(database$Facon_2,database$poids_final,na.rm = TRUE)
weightedMedian(database$Facon_2,database$poids_final,na.rm = TRUE)
ggplot(data=facon_2, aes(x=facon_2_val, y=facon_2_freq)) +
  geom_bar(stat="identity", fill="skyblue4") +
  geom_text(aes(label=paste0(facon_2_freq,"%")), vjust=-0.3, size=3.5) +
  theme_minimal() +
  xlab("\nAnalyse du parieur") +
  ylab("Proportion\n") + 
  scale_x_continuous(breaks=seq(0,10,by = 1)) +
  theme(axis.text.x = element_text(size=11),
        axis.title.x = element_text(size = 14),
        axis.title.y = element_text(size = 14))


# Frequence  x temps de travail 2
weighted.mean(database$Freq_1[database$Modif_tps_trav_2=="Ça n'a rien changé"],database$poids_final[database$Modif_tps_trav_2=="Ça n'a rien changé"])
weighted.mean(database$Freq_1[database$Modif_tps_trav_2=="J'ai plus travaillé qu'à la normale"],database$poids_final[database$Modif_tps_trav_2=="J'ai plus travaillé qu'à la normale"])
weighted.mean(database$Freq_1[database$Modif_tps_trav_2=="J'ai eu plus de temps libre"],database$poids_final[database$Modif_tps_trav_2=="J'ai eu plus de temps libre"])

weightedMedian(database$Freq_1[database$Modif_tps_trav_2=="Ça n'a rien changé"],database$poids_final[database$Modif_tps_trav_2=="Ça n'a rien changé"])
weightedMedian(database$Freq_1[database$Modif_tps_trav_2=="J'ai plus travaillé qu'à la normale"],database$poids_final[database$Modif_tps_trav_2=="J'ai plus travaillé qu'à la normale"])
weightedMedian(database$Freq_1[database$Modif_tps_trav_2=="J'ai eu plus de temps libre"],database$poids_final[database$Modif_tps_trav_2=="J'ai eu plus de temps libre"])

weighted.mean(database$Freq_2[database$Modif_tps_trav_2=="Ça n'a rien changé"],database$poids_final[database$Modif_tps_trav_2=="Ça n'a rien changé"])
weighted.mean(database$Freq_2[database$Modif_tps_trav_2=="J'ai plus travaillé qu'à la normale"],database$poids_final[database$Modif_tps_trav_2=="J'ai plus travaillé qu'à la normale"])
weighted.mean(database$Freq_2[database$Modif_tps_trav_2=="J'ai eu plus de temps libre"],database$poids_final[database$Modif_tps_trav_2=="J'ai eu plus de temps libre"])

weightedMedian(database$Freq_2[database$Modif_tps_trav_2=="Ça n'a rien changé"],database$poids_final[database$Modif_tps_trav_2=="Ça n'a rien changé"])
weightedMedian(database$Freq_2[database$Modif_tps_trav_2=="J'ai plus travaillé qu'à la normale"],database$poids_final[database$Modif_tps_trav_2=="J'ai plus travaillé qu'à la normale"])
weightedMedian(database$Freq_2[database$Modif_tps_trav_2=="J'ai eu plus de temps libre"],database$poids_final[database$Modif_tps_trav_2=="J'ai eu plus de temps libre"])

weighted.mean(database$Freq_avant[database$Modif_tps_trav_2=="Ça n'a rien changé"],database$poids_final[database$Modif_tps_trav_2=="Ça n'a rien changé"])
weighted.mean(database$Freq_avant[database$Modif_tps_trav_2=="J'ai plus travaillé qu'à la normale"],database$poids_final[database$Modif_tps_trav_2=="J'ai plus travaillé qu'à la normale"])
weighted.mean(database$Freq_avant[database$Modif_tps_trav_2=="J'ai eu plus de temps libre"],database$poids_final[database$Modif_tps_trav_2=="J'ai eu plus de temps libre"])

weightedMedian(database$Freq_avant[database$Modif_tps_trav_2=="Ça n'a rien changé"],database$poids_final[database$Modif_tps_trav_2=="Ça n'a rien changé"])
weightedMedian(database$Freq_avant[database$Modif_tps_trav_2=="J'ai plus travaillé qu'à la normale"],database$poids_final[database$Modif_tps_trav_2=="J'ai plus travaillé qu'à la normale"])
weightedMedian(database$Freq_avant[database$Modif_tps_trav_2=="J'ai eu plus de temps libre"],database$poids_final[database$Modif_tps_trav_2=="J'ai eu plus de temps libre"])




fit <- aov(Freq_2 ~ Modif_tps_trav_2, data=database,weight=database$poids_final)
summary(fit)


# Facon 2 x temps de travail
weighted.mean(database$Facon_1[database$Modif_tps_trav_2=="Ça n'a rien changé"],database$poids_final[database$Modif_tps_trav_2=="Ça n'a rien changé"], na.rm = TRUE)
weighted.mean(database$Facon_1[database$Modif_tps_trav_2=="J'ai plus travaillé qu'à la normale"],database$poids_final[database$Modif_tps_trav_2=="J'ai plus travaillé qu'à la normale"],na.rm = TRUE)
weighted.mean(database$Facon_1[database$Modif_tps_trav_2=="J'ai eu plus de temps libre"],database$poids_final[database$Modif_tps_trav_2=="J'ai eu plus de temps libre"],na.rm = TRUE)

weightedMedian(database$Facon_1[database$Modif_tps_trav_2=="Ça n'a rien changé"],database$poids_final[database$Modif_tps_trav_2=="Ça n'a rien changé"], na.rm = TRUE)
weightedMedian(database$Facon_1[database$Modif_tps_trav_2=="J'ai plus travaillé qu'à la normale"],database$poids_final[database$Modif_tps_trav_2=="J'ai plus travaillé qu'à la normale"],na.rm = TRUE)
weightedMedian(database$Facon_1[database$Modif_tps_trav_2=="J'ai eu plus de temps libre"],database$poids_final[database$Modif_tps_trav_2=="J'ai eu plus de temps libre"],na.rm = TRUE)

weighted.mean(database$Facon_2[database$Modif_tps_trav_2=="Ça n'a rien changé"],database$poids_final[database$Modif_tps_trav_2=="Ça n'a rien changé"], na.rm = TRUE)
weighted.mean(database$Facon_2[database$Modif_tps_trav_2=="J'ai plus travaillé qu'à la normale"],database$poids_final[database$Modif_tps_trav_2=="J'ai plus travaillé qu'à la normale"],na.rm = TRUE)
weighted.mean(database$Facon_2[database$Modif_tps_trav_2=="J'ai eu plus de temps libre"],database$poids_final[database$Modif_tps_trav_2=="J'ai eu plus de temps libre"],na.rm = TRUE)

weightedMedian(database$Facon_2[database$Modif_tps_trav_2=="Ça n'a rien changé"],database$poids_final[database$Modif_tps_trav_2=="Ça n'a rien changé"], na.rm = TRUE)
weightedMedian(database$Facon_2[database$Modif_tps_trav_2=="J'ai plus travaillé qu'à la normale"],database$poids_final[database$Modif_tps_trav_2=="J'ai plus travaillé qu'à la normale"],na.rm = TRUE)
weightedMedian(database$Facon_2[database$Modif_tps_trav_2=="J'ai eu plus de temps libre"],database$poids_final[database$Modif_tps_trav_2=="J'ai eu plus de temps libre"],na.rm = TRUE)

weighted.mean(database$Facon_avant[database$Modif_tps_trav_2=="Ça n'a rien changé"],database$poids_final[database$Modif_tps_trav_2=="Ça n'a rien changé"], na.rm = TRUE)
weighted.mean(database$Facon_avant[database$Modif_tps_trav_2=="J'ai plus travaillé qu'à la normale"],database$poids_final[database$Modif_tps_trav_2=="J'ai plus travaillé qu'à la normale"],na.rm = TRUE)
weighted.mean(database$Facon_avant[database$Modif_tps_trav_2=="J'ai eu plus de temps libre"],database$poids_final[database$Modif_tps_trav_2=="J'ai eu plus de temps libre"],na.rm = TRUE)

weightedMedian(database$Facon_avant[database$Modif_tps_trav_2=="Ça n'a rien changé"],database$poids_final[database$Modif_tps_trav_2=="Ça n'a rien changé"], na.rm = TRUE)
weightedMedian(database$Facon_avant[database$Modif_tps_trav_2=="J'ai plus travaillé qu'à la normale"],database$poids_final[database$Modif_tps_trav_2=="J'ai plus travaillé qu'à la normale"],na.rm = TRUE)
weightedMedian(database$Facon_avant[database$Modif_tps_trav_2=="J'ai eu plus de temps libre"],database$poids_final[database$Modif_tps_trav_2=="J'ai eu plus de temps libre"],na.rm = TRUE)


fit <- aov(Facon_2 ~ Modif_tps_trav_2, data=database,weight=database$poids_final)
summary(fit)














## Fréquence population globale
# Avant
mean_freq_avant <- weighted.mean(database$Freq_avant,database$poids_final)
mean_freq_avant
weightedMedian(database$Freq_avant,database$poids_final)
# Confinement 1
weighted.mean(database$Freq_1,database$poids_final)
weightedMedian(database$Freq_1,database$poids_final)
# Confinement 2
weighted.mean(database$Freq_2,database$poids_final)
weightedMedian(database$Freq_2,database$poids_final)

## Analyse population globale
# Avant
weighted.mean(database$Facon_avant,database$poids_final,na.rm = TRUE)
weightedMedian(database$Facon_avant,database$poids_final,na.rm = TRUE)
# Confinement 1
weighted.mean(database$Facon_1,database$poids_final,na.rm = TRUE)
weightedMedian(database$Facon_1,database$poids_final,na.rm = TRUE)
# Confinement 2
weighted.mean(database$Facon_2,database$poids_final,na.rm = TRUE)
weightedMedian(database$Facon_2,database$poids_final,na.rm = TRUE)



## Fréquence population qui parie par période
# Avant
weighted.mean(database$Freq_avant[database$Freq_avant!=0],database$poids_final[database$Freq_avant!=0])
weightedMedian(database$Freq_avant[database$Freq_avant!=0],database$poids_final[database$Freq_avant!=0])
# Confinement 1
weighted.mean(database$Freq_1[database$Freq_1!=0],database$poids_final[database$Freq_1!=0])
weightedMedian(database$Freq_1[database$Freq_1!=0],database$poids_final[database$Freq_1!=0])
# Confinement 2
weighted.mean(database$Freq_2[database$Freq_2!=0],database$poids_final[database$Freq_2!=0])
weightedMedian(database$Freq_2[database$Freq_2!=0],database$poids_final[database$Freq_2!=0])



## Fréquence population qui a continé a parier au confinement 1
# Avant
weighted.mean(database$Freq_avant[database$Cont_paris_1=='Oui'],database$poids_final[database$Cont_paris_1=='Oui'])
weightedMedian(database$Freq_avant[database$Cont_paris_1=='Oui'],database$poids_final[database$Cont_paris_1=='Oui'])
# Confinement 1
weighted.mean(database$Freq_1[database$Cont_paris_1=='Oui'],database$poids_final[database$Cont_paris_1=='Oui'])
weightedMedian(database$Freq_1[database$Cont_paris_1=='Oui'],database$poids_final[database$Cont_paris_1=='Oui'])
# Confinement 2
weighted.mean(database$Freq_2[database$Cont_paris_1=='Oui'],database$poids_final[database$Cont_paris_1=='Oui'])
weightedMedian(database$Freq_2[database$Cont_paris_1=='Oui'],database$poids_final[database$Cont_paris_1=='Oui'])



## Analyse population qui a continé a parier au confinement 1
# Avant
weighted.mean(database$Facon_avant[database$Cont_paris_1=='Oui'],database$poids_final[database$Cont_paris_1=='Oui'],na.rm = TRUE)
weightedMedian(database$Facon_avant[database$Cont_paris_1=='Oui'],database$poids_final[database$Cont_paris_1=='Oui'],na.rm = TRUE)
# Confinement 1
weighted.mean(database$Facon_1[database$Cont_paris_1=='Oui'],database$poids_final[database$Cont_paris_1=='Oui'],na.rm = TRUE)
weightedMedian(database$Facon_1[database$Cont_paris_1=='Oui'],database$poids_final[database$Cont_paris_1=='Oui'],na.rm = TRUE)
# Confinement 2
weighted.mean(database$Facon_2[database$Cont_paris_1=='Oui'],database$poids_final[database$Cont_paris_1=='Oui'],na.rm = TRUE)
weightedMedian(database$Facon_2[database$Cont_paris_1=='Oui'],database$poids_final[database$Cont_paris_1=='Oui'],na.rm = TRUE)



# Freq precovid x cont_1
ggplot(database,aes(x=Cont_paris_1, y=Freq_avant, weight=poids_final)) +
  geom_boxplot() + labs(x="Avez vous continué à parier pendant le premier confinement ?", y = "Fréquence pré-Covid") + 
  scale_y_continuous(breaks=c(0,2,4,6,8,10)) +
  geom_hline(yintercept=mean_freq_avant, linetype="dashed", color = "red") +
  annotate("text",x=0.5,y=5.6,label="5.9",col="red",cex=3.5) +
  annotate("point",x=1,y=5.32,col='brown') +
  annotate("point",x=2,y=6.48,col='brown') +
  annotate("text",x=1.15,y=5.3,col='brown',label="5.32",cex=3.5) +
  annotate("text",x=2.15,y=6.5,col='brown',label="6.48",cex=3.5)







# Freq de jeu de la pop totale
# Pendant pré covid confinement 1 et confinement 2
data_global <- melt(database[,c(4,8,12,16)],id.vars = "poids_final")
ggplot(data_global,aes(x=variable,y=value, weight=poids_final)) +
  geom_boxplot() +
  scale_y_continuous(breaks=c(0,2,4,6,8,10)) +
  scale_x_discrete(labels=c("Pré-Covid","1er confinement","2eme confinement")) +
  labs(x="Période",y="Fréquence de jeu") +
  annotate("point",x=1,y=5.9,col='brown') +
  annotate("point",x=2,y=3.1,col='brown') +
  annotate("point",x=3,y=7.2,col='brown') +
  annotate("text",x=1,y=5.6,col='brown', label="5.9") +
  annotate("text",x=2,y=3.4,col='brown', label="3.1") +
  annotate("text",x=3,y=7.5,col='brown', label="7.2")

# Freq de jeu de la pop qui parie
# Pendant pré covid confinement 1 et confinement 2
data_global <- melt(database[Freq_avant!=0 & Freq_1!=0 & Freq_2!=0,c(4,8,12,16)],
                    id.vars = "poids_final")
ggplot(data_global,aes(x=variable,y=value, weight=poids_final)) +
  geom_boxplot() +
  scale_y_continuous(breaks=c(0,2,4,6,8,10)) +
  scale_x_discrete(labels=c("Pré-Covid","1er confinement","2eme confinement")) +
  labs(x="Période",y="Fréquence de jeu") +
  annotate("point",x=1,y=6,col='brown') +
  annotate("point",x=2,y=6.54,col='brown') +
  annotate("point",x=3,y=7.3,col='brown') +
  annotate("text",x=1,y=6.3,col='brown', label="6") +
  annotate("text",x=2,y=6.84,col='brown', label="6.54") +
  annotate("text",x=3,y=7,col='brown', label="7.3")

# Analyse de jeu de la pop qui parie
# Pendant pré covid confinement 1 et confinement 2
data_global <- melt(database[,c(5,10,13,16)],id.vars = "poids_final")
ggplot(data_global,aes(x=variable,y=value, weight=poids_final)) +
  geom_boxplot() +
  scale_y_continuous(breaks=c(0,2,4,6,8,10)) +
  scale_x_discrete(labels=c("Pré-Covid","1er confinement","2eme confinement")) +
  labs(x="Période",y="Analyse du parieur") +
  annotate("point",x=1,y=7,col='brown') +
  annotate("point",x=2,y=7.09,col='brown') +
  annotate("point",x=3,y=7.8,col='brown') +
  annotate("text",x=1,y=7.3,col='brown', label="7") +
  annotate("text",x=2,y=7.39,col='brown', label="7.09") +
  annotate("text",x=3,y=7.5,col='brown', label="7.8")

# Freq de jeu de la pop qui a continueé à parier lors du confinement 1
# Pendant pré covid confinement 1 et confinement 2
data_cont1 <- melt(database[Cont_paris_1=="Oui",c(4,8,12,16)],id.vars = "poids_final")
ggplot(data_cont1,aes(x=variable,y=value, weight=poids_final)) +
  geom_boxplot() +
  scale_y_continuous(breaks=c(0,2,4,6,8,10)) +
  scale_x_discrete(labels=c("Pré-Covid","1er confinement","2eme confinement")) +
  labs(x="Période",y="Fréquence de jeu") +
  annotate("point",x=1,y=6.48,col='brown') +
  annotate("point",x=2,y=6.54,col='brown') +
  annotate("point",x=3,y=7.31,col='brown') +
  annotate("text",x=1,y=6.18,col='brown', label="6.48") +
  annotate("text",x=2,y=6.84,col='brown', label="6.54") +
  annotate("text",x=3,y=7.01,col='brown', label="7.31")

# Analyse de jeu de la pop qui a continueé à parier lors du confinement 1
# Pendant pré covid confinement 1 et confinement 2
data_cont1 <- melt(database[Cont_paris_1=="Oui",c(5,10,13,16)],id.vars = "poids_final")
ggplot(data_cont1,aes(x=variable,y=value, weight=poids_final)) +
  geom_boxplot() +
  scale_y_continuous(breaks=c(0,2,4,6,8,10)) +
  scale_x_discrete(labels=c("Pré-Covid","1er confinement","2eme confinement")) +
  labs(x="Période",y="Analyse du parieur") +
  annotate("point",x=1,y=7.06,col='brown') +
  annotate("point",x=2,y=7.09,col='brown') +
  annotate("point",x=3,y=7.96,col='brown') +
  annotate("text",x=1,y=7.36,col='brown', label="7.06") +
  annotate("text",x=2,y=7.39,col='brown', label="7.09") +
  annotate("text",x=3,y=8.26,col='brown', label="7.96")

# Freq de jeu de de la pop qui a plus travaillé lors du confinement 2
# Pendant pré covid confinement 1 et confinement 2
data_plus <- melt(database[Modif_tps_trav_2=="J'ai plus travaillé qu'à la normale",
                           c(4,8,12,16)],id.vars = "poids_final")
ggplot(data_plus,aes(y=variable,x=value, weight=poids_final)) +
  geom_boxplot() +
  scale_x_continuous(breaks=c(0,2,4,6,8,10)) +
  scale_y_discrete(labels=c("Pré-Covid","1er confinement","2eme confinement")) +
  labs(y="Période",x="Fréquence de jeu") +
  annotate("point",y=1,x=6.3,col='brown') +
  annotate("point",y=2,x=3.35,col='brown') +
  annotate("point",y=3,x=6.7,col='brown') +
  annotate("text",y=1,x=6,col='brown', label="6.3") +
  annotate("text",y=2,x=2.98,col='brown', label="3.28") +
  annotate("text",y=3,x=6.4,col='brown', label="6.7")

# Analyse de la pop qui a plus travaillé lors du confinement 2
# Pendant pré covid confinement 1 et confinement 2
data_plus <- melt(database[Modif_tps_trav_2=="J'ai plus travaillé qu'à la normale",
                           c(5,10,13,16)],id.vars = "poids_final")
ggplot(data_plus,aes(x=variable,y=value, weight=poids_final)) +
  geom_boxplot() +
  scale_y_continuous(breaks=c(0,2,4,6,8,10)) +
  scale_x_discrete(labels=c("Pré-Covid","1er confinement","2eme confinement")) +
  labs(x="Période",y="Analyse du parieur") +
  annotate("point",x=1,y=6,col='brown') +
  annotate("point",x=2,y=7.1,col='brown') +
  annotate("point",x=3,y=7.1,col='brown') +
  annotate("text",x=1,y=6.3,col='brown', label="6") +
  annotate("text",x=2,y=7.4,col='brown', label="7.1") +
  annotate("text",x=3,y=7.4,col='brown', label="7.1")

# Freq de jeu de la pop qui a autant travaillé lors du confinement 2
# Pendant pré covid confinement 1 et confinement 2
data_autant <- melt(database[Modif_tps_trav_2=="Ça n'a rien changé",
                           c(4,8,12,16)],id.vars = "poids_final")
ggplot(data_autant,aes(x=variable,y=value, weight=poids_final)) +
  geom_boxplot() +
  scale_y_continuous(breaks=c(0,2,4,6,8,10)) +
  scale_x_discrete(labels=c("Pré-Covid","1er confinement","2eme confinement")) +
  labs(x="Période",y="Fréquence de jeu") +
  annotate("point",x=1,y=5.5,col='brown') +
  annotate("point",x=2,y=2.55,col='brown') +
  annotate("point",x=3,y=7.1,col='brown') +
  annotate("text",x=1,y=5.8,col='brown', label="5.5") +
  annotate("text",x=2,y=2.25,col='brown', label="2.55") +
  annotate("text",x=3,y=6.8,col='brown', label="7.1")

# Analyse de la pop qui a autant travaillé lors du confinement 2
# Pendant pré covid confinement 1 et confinement 2
data_autant <- melt(database[Modif_tps_trav_2=="Ça n'a rien changé",
                           c(5,10,13,16)],id.vars = "poids_final")
ggplot(data_autant,aes(x=variable,y=value, weight=poids_final)) +
  geom_boxplot() +
  scale_y_continuous(breaks=c(0,2,4,6,8,10)) +
  scale_x_discrete(labels=c("Pré-Covid","1er confinement","2eme confinement")) +
  labs(x="Période",y="Analyse du parieur") +
  annotate("point",x=1,y=7.2,col='brown') +
  annotate("point",x=2,y=6.89,col='brown') +
  annotate("point",x=3,y=7.7,col='brown') +
  annotate("text",x=1,y=7.5,col='brown', label="7.2") +
  annotate("text",x=2,y=6.59,col='brown', label="6.89") +
  annotate("text",x=3,y=7.4,col='brown', label="7.7")

# Freq de jeu de la pop qui a moins travaillé lors du confinement 2
# Pendant pré covid confinement 1 et confinement 2
data_moins <- melt(database[Modif_tps_trav_2=="J'ai eu plus de temps libre",
                             c(4,8,12,16)],id.vars = "poids_final")
ggplot(data_moins,aes(x=variable,y=value, weight=poids_final)) +
  geom_boxplot() +
  scale_y_continuous(breaks=c(0,2,4,6,8,10)) +
  scale_x_discrete(labels=c("Pré-Covid","1er confinement","2eme confinement")) +
  labs(x="Période",y="Fréquence de jeu") +
  annotate("point",x=1,y=6.4,col='brown') +
  annotate("point",x=2,y=4.03,col='brown') +
  annotate("point",x=3,y=7.9,col='brown') +
  annotate("text",x=1,y=6.1,col='brown', label="6.4") +
  annotate("text",x=2,y=4.33,col='brown', label="4.03") +
  annotate("text",x=3,y=7.6,col='brown', label="7.9")

# Analyse de la pop qui a moins travaillé lors du confinement 2
# Pendant pré covid confinement 1 et confinement 2
data_moins <- melt(database[Modif_tps_trav_2=="J'ai eu plus de temps libre",
                             c(5,10,13,16)],id.vars = "poids_final")
ggplot(data_moins,aes(x=variable,y=value, weight=poids_final)) +
  geom_boxplot() +
  scale_y_continuous(breaks=c(0,2,4,6,8,10)) +
  scale_x_discrete(labels=c("Pré-Covid","1er confinement","2eme confinement")) +
  labs(x="Période",y="Analyse du parieur") +
  annotate("point",x=1,y=7.3,col='brown') +
  annotate("point",x=2,y=7.38,col='brown') +
  annotate("point",x=3,y=8.3,col='brown') +
  annotate("text",x=1,y=7.6,col='brown', label="7.3") +
  annotate("text",x=2,y=7.68,col='brown', label="7.38") +
  annotate("text",x=3,y=8.6,col='brown', label="8.3")

# Evolution des parieurs réguliers/analystes sur la population qui parie
evol <- data.frame(c("avant","conf1","conf2"),c(77.93,90.88,93.96),c(77.08,76.52,91.65))
colnames(evol) <- c("periode","reguliers","analystes")
plot(c(1,2,3),c(evol$reguliers),col="blue",type="l",xlab = "périodes", ylab = "pourcentage",
     xlim=c(1,3), ylim=c(70,100),xaxt='n')
axis(1,at=c(1:3), labels=c("Pré-Covid","1er confinement","2e confinement"))
lines(c(1,2,3),c(evol$analystes),col="red")
legend(1, 100, legend=c("Parieurs réguliers", "Parieurs analystes"),
       col=c("red", "blue"), lty=1, cex=0.8)











data_plus_freq <- melt(database[Modif_tps_trav_2=="J'ai plus travaillé qu'à la normale",
                           c(4,8,12,16)],id.vars = "poids_final")
sdata_plus_freq$x <- "P"
data_autant_freq <- melt(database[Modif_tps_trav_2=="Ça n'a rien changé",
                             c(4,8,12,16)],id.vars = "poids_final")
data_autant_freq$x <- "A"
data_moins_freq <- melt(database[Modif_tps_trav_2=="J'ai eu plus de temps libre",
                            c(4,8,12,16)],id.vars = "poids_final")
data_moins_freq$x <- "M"
data_freq <- bind_rows(data_plus_freq,data_autant_freq,data_moins_freq)
colnames(data_freq) <- c("poids","Période","val","trav")
data_freq$Période <- str_replace_all(data_freq$Période,"Freq_avant","Pré-Covid")
data_freq$Période <- str_replace_all(data_freq$Période,"Freq_1","1er confinement")
data_freq$Période <- str_replace_all(data_freq$Période,"Freq_2","2e confinement")

data_freq %>%
  mutate(Période=factor(Période, levels=c("1er confinement","2e confinement","Pré-Covid"))) %>% 
  mutate(trav=factor(trav, levels=c("M","A","P"))) %>% 
  ggplot(aes(x=val, y=trav, fill=Période, weight=poids)) + 
    geom_boxplot() +
    scale_x_continuous(breaks=c(0,2,4,6,8,10)) +
    scale_y_discrete(labels=c("Moins","Autant","Plus")) +
    labs(y="Évolution du temps de travail",x="") + ggtitle("Fréquence de jeu") +
    theme(plot.title = element_text(hjust = 0.5)) +
    scale_fill_manual(values = c("gray88", "gray78", "gray68")) +
    annotate("point",y=2.75,x=6.3,col='brown3') +
    annotate("text",y=2.75,x=5.9,col='brown3', label="6.3") +
    annotate("point",y=3,x=3.3,col='brown3') +
    annotate("text",y=3,x=2.9,col='brown3', label="3.3") +
    annotate("point",y=3.25,x=6.7,col='brown3') +
    annotate("text",y=3.25,x=6.3,col='brown3', label="6.7") +
    annotate("point",y=1.75,x=5.5,col='brown3') +
    annotate("text",y=1.75,x=5.9,col='brown3', label="5.5") +
    annotate("point",y=2,x=2.6,col='brown3') +
    annotate("text",y=2,x=2.2,col='brown3', label="2.6") +
    annotate("point",y=2.25,x=7.1,col='brown3') +
    annotate("text",y=2.25,x=6.7,col='brown3', label="7.1") +
    annotate("point",y=0.75,x=6.4,col='brown3') +
    annotate("text",y=0.75,x=6,col='brown3', label="6.4") +
    annotate("point",y=1,x=4,col='brown3') +
    annotate("text",y=1,x=3.6,col='brown3', label="4.0") +
    annotate("point",y=1.25,x=7.9,col='brown3') +
    annotate("text",y=1.25,x=7.5,col='brown3', label="7.9")





data_plus_ana <- melt(database[Modif_tps_trav_2=="J'ai plus travaillé qu'à la normale",
                           c(5,10,13,16)],id.vars = "poids_final")
data_plus_ana$x <- "P"
data_autant_ana <- melt(database[Modif_tps_trav_2=="Ça n'a rien changé",
                             c(5,10,13,16)],id.vars = "poids_final")
data_autant_ana$x <- "A"
data_moins_ana <- melt(database[Modif_tps_trav_2=="J'ai eu plus de temps libre",
                            c(5,10,13,16)],id.vars = "poids_final")
data_moins_ana$x <- "M"
data_ana <- bind_rows(data_plus_ana,data_autant_ana,data_moins_ana)
colnames(data_ana) <- c("poids","Période","val","trav")
data_ana$Période <- str_replace_all(data_ana$Période,"Facon_avant","Pré-Covid")
data_ana$Période <- str_replace_all(data_ana$Période,"Facon_1","1er confinement")
data_ana$Période <- str_replace_all(data_ana$Période,"Facon_2","2e confinement")

data_ana %>%
  mutate(Période=factor(Période, levels=c("Pré-Covid","1er confinement","2e confinement"))) %>% 
  mutate(trav=factor(trav, levels=c("M","A","P"))) %>% 
  ggplot(aes(x=val, y=trav, fill=Période, weight=poids)) + 
  geom_boxplot() +
  scale_x_continuous(breaks=c(0,2,4,6,8,10)) +
  scale_y_discrete(labels=c("Moins","Autant","Plus")) +
  labs(y="Évolution du temps de travail",x="") + ggtitle("Analyse du pari") +
  theme(plot.title = element_text(hjust = 0.5)) +
  scale_fill_manual(values = c("gray88", "gray78", "gray68")) +
  annotate("point",y=2.75,x=6,col='brown3') +
  annotate("text",y=2.75,x=5.6,col='brown3', label="6.0") +
  annotate("point",y=3,x=7.1,col='brown3') +
  annotate("text",y=3,x=6.7,col='brown3', label="7.1") +
  annotate("point",y=3.25,x=7.1,col='brown3') +
  annotate("text",y=3.25,x=6.7,col='brown3', label="7.1") +
  annotate("point",y=1.75,x=7.2,col='brown3') +
  annotate("text",y=1.75,x=6.8,col='brown3', label="7.2") +
  annotate("point",y=2,x=6.9,col='brown3') +
  annotate("text",y=2,x=6.5,col='brown3', label="6.9") +
  annotate("point",y=2.25,x=7.7,col='brown3') +
  annotate("text",y=2.25,x=7.3,col='brown3', label="7.7") +
  annotate("point",y=0.75,x=7.3,col='brown3') +
  annotate("text",y=0.75,x=6.9,col='brown3', label="7.3") +
  annotate("point",y=1,x=7.4,col='brown3') +
  annotate("text",y=1,x=7,col='brown3', label="7.4") +
  annotate("point",y=1.25,x=8.3,col='brown3') +
  annotate("text",y=1.25,x=8.7,col='brown3', label="8.3")
