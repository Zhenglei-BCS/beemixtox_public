#Bees combination data, prevalence of GTA
#ismaelm.rodeapalomares@bayer.com
#02/25/2021

#cleaning the environemt
rm(list=ls())

#Define and set paths
Dir_Data = "C:/Users/GIGUF/OneDrive - Bayer/Personal Data/MIXTURES/Bees/USEPA_ECOTOX_02-25-2021"
Dir_Data.1 ="C:/Users/GIGUF/OneDrive - Bayer/Personal Data/MIXTURES/Bees/Bayer_Datasets"
Dir_Data_SSD ="C:/Users/GIGUF/OneDrive - Bayer/Personal Data/DATASETS/Posthuma.SSD2.019/" #only for retrieving CAS number names
Dir_Results = "C:/Users/GIGUF/OneDrive - Bayer/Personal Data/MIXTURES/Bees/USEPA_ECOTOX_02-25-2021/Results"

#Required packages
if (!require("car")) {install.packages("car", dependencies = TRUE) ; library(car)}
if (!require("fmsb")) {install.packages("fmsb", dependencies = TRUE) ; library(fmsb)}#VIF function
if (!require("mvabund")) {install.packages("mvabund", dependencies = TRUE) ; library(mvabund)}
if (!require("MuMIn")) {install.packages("MuMIn", dependencies = TRUE) ; library(MuMIn)}
if (!require("ape")) {install.packages("ape", dependencies = TRUE) ; library(ape)}
if (!require("PresenceAbsence")) {install.packages("PresenceAbsence", dependencies = TRUE) ; library(PresenceAbsence)}
#if (!require("hier.part")) {install.packages("hier.part", dependencies = TRUE) ; library(hier.part)}
if (!require("reshape2")) {install.packages("reshape2", dependencies = TRUE) ; library(reshape2)}
if (!require("ggplot2")) {install.packages("ggplot2", dependencies = TRUE) ; library(ggplot2)}
if (!require("ggthemes")) {install.packages("ggthemes", dependencies = TRUE) ; library(ggthemes)}
if (!require("RColorBrewer")) {install.packages("RColorBrewer", dependencies = TRUE) ; library(RColorBrewer)}
# if (!require("VennDiagram")) {install.packages("VennDiagram", dependencies = TRUE) ; library(VennDiagram)}


# if (!require("randomForest")) {install.packages("randomForest", dependencies = TRUE) ; library(randomForest)}
# if (!require("caret")) {install.packages("caret", dependencies = TRUE) ; library(caret)}

# super useful and easy multi-pannel plotting package
# devtools::install_github("thomasp85/patchwork");
library(patchwork)

# devtools::install_github("gaospecial/ggVennDiagram")

#User defined functions-------------------------------------------------------------

# # define NSE (Nash Succliff efficiency coeficient)
# summaryNSC = function(pred, obs) { 1 - mean((obs - pred)^2)/var(obs) }
#
# summaryRMSE = function(pred, obs){
#   sqrt(mean((pred - obs)^2))
# }


# Load SSD file (for CAS names only)
#L.Posthuma SSDs
setwd(Dir_Data_SSD)
Data.SSD = read.csv("Copy of etc4373-sup-0002-supmat.csv")


#Load data
setwd(Dir_Data)
Data = read.csv(file = "TerrestrialReport.csv")
length(unique(Data$CAS.Number))
#578 CAS
table(Data$Chemical.Grade)
table(Data$Organism.Lifestage)
table(Data$Exposure.Type)
table(Data$Number.of.Doses)
table(Data$Observed.Response.Mean.Op)
table(Data$Observed.Response.Units)
table(Data$Observed.Duration..Days.)

#Subset dataset for relevant parameters
Data.1 = Data
Data.1 = Data.1[Data.1$Organism.Lifestage%in%c("Adult"),]

#Subset for relevant exposure times
Data.1$Observed.Duration..Days.[Data.1$Observed.Duration..Days.=="NR"] = NA
Data.1$Observed.Duration..Days. = as.numeric(as.character(Data.1$Observed.Duration..Days.))

# Restrict to 24 h & 48 h end-points
Data.1 = Data.1[Data.1$Observed.Duration..Days.>=1 & Data.1$Observed.Duration..Days.<=2,]

#Restrict to definitive LDx
Data.1 = Data.1[Data.1$Observed.Response.Mean.Op%in%c(""),]

Data.1 = droplevels(Data.1)

length(unique(Data.1$CAS.Number))

# How many end-points per CAS number?
NPerCAS = aggregate(Observed.Response.Mean ~ CAS.Number, data =Data.1, length)
NPerCAS = NPerCAS[order(NPerCAS$Observed.Response.Mean, decreasing = T),]

# Restrict to more than 4 observations
NPerCAS = NPerCAS[NPerCAS$Observed.Response.Mean>=3,]
#29 compunts

# Restrict Data.1 for these 29 compounds
Data.1 = Data.1[Data.1$CAS.Number%in%c(NPerCAS$CAS.Number),]; Data.1 = droplevels(Data.1)

# Homogenize units
table(Data.1$Observed.Response.Units)

## UNITS
Data.1$Observed.Response.Mean = as.numeric(as.character(Data.1$Observed.Response.Mean))
# Retain only useful units
Data.1 = Data.1[Data.1$Observed.Response.Units%in%c("AI mg/org","AI ng/org","AI ug/org","mg/bee","mg/org","ng/org","ug/bee","ug/org"),]; Data.1 = droplevels(Data.1)

#Transform mg to microgram
Data.1$Observed.Response.Mean[Data.1$Observed.Response.Units%in%c("AI mg/org","mg/bee","mg/org")] = Data.1$Observed.Response.Mean[Data.1$Observed.Response.Units%in%c("AI mg/org","mg/bee","mg/org")]*1000 #to ug/bee
Data.1$Observed.Response.Units[Data.1$Observed.Response.Units%in%c("AI mg/org","mg/bee","mg/org")] = "ug/bee"

#ng to microgram
Data.1$Observed.Response.Mean[Data.1$Observed.Response.Units%in%c("AI ng/org","ng/org")] = Data.1$Observed.Response.Mean[Data.1$Observed.Response.Units%in%c("AI ng/org","ng/org")]/1000 #to ug/bee
Data.1$Observed.Response.Units[Data.1$Observed.Response.Units%in%c("AI ng/org","ng/org")] = "ug/bee"

# Homogenize ng/bee
Data.1$Observed.Response.Units[Data.1$Observed.Response.Units%in%c("AI ug/org","ug/bee","ug/org")] = "ug/bee"

table(Data.1$Observed.Response.Units) ok
Data.1 = droplevels(Data.1)

# Exposure media
table(Data.1$Exposure.Type)
Data.1 = Data.1[!Data.1$Exposure.Type%in%c("Oral via capsule","Spray"),]; Data.1 = droplevels(Data.1)

# Approximate variability
Bee.var.N = aggregate(Observed.Response.Mean ~ CAS.Number, data =Data.1, length)
Bee.var.Ave = aggregate(Observed.Response.Mean ~ CAS.Number, data =Data.1, mean)
Bee.var.sd = aggregate(Observed.Response.Mean ~ CAS.Number, data =Data.1, sd)

# Merge
Bee.var = merge(Bee.var.N,Bee.var.Ave, by="CAS.Number")
Bee.var = merge(Bee.var,Bee.var.sd, by="CAS.Number")
names(Bee.var) [c(2:4)] = c("N", "Mean", "sd")

# Restrict to at least 4 cases per CAS
Bee.var = Bee.var[Bee.var$N>3,] # 16 chemicals

Bee.var$CV.Perc = 100*(Bee.var$sd/Bee.var$Mean)
Bee.var = merge(Bee.var, Data.SSD[,c(1,3)], by.x = "CAS.Number", by.y = "CAS.", all.x = T)
Bee.var = Bee.var[order(Bee.var$N, decreasing = T),]

# Summary of CV%
summary(Bee.var)

# Check for independency of N and Mean
plot(Bee.var$N,Bee.var$CV.Perc)
plot(log10(Bee.var$N),Bee.var$CV.Perc)
plot(log10(Bee.var$Mean), Bee.var$CV.Perc)

# Which chemicals do we have represented?

#restrict Data.1 to the selected CAS.N
Data.2 = Data.1
Data.2 = Data.2[Data.2$CAS.Number%in%Bee.var$CAS.Number,]; Data.2 = droplevels(Data.2)
Data.2$CAS.Number = as.character(Data.2$CAS.Number)

# Check consistency again
table(Data.2$Chemical.Grade)
# Chemical grade is not reported in most cases
table(Data.2$Organism.Lifestage)
table(Data.2$Exposure.Type)
table(Data.2$Number.of.Doses)
table(Data.2$Observed.Duration..Days.)


aov.1 = aov(Observed.Response.Mean ~ CAS.Number + Conc.1.Type..Author. + Exposure.Type + Observed.Duration..Days., data = Data.2)
summary(aov.1)

# save data files
setwd(Dir_Results)

# Curated dataset
write.csv(Data.2, file = "Data.2_ECOTOX_BeeLD50_Curated.csv", row.names = F)
write.csv(Bee.var, file = "Bee.var_ECOTOX_BeeLD50_Curated.csv", row.names = F)

# Save reference list only
Data.2.Ref = unique(Data.2[,c(70,71,72,73,74)])
write.csv(Data.2.Ref, file = "Data.2.Ref_ECOTOX_BeeLD50_Curated.csv", row.names = F)

