#Bees combination data, prevalence of GTA
#ismaelm.rodeapalomares@bayer.com
#04/25/2022

#cleaning the environemt
rm(list=ls())

library(tidyverse)
library(patchwork)



# Load SSD file (for CAS names only)
#L.Posthuma SSDs

Data.SSD = read.csv("~/Projects/beemixtox_public/data-raw/data/Copy of etc4373-sup-0002-supmat.csv")


#Load data

Data = read.csv(file = "~/Projects/beemixtox_public/data-raw/data/TerrestrialReport.csv")
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
NPerCAS = NPerCAS[NPerCAS$Observed.Response.Mean>3,]
#48 compound

# Restrict Data.1 for these compounds
Data.1 = Data.1[Data.1$CAS.Number%in%c(NPerCAS$CAS.Number),]; Data.1 = droplevels(Data.1)

# Exposure media
table(Data.1$Exposure.Type)
Data.1 = Data.1[!Data.1$Exposure.Type%in%c("Oral via capsule","Spray"),]; Data.1 = droplevels(Data.1)

# Homogenize units
table(Data.1$Observed.Response.Units)

## UNITS
Data.1$Observed.Response.Mean = as.numeric(as.character(Data.1$Observed.Response.Mean))
# Retain only useful units
Data.1 = Data.1[Data.1$Observed.Response.Units%in%c("AI mg/org","AI ng/org","AI ug/org","mg/bee","mg/org","ng/org","ug/bee","ug/org"),]; Data.1 = droplevels(Data.1)

#Transform mg to microgram
Data.1$Observed.Response.Mean[Data.1$Observed.Response.Units%in%c("AI mg/org","mg/bee","mg/org")] = Data.1$Observed.Response.Mean[Data.1$Observed.Response.Units%in%c("AI mg/org","mg/bee","mg/org")]*1000 #to ug/bee
Data.1$Observed.Response.Units[Data.1$Observed.Response.Units%in%c("AI mg/org")] = "AI ug/bee"
Data.1$Observed.Response.Units[Data.1$Observed.Response.Units%in%c("mg/bee","mg/org")] = "ug/bee"

#ng to microgram
Data.1$Observed.Response.Mean[Data.1$Observed.Response.Units%in%c("AI ng/org","ng/org")] = Data.1$Observed.Response.Mean[Data.1$Observed.Response.Units%in%c("AI ng/org","ng/org")]/1000 #to ug/bee
Data.1$Observed.Response.Units[Data.1$Observed.Response.Units%in%c("AI ng/org")] = "AI ug/bee"
Data.1$Observed.Response.Units[Data.1$Observed.Response.Units%in%c("ng/org")] = "ug/bee"

# Homogenize other units
Data.1$Observed.Response.Units[Data.1$Observed.Response.Units%in%c("AI ug/org")] = "AI ug/bee"
Data.1$Observed.Response.Units[Data.1$Observed.Response.Units%in%c("ug/org")] = "ug/bee"

#There are some more that can be labeled as "AI ug/bee because the study is based on TGAI and not formulation
Data.1$Observed.Response.Units[Data.1$Observed.Response.Units=="ug/bee"& Data.1$Conc.1.Type..Author.=="Active ingredient"] = "AI ug/bee"

table(Data.1$Observed.Response.Units)# ok
#169 AI ug/bee
Data.1 = droplevels(Data.1)

# How many of the formulation data has %purity data?
table(Data.1$CAS.Number[Data.1$Observed.Response.Units=="ug/bee" & Data.1$Chemical.Purity.Mean...=="NR"])
#66 out of 91 comes out from just one CAS number

# Transform formulation data into ai basis using purity data
Data.1$Observed.Response.Mean.t = Data.1$Observed.Response.Mean
Data.1$Observed.Response.Mean.t[Data.1$Observed.Response.Units=="ug/bee"] = Data.1$Observed.Response.Mean[Data.1$Observed.Response.Units=="ug/bee"]*as.numeric(Data.1$Chemical.Purity.Mean...[Data.1$Observed.Response.Units=="ug/bee"])/100


# Approximate variability
Bee.var.N = aggregate(Observed.Response.Mean ~ CAS.Number, data =Data.1, length)
Bee.var.Ave = aggregate(Observed.Response.Mean ~ CAS.Number, data =Data.1, mean)
Bee.var.sd = aggregate(Observed.Response.Mean ~ CAS.Number, data =Data.1, sd)
## note that the length are automatically using na.rm
Bee.var.N = aggregate(Observed.Response.Mean.t ~ CAS.Number, data =Data.1, length)
Bee.var.Ave = aggregate(Observed.Response.Mean.t ~ CAS.Number, data =Data.1, mean)
Bee.var.sd = aggregate(Observed.Response.Mean.t ~ CAS.Number, data =Data.1, sd)

# Merge
Bee.var = merge(Bee.var.N,Bee.var.Ave, by="CAS.Number")
Bee.var = merge(Bee.var,Bee.var.sd, by="CAS.Number")
names(Bee.var) [c(2:4)] = c("N", "Mean", "sd")

# Restrict to at least 4 cases per CAS
Bee.var = Bee.var[Bee.var$N>3,] # 26 chemicals; 21 chemicals if using corrected by purity

Bee.var$CV.Perc = 100*(Bee.var$sd/Bee.var$Mean)
Bee.var = merge(Bee.var, Data.SSD[,c(1,3)], by.x = "CAS.Number", by.y = "CAS.", all.x = T)
Bee.var = Bee.var[order(Bee.var$N, decreasing = T),]

# Summary of CV%
summary(Bee.var)
summary(Bee.var$CV.Perc)

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

usethis::use_data(Data.2,overwrite = T)

nrow(Bee.var) == length(unique(Data.2$CAS.Number))
