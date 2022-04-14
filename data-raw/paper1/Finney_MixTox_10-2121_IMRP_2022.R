#Bees combination data, prevalence of GTA
#ismaelm.rodeapalomares@bayer.com
#01/26/2022

#cleaning the environemt
rm(list=ls())

#Define and set paths
Dir_Data = "C:/Users/GIGUF/OneDrive - Bayer/Personal Data/MIXTURES/Bees/Bayer_Datasets/Update_Jan2022"
Dir_Results = "C:/Users/GIGUF/OneDrive - Bayer/Personal Data/MIXTURES/Bees/Bayer_Datasets/Update_Jan2022"

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
# devtools::install_github("thomasp85/patchwork")
library(patchwork)

# devtools::install_github("gaospecial/ggVennDiagram")

#User defined functions-------------------------------------------------------------

# # define NSE (Nash Succliff efficiency coeficient)
# summaryNSC = function(pred, obs) { 1 - mean((obs - pred)^2)/var(obs) }
#
# summaryRMSE = function(pred, obs){
#   sqrt(mean((pred - obs)^2))
# }

#Load data
setwd(Dir_Data)
Data = read.csv("final_data_20220106_1.csv") #New curation
Chem.Table =read.csv("Chemicals.Table.csv")


# Replacing "-" and empty spaces by NA
# all variables to character
for (i in 1:ncol(Data)){
  Data[,i] = as.character(Data[,i])
}

Data[Data[,]=="-"] = NA
Data[Data[,]==" "] = NA
Data[Data[,]==""] = NA

# #Replace "Confidor SC 200" by imidacloprid SC 200
# Data[Data[,]=="Confidor SC 200"] = "imidacloprid SC 200"

# Homogenize Type
Data$Type[Data$Type=="insecticide & fungicide"] = "Insecticide + Fungicide"
Data$Type[Data$Type=="Insecticide+Fungicide"] = "Insecticide + Fungicide"
Data$Type[Data$Type=="Insecticide+Fungicide"] = "Insecticide + Fungicide"
Data$Type[Data$Type=="insecticide"] = "Insecticide + Insecticide"
Data$Type[Data$Type=="fungicide"] = "Fungicide + Fungicide"
Data$Type[Data$Type=="Fungicide"] = "Fungicide + Fungicide"
Data$Type[Data$Type=="Insecticide"] = "Insecticide + Insecticide"

# Data = Data[,c(1:54)]

names(Data)

# Numeric variables Back to numeric
cat.vars = c("Type","Mix.type","Mix.type.OD", "Solo.matchings","nAI.ED_NO","PLT_S_DART","PLT_S_DART.1","CLASSIFICATION","Type.mixture",
             "AI_1.PLT_S_DART","AI_1.ED_NO","AI_2.PLT_S_DART","AI_2.PLT_S_DART.1" ,"AI_2.ED_NO","AI_3.PLT_S_DART",
             "AI_3.ED_NO","AI_4.PLT_S_DART","AI_4.ED_NO")
num.var = names(Data)[!names(Data)%in%cat.vars]

for (i in 1:length(num.var)){
  Data[,names(Data)%in%num.var[i]] = as.numeric(Data[,names(Data)%in%num.var[i]])
}

# From wide to long format
Data.melt = melt(Data, id.vars = cat.vars)
# Character to factor
for (i in 1:(ncol(Data.melt)-1)){
  Data.melt[,i] = as.factor(Data.melt[,i])
}

# from long to wide

# For the moment will keep only the "ratio" (MDR) data
Data.MDR = Data.melt[grep("Ratio",Data.melt$variable),]
Data.MDR = droplevels(Data.MDR)

Data.ini = Data

Data = Data.MDR

names(Data)[names(Data)=="value"] = "quotient"

#Addying additional column variables to be able to better summarize the data
#Exposure.Type
#Time
#Combo.name
#Formulation.Type
#n.Chem

#Exposure.Type
Data$Exposure.Type =NA
Data$Exposure.Type [c(grep("cont", Data$variable))] = "cont"
Data$Exposure.Type [c(grep("oral", Data$variable))] = "oral"
table(Data$Exposure.Type) #ok

#Time
Data$Time =NA
Data$Time [c(grep("24", Data$variable))] = "24"
Data$Time [c(grep("48", Data$variable))] = "48"
table(Data$Time) #ok

# Name colums to character
Data$PLT_S_DART = as.character(Data$PLT_S_DART)
Data$AI_1.PLT_S_DART = as.character(Data$AI_1.PLT_S_DART)
Data$AI_2.PLT_S_DART = as.character(Data$AI_2.PLT_S_DART)
Data$AI_3.PLT_S_DART = as.character(Data$AI_3.PLT_S_DART)
Data$AI_4.PLT_S_DART = as.character(Data$AI_4.PLT_S_DART)

# remove lines for missing values of MDR
Data = Data[complete.cases(Data$quotient),]
table(Data$Time)
# 24  48
# 83 128
table(Data$Exposure.Type)
# cont oral
# 109  102

#Replace names (already replaced in excel)
# Karate Zeon =	lambda-cyhalothrin = LCY
# Fastac = alpha-cypermethrin = ACY
# bifenthrin = BFN
# Etofenprox = ETF
# Sportak = Prochloraz


#Add a column with for each IA for logical variables indicating if they are present or not in the mix
Short.names = data.frame(matrix(nrow=nrow(Data), ncol=nrow(Chem.Table)))
names(Short.names) = Chem.Table$Name
Short.names[,] = 0
#Assing Yes/No depending on names in the colums
for (i in 1:ncol(Short.names)){
  chem1 = colnames(Short.names)[i]
  shot1 = Chem.Table$Short.name[Chem.Table$Name==chem1]
  # grep(chem1, as.character(Data[1,]))
  sel = t(as.data.frame((apply(Data,1,grepl, pattern=shot1, ignore.case = TRUE))))
  sel = apply(sel,1,sum)
  Short.names[,i][sel>0] = 1
  }
casesbychem = apply(Short.names, 2, sum)

#Add combo name
combo.name = NULL
combo.S.name = NULL
for (i in 1:nrow(Short.names)){
  names = as.character(names(Short.names)[Short.names[i,]==1])
  S.names = as.character(Chem.Table$Short.name[Chem.Table$Name%in%names])
  combo.name = rbind(combo.name, paste(names, collapse=" + "))
  combo.S.name = rbind(combo.S.name, paste(S.names, collapse=" + "))
}
combo.name =as.character(combo.name)
combo.S.name = as.character(combo.S.name)
Short.names$combo.name = combo.name
Short.names$N.combo = apply(Short.names[,1:(ncol(Short.names)-1)],1,sum)
Short.names$combo.S.name = combo.S.name

#Add MoA. We will generate as many columns as MoAs, and them label 1 or 0 if present or not in the combination

#Add a column with for each MoA for logical variables indicating if they are present or not in the mix
MoA = data.frame(matrix(nrow=nrow(Data), ncol=length(unique(Chem.Table$MoA))))
names(MoA) = unique(Chem.Table$MoA)
MoA[,] = 0
#Assing Yes/No depending on names in the columns
MoA.list = list()
for (i in 1:length(unique(Chem.Table$MoA))){
  chem1 = unique(Chem.Table$MoA)[i]
  # Defining group of AIs per MoA
  set = as.character(unique(Chem.Table$Short.name[Chem.Table$MoA==chem1]))
  set = set[!is.na(set)]
  MoA.list[[i]] = set
  names(MoA.list)[i] = chem1
  # grep(chem1, as.character(Data[1,]))
  sel = t(as.data.frame((apply(Data,1,grepl,ignore.case = TRUE, pattern=paste(set, collapse = "|")))))
  sel = apply(sel,1,sum)
  MoA[,i][sel>0] = 1
}
casesbyMoA = apply(MoA, 2, sum)
casesbyMoA

#Add MoA.class.name
MoA.class.name = NULL
for (i in 1:nrow(MoA)){
  names = as.character(names(MoA)[MoA[i,]==1])
  if (length(names)==1){MoA.class.name = rbind(MoA.class.name, paste(c(names, names), collapse=" + "))}
  else{MoA.class.name = rbind(MoA.class.name, paste(names, collapse=" + "))}
}
MoA.class.name =as.character(MoA.class.name)
MoA$MoA.class.name = MoA.class.name
MoA$N.MoA.class = apply(MoA[,1:(ncol(MoA)-1)],1,sum)

#Add Label. We will generate as many columns as MoAs, and them label 1 or 0 if present or not in the combination

#Add a column with for each Label for logical variables indicating if they are present or not in the mix
Label = data.frame(matrix(nrow=nrow(Data), ncol=length(unique(Chem.Table$Label))))
names(Label) = unique(Chem.Table$Label)
Label[,] = 0
#Assing Yes/No depending on names in the columns
Label.list = list()
for (i in 1:length(unique(Chem.Table$Label))){
  chem1 = unique(Chem.Table$Label)[i]
  # Defining group of AIs per MoA
  set = as.character(unique(Chem.Table$Short.name[Chem.Table$Label==chem1]))
  set = set[!is.na(set)]
  Label.list[[i]] = set
  names(Label.list)[i] = chem1
  # grep(chem1, as.character(Data[1,]))
  sel = t(as.data.frame((apply(Data,1,grepl,ignore.case = TRUE, pattern=paste(set, collapse = "|")))))
  sel = apply(sel,1,sum)
  Label[,i][sel>0] = 1
}
casesbyLabel = apply(Label, 2, sum)
casesbyLabel

#Add Label.class.name
Label.class.name = NULL
for (i in 1:nrow(Label)){
  names = as.character(names(Label)[Label[i,]==1])
  if (length(names)==1){Label.class.name = rbind(Label.class.name, paste(c(names, names), collapse=" + "))}
  else{Label.class.name = rbind(Label.class.name, paste(names, collapse=" + "))}
}
Label.class.name =as.character(Label.class.name)
Label$Label.class.name = Label.class.name
Label$N.Label.class = apply(Label[,1:(ncol(Label)-1)],1,sum)


# Add chem.Type
#Add a column with for each chem.Type for logical variables indicating if they are present or not in the mix
chem.Type = data.frame(matrix(nrow=nrow(Data), ncol=length(unique(Chem.Table$chem.Type))))
names(chem.Type) = unique(Chem.Table$chem.Type)
chem.Type[,] = 0
#Assing Yes/No depending on names in the columns
chem.Type.list = list()
for (i in 1:length(unique(Chem.Table$chem.Type))){
  chem1 = unique(Chem.Table$chem.Type)[i]
  # Defining group of AIs per MoA
  set = as.character(unique(Chem.Table$Short.name[Chem.Table$chem.Type==chem1]))
  set = set[!is.na(set)]
  chem.Type.list[[i]] = set
  names(chem.Type.list)[i] = chem1
  # grep(chem1, as.character(Data[1,]))
  sel = t(as.data.frame((apply(Data,1,grepl, pattern=paste(set, collapse = "|")))))
  sel = apply(sel,1,sum)
  chem.Type[,i][sel>0] = 1
}
casesbychem.Type = apply(chem.Type, 2, sum)
casesbychem.Type

#Add combo.Type.name
combo.Type.name = NULL
for (i in 1:nrow(chem.Type)){
  names = as.character(names(chem.Type)[chem.Type[i,]==1])
  if (length(names)==1){combo.Type.name = rbind(combo.Type.name, paste(c(names, names), collapse=" + "))}
  else{combo.Type.name = rbind(combo.Type.name, paste(names, collapse=" + "))}
}
combo.Type.name =as.character(combo.Type.name)
chem.Type$combo.Type.name = combo.Type.name
chem.Type$N.combo.Type = apply(chem.Type[,1:(ncol(chem.Type)-1)],1,sum)


# Put the data sets with the  labels together
#Merge Data with the Short.names column
Data = cbind(Data, Short.names)
Data = cbind(Data, MoA)
Data = cbind(Data, Label)
Data = cbind(Data, chem.Type)


# Restrict to combos with ratio data
Data$log10Q = log10(Data$quotient)
Data = Data[complete.cases(Data$log10Q),]
Data = droplevels(Data)
Data = Data[order(Data$log10Q, decreasing = TRUE),]

# Mix.type.OD to logical
Data$Mix.type.OD =as.logical(as.integer(as.character(Data$Mix.type.OD)))

write.csv(Data, file="final_data_20220106_1.1.csv")

# Save unique AI combos
write.csv(unique(Data[,names(Data)%in%c("combo.name","combo.S.name")]), file="final_data_170921_1_combo.names.csv", row.names = F)

#Some summaries
#studies
Data.combo = unique(Data$combo.name)
length(unique(Data$combo.name))
table(Data$Exposure.Type)
table(Data$Time)
table(Data$combo.Type.name)
table(Data$CLASSIFICATION)
table(Data$Type.mixture)

unique(Data$combo.name)

# N combos by chemical combination
length(unique(Data$combo.name[Data$combo.Type.name=="INS + INS"]))
length(unique(Data$combo.name[Data$combo.Type.name=="FUN + FUN"]))
length(unique(Data$combo.name[Data$combo.Type.name=="INS + FUN"]))

# N combos by combination type
length(unique(Data$Label.class.name[Data$combo.Type.name=="INS + INS"]))
length(unique(Data$Label.class.name[Data$combo.Type.name=="FUN + FUN"]))
length(unique(Data$Label.class.name[Data$combo.Type.name=="INS + FUN"]))

length(unique(Data$nAI.ED_NO))
#76
length(unique(Data$PLT_S_DART))
#54


# Summary Table by MoA
Data.combo.names =aggregate(log10Q  ~ combo.name + combo.S.name + Label.class.name + combo.Type.name, data=Data, length)
names(Data.combo.names)[5] = "N.Observations"
write.csv(Data.combo.names, file = "Summary.Combinations.Label.csv", row.names = F)

# Summary Table by Label
Data.combo.names.1 =aggregate(log10Q  ~ combo.name + combo.S.name + MoA.class.name + combo.Type.name, data=Data, length)
names(Data.combo.names.1)[5] = "N.Observations"
write.csv(Data.combo.names.1, file = "Summary.Combinations.MoA.csv", row.names = F)

# combine both
Data.combo.names = merge(Data.combo.names,Data.combo.names.1[,c(1,3)], by = c("combo.name"))
Data.combo.names = Data.combo.names[order(Data.combo.names$Label.class.name),]
Data.combo.names = Data.combo.names[,c(1,2,3,4,6,5)]
write.csv(Data.combo.names, file = "Summary.Combinations.Label.MoA.csv", row.names = F)

# Graphical summaries

# Cumulative density functions

Plot.0 = ggplot(data=Data, aes(log10Q)) +
  stat_ecdf(pad = F, geom ="point") +
  stat_ecdf(pad = F, geom ="step") +
  geom_vline(xintercept = c(0, log10(2), log10(5), log10(0.5), log10(0.2)), linetype = c(1,2,3,2,3)) +
  annotate("rect", xmin=log10(0.5), xmax=log10(2), ymin=-Inf, ymax=Inf, alpha=0.2) +
  annotate("rect", xmin=log10(0.2), xmax=log10(5), ymin=-Inf, ymax=Inf, alpha=0.2) +
  theme_classic()+
  theme(plot.margin = unit(c(0.1,0.1,0.1,0.1), "cm"), plot.title = element_text(size = 18), strip.text.x =element_text(size = 14), rect = element_blank(), panel.border=element_rect(size=2,fill=NA, colour='black'),
        axis.text.y = element_text(size = 9), axis.text.x = element_text(size = 14), axis.title.x=element_text(size=12), axis.title.y=element_text(size=12), legend.text=element_text(size=14), legend.title=element_text(size=14),
        legend.key.size=unit(1, "cm"), legend.position="right", legend.box.background = element_rect(size=1,fill=NA))+
  labs(x="log10(MDR)",y="Fraction", title= "Cumulative probability")
Plot.0
ggsave(paste0("Bee.CA.ecdf.Plot_All.pdf"),width=10, height = 7.19, units = "in")


Plot.1 = ggplot(data=Data, aes(log10Q, colour = Type, shape = Exposure.Type)) +
  stat_ecdf(pad = F, geom ="point") +
  stat_ecdf(pad = F, geom ="step") +
  geom_vline(xintercept = c(0, log10(2), log10(5), log10(0.5), log10(0.2)), linetype = c(1,2,3,2,3)) +
  annotate("rect", xmin=log10(0.5), xmax=log10(2), ymin=-Inf, ymax=Inf, alpha=0.2) +
  annotate("rect", xmin=log10(0.2), xmax=log10(5), ymin=-Inf, ymax=Inf, alpha=0.2) +
  theme_classic()+
  theme(plot.margin = unit(c(0.1,0.1,0.1,0.1), "cm"), plot.title = element_text(size = 18), strip.text.x =element_text(size = 14), rect = element_blank(), panel.border=element_rect(size=2,fill=NA, colour='black'),
        axis.text.y = element_text(size = 9), axis.text.x = element_text(size = 14), axis.title.x=element_text(size=12), axis.title.y=element_text(size=12), legend.text=element_text(size=14), legend.title=element_text(size=14),
        legend.key.size=unit(1, "cm"), legend.position="right", legend.box.background = element_rect(size=1,fill=NA))+
  labs(x="log10(MDR)",y="Fraction", title= "Cumulative probability")
Plot.1
ggsave(paste0("Bee.CA.ecdf.Plot_All_ByTypeANDExposureType.pdf"),width=10, height = 7.19, units = "in")



Fig1.list = list ()
# By exposure tipe
Plot.1.1 = ggplot(data=Data, aes(log10Q, colour = Type)) +
  stat_ecdf(pad = F, geom ="point") +
  stat_ecdf(pad = F, geom ="step") +
  geom_vline(xintercept = c(0, log10(2), log10(5), log10(0.5), log10(0.2)), linetype = c(1,2,3,2,3)) +
  annotate("rect", xmin=log10(0.5), xmax=log10(2), ymin=-Inf, ymax=Inf, alpha=0.2) +
  annotate("rect", xmin=log10(0.2), xmax=log10(5), ymin=-Inf, ymax=Inf, alpha=0.2) +
  theme_classic()+
  theme(plot.margin = unit(c(0.1,0.1,0.1,0.1), "cm"), plot.title = element_text(size = 18), strip.text.x =element_text(size = 14), rect = element_blank(), panel.border=element_rect(size=2,fill=NA, colour='black'),
        axis.text.y = element_text(size = 9), axis.text.x = element_text(size = 14), axis.title.x=element_text(size=12), axis.title.y=element_text(size=12), legend.text=element_text(size=14), legend.title=element_text(size=14),
        # legend.key.size=unit(1, "cm"), legend.position="right", legend.box.background = element_rect(size=1,fill=NA))+
        legend.position="top")+
  labs(x="log10(MDR)",y="Prob")
Fig1.list[[1]] = Plot.1.1
Plot.1.1
ggsave(paste0("Bee.CA.ecdf.Plot_All.pdf"),width=10, height = 7.19, units = "in")

# By exposure time point
Plot.1.2 = ggplot(data=Data, aes(log10Q, colour = Time)) +
  stat_ecdf(pad = F, geom ="point") +
  stat_ecdf(pad = F, geom ="step") +
  geom_vline(xintercept = c(0, log10(2), log10(5), log10(0.5), log10(0.2)), linetype = c(1,2,3,2,3)) +
  annotate("rect", xmin=log10(0.5), xmax=log10(2), ymin=-Inf, ymax=Inf, alpha=0.2) +
  annotate("rect", xmin=log10(0.2), xmax=log10(5), ymin=-Inf, ymax=Inf, alpha=0.2) +
  theme_classic()+
  theme(plot.margin = unit(c(0.1,0.1,0.1,0.1), "cm"), plot.title = element_text(size = 18), strip.text.x =element_text(size = 14), rect = element_blank(), panel.border=element_rect(size=2,fill=NA, colour='black'),
        axis.text.y = element_text(size = 9), axis.text.x = element_text(size = 14), axis.title.x=element_text(size=12), axis.title.y=element_text(size=12), legend.text=element_text(size=14), legend.title=element_text(size=14),
        # legend.key.size=unit(1, "cm"), legend.position="right", legend.box.background = element_rect(size=1,fill=NA))+
        legend.position="top")+
  labs(x="log10(MDR)",y="Prob")
Fig1.list[[2]] = Plot.1.2
Plot.1.2
ggsave(paste0("Bee.CA.ecdf.Plot_All_ByTime.pdf"),width=10, height = 7.19, units = "in")

# By exposure type
Plot.1.3 = ggplot(data=Data, aes(log10Q, colour = Exposure.Type)) +
  stat_ecdf(pad = F, geom ="point") +
  stat_ecdf(pad = F, geom ="step") +
  geom_vline(xintercept = c(0, log10(2), log10(5), log10(0.5), log10(0.2)), linetype = c(1,2,3,2,3)) +
  annotate("rect", xmin=log10(0.5), xmax=log10(2), ymin=-Inf, ymax=Inf, alpha=0.2) +
  annotate("rect", xmin=log10(0.2), xmax=log10(5), ymin=-Inf, ymax=Inf, alpha=0.2) +
  theme_classic()+
  theme(plot.margin = unit(c(0.1,0.1,0.1,0.1), "cm"), plot.title = element_text(size = 18), strip.text.x =element_text(size = 14), rect = element_blank(), panel.border=element_rect(size=2,fill=NA, colour='black'),
        axis.text.y = element_text(size = 9), axis.text.x = element_text(size = 14), axis.title.x=element_text(size=12), axis.title.y=element_text(size=12), legend.text=element_text(size=14), legend.title=element_text(size=14),
        # legend.key.size=unit(1, "cm"), legend.position="right", legend.box.background = element_rect(size=1,fill=NA))+
        legend.position="top")+
      labs(x="log10(MDR)",y="Prob")
Fig1.list[[3]] = Plot.1.3
Plot.1.3
ggsave(paste0("Bee.CA.ecdf.Plot_All_ByExposureType.pdf"),width=10, height = 7.19, units = "in")


#Plots as a function of quality class
Plot.1.4<-ggplot(Data, aes(x=as.factor(CLASSIFICATION),y=log10Q))+
  # Plot9<-ggplot(Data,aes(x=as.factor(N.combo.class),y=log10Q))+
  geom_boxplot(fill="grey75")+
  geom_jitter(width = 0.2, alpha=0.7, aes(colour = Exposure.Type)) +
  # geom_boxplot(aes(fill=combo.class.name))+
  # geom_boxplot(aes(fill=combo.Type.name))+
  geom_hline(yintercept = c(0, log10(2), log10(5), log10(0.5), log10(0.2)), linetype = c(1,2,3,2,3)) +
  annotate("rect", xmin=-Inf, xmax=Inf, ymin=log10(0.5), ymax=log10(2), alpha=0.2) +
  annotate("rect", xmin=-Inf, xmax=Inf, ymin=log10(0.2), ymax=log10(5), alpha=0.2) +
  theme_classic()+
  theme(plot.margin = unit(c(0.1,0.1,0.1,0.1), "cm"), plot.title = element_text(size = 18), strip.text.x =element_text(size = 14), rect = element_blank(), panel.border=element_rect(size=2,fill=NA, colour='black'),
        axis.text.y = element_text(size = 9), axis.text.x = element_text(size = 14), axis.title.x=element_text(size=12), axis.title.y=element_text(size=12), legend.text=element_text(size=14), legend.title=element_text(size=14),
        # legend.key.size=unit(1, "cm"), legend.position="right", legend.box.background = element_rect(size=1,fill=NA))+
        legend.position="top")+
  labs(x="Matching class",y="log10(MDR)")
Fig1.list[[4]] = Plot.1.4
Plot.1.4
ggsave(paste0("Bee.CA.Boxplots_ByMixtureCLASSIFICATION.pdf"),width=4.5, height = 4.5, units = "in")
ggsave(paste0("Bee.CA.Boxplots_ByMixtureCLASSIFICATION.png"),width=4.5, height = 4.5, units = "in")

#Plots as a function of Formulation type
Plot.1.5<-ggplot(Data, aes(x=as.factor(Mix.type.OD),y=log10Q))+
  # Plot9<-ggplot(Data,aes(x=as.factor(N.combo.class),y=log10Q))+
  geom_boxplot(fill="grey75")+
  geom_jitter(width = 0.2, alpha=0.7, aes(colour = Exposure.Type)) +
  # geom_boxplot(aes(fill=combo.class.name))+
  # geom_boxplot(aes(fill=combo.Type.name))+
  geom_hline(yintercept = c(0, log10(2), log10(5), log10(0.5), log10(0.2)), linetype = c(1,2,3,2,3)) +
  annotate("rect", xmin=-Inf, xmax=Inf, ymin=log10(0.5), ymax=log10(2), alpha=0.2) +
  annotate("rect", xmin=-Inf, xmax=Inf, ymin=log10(0.2), ymax=log10(5), alpha=0.2) +
  theme_classic()+
  theme(plot.margin = unit(c(0.1,0.1,0.1,0.1), "cm"), plot.title = element_text(size = 18), strip.text.x =element_text(size = 14), rect = element_blank(), panel.border=element_rect(size=2,fill=NA, colour='black'),
        axis.text.y = element_text(size = 9), axis.text.x = element_text(size = 14), axis.title.x=element_text(size=12), axis.title.y=element_text(size=12), legend.text=element_text(size=14), legend.title=element_text(size=14),
        # legend.key.size=unit(1, "cm"), legend.position="right", legend.box.background = element_rect(size=1,fill=NA))+
        legend.position="top")+
  labs(x="Matching class",y="log10(MDR)")
Fig1.list[[5]] = Plot.1.5
Plot.1.5
ggsave(paste0("Bee.CA.Boxplots_ByMixtureCLASSIFICATION.pdf"),width=4.5, height = 4.5, units = "in")
ggsave(paste0("Bee.CA.Boxplots_ByMixtureCLASSIFICATION.png"),width=4.5, height = 4.5, units = "in")


#Combine
# Arrange the plots using patchworks syntax
Fig1.list[[2]] + Fig1.list[[3]] + Fig1.list[[4]] +
  # plot_layout(guides = 'collect') +
  plot_annotation(tag_levels = 'A')
ggsave("Paper_Figure1_ecdfs.pdf", width=12, height = 5, units = "in")
ggsave("Paper_Figure1_ecdfs.png", width=12, height = 5, units = "in")


#AOV TuckeyHSD
#combo.class.aov
combo.class.aov = aov(Data$log10Q~Data$combo.class)
summary(combo.class.aov)
TukeyHSD(combo.class.aov)

combo.class.aov = aov(Data$log10Q~Data$CLASSIFICATION)
summary(combo.class.aov)
TukeyHSD(combo.class.aov)

# BOXPLOTS & Summary Tables---------------------------------------------------------------------------------------------
# Boxplot by combo. Each boxplot summarize (time, route) per combo.name
Data_Ave = aggregate( log10Q  ~ combo.name , data=Data, median, na.rm=TRUE)
Data_Ave = Data_Ave[order(Data_Ave$log10Q, decreasing =FALSE),]

# Data_Ave$indicators = YM
Data$combo.name = factor(Data$combo.name, levels = Data_Ave$combo.name) # First ordering for var importance

# # order combo.name by log10Q for Part dep plots (decreasing TRUE)
# Data_Ave = Data_Ave[order(Data_Ave$log10Q, decreasing =TRUE),]
# Data_Ave.All = rbind(Data_Ave.All,Data_Ave)

Plot2<-ggplot(Data,aes(x=combo.name,y=log10Q))+
  geom_boxplot(fill="grey75", outlier.shape = NA)+
  geom_jitter(width = 0.2, alpha=0.7, aes(colour = Exposure.Type)) +
  # geom_boxplot(aes(fill=combo.class.name))+
  # geom_boxplot(aes(fill=combo.Type.name))+
  geom_hline(yintercept = c(0, log10(2), log10(5), log10(0.5), log10(0.2)), linetype = c(1,2,3,2,3)) +
  annotate("rect", xmin=-Inf, xmax=Inf, ymin=log10(0.5), ymax=log10(2), alpha=0.2) +
  annotate("rect", xmin=-Inf, xmax=Inf, ymin=log10(0.2), ymax=log10(5), alpha=0.2) +
  theme_classic()+
  theme(plot.margin = unit(c(0.1,0.5,0.3,0.1), "cm"), plot.title = element_text(size = 18), strip.text.x =element_text(size = 14), rect = element_blank(), panel.border=element_rect(size=2,fill=NA, colour='black'),
        axis.text.y = element_text(size = 9), axis.text.x = element_text(size = 14), axis.title.x=element_text(size=12), axis.title.y=element_text(size=12), legend.text=element_text(size=14), legend.title=element_text(size=14),
        # legend.key.size=unit(1, "cm"), legend.position="right", legend.box.background = element_rect(size=1,fill=NA))+
        legend.position="top")+
  labs(x=NULL,y="log10(MDR)")+
  coord_flip(ylim = c(min(Data$log10Q, na.rm = T), max(Data$log10Q, na.rm = T)))
Plot2
ggsave(paste0("Bee.CA.Boxplots_ByCombo_All.pdf"),width=7, height = 7, units = "in")
ggsave(paste0("Bee.CA.Boxplots_ByCombo_All.png"),width=7, height = 7, units = "in")


# Tabulated summary of N data and % depending on TRHS
# All
# Merge Data Ave to get combo.Type.name info
Data_Ave = merge(Data_Ave, Data[,names(Data)%in%c("combo.name","combo.Type.name")], all.x = T)
cases = c("all", "INS + INS", "FUN + FUN", "INS + FUN")
line.All =NULL
for (i in (1:length(cases))){
  case = cases[i]
  if (case == "all"){Datai = Data}else{Datai=Data[Data$combo.Type.name==case,]}
  N = nrow(Datai)
  N1 = sum(Datai$log10Q < log10(0.1))
  N2 = sum(Datai$log10Q >= log10(0.1) & Datai$log10Q < log10(0.2))
  N3 = sum(Datai$log10Q >= log10(0.2) & Datai$log10Q < log10(0.5))
  N4 =sum(Datai$log10Q >= log10(0.5) & Datai$log10Q < log10(2))
  N5 =sum(Datai$log10Q >= log10(2) & Datai$log10Q < log10(5))
  N6 =sum(Datai$log10Q >= log10(5) & Datai$log10Q < log10(10))
  N7 =sum(Datai$log10Q > log10(10))
  P1 = round(100*(N1/N),1)
  P2 = round(100*(N2/N),1)
  P3 = round(100*(N3/N),1)
  P4 = round(100*(N4/N),1)
  P5 = round(100*(N5/N),1)
  P6 = round(100*(N6/N),1)
  P7 = round(100*(N7/N),1)

  line = cbind(N,paste0(N1," (",P1,"%)"),paste0(N2," (",P2,"%)"), paste0(N3," (",P3,"%)"),paste0(N4," (",P4,"%)"),
               paste0(N5," (",P5,"%)"),paste0(N6," (",P6,"%)"), paste0(N7," (",P7,"%)"))
  # line$case = case
  line.All=rbind(line.All,line)
}
line.All = as.data.frame(line.All)
line.All$case = cases
line.All = line.All[,c(9,1:8)]
names(line.All) = c("Case", "N", "MDR<0.1","MDR 0.1-0.2", "MDR 0.2-0.5",
                    "MDR 0.5-2","MDR 2-5", "MDR 5-10", "MDR >10")
write.csv(line.All, file ="Table_SummaryNSamplesTRHS.csv" , row.names = F)

# Summary Table N unique AI combinations > TRHS

# Unique AI combinations
Data_Ave = Data_Ave[!duplicated(Data_Ave),] # to unique AI combinations
cases = c("all", "INS + INS", "FUN + FUN", "INS + FUN")
line.All =NULL
for (i in (1:length(cases))){
  case = cases[i]
  if (case == "all"){Datai = Data_Ave}else{Datai=Data_Ave[Data_Ave$combo.Type.name==case,]}
  N = nrow(Datai)
  N1 = sum(Datai$log10Q < log10(0.1))
  N2 = sum(Datai$log10Q >= log10(0.1) & Datai$log10Q < log10(0.2))
  N3 = sum(Datai$log10Q >= log10(0.2) & Datai$log10Q < log10(0.5))
  N4 =sum(Datai$log10Q >= log10(0.5) & Datai$log10Q < log10(2))
  N5 =sum(Datai$log10Q >= log10(2) & Datai$log10Q < log10(5))
  N6 =sum(Datai$log10Q >= log10(5) & Datai$log10Q < log10(10))
  N7 =sum(Datai$log10Q > log10(10))
  P1 = round(100*(N1/N),1)
  P2 = round(100*(N2/N),1)
  P3 = round(100*(N3/N),1)
  P4 = round(100*(N4/N),1)
  P5 = round(100*(N5/N),1)
  P6 = round(100*(N6/N),1)
  P7 = round(100*(N7/N),1)

  line = cbind(N,paste0(N1," (",P1,"%)"),paste0(N2," (",P2,"%)"), paste0(N3," (",P3,"%)"),paste0(N4," (",P4,"%)"),
               paste0(N5," (",P5,"%)"),paste0(N6," (",P6,"%)"), paste0(N7," (",P7,"%)"))
  # line$case = case
  line.All=rbind(line.All,line)
}
line.All = as.data.frame(line.All)
line.All$case = cases
line.All = line.All[,c(9,1:8)]
names(line.All) = c("Case", "N", "MDR<0.1","MDR 0.1-0.2", "MDR 0.2-0.5",
                    "MDR 0.5-2","MDR 2-5", "MDR 5-10", "MDR >10")
write.csv(line.All, file ="Table_SummaryNCombosTRHS.csv" , row.names = F)



# BOXPLOTS All combos by MoA.Class combination ------------------------------------------------
# Boxplot by AI. Each boxplot summarize (time, route, and combination) per AI
Data_Ave = aggregate( log10Q  ~ MoA.class.name , data=Data, median, na.rm=TRUE)
Data_Ave = Data_Ave[order(Data_Ave$log10Q, decreasing =FALSE),]

# Data_Ave$indicators = YM
Data$MoA.class.name = factor(Data$MoA.class.name, levels = Data_Ave$MoA.class.name) # First ordering for var importance

# # order combo.name by log10Q for Part dep plots (decreasing TRUE)
# Data_Ave = Data_Ave[order(Data_Ave$log10Q, decreasing =TRUE),]
# Data_Ave.All = rbind(Data_Ave.All,Data_Ave)

Plot3<-ggplot(Data,aes(x=MoA.class.name,y=log10Q))+
  geom_boxplot(fill="grey75", outlier.shape = NA)+
    geom_jitter(width = 0.2, alpha=0.7, aes(colour = Exposure.Type, share = )) +
  # geom_boxplot(aes(fill=MoA.class.name))+
  # geom_boxplot(aes(fill=combo.Type.name))+
  geom_hline(yintercept = c(0, log10(2), log10(5), log10(0.5), log10(0.2)), linetype = c(1,2,3,2,3)) +
  annotate("rect", xmin=-Inf, xmax=Inf, ymin=log10(0.5), ymax=log10(2), alpha=0.2) +
  annotate("rect", xmin=-Inf, xmax=Inf, ymin=log10(0.2), ymax=log10(5), alpha=0.2) +
  theme_classic()+
  theme(plot.margin = unit(c(0.1,0.5,0.3,0.1), "cm"), plot.title = element_text(size = 18), strip.text.x =element_text(size = 14), rect = element_blank(), panel.border=element_rect(size=2,fill=NA, colour='black'),
        axis.text.y = element_text(size = 9), axis.text.x = element_text(size = 14), axis.title.x=element_text(size=12), axis.title.y=element_text(size=12), legend.text=element_text(size=14), legend.title=element_text(size=14),
        # legend.key.size=unit(1, "cm"), legend.position="right", legend.box.background = element_rect(size=1,fill=NA))+
        legend.position="top")+
  labs(x=NULL,y="log10(MDR)")+
  coord_flip(ylim = c(min(Data$log10Q, na.rm = T), max(Data$log10Q, na.rm = T)))
Plot3
ggsave(paste0("Bee.CA.Boxplots_ByMoAClass_All.pdf"),width=4.5, height = 7, units = "in")
ggsave(paste0("Bee.CA.Boxplots_ByMoAClass_All.png"),width=4.5, height = 7, units = "in")

# BOXPLOTS All combos by Label.Class combination ------------------------------------------------
# Boxplot by AI. Each boxplot summarize (time, route, and combination) per AI
Data_Ave = aggregate( log10Q  ~ Label.class.name , data=Data, median, na.rm=TRUE)
Data_Ave = Data_Ave[order(Data_Ave$log10Q, decreasing =FALSE),]

# Data_Ave$indicators = YM
Data$Label.class.name = factor(Data$Label.class.name, levels = Data_Ave$Label.class.name) # First ordering for var importance

# # order combo.name by log10Q for Part dep plots (decreasing TRUE)
# Data_Ave = Data_Ave[order(Data_Ave$log10Q, decreasing =TRUE),]
# Data_Ave.All = rbind(Data_Ave.All,Data_Ave)

Plot3<-ggplot(Data,aes(x=Label.class.name,y=log10Q))+
  geom_boxplot(fill="grey75", outlier.shape = NA)+
  geom_jitter(width = 0.2, alpha=0.7, aes(colour = Exposure.Type, share = )) +
  # geom_boxplot(aes(fill=Label.class.name))+
  # geom_boxplot(aes(fill=combo.Type.name))+
  geom_hline(yintercept = c(0, log10(2), log10(5), log10(0.5), log10(0.2)), linetype = c(1,2,3,2,3)) +
  annotate("rect", xmin=-Inf, xmax=Inf, ymin=log10(0.5), ymax=log10(2), alpha=0.2) +
  annotate("rect", xmin=-Inf, xmax=Inf, ymin=log10(0.2), ymax=log10(5), alpha=0.2) +
  theme_classic()+
  theme(plot.margin = unit(c(0.1,0.5,0.3,0.1), "cm"), plot.title = element_text(size = 18), strip.text.x =element_text(size = 14), rect = element_blank(), panel.border=element_rect(size=2,fill=NA, colour='black'),
        axis.text.y = element_text(size = 9), axis.text.x = element_text(size = 14), axis.title.x=element_text(size=12), axis.title.y=element_text(size=12), legend.text=element_text(size=14), legend.title=element_text(size=14),
        # legend.key.size=unit(1, "cm"), legend.position="right", legend.box.background = element_rect(size=1,fill=NA))+
        legend.position="top")+
  labs(x=NULL,y="log10(MDR)")+
  coord_flip(ylim = c(min(Data$log10Q, na.rm = T), max(Data$log10Q, na.rm = T)))
Plot3
ggsave(paste0("Bee.CA.Boxplots_ByLabelClass_All.pdf"),width=6, height = 7, units = "in")
ggsave(paste0("Bee.CA.Boxplots_ByLAbelClass_All.png"),width=6, height = 7, units = "in")



# # Plot by combo/class only for selected chemical classes or AIs
#
# # By combo.class.name
# #Only P450
# Plot4<-ggplot(subset(Data, P450==1),aes(x=combo.class.name,y=log10Q))+
#   geom_boxplot(fill="grey75", outlier.shape = NA)+
#   geom_jitter(width = 0.2, alpha=0.7, aes(colour = Exposure.Type)) +
#   # geom_boxplot(aes(fill=combo.class.name))+
#   # geom_boxplot(aes(fill=combo.Type.name))+
#   geom_hline(yintercept = c(0, log10(2), log10(5), log10(0.5), log10(0.2)), linetype = c(1,2,3,2,3)) +
#   annotate("rect", xmin=-Inf, xmax=Inf, ymin=log10(0.5), ymax=log10(2), alpha=0.2) +
#   annotate("rect", xmin=-Inf, xmax=Inf, ymin=log10(0.2), ymax=log10(5), alpha=0.2) +
#   theme_classic()+
#   theme(plot.margin = unit(c(0.1,0.1,0.1,0.1), "cm"), plot.title = element_text(size = 18), strip.text.x =element_text(size = 14), rect = element_blank(), panel.border=element_rect(size=2,fill=NA, colour='black'),
#         axis.text.y = element_text(size = 9), axis.text.x = element_text(size = 14), axis.title.x=element_text(size=12), axis.title.y=element_text(size=12), legend.text=element_text(size=14), legend.title=element_text(size=14),
#         legend.key.size=unit(1, "cm"), legend.position="right", legend.box.background = element_rect(size=1,fill=NA))+
#   labs(x=NULL,y="log10(MDR)", title= "P450 - All contact & oral by combo class")+
#   coord_flip(ylim = c(min(Data$log10Q, na.rm = T), max(Data$log10Q, na.rm = T)))
# Plot4
# ggsave(paste0("Bee.CA.Boxplots_ByComboClass_P450.pdf"),width=10, height = 7.19, units = "in")
#
# ## Only combinations including NEO
# Plot5<-ggplot(subset(Data, NEO==1),aes(x=combo.class.name,y=log10Q))+
#   geom_boxplot(fill="grey75", outlier.shape = NA)+
#   geom_jitter(width = 0.2, alpha=0.7, aes(colour = Exposure.Type)) +
#   # geom_boxplot(aes(fill=combo.class.name))+
#   # geom_boxplot(aes(fill=combo.Type.name))+
#   geom_hline(yintercept = c(0, log10(2), log10(5), log10(0.5), log10(0.2)), linetype = c(1,2,3,2,3)) +
#   annotate("rect", xmin=-Inf, xmax=Inf, ymin=log10(0.5), ymax=log10(2), alpha=0.2) +
#   annotate("rect", xmin=-Inf, xmax=Inf, ymin=log10(0.2), ymax=log10(5), alpha=0.2) +
#   theme_classic()+
#   theme(plot.margin = unit(c(0.1,0.1,0.1,0.1), "cm"), plot.title = element_text(size = 18), strip.text.x =element_text(size = 14), rect = element_blank(), panel.border=element_rect(size=2,fill=NA, colour='black'),
#         axis.text.y = element_text(size = 9), axis.text.x = element_text(size = 14), axis.title.x=element_text(size=12), axis.title.y=element_text(size=12), legend.text=element_text(size=14), legend.title=element_text(size=14),
#         legend.key.size=unit(1, "cm"), legend.position="right", legend.box.background = element_rect(size=1,fill=NA))+
#   labs(x=NULL,y="log10(MDR)", title= "NEONIC - All contact & oral by combo class")+
#   coord_flip(ylim = c(min(Data$log10Q, na.rm = T), max(Data$log10Q, na.rm = T)))
# Plot5
# ggsave(paste0("Bee.CA.Boxplots_ByComboClass_NEO.pdf"),width=10, height = 7.19, units = "in")


# By combo.name
# Only combinations including P450

NEO.P450.list = list()
Plot6<-ggplot(subset(Data, P450==1),aes(x=combo.name,y=log10Q))+
  geom_boxplot(fill="grey75", outlier.shape = NA)+
  geom_jitter(width = 0.2, alpha=0.7, aes(colour = Exposure.Type)) +
  # geom_boxplot(aes(fill=combo.class.name))+
  # geom_boxplot(aes(fill=combo.Type.name))+
  geom_hline(yintercept = c(0, log10(2), log10(5), log10(0.5), log10(0.2)), linetype = c(1,2,3,2,3)) +
  annotate("rect", xmin=-Inf, xmax=Inf, ymin=log10(0.5), ymax=log10(2), alpha=0.2) +
  annotate("rect", xmin=-Inf, xmax=Inf, ymin=log10(0.2), ymax=log10(5), alpha=0.2) +
  theme_classic()+
  theme(plot.margin = unit(c(0.1,0.5,0.3,0.1), "cm"), plot.title = element_text(size = 18), strip.text.x =element_text(size = 14), rect = element_blank(), panel.border=element_rect(size=2,fill=NA, colour='black'),
        axis.text.y = element_text(size = 12), axis.text.x = element_text(size = 12), axis.title.x=element_text(size=14), axis.title.y=element_text(size=14), legend.text=element_text(size=14), legend.title=element_text(size=14),
        # legend.key.size=unit(1, "cm"), legend.position="right", legend.box.background = element_rect(size=1,fill=NA))+
        legend.position="right")+
  labs(x=NULL,y="log10(MDR)")+
  coord_flip(ylim = c(min(Data$log10Q, na.rm = T), max(Data$log10Q, na.rm = T)))
Plot6
NEO.P450.list[[1]] = Plot6
# ggsave(paste0("Bee.CA.Boxplots_ByComboName_P450-INS_ColorbyExp.pdf"),width=7.5, height = 7, units = "in")
# ggsave(paste0("Bee.CA.Boxplots_ByComboName_P450-INS_ColorbyExp.png"),width=7.5, height = 7, units = "in")

# USE. Only NEONIC combinations
Plot7<-ggplot(subset(Data, NEO==1),aes(x=combo.name,y=log10Q))+
  geom_boxplot(fill="grey75", outlier.shape = NA)+
  geom_jitter(width = 0.2, alpha=0.7, aes(colour = Exposure.Type)) +
  # geom_boxplot(aes(fill=combo.class.name))+
  # geom_boxplot(aes(fill=combo.Type.name))+
  geom_hline(yintercept = c(0, log10(2), log10(5), log10(0.5), log10(0.2)), linetype = c(1,2,3,2,3)) +
  annotate("rect", xmin=-Inf, xmax=Inf, ymin=log10(0.5), ymax=log10(2), alpha=0.2) +
  annotate("rect", xmin=-Inf, xmax=Inf, ymin=log10(0.2), ymax=log10(5), alpha=0.2) +
  theme_classic()+
  theme(plot.margin = unit(c(0.1,0.5,0.3,0.1), "cm"), plot.title = element_text(size = 18), strip.text.x =element_text(size = 14), rect = element_blank(), panel.border=element_rect(size=2,fill=NA, colour='black'),
        axis.text.y = element_text(size = 9), axis.text.x = element_text(size = 14), axis.title.x=element_text(size=12), axis.title.y=element_text(size=12), legend.text=element_text(size=12), legend.title=element_text(size=12),
        # legend.key.size=unit(1, "cm"), legend.position="top", legend.box.background = element_rect(size=1,fill=NA))+
        legend.position="none")+
  labs(x=NULL,y="log10(MDR)")+
  coord_flip(ylim = c(min(Data$log10Q, na.rm = T), max(Data$log10Q, na.rm = T)))
Plot7
NEO.P450.list[[2]] = Plot7
# ggsave(paste0("Bee.CA.Boxplots_ByComboName_NEO.ColorbyExp.pdf"),width=6.5, height = 6.5, units = "in")
# ggsave(paste0("Bee.CA.Boxplots_ByComboName_NEO.ColorbyExp.png"),width=6.5, height = 6.5, units = "in")

#Combine
# Arrange the plots using patchworks syntax
NEO.P450.list[[1]] + NEO.P450.list[[2]] +
  plot_layout(guides = 'collect') +
  plot_annotation(tag_levels = 'A')
ggsave("Paper_SM_NEO_P450.pdf", width=14, height = 5, units = "in")
ggsave("Paper_SM_NEO_P450.png", width=14, height = 5, units = "in")



#Plots as a function of mixture complexity
#Only NEONIC & KETOENOL
Plot9<-ggplot(subset(Data, NEO==1 & P450==1 | NEO==1 & PYR==1| PYR==1 & P450==1),aes(x=as.factor(N.combo),y=log10Q))+
# Plot9<-ggplot(Data,aes(x=as.factor(N.combo.class),y=log10Q))+
  geom_boxplot(fill="grey75")+
  geom_jitter(width = 0.2, alpha=0.7, aes(colour = Exposure.Type)) +
  # geom_boxplot(aes(fill=combo.class.name))+
  # geom_boxplot(aes(fill=combo.Type.name))+
  geom_hline(yintercept = c(0, log10(2), log10(5), log10(0.5), log10(0.2)), linetype = c(1,2,3,2,3)) +
  annotate("rect", xmin=-Inf, xmax=Inf, ymin=log10(0.5), ymax=log10(2), alpha=0.2) +
  annotate("rect", xmin=-Inf, xmax=Inf, ymin=log10(0.2), ymax=log10(5), alpha=0.2) +
  theme_classic()+
  theme(plot.margin = unit(c(0.1,0.1,0.1,0.1), "cm"), plot.title = element_text(size = 18), strip.text.x =element_text(size = 14), rect = element_blank(), panel.border=element_rect(size=2,fill=NA, colour='black'),
        axis.text.y = element_text(size = 9), axis.text.x = element_text(size = 14), axis.title.x=element_text(size=12), axis.title.y=element_text(size=12), legend.text=element_text(size=14), legend.title=element_text(size=14),
        legend.key.size=unit(1, "cm"), legend.position="right", legend.box.background = element_rect(size=1,fill=NA))+
  labs(x="Number of mixture components",y="log10(MDR)")
Plot9
ggsave(paste0("Bee.CA.Boxplots_ByMixtureComplexity.pdf"),width=10, height = 7.19, units = "in")
ggsave(paste0("Bee.CA.Boxplots_ByMixtureComplexity.png"),width=6, height = 4.5, units = "in")

