#EXPLORE WATERBASE WATERQUALITY DATABASE. COMPUTE MCR AND MAF. PRODUCE PLOTS AND SUMMARY TABLES
#ismaelm.rodeapalomares@bayer.com
#03/26/2021

#cleaning the environemt
rm(list=ls())

#Define and set paths
Dir_Data = "C:/Users/GIGUF/OneDrive - Bayer/Personal Data/DATASETS/Waterbase_vMay2021/Waterbase-WaterQuality ICM"

Dir_Data_SSD = "C:/Users/GIGUF/OneDrive - Bayer/Personal Data/DATASETS/Posthuma.SSD2.019/"

#Required packages
if (!require("car")) {install.packages("car", dependencies = TRUE) ; library(car)}
if (!require("fmsb")) {install.packages("fmsb", dependencies = TRUE) ; library(fmsb)}#VIF function
# if (!require("mvabund")) {install.packages("mvabund", dependencies = TRUE) ; library(mvabund)}
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
devtools::install_github("thomasp85/patchwork"); library(patchwork)

# devtools::install_github("gaospecial/ggVennDiagram")

#User defined functions-------------------------------------------------------------

# # define NSE (Nash Succliff efficiency coeficient)
# summaryNSC = function(pred, obs) { 1 - mean((obs - pred)^2)/var(obs) }
#
# summaryRMSE = function(pred, obs){
#   sqrt(mean((pred - obs)^2))
# }

#####################################################################################################
# LOAD DATASETS
#####################################################################################################

# Load CAS for heavy metals and priority pollutants
setwd(Dir_Data)
CAS.Metals = read.csv("CAS.Metals.csv")
CAS.Metals$CAS = as.character(CAS.Metals$CAS)
CAS.Priority = read.csv("EU_WFD_45PriorityPollutants.csv")
CAS.Priority$CAS = gsub("CAS_|-|_", "", CAS.Priority$CAS)
CAS.Priority$CAS = as.character(CAS.Priority$CAS)
CAS.Current.Use = read.csv("WB-WQ-CAS-CLASIFICATION.csv")

# Load Endpoint data
#L.Posthuma SSDs
setwd(Dir_Data_SSD)
Data.SSD = read.csv("Copy of etc4373-sup-0002-supmat.csv")


#Load data. Basic exploration of stations information
setwd(Dir_Data)
Stations = read.csv("Waterbase_v2020_1_S_WISE6_SpatialObject_DerivedData.csv") # v2021 35523!

names(Stations)
# names(Stations)[1] = "monitoringSiteIdentifier"

# "" to NA
Stations[Stations==""] = NA

#How many Stations?
Stations$monitoringSiteIdentifier = as.factor(Stations$monitoringSiteIdentifier)
length(levels(Stations$monitoringSiteIdentifier))
#34521
#How many have complete lat/long info?
sum(complete.cases(Stations[names(Stations)%in%c("lon", "lat")]))
#28192

# # Exploration  Data Agg by WaterBody
# Data.AggByWB = read.csv("Waterbase_v2018_1_T_WISE4_AggregatedDataByWaterBody.csv")
# length(levels(Data.AggByWB$observedPropertyDeterminandCode))
# # 4; just 4 determinads, seems to be macronutrients
# levels(Data.AggByWB$observedPropertyDeterminandCode)
# # "CAS_14797-55-8" "CAS_14797-65-0" "CAS_14798-03-9" "EEA_3132-01-2"
# levels(Data.AggByWB$resultUom)
# # [1] "mg/L"      "mg{NH4}/L" "mg{NO2}/L" "mg{NO3}/L"

###################################################################################################
# Chemical monitoring data curation
###################################################################################################

# Data by SiteID, aggregated by Year ----------------------------------------------------
# Data.AggBySiteID = read.csv("Waterbase_v2018_1_T_WISE4_AggregatedData.csv") #v2018 #3211183 rows

# Data.AggBySiteID = read.csv("Waterbase_v2019_1_T_WISE6_AggregatedData.csv") #2020 #3510775 rows

Data.AggBySiteID = read.csv("Waterbase_v2020_1_T_WISE6_AggregatedData.csv") #2021 #3962763 rows


# Data.AggBySiteID = Data.AggByWB.1
# rm(Data.AggByWB.1)

names(Data.AggBySiteID)[1] = "monitoringSiteIdentifier"

# Data.AggBySiteID[Data.AggBySiteID==""] = NA

Data.AggBySiteID$monitoringSiteIdentifier = as.factor(Data.AggBySiteID$monitoringSiteIdentifier)

# How many of the sites are in the "Sites" dataset?
sum(levels(Data.AggBySiteID$monitoringSiteIdentifier)%in%levels(Stations$monitoringSiteIdentifier))
# 15990 Sites
# How many have lat long info?
sum(levels(Data.AggBySiteID$monitoringSiteIdentifier)%in%Stations$monitoringSiteIdentifier[complete.cases(Stations[names(Stations)%in%c("lon", "lat")])])
#13410 Sites have lat long info

head(Data.AggBySiteID)
names(Data.AggBySiteID)
# [1] "monitoringSiteIdentifier"             "monitoringSiteIdentifierScheme"       "parameterWaterBodyCategory"
# [4] "observedPropertyDeterminandCode"      "observedPropertyDeterminandLabel"     "procedureAnalysedMatrix"
# [7] "resultUom"                            "phenomenonTimeReferenceYear"          "parameterSamplingPeriod"
# [10] "procedureLOQValue"                    "resultNumberOfSamples"                "resultQualityNumberOfSamplesBelowLOQ"
# [13] "resultQualityMinimumBelowLOQ"         "resultMinimumValue"                   "resultQualityMeanBelowLOQ"
# [16] "resultMeanValue"                      "resultQualityMaximumBelowLOQ"         "resultMaximumValue"
# [19] "resultQualityMedianBelowLOQ"          "resultMedianValue"                    "resultStandardDeviationValue"
# [22] "procedureAnalyticalMethod"            "parameterSampleDepth"                 "resultObservationStatus"
# [25] "remarks"                              "metadata_versionId"                   "metadata_beginLifeSpanVersion"
# [28] "metadata_statusCode"                  "metadata_observationStatus"           "metadata_statements"
# [31] "UID"
summary(Data.AggBySiteID$phenomenonTimeReferenceYear)
# Min. 1st Qu.  Median    Mean 3rd Qu.    Max.
# 1931    2007    2011    2009    2015    2019
barplot(table(Data.AggBySiteID$phenomenonTimeReferenceYear))

# How many determinads included in the L.Posthuma SSDs?
# Prepare CAS numbers
Data.AggBySiteID$CAS = gsub("CAS_|-|_", "", Data.AggBySiteID$observedPropertyDeterminandCode)

#How many CAS numbers in SSDs?
length(unique(Data.AggBySiteID$CAS)) #552 determinads


# How many entries from the exisiting data we will be able to use?
CAS.SSD = unique(Data.AggBySiteID$CAS)[unique(Data.AggBySiteID$CAS)%in%as.character(unique(Data.SSD$CAS.))]
# 373 CAS
length(Data.AggBySiteID$CAS[Data.AggBySiteID$CAS%in%CAS.SSD])
#2254186 entires out of 3510775
length(Data.AggBySiteID$CAS[Data.AggBySiteID$CAS%in%CAS.SSD])/nrow(Data.AggBySiteID)
# 64 % of the data

#Which are the "CAS" not matched?
CAS.SSD.no = unique(Data.AggBySiteID$CAS)[!unique(Data.AggBySiteID$CAS)%in%as.character(unique(Data.SSD$CAS.))]
table(grepl("EE",CAS.SSD.no))
# FALSE  TRUE
# 65    58
# 65 determinads have a wierd code instead of the CAS number

# Further reduce by the type of sample analyze
table(Data.AggBySiteID$procedureAnalysedMatrix)
# W   W-DIS   W-SPM
# 3469602  492896     265
# http://dd.eionet.europa.eu/vocabulary/wise/Matrix/view?page=2#vocabularyConceptResults
# W: Water - Total
# W-DIS: Water - Dissolved (filtered)
# W-SPM:  	Water - Suspended particulate matter
# Keep only "W" to avoid replications
Data.AggBySiteID = Data.AggBySiteID[Data.AggBySiteID$procedureAnalysedMatrix=="W",] #v2020

# Keep only correct entries without QA flags
# resultObservationStatus,"Status of the observed value in terms of its availability, relevancy, correctness or specifics of its source category.","A: Record is confirmed as correct;
## http://dd.eionet.europa.eu/vocabulary/wise/Matrix/view?page=2#vocabularyConceptResults
# A: Record is confirmed as correct;
# L: Missing observed value, the data were not collected;
# M: Missing observed value, the data can not exist;
# N: Missing observed value, observed value is not relevant or not significant;
# O: Missing observed value, no further information is available or record reported in the past should be deleted;
# W: Missing observed value, data are included in another source category;
# X: Reported value includes data from another source category (categories);
# Y: The source category does not exactly match the standard definition
table(Data.AggBySiteID$resultObservationStatus)
# records validated as correct very few, but no records verified as incorrect
# A
# 383780
Data.AggBySiteID =Data.AggBySiteID[Data.AggBySiteID$resultObservationStatus!="O",]

# metadata_observationStatus,Status of the record regarding its reliability.
# A: Normal record;
# U: Record with lower reliability;
# V: Unvalidated record"
# metadata_statements,Aditional information or statements regarding the reliability of the feature or record.
table(Data.AggBySiteID$metadata_observationStatus)
# A       U
# 3343206  126396
#126396  unreliable records
Data.AggBySiteID =Data.AggBySiteID[Data.AggBySiteID$metadata_observationStatus!="U",]

Data.AggBySiteID = droplevels(Data.AggBySiteID)

#metadata_statements
Data.AggBySiteID$metadata_statements = as.factor(Data.AggBySiteID$metadata_statements)
metadata_statements = levels(Data.AggBySiteID$metadata_statements)
# [1] ""
# [2] "NOTE_LEGACY: Measurement has been confirmed by country to be taken from a highly polluted area "
# [3] "NOTE_LEGACY: Outlier has been confirmed by country as correct value"
# [4] "NOTE_LEGACY: The parameterWaterBodyCategory changed from LW to RW based on the the monitoring site data"
# [5] "NOTE_LEGACY: The parameterWaterBodyCategory changed from RW to LW based on the the monitoring site data"
# [6] "NOTE_WBCAT_CHANGE: The parameterWaterBodyCategory has been changed to match the one officialy reported for the given spatial identifier (old: LW, new: RW)"
# [7] "NOTE_WBCAT_CHANGE: The parameterWaterBodyCategory has been changed to match the one officialy reported for the given spatial identifier (old: LW, new: TW)"
table(Data.AggBySiteID$metadata_statements!="NOTE_LEGACY: Measurement has been confirmed by country to be taken from a highly polluted area ")
# FALSE    TRUE
# 85 3343121
#Remove 85 because MAF is not to cover hot spot contamination
Data.AggBySiteID =Data.AggBySiteID[Data.AggBySiteID$metadata_statements!="NOTE_LEGACY: Measurement has been confirmed by country to be taken from a highly polluted area ",]

# Waterbody category
# http://dd.eionet.europa.eu/vocabulary/wise/WFDWaterBodyCategory/
# Coastal water body		CW
# Groundwater body		GW
# Lake water body		LW
# River water body		RW
# Transitional water body		TW
# Territorial waters		TeW
# Marine waters		MW

#only LW, RW
table(Data.AggBySiteID$parameterWaterBodyCategory)
# CW      GW      LW      RW      TW
# 322  343589  387473 2602523    9214
Data.AggBySiteID =Data.AggBySiteID[Data.AggBySiteID$parameterWaterBodyCategory%in%c("LW", "RW"),]

# CAREFULL, THIS SUBSET IS INTERIM, ONLY TO MAKE IT EASY TO MANIPULATE -------

# Subset for the AIs that were matched (Data.AggSiteID.1)
Data.AggBySiteID.1 = Data.AggBySiteID[Data.AggBySiteID$CAS%in%CAS.SSD,]; Data.AggBySiteID.1 = droplevels(Data.AggBySiteID.1) #1816359
Data.SSD.1 = Data.SSD[Data.SSD$CAS.%in%CAS.SSD,]; Data.SSD.1 = droplevels(Data.SSD.1) #366

# Units
table(Data.AggBySiteID.1$resultUom)
# mg/L mg{NH4}/L   mg{P}/L   mg{S}/L      ug/L
# 97814    102903    186232        10   1429400

#Remove all but mg/L and ug/L and transform all to mg/L
Data.AggBySiteID.1 = Data.AggBySiteID.1[Data.AggBySiteID.1$resultUom%in%c("mg/L", "ug/L"),] #1469931
#transform to ug/L
Data.AggBySiteID.1$resultMinimumValue[Data.AggBySiteID.1$resultUom=="mg/L"] = Data.AggBySiteID.1$resultMinimumValue[Data.AggBySiteID.1$resultUom=="mg/L"]*1000 # to ug/L
Data.AggBySiteID.1$resultMeanValue[Data.AggBySiteID.1$resultUom=="mg/L"] = Data.AggBySiteID.1$resultMeanValue[Data.AggBySiteID.1$resultUom=="mg/L"]*1000 # to ug/L
Data.AggBySiteID.1$resultMedianValue[Data.AggBySiteID.1$resultUom=="mg/L"] = Data.AggBySiteID.1$resultMedianValue[Data.AggBySiteID.1$resultUom=="mg/L"]*1000 # to ug/L
Data.AggBySiteID.1$resultMaximumValue[Data.AggBySiteID.1$resultUom=="mg/L"] = Data.AggBySiteID.1$resultMaximumValue[Data.AggBySiteID.1$resultUom=="mg/L"]*1000 # to ug/L
# NOTE: Unit label is changed later after removing unreliable LOQs

# Subset for complete data for Average and Maximun values
Data.AggBySiteID.1 = Data.AggBySiteID.1[!is.na(Data.AggBySiteID.1$resultMeanValue),]
Data.AggBySiteID.1 = Data.AggBySiteID.1[!is.na(Data.AggBySiteID.1$resultMaximumValue),]

###################################################################################
# SSD data quality and curation (origing, and number of taxa per SSD)----------
###################################################################################

table(Data.SSD.1$OrigenQuality.Acute.EC50)
#Acute
# AllData
# 368
# ReadAcross
# 2
# Yellow highlight-Potential to extrapolate acute SSD median concentration from poorly represented chronic NOEC data (AcuteMu = ChronicMu+1, AcuteSigma = ChronicSigma or 0.7 as a default)
# 3
#Chronic
table(Data.SSD.1$OrigenQuality.Chronic.NOEC)
# AllData
# 351
# Green highlight-Potential to extrapolate Chronic NOEC SSD median concentration from poorly represented Acute EC50 data (ChronicMu = AcuteMu-1, ChronicSigma = AcuteSigma or 0.7 as a default)
# 22

#Number of taxa per SSD
summary(Data.SSD.1$X.Species.Acute.EC50)
#Acute
# Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's
#    1.00    8.00   16.00   23.76   26.00  210.00       3 hist(log10(Data.SSD.1$X.Species.Acute.EC50))
hist(Data.SSD.1$X.TaxClass.Acute.EC50)
# How many more than 3 organism
table(Data.SSD.1$X.Species.Acute.EC50>=3)
# FALSE  TRUE
# 22   348
#CHR
summary(Data.SSD.1$X.Species.Chronic.NOEC)
# Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's
#    1.00   10.00   15.00   19.44   24.00  182.00      22
hist(log10(Data.SSD.1$X.Species.Chronic.NOEC))
table(Data.SSD.1$X.Species.Chronic.NOEC>=3)
# FALSE  TRUE
# 5   346

# Remove chem that have less than 3 sp per SSD
Data.SSD.1 = Data.SSD.1[Data.SSD.1$X.Species.Acute.EC50>=3,]
Data.SSD.1 = Data.SSD.1[Data.SSD.1$X.Species.Chronic.NOEC>=3,]
Data.SSD.1 = Data.SSD.1[!is.na(Data.SSD.1$X.Species.Chronic.NOEC),]
Data.SSD.1 = Data.SSD.1[!is.na(Data.SSD.1$X.Species.Acute.EC50),] #339 chemicals


# Data quality
table(Data.SSD.1$OrigenQuality.Chronic.NOEC)
table(Data.SSD.1$OrigenQuality.Acute.EC50)

# Highest data quality in all cases
summary(Data.SSD.1$X.Species.Acute.EC50)
summary(Data.SSD.1$X.Species.Chronic.NOEC)
summary(Data.SSD.1$X.TaxClass.Acute.EC50)

# Taxa clasess
# Different taxa classes, need to evaluate what exaclty is this.
table(Data.SSD.1$X.TaxClass.Acute.EC50>=3)
# FALSE  TRUE
# 39   307
table(Data.SSD.1$X.TaxClass.Chronic.NOEC>=3)
# FALSE  TRUE
# 61   285
# Final interim set of chemicals for analysis
CAS.SSD.1 = CAS.SSD[CAS.SSD%in%Data.SSD.1$CAS.]
Data.SSD.1 = Data.SSD.1[Data.SSD.1$CAS.%in%CAS.SSD.1,]; Data.SSD.1 = droplevels(Data.SSD.1)

# SSD data with Slope data
Data.SSD.1 = Data.SSD.1[!is.na(Data.SSD.1$X10LogSSDSlope.ug.L..SigmaChronic.NOEC),]
Data.SSD.1 = Data.SSD.1[!is.na(Data.SSD.1$X10LogSSDSlope.ug.L..SigmaAcute.EC50),]

#########################################################################
# Subset Water-base rivers based on available SSDs

Data.AggBySiteID.1 = Data.AggBySiteID.1[Data.AggBySiteID.1$CAS%in%CAS.SSD.1,]; Data.AggBySiteID.1 = droplevels(Data.AggBySiteID.1) #1334091 entries

summary(Data.SSD.1$X.Species.Chronic.NOEC)
summary(Data.SSD.1$X.Species.Acute.EC50)

##########################################################################
# Subset the dataset based on the number of measurements per CAS per year

#Number of samples
summary(Data.AggBySiteID.1$resultNumberOfSamples)
#Stations reporting number of samples
# Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's
#   1.000   4.000  10.000   8.705  12.000 366.000      47

# Remove stations do not reporting number of samples
Data.AggBySiteID.1 = Data.AggBySiteID.1[!is.na(Data.AggBySiteID.1$resultNumberOfSamples),]

# Stations reporting more than 5 samples per year to report annual aggregates
Data.AggBySiteID.1 = Data.AggBySiteID.1[Data.AggBySiteID.1$resultNumberOfSamples>=5,] #913028

###########################################################################
# LOQs: Remove unreliable entries based on missing/unreliable LOQs reporting

# 1. Remove unreliable observations
# Remove data for which LOQ is not reported
Data.AggBySiteID.1 = Data.AggBySiteID.1[!is.na(Data.AggBySiteID.1$procedureLOQValue),]

# Remove data with LOQ <= 0
Data.AggBySiteID.1 = Data.AggBySiteID.1[Data.AggBySiteID.1$procedureLOQValue> 0,]

# N Samples below LOQ
summary(Data.AggBySiteID.1$resultQualityNumberOfSamplesBelowLOQ)

# Remove data that do not report "resultQualityNumberOfSamplesBelowLOQ"
Data.AggBySiteID.1 = Data.AggBySiteID.1[!is.na(Data.AggBySiteID.1$resultQualityNumberOfSamplesBelowLOQ),] #640197

#N samples below LOQ
summary(Data.AggBySiteID.1$resultQualityNumberOfSamplesBelowLOQ)
# Min. 1st Qu.  Median    Mean 3rd Qu.    Max.
# 0.000   6.000  11.000   9.783  12.000 366.000

# Convert reported LOQs to ug/L
Data.AggBySiteID.1$procedureLOQValue[Data.AggBySiteID.1$resultUom=="mg/L"] = Data.AggBySiteID.1$procedureLOQValue[Data.AggBySiteID.1$resultUom=="mg/L"]*1000 # to ug/L
# Change unit label
Data.AggBySiteID.1$resultUom[Data.AggBySiteID.1$resultUom=="mg/L"] = "ug/L"; Data.AggBySiteID.1 = droplevels(Data.AggBySiteID.1)


#2. Set samples with Max <= LOQ to below LOQ
Data.AggBySiteID.1$AboveLOQ = Data.AggBySiteID.1$resultNumberOfSamples > Data.AggBySiteID.1$resultQualityNumberOfSamplesBelowLOQ
Data.AggBySiteID.1$AboveLOQ[Data.AggBySiteID.1$resultMaximumValue<=Data.AggBySiteID.1$procedureLOQValue] = FALSE
summary(Data.AggBySiteID.1$AboveLOQ)

# Number of samples above LOQ
table(Data.AggBySiteID.1$resultNumberOfSamples > Data.AggBySiteID.1$resultQualityNumberOfSamplesBelowLOQ)
# FALSE   TRUE
# 471043 169154
100*(471043/(471043+169154)) #73.5% of samples below LOQ; 30% actual measurements
# This is very important to see what to do with BelowLOQ values!

#if N samples > 1, and N.detects > 2, then max > mean. Therefore, max > mean can be used to separate the samples with detects from those with non-detects
table(Data.AggBySiteID.1$resultMaximumValue>Data.AggBySiteID.1$resultMeanValue)
# FALSE   TRUE
# 461365 178832
# 100*(429987/(429987+160803))
# 72.78 below LOQ, 37% avobe LOQ
# The number should be equal to the value in the previous logic

#Remove useless variables from datase
usefull.var = c("monitoringSiteIdentifier","phenomenonTimeReferenceYear", "Site.Year", "resultMeanValue", "resultMaximumValue","CAS","N.Measured","N.Detected","procedureLOQValue", "AboveLOQ", "resultNumberOfSamples", "resultQualityNumberOfSamplesBelowLOQ")
Data.AggBySiteID.1 = Data.AggBySiteID.1[,names(Data.AggBySiteID.1)%in%usefull.var]

# How many entries for the same CAS per Site_Year ##################################################
EntriesPerCAS = aggregate(resultMeanValue ~ CAS + monitoringSiteIdentifier + phenomenonTimeReferenceYear, data =Data.AggBySiteID.1, length)
table(EntriesPerCAS$resultMeanValue)
# 1      2      3      4      5      6      7      8      9
# 638102    211    162    167     54     21      8      5      3
# There are some repeated entries (multiple Averages for the same CAS, station and Year)
# Will take the maximum as worst-case scenario for the Average
EntriesPerCAS = EntriesPerCAS[EntriesPerCAS$resultMeanValue>1,]
unique(EntriesPerCAS$monitoringSiteIdentifier) #49 Sites from Italy with repeated entries

# Aggregate to keep the maximum
# @Zhenglei: is there a better way to keep only the entry with the maximum value for the Mean?
Data.AggBySiteID.1 = aggregate(. ~ monitoringSiteIdentifier + phenomenonTimeReferenceYear + CAS, data = Data.AggBySiteID.1, max)
Data.AggBySiteID.1$AboveLOQ = as.logical(Data.AggBySiteID.1$AboveLOQ)


###########################################################################
## RESTRICT THE DATASET BASED ON THE N CHEMICALS MEASURED PER STATION

# How many determinads measured per station and Year? ################################################
#variable to keep
var = c("monitoringSiteIdentifier","CAS","phenomenonTimeReferenceYear")
DataForNChem = Data.AggBySiteID.1[,names(Data.AggBySiteID.1)%in%c(var)]
measured.chem = aggregate(CAS ~ monitoringSiteIdentifier + phenomenonTimeReferenceYear, data =DataForNChem, length)
summary(measured.chem$CAS)
# Min. 1st Qu.  Median    Mean 3rd Qu.    Max.
# 1.00    2.00   22.00   31.46   45.00  191.00

# How many determinads measured per station and Year?
detected.chem = aggregate(CAS ~ monitoringSiteIdentifier + phenomenonTimeReferenceYear, data =DataForNChem[Data.AggBySiteID.1$resultNumberOfSamples>Data.AggBySiteID.1$resultQualityNumberOfSamplesBelowLOQ,], length)
summary(detected.chem$CAS)
# Min. 1st Qu.  Median    Mean 3rd Qu.    Max.
# 1.000   1.000   5.000   9.637  12.000 125.000

# Will only keep stations measuring at least 10 chemicals the same year
measured.chem = measured.chem[measured.chem$CAS>9,] #11624 StationID - Year combinations

summary(measured.chem$CAS)
# Min. 1st Qu.  Median    Mean 3rd Qu.    Max.
# 10.00   18.00   36.00   42.94   53.00  271.00
summary(measured.chem$phenomenonTimeReferenceYear)
#1998 - 2018
length(unique(measured.chem$monitoringSiteIdentifier))
# 4584 Station IDs

#Subset data for these selection of Station Year combinations
Data.AggBySiteID.1$Site.Year = paste(Data.AggBySiteID.1$monitoringSiteIdentifier,Data.AggBySiteID.1$phenomenonTimeReferenceYear, sep=".")
measured.chem$Site.Year = paste(measured.chem$monitoringSiteIdentifier,measured.chem$phenomenonTimeReferenceYear, sep=".")
detected.chem$Site.Year = paste(detected.chem$monitoringSiteIdentifier,detected.chem$phenomenonTimeReferenceYear, sep=".")

# Merge measured & detected
measured.chem = merge(measured.chem, detected.chem, by=c("monitoringSiteIdentifier","phenomenonTimeReferenceYear","Site.Year"), all.x = T)
names(measured.chem)[c(4,5)] = c("N.Measured", "N.Detected")
measured.chem$N.Detected[is.na(measured.chem$N.Detected)] = 0 # NAs are non-detected
summary(measured.chem)
hist(measured.chem$N.Measured)
hist(measured.chem$N.Detected)
measured.chem$Ratio.MD = measured.chem$N.Detected/measured.chem$N.Measured
summary(measured.chem$Ratio.MD)
# Min. 1st Qu.  Median    Mean 3rd Qu.    Max.
# 0.00000 0.06809 0.19048 0.24554 0.33333 1.00000

# Some summaries
table(measured.chem$N.Measured>9)
# TRUE
# 11668
table(measured.chem$N.Measured>50)
# FALSE  TRUE
# 8098  3570
table(measured.chem$N.Measured>100)
# FALSE  TRUE
# 10559  1109
table(measured.chem$N.Detected>10)
# FALSE  TRUE
# 7316  4352
table(measured.chem$N.Detected>50)
# FALSE  TRUE
# 11201   467

#Subset the original dataset by merging to measured chem
Data.AggBySiteID.2 = merge(Data.AggBySiteID.1,measured.chem, by=c("monitoringSiteIdentifier","phenomenonTimeReferenceYear","Site.Year"), all.y = T); Data.AggBySiteID.2 = droplevels(Data.AggBySiteID.2)
#571999 observations
length(unique(Data.AggBySiteID.2$monitoringSiteIdentifier)) #4584 Site IDs
length(unique(Data.AggBySiteID.2$CAS)) #327 CAS


#########################################################################################
###### MAKE SURE YOU SELECT THE RIGHT LOQ TREATMENT OPTION BELOW BEFORE CONTINUING!!!!
#########################################################################################

# Options for samples below LOQ
# Option 1: BelowLOQ = 0
# Option 2: BelowLOW = 1/2 LOQ


###################################################################################
# Implement option 1: BelowLOQ = 0  -----------------------------------------------
###################################################################################
Dir_Results = "C:/Users/GIGUF/OneDrive - Bayer/Personal Data/DATASETS/Waterbase_vMay2021/Waterbase_Analysis/LOQ.T0/"
dataset.name = basename(Dir_Results)
setwd(Dir_Results)

# The values that are below LOQ are set to 0 ug/L
Data.AggBySiteID.3 = Data.AggBySiteID.2
Data.AggBySiteID.3$resultMaximumValue[Data.AggBySiteID.3$AboveLOQ==FALSE] = 0
Data.AggBySiteID.3$resultMeanValue[Data.AggBySiteID.3$AboveLOQ==FALSE] = 0


####################################################################################
# Implementent option 2 Implicit) --------------------------------------------------
####################################################################################
#This option is esentially to keep the data as it is, not removing the Below LOQ values (assuming the are reporting 1/2 LOQ)
Dir_Results = "~/DATASETS/Waterbase_vMay2021/Waterbase_Analysis/LOQ.T1/"
dataset.name = basename(Dir_Results)

Data.AggBySiteID.3 = Data.AggBySiteID.2
table(Data.AggBySiteID.3$resultMaximumValue[Data.AggBySiteID.3$AboveLOQ=="FALSE"])

# The values that are below LOQ are set to 1/2 LOQ
Data.AggBySiteID.3$resultMaximumValue[Data.AggBySiteID.3$AboveLOQ=="FALSE"] = 0.5*Data.AggBySiteID.3$procedureLOQValue[Data.AggBySiteID.3$AboveLOQ=="FALSE"]
Data.AggBySiteID.3$resultMeanValue[Data.AggBySiteID.3$AboveLOQ=="FALSE"] = 0.5*Data.AggBySiteID.3$procedureLOQValue[Data.AggBySiteID.3$AboveLOQ=="FALSE"]

####################################################################################################




#####################################################################################################
# DO YOU WANT TO EXCLUDE ANY CAS NUMBER FROM THE ANALYSIS? I.E METAL ELEMENTS, OR PRIORITY POLLUANTS?
#####################################################################################################
# Three options:
# A: run as is (no exclussion)
# B: exclude metal elements
# C: exclude 45 priority polluants.

########################################################################################
# Option A Run as is
########################################################################################
# You do not need to do anything in this section to run as is
Dir_Results_exclude = Dir_Results
Data.AggBySiteID.3a = Data.AggBySiteID.3
setwd(Dir_Results_exclude)
Exclusion.CAS = "Excluded_None"
getwd()

########################################################################################
# OR Option B: exclude metal elements
########################################################################################

# Remove metal entries
Exclusion.CAS = "Excluded_Metals"
Data.AggBySiteID.3a = Data.AggBySiteID.3a[!Data.AggBySiteID.3a$CAS%in%CAS.Metals$CAS,]
Dir_Results_exclude = paste0(Dir_Results, "Metals.RM/")
setwd(Dir_Results_exclude)
getwd()

#########################################################################################
# OR Option C: exclude priority pollutants & Metals
#########################################################################################

# Remove priority polluants entries and metals
Exclusion.CAS = "Excluded_Priority & Metals"
Data.AggBySiteID.3a = Data.AggBySiteID.3a[!Data.AggBySiteID.3a$CAS%in%c(CAS.Metals$CAS,CAS.Priority$CAS),]
Dir_Results_exclude = paste0(Dir_Results, "Priority.Metals.RM/")
setwd(Dir_Results_exclude)
getwd()

#########################################################################################
# OR Option D: keep only priority pollutants & Metals
#########################################################################################

# Remove all except priority polluants entries and metals
Exclusion.CAS = "Only_Priority & Metals"
Data.AggBySiteID.3a = Data.AggBySiteID.3a[Data.AggBySiteID.3a$CAS%in%c(CAS.Metals$CAS,CAS.Priority$CAS),]
Dir_Results_exclude = paste0(Dir_Results, "Metals.Priority/")
setwd(Dir_Results_exclude)
getwd()

#########################################################################################
# OR Option E: Truely current use
#########################################################################################
# Remove metals
# Remove PAHs
# Remove not approved pesticides
# Remove Priority pollutants (WFD and STC)

Exclusion.CAS = "Current_Use"
Data.AggBySiteID.3a = Data.AggBySiteID.3a[Data.AggBySiteID.3a$CAS%in%c(CAS.Current.Use$CAS[CAS.Current.Use$Approved=="Y"&CAS.Current.Use$Chem.Group!="Metal"&CAS.Current.Use$Chem.Group!="PAH"&CAS.Current.Use$Priority==""]),]
Dir_Results_exclude = paste0(Dir_Results, "Current.Use/")
setwd(Dir_Results_exclude)
getwd()



#########################################################################################
###### MAKE SURE YOU SELECTECTED THE RIGHT LOQ TREATMENT ABOVE BEFORE CONTINUING!!!! ####
#########################################################################################


#########################################################################################
# This section computes N.Measured, N.Detected, by CAS PAF, by CAS RQ (Risk quotient),
# Aggregated HI (sumRQ), and  msPAF (multisubstance PAF)
#########################################################################################

#########################################################################################
# Recalculate Measured and detected chemicals based on the exclussions

# How many determinads measured per station and Year?
#variable to keep
var = c("monitoringSiteIdentifier","CAS","phenomenonTimeReferenceYear")
DataForNChem.1 = Data.AggBySiteID.3a[,names(Data.AggBySiteID.3a)%in%c(var)]

#measured chemicals
measured.chem.1 = aggregate(CAS ~ monitoringSiteIdentifier + phenomenonTimeReferenceYear, data =DataForNChem.1, length)
#detected chem
detected.chem.1 = aggregate(CAS ~ monitoringSiteIdentifier + phenomenonTimeReferenceYear, data =DataForNChem.1[Data.AggBySiteID.3a$resultMaximumValue!=0,], length)

#Subset data for these selection of Station Year combinations
measured.chem.1$Site.Year = paste(measured.chem.1$monitoringSiteIdentifier,measured.chem.1$phenomenonTimeReferenceYear, sep=".")
detected.chem.1$Site.Year = paste(detected.chem.1$monitoringSiteIdentifier,detected.chem.1$phenomenonTimeReferenceYear, sep=".")

# Merge measured & detected
measured.chem.1 = merge(measured.chem.1, detected.chem.1, by=c("monitoringSiteIdentifier","phenomenonTimeReferenceYear","Site.Year"), all.x = T)
names(measured.chem.1)[c(4,5)] = c("N.Measured.1", "N.Detected.1")
measured.chem.1$N.Detected.1[is.na(measured.chem.1$N.Detected.1)] = 0 #There are some NAs that needs attention, may be are samples where all is non-detected?
measured.chem.1$Ratio.MD.1 = measured.chem.1$N.Detected.1/measured.chem.1$N.Measured.1
summary(measured.chem.1)

# Merge
Data.AggBySiteID.3a = merge(Data.AggBySiteID.3a,measured.chem.1, by=c("monitoringSiteIdentifier","phenomenonTimeReferenceYear","Site.Year"), all.x = T)
measured.chem.All = merge(measured.chem, measured.chem.1, by=c("monitoringSiteIdentifier","phenomenonTimeReferenceYear","Site.Year"), all.x = T)
# No measure, no detection to 0
measured.chem.All$N.Measured.1[is.na(measured.chem.All$N.Measured.1)] = 0
measured.chem.All$N.Detected.1[is.na(measured.chem.All$N.Detected.1)] = 0
summary(measured.chem.All)
hist(measured.chem.All$N.Measured.1)
hist(measured.chem.All$N.Detected.1)
table(measured.chem.All$N.Measured.1>10) # 9407 combinations measuring 10 or more
table(measured.chem.All$N.Detected.1>10) # 2135 combinations detecting 10 or more

### check there is no NAs
summary(Data.AggBySiteID.3a$resultMaximumValue)
summary(Data.AggBySiteID.3a$resultMeanValue)


# Merge with SSD info (mu.sig) by CAS numner
Data.AggBySiteID.3a = merge(Data.AggBySiteID.3a, Data.SSD.1[,names(Data.SSD.1)%in%c("CAS.","X10LogSSDMedianConcentration.ug.L..MuAcute.EC50","X10LogSSDMedianConcentration.ug.L..MuChronic.NOEC", "X10LogSSDSlope.ug.L..SigmaAcute.EC50","X10LogSSDSlope.ug.L..SigmaChronic.NOEC")], by.x = "CAS", by.y="CAS.")
names(Data.AggBySiteID.3a)[(17:20)] = c("SSDLOG10.Mu.Acute.EC50", "SSDLOG10.Sigma.Acute.EC50", "SSDLOG10.Mu.chronic.NOEC","SSDLOG10.Sigma.chronic.NOEC")

# Calculate PAF by row.
# Equivalent to excel NORMDIST(log10[sumHU],0,0.7,1) is pnorm(log10(Sum(HU),0,07,log=F)
# Carefull because Mu and sigma are given in log10 monitoring data is given in linear scale
# It is necessary to backtransform Mu values to linear scale!?

#Hazard.Index to the midpoint SSD by chem (reference to the HC05)
#Hazard.Index to the HC05 SSD by chem
# Taken from: https://edild.github.io/ssd/
# Will use compoud-specific slopes. Assuming 0.7 SSD slope is quite worst case (as how the data comes out)

#Compute HC05 by chemical ############################################################################
#The mu and sig parameters are given in log10 units!
Data.AggBySiteID.3a$HC50.Acute = 10^qnorm(0.5,Data.AggBySiteID.3a$SSDLOG10.Mu.Acute.EC50, Data.AggBySiteID.3a$SSDLOG10.Sigma.Acute.EC50)
Data.AggBySiteID.3a$HC05.Chronic = 10^qnorm(0.05,Data.AggBySiteID.3a$SSDLOG10.Mu.Acute.EC50, Data.AggBySiteID.3a$SSDLOG10.Sigma.chronic.NOEC)

Data.AggBySiteID.3a$HQ.Acute = Data.AggBySiteID.3a$resultMaximumValue/Data.AggBySiteID.3a$HC50.Acute
Data.AggBySiteID.3a$HQ.Chronic = Data.AggBySiteID.3a$resultMeanValue/Data.AggBySiteID.3a$HC05.Chronic

# Summaries
summary(Data.AggBySiteID.3a$HQ.Acute)
summary(Data.AggBySiteID.3a$HQ.Chronic)

hist(log10(Data.AggBySiteID.3a$HQ.Acute))
hist(log10(Data.AggBySiteID.3a$HQ.Chronic))

#How many samples greater than HC05?
summary(Data.AggBySiteID.3a$HQ.Acute>1)
summary(Data.AggBySiteID.3a$HQ.Chronic>1)


# PAF by chemical ##################################################################################
# Calculate HU (Hazard Unit) Anti-log10 are taken on Mu because HUs needs to be calculated in log-non-tramsformed data.
Data.AggBySiteID.3a$HU.Acute.EC50 = Data.AggBySiteID.3a$resultMaximumValue/(10^Data.AggBySiteID.3a$SSDLOG10.Mu.Acute.EC50)
Data.AggBySiteID.3a$HU.Chronic.NOEC = Data.AggBySiteID.3a$resultMeanValue/(10^Data.AggBySiteID.3a$SSDLOG10.Mu.chronic.NOEC)
# Calculate PAF
Data.AggBySiteID.3a$PAF.Acute.EC50 = pnorm(log10(Data.AggBySiteID.3a$HU.Acute.EC50),0,Data.AggBySiteID.3a$SSDLOG10.Sigma.Acute.EC50,log=F)
Data.AggBySiteID.3a$PAF.Chronic.NOEC = pnorm(log10(Data.AggBySiteID.3a$HU.Chronic.NOEC),0,Data.AggBySiteID.3a$SSDLOG10.Sigma.chronic.NOEC,log=F)

# Summaries
summary(Data.AggBySiteID.3a$PAF.Acute.EC50)
summary(Data.AggBySiteID.3a$PAF.Chronic.NOEC)

summary(Data.AggBySiteID.3a$PAF.Acute.EC50>0.05)
# Mode   FALSE    TRUE
# logical  243451    6495
4245/(4245+612550) #0.6% of samples revised 07/08/2021

summary(Data.AggBySiteID.3a$PAF.Chronic.NOEC>0.05)
# Mode   FALSE    TRUE
# logical  225399   24547
14017/(14017+602778) #2 % of samples revised 07/08/2021

hist(Data.AggBySiteID.3a$PAF.Acute.EC50)
hist(Data.AggBySiteID.3a$PAF.Chronic.NOEC)


# Aggregate HU over all chemicals per station to compute MaxHQ, HI, HU, msPAF and maxPAF ##########################################################
HU.sum = aggregate(. ~ monitoringSiteIdentifier + Site.Year, data=Data.AggBySiteID.3a[,names(Data.AggBySiteID.3a)%in%c("monitoringSiteIdentifier","Site.Year","HU.Acute.EC50","HU.Chronic.NOEC", "HQ.Acute", "HQ.Chronic")], sum, na.rm=TRUE)
HQ.Max = aggregate(. ~ monitoringSiteIdentifier + Site.Year, data=Data.AggBySiteID.3a[,names(Data.AggBySiteID.3a)%in%c("monitoringSiteIdentifier","Site.Year","HU.Acute.EC50","HU.Chronic.NOEC","HQ.Acute", "HQ.Chronic", "PAF.Acute.EC50", "PAF.Chronic.NOEC")], max, na.rm=TRUE)

# summaries
summary(Data.AggBySiteID.3a$SSDLOG10.Sigma.Acute.EC50)
# Min. 1st Qu.  Median    Mean 3rd Qu.    Max.
# 0.1500  0.7000  0.8500  0.9236  1.1300  2.4000
summary(Data.AggBySiteID.3a$SSDLOG10.Sigma.chronic.NOEC)
# Min. 1st Qu.  Median    Mean 3rd Qu.    Max.
# 0.0000  0.7024  0.7799  0.8594  1.0202  2.1512

# Compute msPAF (multi-substance predicted affected fractions)
HU.sum$msPAF.Acute.EC50 = pnorm(log10(HU.sum$HU.Acute.EC50),0,mean(Data.AggBySiteID.3a$SSDLOG10.Sigma.Acute.EC50),log=F)
HU.sum$msPAF.Chronic.NOEC = pnorm(log10(HU.sum$HU.Chronic.NOEC),0,mean(Data.AggBySiteID.3a$SSDLOG10.Sigma.chronic.NOEC),log=F)

# Compute maxPAF (maximum PAF per site)
HQ.Max$maxPAF.Acute.EC50 = pnorm(log10(HQ.Max$HU.Acute.EC50),0,mean(Data.AggBySiteID.3a$SSDLOG10.Sigma.Acute.EC50),log=F)
HQ.Max$maxPAF.Chronic.NOEC = pnorm(log10(HQ.Max$HU.Chronic.NOEC),0,mean(Data.AggBySiteID.3a$SSDLOG10.Sigma.chronic.NOEC),log=F)

######################
# Zhenglei don't go further than here
names (HQ.Max)
# [1] "monitoringSiteIdentifier" "Site.Year"                "HQ.Acute"
# [4] "HQ.Chronic"               "HU.Acute.EC50"            "HU.Chronic.NOEC"
# [7] "PAF.Acute.EC50"           "PAF.Chronic.NOEC"         "maxPAF.Acute.EC50"
# [10] "maxPAF.Chronic.NOEC"
names (HU.sum)
# [1] "monitoringSiteIdentifier" "Site.Year"                "HQ.Acute"
# [4] "HQ.Chronic"               "HU.Acute.EC50"            "HU.Chronic.NOEC"
# [7] "msPAF.Acute.EC50"         "msPAF.Chronic.NOEC"

# I need you to check if the following calculations for computing are correct:
# "msPAF.Chronic.NOEC"
# "msPAF.Acute.EC50"
# I think the assumption is that all chemicals have equal slope SSD to be able to compute the aggregated PAF

# Further, I computed the maxPAF in two different ways (lines 697) aggregating over the dataset, and in line 713 and 714 (aggregating to maxHU, and computing maxPAF using the average slope).
# Results are quite different. I believe this points at an overstimation of msPAF when calculating it over many chemicals assuming equal SSD slope ~ 0.8
# "PAF.Acute.EC50"
# "maxPAF.Acute.EC50"
# "PAF.Chronic.NOEC"
# "maxPAF.Chronic.NOEC"
quantile(HQ.Max$PAF.Chronic.NOEC, probs = c(0.90, 0.95))
# 90%        95%
#   0.09996921 0.16157005
quantile(HQ.Max$maxPAF.Chronic.NOEC, probs = c(0.90, 0.95))
# 90%       95%
#   0.1504279 0.2159670
quantile(HQ.Max$PAF.Acute.EC50, probs = c(0.90, 0.95))
# 90%        95%
#   0.03516125 0.05845606
quantile(HQ.Max$maxPAF.Acute.EC50, probs = c(0.90, 0.95))
# 90%        95%
#   0.05738329 0.09704670

# Merge all metrics
Data.msPAF = merge(HU.sum, HQ.Max, by = c("monitoringSiteIdentifier","Site.Year"))
names(Data.msPAF)[c(3,4,9,10)] = c("HI.HC50.Acute.EC50","HI.HC05.Chronic.NOEC", "MaxHQ.HC50.Acute.EC50","MaxHQ.HC05.Chronic.NOEC")

#summaries
table (Data.msPAF$msPAF.Acute.EC50>0.05) # ~14% station year combinations
# FALSE  TRUE
# 7550  4806
table (Data.msPAF$PAF.Acute.EC50>0.05) # ~7% station year combinations
# FALSE  TRUE
# 10577   798
table (Data.msPAF$maxPAF.Acute.EC50>0.05) # ~11% station year combinations
# FALSE  TRUE
# 10577   1287

table (Data.msPAF$msPAF.Chronic.NOEC>0.05) # 31% station year combinations
# FALSE  TRUE
# 7828  3547
table (Data.msPAF$PAF.Chronic.NOEC>0.05) # 18% station year combinations
# FALSE  TRUE
# 9310  2065
table (Data.msPAF$maxPAF.Chronic.NOEC>0.05) # 28% station year combinations
# FALSE  TRUE
# 8199  3176


#Compute MCR based on HI ###########################################################################
Data.msPAF$MCR.HC05.Chronic.NOEC = Data.msPAF$HI.HC05.Chronic.NOEC/Data.msPAF$MaxHQ.HC05.Chronic.NOEC
Data.msPAF$MCR.HC50.Acute.EC50 = Data.msPAF$HI.HC50.Acute.EC50/Data.msPAF$MaxHQ.HC50.Acute.EC50

# # Merge with previous datasets to get more insights on the number of chemicals measured
# Data.msPAF = merge(Data.msPAF, measured.chem.All, by = c("monitoringSiteIdentifier","Site.Year"), all.x = T)
# table(Data.msPAF$N.Detected>0)
# table(Data.msPAF$N.Detected.1>0)

####################################################################################################
#### GENERATE AGGREGATED DATASET OVER YEAR (ONLY ONE DATA POINT PER STATION ID)
####################################################################################################

# Aggregate over "Year" (based on worst case for HI.Acute)
Data.msPAF.Max.HI = aggregate(HI.HC05.Chronic.NOEC~ monitoringSiteIdentifier, data = Data.msPAF, max, na.rm=TRUE)
# Merge to include the rest of information
Data.msPAF.Max.HI = merge(Data.msPAF.Max.HI,Data.msPAF, by = c("monitoringSiteIdentifier","HI.HC05.Chronic.NOEC"), all.x = T)

#########################################################################
# END DATA MANIPULATION
#########################################################################

# Summary of MCR & HI quantiles & Saving datasets #######################
#Quantiles for the plots
# Site-Year
print("MCR")
quantile(Data.msPAF$MCR.HC05.Chronic.NOEC, c(0.5, 0.9, 0.95), na.rm = T)
quantile(Data.msPAF$MCR.HC50.Acute.EC50, c(0.5, 0.9, 0.95), na.rm = T)
#Aggregated
quantile(Data.msPAF.Max.HI$MCR.HC05.Chronic.NOEC, c(0.5, 0.9, 0.95), na.rm = T)
quantile(Data.msPAF.Max.HI$MCR.HC50.Acute.EC50, c(0.5, 0.9, 0.95), na.rm = T)

print("HI")
quantile(Data.msPAF$HI.HC05.Chronic.NOEC, c(0.5, 0.9, 0.95), na.rm = T)
quantile(Data.msPAF$HI.HC50.Acute.EC50, c(0.5, 0.9, 0.95), na.rm = T)
#Aggregated
quantile(Data.msPAF.Max.HI$HI.HC05.Chronic.NOEC, c(0.5, 0.9, 0.95), na.rm = T)
quantile(Data.msPAF.Max.HI$HI.HC50.Acute.EC50, c(0.5, 0.9, 0.95), na.rm = T)


# Save main datasets ########################################################################
Data.AggBySiteID.3a$Case = dataset.name
Data.AggBySiteID.3a$Exclussion.CAS = Exclusion.CAS
Data.msPAF$Case = dataset.name
Data.msPAF$Exclussion.CAS = Exclusion.CAS


write.csv(Data.AggBySiteID.3a, file = paste("Data.AggBySiteID.3a",dataset.name,Exclusion.CAS, "csv", sep="."),row.names = F)
write.csv(Data.msPAF, file = paste("Data.msPAF",dataset.name,Exclusion.CAS, "csv", sep="."), row.names = F)
write.csv(Data.msPAF.Max.HI, file = paste("Data.msPAF.Max.HI",dataset.name,Exclusion.CAS, "csv", sep="."), row.names = F)

##################################################################################
# DATA ANALYSIS RESULTS
##################################################################################

# CEFIC-MIAT decission tree table (Valloton &Proce 2016) #########################

# Group I: Risk driven by single chemicals HI >1 & MaxHQ >1
# Group II:No concern HI < 1, MaxHQ < 1
# Group IIIA: Mixtures concern driven by 1 or 2 chemicals. HI>1 & Max HI <1 & MCR < 2
# Group IIIB: Mixture risk. HI > 1 & MaxHQ < 1 & MCR >2

#CHR
GroupII.CHR.AF1 = table(Data.msPAF$HI.HC05.Chronic.NOEC<= 1 & Data.msPAF$MaxHQ.HC05.Chronic.NOEC<=1)
GroupII.CHR.AF3 = table(Data.msPAF$HI.HC05.Chronic.NOEC<= 0.33 & Data.msPAF$MaxHQ.HC05.Chronic.NOEC<=0.33)
GroupII.CHR.AF10 = table(Data.msPAF$HI.HC05.Chronic.NOEC<= 0.1 & Data.msPAF$MaxHQ.HC05.Chronic.NOEC<= 0.1)

GroupI.CHR.AF1 = table(Data.msPAF$HI.HC05.Chronic.NOEC> 1 & Data.msPAF$MaxHQ.HC05.Chronic.NOEC>1)
GroupI.CHR.AF3 = table(Data.msPAF$HI.HC05.Chronic.NOEC> 0.33 & Data.msPAF$MaxHQ.HC05.Chronic.NOEC>0.33)
GroupI.CHR.AF10 = table(Data.msPAF$HI.HC05.Chronic.NOEC> 0.1 & Data.msPAF$MaxHQ.HC05.Chronic.NOEC>0.1)

GroupIIIA.CHR.AF1 = table(Data.msPAF$HI.HC05.Chronic.NOEC> 1 & Data.msPAF$MaxHQ.HC05.Chronic.NOEC<=1 & Data.msPAF$MCR.HC05.Chronic.NOEC <=2)
GroupIIIA.CHR.AF3 = table(Data.msPAF$HI.HC05.Chronic.NOEC> 0.33 & Data.msPAF$MaxHQ.HC05.Chronic.NOEC<=0.33 & Data.msPAF$MCR.HC05.Chronic.NOEC <=2)
GroupIIIA.CHR.AF10 = table(Data.msPAF$HI.HC05.Chronic.NOEC> 0.1 & Data.msPAF$MaxHQ.HC05.Chronic.NOEC<=0.1 & Data.msPAF$MCR.HC05.Chronic.NOEC <=2)

GroupIIIB.CHR.AF1 = table(Data.msPAF$HI.HC05.Chronic.NOEC> 1 & Data.msPAF$MaxHQ.HC05.Chronic.NOEC<=1 & Data.msPAF$MCR.HC05.Chronic.NOEC >2)
GroupIIIB.CHR.AF3 = table(Data.msPAF$HI.HC05.Chronic.NOEC> 0.33 & Data.msPAF$MaxHQ.HC05.Chronic.NOEC<=0.33 & Data.msPAF$MCR.HC05.Chronic.NOEC >2)
GroupIIIB.CHR.AF10 = table(Data.msPAF$HI.HC05.Chronic.NOEC> 0.1 & Data.msPAF$MaxHQ.HC05.Chronic.NOEC<=0.1 & Data.msPAF$MCR.HC05.Chronic.NOEC >2)

#Acute
GroupII.Acute.AF1 = table(Data.msPAF$HI.HC50.Acute.EC50<= 1 & Data.msPAF$MaxHQ.HC50.Acute.EC50<=1)
GroupII.Acute.AF3 = table(Data.msPAF$HI.HC50.Acute.EC50<= 0.33 & Data.msPAF$MaxHQ.HC50.Acute.EC50<=0.33)
GroupII.Acute.AF10 = table(Data.msPAF$HI.HC50.Acute.EC50<= 0.1 & Data.msPAF$MaxHQ.HC50.Acute.EC50<= 0.1)

GroupI.Acute.AF1 = table(Data.msPAF$HI.HC50.Acute.EC50> 1 & Data.msPAF$MaxHQ.HC50.Acute.EC50>1)
GroupI.Acute.AF3 = table(Data.msPAF$HI.HC50.Acute.EC50> 0.33 & Data.msPAF$MaxHQ.HC50.Acute.EC50>0.33)
GroupI.Acute.AF10 = table(Data.msPAF$HI.HC50.Acute.EC50> 0.1 & Data.msPAF$MaxHQ.HC50.Acute.EC50>0.1)

GroupIIIA.Acute.AF1 = table(Data.msPAF$HI.HC50.Acute.EC50> 1 & Data.msPAF$MaxHQ.HC50.Acute.EC50<=1 & Data.msPAF$MCR.HC50.Acute.EC50 <=2)
GroupIIIA.Acute.AF3 = table(Data.msPAF$HI.HC50.Acute.EC50> 0.33 & Data.msPAF$MaxHQ.HC50.Acute.EC50<=0.33 & Data.msPAF$MCR.HC50.Acute.EC50 <=2)
GroupIIIA.Acute.AF10 = table(Data.msPAF$HI.HC50.Acute.EC50> 0.1 & Data.msPAF$MaxHQ.HC50.Acute.EC50<=0.1 & Data.msPAF$MCR.HC50.Acute.EC50 <=2)

GroupIIIB.Acute.AF1 = table(Data.msPAF$HI.HC50.Acute.EC50> 1 & Data.msPAF$MaxHQ.HC50.Acute.EC50<=1 & Data.msPAF$MCR.HC50.Acute.EC50 >2)
GroupIIIB.Acute.AF3 = table(Data.msPAF$HI.HC50.Acute.EC50> 0.33 & Data.msPAF$MaxHQ.HC50.Acute.EC50<=0.33 & Data.msPAF$MCR.HC50.Acute.EC50 >2)
GroupIIIB.Acute.AF10 = table(Data.msPAF$HI.HC50.Acute.EC50> 0.1 & Data.msPAF$MaxHQ.HC50.Acute.EC50<=0.1 & Data.msPAF$MCR.HC50.Acute.EC50 >2)

#Combine all cases
All.AF = as.data.frame(rbind(GroupII.CHR.AF1, GroupII.CHR.AF3,GroupII.CHR.AF10,
                             GroupII.Acute.AF1,GroupII.Acute.AF3,GroupII.Acute.AF10,
                             GroupI.CHR.AF1,GroupI.CHR.AF3,GroupI.CHR.AF10,
                             GroupI.Acute.AF1,GroupI.Acute.AF3,GroupI.Acute.AF10,
                             GroupIIIA.CHR.AF1,GroupIIIA.CHR.AF3,GroupIIIA.CHR.AF10,
                             GroupIIIA.Acute.AF1,GroupIIIA.Acute.AF3,GroupIIIA.Acute.AF10,
                             GroupIIIB.CHR.AF1,GroupIIIB.CHR.AF3,GroupIIIB.CHR.AF10,
                             GroupIIIB.Acute.AF1,GroupIIIB.Acute.AF3,GroupIIIB.Acute.AF10))
names(All.AF) = c("N.Out","N.In")
All.AF$Total.N.Samples = nrow(Data.msPAF)
All.AF$Perc.in.Group = round((All.AF$N.In/All.AF$Total.N.Samples)*100,1)
All.AF$Case = dataset.name
All.AF$Exclussion.CAS = Exclusion.CAS
All.AF
write.csv(All.AF, file=paste("Table.CEFIC.MIAT.MCR.Group.Class.LOQ0", dataset.name,Exclusion.CAS, "csv", sep="."), row.names = T)

#Define Table for HI greater than HI limit #############################
#AF = 1, 3, 5 (Chronic), 1, 100 (Acute)

#Above TRHS CHR (HI)
CHR.AF1 = table(Data.msPAF$HI.HC05.Chronic.NOEC> 1)
CHR.AF3 = table(Data.msPAF$HI.HC05.Chronic.NOEC> 0.33)
CHR.AF10 = table(Data.msPAF$HI.HC05.Chronic.NOEC> 0.1)

#Above TRHS Acute (HI)
Acute.AF1 = table(Data.msPAF$HI.HC50.Acute.EC50> 1)
Acute.AF3 = table(Data.msPAF$HI.HC50.Acute.EC50> 0.33)
Acute.AF10 = table(Data.msPAF$HI.HC50.Acute.EC50> 0.1)


All.AF = as.data.frame(rbind(CHR.AF1,CHR.AF3,CHR.AF10,Acute.AF1,Acute.AF3,Acute.AF10))
names(All.AF) = c("N.Below.TRHS","N.Above.TRHS")
All.AF$Total.N.Samples = nrow(Data.msPAF)
All.AF$Perc.Avobe.TRHS = round((All.AF$N.Above.TRHS/All.AF$Total.N.Samples)*100,1)
All.AF

#Add samples that are a concern for mxiture (HI), but not for individual chem (MaxHQ)
#Above TRHS CHR (HI)
CHR.AF1 = table(Data.msPAF$HI.HC05.Chronic.NOEC> 1 & Data.msPAF$MaxHQ.HC05.Chronic.NOEC<= 1 & Data.msPAF$MCR.HC05.Chronic.NOEC> 2)
CHR.AF3 = table(Data.msPAF$HI.HC05.Chronic.NOEC> 0.33 & Data.msPAF$MaxHQ.HC05.Chronic.NOEC<= 0.33 & Data.msPAF$MCR.HC05.Chronic.NOEC> 2)
CHR.AF10 = table(Data.msPAF$HI.HC05.Chronic.NOEC> 0.1 & Data.msPAF$MaxHQ.HC05.Chronic.NOEC<= 0.1 & Data.msPAF$MCR.HC05.Chronic.NOEC> 2)

Acute.AF1 = table(Data.msPAF$HI.HC50.Acute.EC50> 1 & Data.msPAF$MaxHQ.HC50.Acute.EC50 <= 1 & Data.msPAF$MCR.HC05.Chronic.NOEC> 2)
Acute.AF3 = table(Data.msPAF$HI.HC50.Acute.EC50> 0.33 & Data.msPAF$MaxHQ.HC50.Acute.EC50 <= 0.33 & Data.msPAF$MCR.HC05.Chronic.NOEC> 2)
Acute.AF10 = table(Data.msPAF$HI.HC50.Acute.EC50> 0.1 & Data.msPAF$MaxHQ.HC50.Acute.EC50 <= 0.1 & Data.msPAF$MCR.HC05.Chronic.NOEC> 2)


All.AF.1 = as.data.frame(rbind(CHR.AF1,CHR.AF3,CHR.AF10,Acute.AF1,Acute.AF3,Acute.AF10))
names(All.AF.1) = c("N.Below.TRHS.Mix","N.Above.TRHS.Mix")
#merge with previous table
All.AF = cbind(All.AF, All.AF.1)
All.AF$Perc.GroupIIIB = round((All.AF$N.Above.TRHS.Mix/All.AF$Total.N.Samples)*100,1)
All.AF$Case = dataset.name
All.AF$Exclussion.CAS = Exclusion.CAS
All.AF

write.csv(All.AF, file=paste("Table.SUmHI.TRHS",dataset.name,Exclusion.CAS, "csv", sep = "."), row.names = T)

#MCR & HI quantile Tables ##############################################################
#MCRs for quantile 0.5, 0.9, 0.95

#SUMMARY HAZARD INDEX QUANTILES
#CHR
CHR.All = round(quantile(Data.msPAF$HI.HC05.Chronic.NOEC, c(0.5,0.9,0.95), na.rm =T),1)
CHR.HI.AF1 = round(quantile(Data.msPAF$HI.HC05.Chronic.NOEC[Data.msPAF$HI.HC05.Chronic.NOEC>1], c(0.5,0.9,0.95), na.rm =T),1)
CHR.HI.AF3 = round(quantile(Data.msPAF$HI.HC05.Chronic.NOEC[Data.msPAF$HI.HC05.Chronic.NOEC>0.33], c(0.5,0.9,0.95), na.rm =T),1)
CHR.HI.AF10 = round(quantile(Data.msPAF$HI.HC05.Chronic.NOEC[Data.msPAF$HI.HC05.Chronic.NOEC>0.1], c(0.5,0.9,0.95), na.rm =T),1)

#Acute
#All
Acute.All = round(quantile(Data.msPAF$HI.HC50.Acute.EC50, c(0.5,0.9,0.95), na.rm =T),1)
Acute.HI.AF1 = round(quantile(Data.msPAF$HI.HC50.Acute.EC50[Data.msPAF$HI.HC50.Acute.EC50>1], c(0.5,0.9,0.95), na.rm =T),1)
Acute.HI.AF3 = round(quantile(Data.msPAF$HI.HC50.Acute.EC50[Data.msPAF$HI.HC50.Acute.EC50>0.33], c(0.5,0.9,0.95), na.rm =T),1)
Acute.HI.AF10 = round(quantile(Data.msPAF$HI.HC50.Acute.EC50[Data.msPAF$HI.HC50.Acute.EC50>0.1], c(0.5,0.9,0.95), na.rm =T),1)


All.AF = as.data.frame(rbind(CHR.All,CHR.HI.AF1,CHR.HI.AF3,CHR.HI.AF10,
                             Acute.All,Acute.HI.AF1,Acute.HI.AF3, Acute.HI.AF10))
#merge with previous table
All.AF$Case = dataset.name
All.AF$Exclussion.CAS = Exclusion.CAS
All.AF
write.csv(All.AF, file=paste("Table.HI",dataset.name,Exclusion.CAS, "csv", sep = "."), row.names = T)


# SUMMARY MCR QUantiles
#CHR
CHR.All = round(quantile(Data.msPAF$MCR.HC05.Chronic.NOEC, c(0.5,0.9,0.95), na.rm =T),1)
CHR.HI.AF1 = round(quantile(Data.msPAF$MCR.HC05.Chronic.NOEC[Data.msPAF$HI.HC05.Chronic.NOEC>1], c(0.5,0.9,0.95), na.rm =T),1)
CHR.HI.AF3 = round(quantile(Data.msPAF$MCR.HC05.Chronic.NOEC[Data.msPAF$HI.HC05.Chronic.NOEC>0.33], c(0.5,0.9,0.95), na.rm =T),1)
CHR.HI.AF10 = round(quantile(Data.msPAF$MCR.HC05.Chronic.NOEC[Data.msPAF$HI.HC05.Chronic.NOEC>0.1], c(0.5,0.9,0.95), na.rm =T),1)

#Acute
#All
Acute.All = round(quantile(Data.msPAF$MCR.HC50.Acute.EC50, c(0.5,0.9,0.95), na.rm =T),1)
Acute.HI.AF1 = round(quantile(Data.msPAF$MCR.HC50.Acute.EC50[Data.msPAF$HI.HC50.Acute.EC50>1], c(0.5,0.9,0.95), na.rm =T),1)
Acute.HI.AF3 = round(quantile(Data.msPAF$MCR.HC50.Acute.EC50[Data.msPAF$HI.HC50.Acute.EC50>0.33], c(0.5,0.9,0.95), na.rm =T),1)
Acute.HI.AF10 = round(quantile(Data.msPAF$MCR.HC50.Acute.EC50[Data.msPAF$HI.HC50.Acute.EC50>0.1], c(0.5,0.9,0.95), na.rm =T),1)


All.AF = as.data.frame(rbind(CHR.All,CHR.HI.AF1,CHR.HI.AF3,CHR.HI.AF10,
                             Acute.All,Acute.HI.AF1,Acute.HI.AF3, Acute.HI.AF10))
#merge with previous table
All.AF$Case = dataset.name
All.AF$Exclussion.CAS = Exclusion.CAS
All.AF
write.csv(All.AF, file=paste("Table.MCR",dataset.name,Exclusion.CAS, "csv", sep = "."), row.names = T)


# # What if scenario ########################################################################################
# # Apply a MAF of 2
# # Apply a MAF of 3
# # Apply a MAF of 4
# # Apply a MAF of 5
# # Check the number of samples in Group IIIB when are below 5%
#
# MAF.Variables =c("HI.HC50.Acute.EC50","HI.HC05.Chronic.NOEC","MaxHQ.HC50.Acute.EC50","MaxHQ.HC05.Chronic.NOEC")
# MAF = c(1,1.5,2,3,4,5,6,7,8,9,10)
# Table.What.If.MAF = NULL
# for (i in 1:length(MAF)){
#   MAFi = MAF[i]
#   Data.msPAF.1 = Data.msPAF
#   Data.msPAF.1[,names(Data.msPAF.1)%in%MAF.Variables] = Data.msPAF.1[,names(Data.msPAF.1)%in%MAF.Variables]/MAFi
#   #CHR
#   GroupIIIB.CHR.AF1 = table(Data.msPAF.1$HI.HC05.Chronic.NOEC> 1 & Data.msPAF.1$MaxHQ.HC05.Chronic.NOEC<=1 & Data.msPAF.1$MCR.HC05.Chronic.NOEC >2)
#   GroupIIIB.CHR.AF3 = table(Data.msPAF.1$HI.HC05.Chronic.NOEC> 0.33 & Data.msPAF.1$MaxHQ.HC05.Chronic.NOEC<=0.33 & Data.msPAF.1$MCR.HC05.Chronic.NOEC >2)
#   GroupIIIB.CHR.AF10 = table(Data.msPAF.1$HI.HC05.Chronic.NOEC> 0.1 & Data.msPAF.1$MaxHQ.HC05.Chronic.NOEC<= 0.1 & Data.msPAF.1$MCR.HC05.Chronic.NOEC >2)
#   #Acute
#   GroupIIIB.Acute.AF1 = table(Data.msPAF.1$HI.HC50.Acute.EC50> 1 & Data.msPAF.1$MaxHQ.HC50.Acute.EC50<=1 & Data.msPAF.1$MCR.HC50.Acute.EC50 >2)
#   GroupIIIB.Acute.AF3 = table(Data.msPAF.1$HI.HC50.Acute.EC50> 0.33 & Data.msPAF.1$MaxHQ.HC50.Acute.EC50<=0.33 & Data.msPAF.1$MCR.HC50.Acute.EC50 >2)
#
#   All.AF = as.data.frame(rbind(GroupIIIB.CHR.AF1,GroupIIIB.CHR.AF3,GroupIIIB.CHR.AF10,
#                                GroupIIIB.Acute.AF1,GroupIIIB.Acute.AF3))
#   #merge with previous table
#     names(All.AF) = c("N.Out","N.In")
#   All.AF$Type =rownames(All.AF)
#   All.AF$AF = c("AF1","AF3","AF10", "AF1","AF3")
#   All.AF$Exp.Type = c("CHR","CHR","CHR","Acute","Acute")
#   All.AF$Total.N.Samples = nrow(Data.msPAF.1)
#   All.AF$Perc.in.Group = round((All.AF$N.In/All.AF$Total.N.Samples)*100,1)
#
#   All.AF$MAF = MAFi
#   All.AF$Case = dataset.name
#   All.AF$Exclussion.CAS = Exclusion.CAS
#   All.AF
#   Table.What.If.MAF = rbind(Table.What.If.MAF,All.AF)
#   }
# Table.What.If.MAF =Table.What.If.MAF[,c(3,5,4,8,6,1,2,7,9)]
# Table.What.If.MAF
# write.csv(Table.What.If.MAF, file = paste("Table.What.If.MAF", dataset.name,Exclusion.CAS, "csv", sep="."), row.names = F)


##################################################################################################
# DATA ANALYSIS RESULTS PLOTS
###################################################################################################

## ECDF PLOTS ####################################################################################
#Quantiles for the plots
Quantile.MCR.HC05.Chronic.NOEC = quantile(Data.msPAF$MCR.HC05.Chronic.NOEC, c(0.5, 0.9, 0.95), na.rm = T)
Quantile.MCR.HC50.Acute.EC50 = quantile(Data.msPAF$MCR.HC50.Acute.EC50, c(0.5, 0.9, 0.95), na.rm = T)

Quantile.HI.HC05.Chronic.NOEC = quantile(Data.msPAF$HI.HC05.Chronic.NOEC, c(0.5, 0.9, 0.95), na.rm = T)
Quantile.HI.HC50.Acute.EC50 = quantile(Data.msPAF$HI.HC50.Acute.EC50, c(0.5, 0.9, 0.95), na.rm = T)

#ecdf (Quantile plots) for HI (Sum of all RQs) ################################################################
ecdf.Plots = list()
ecdf.Plot.1 = ggplot(data=Data.msPAF, aes(HI.HC05.Chronic.NOEC)) +
  stat_ecdf(pad = F, geom ="point") +
  geom_vline(xintercept = c(Quantile.HI.HC05.Chronic.NOEC), linetype = c(1,2,2), colour = c("black", "black","red")) +
  labs(x="HI.HC05.Chronic.NOEC", y="cumulative probability (p)", title= paste(paste0("HI.HC05.Chronic.NOEC  ","N = ",nrow(Data.msPAF)), dataset.name,Exclusion.CAS, sep = ", "))
# ecdf.Plot.1
ecdf.Plots[[1]] = ecdf.Plot.1
# ggsave(paste("comProb.HI.HC05.Chronic.NOEC", dataset.name,Exclusion.CAS,"pdf", sep = "."),width=6, height = 5, units = "in")

ecdf.Plot.2 = ggplot(data=Data.msPAF, aes(HI.HC50.Acute.EC50)) +
  stat_ecdf(pad = F, geom ="point") +
  geom_vline(xintercept = c(Quantile.MCR.HC50.Acute.EC50), linetype = c(1,2,2), colour = c("black", "black","red")) +
  labs(x="HI.HC50.Acute.EC50", y="cumulative probability (p)", title= paste(paste0("HI.HC50.Acute.EC50  ","N = ",nrow(Data.msPAF)), dataset.name,Exclusion.CAS, sep = ", "))
# ecdf.Plot.2
ecdf.Plots[[2]] = ecdf.Plot.2
# ggsave(paste("comProb.HI.HC50.Acute.EC50", dataset.name,Exclusion.CAS,"pdf", sep = "."),width=6, height = 5, units = "in")


# Arrange the plots using patchworks syntax
ecdf.Plots[[1]] + ecdf.Plots[[2]] +
  # plot_layout(guides = 'collect') +
  plot_annotation(tag_levels = 'A')
ggsave(paste("ecdf.HI.HC05NOEC_HC50EC50", dataset.name,Exclusion.CAS,"pdf", sep = "."),width=12, height = 5, units = "in")


#ecdf plots for MCR
ecdf.Plot.3 = ggplot(data=Data.msPAF, aes(MCR.HC05.Chronic.NOEC)) +
  stat_ecdf(pad = F, geom ="point") +
  geom_vline(xintercept = c(Quantile.MCR.HC05.Chronic.NOEC), linetype = c(1,2,2), colour = c("black", "black","red")) +
  labs(x="MCR.Chronic.NOEC", y="cumulative probability (p)", title= paste(paste0("MCR.Chronic.NOEC  ","N = ",nrow(Data.msPAF)), dataset.name,Exclusion.CAS, sep = ", "))
# ecdf.Plot.3
ecdf.Plots[[3]] = ecdf.Plot.3
# ggsave(paste("comProb.MCR.HC05.Chronic.NOEC", dataset.name,Exclusion.CAS,"pdf", sep = "."),width=6, height = 5, units = "in")

ecdf.Plot.4 = ggplot(data=Data.msPAF, aes(MCR.HC50.Acute.EC50)) +
  stat_ecdf(pad = F, geom ="point") +
  geom_vline(xintercept = c(Quantile.MCR.HC50.Acute.EC50), linetype = c(1,2,2), colour = c("black", "black","red")) +
  labs(x="MCR.Acute.EC50", y="cumulative probability (p)", title= paste(paste0("MCR.Acute.EC50  ","N = ",nrow(Data.msPAF)), dataset.name,Exclusion.CAS, sep = ", "))
# ecdf.Plot.4
ecdf.Plots[[4]] = ecdf.Plot.4
# ggsave(paste("comProb.MCR.HC50.Acute.EC50", dataset.name,Exclusion.CAS,"pdf", sep = "."),width=6, height = 5, units = "in")


# Arrange the plots using patchworks syntax
ecdf.Plots[[3]] + ecdf.Plots[[4]] +
  # plot_layout(guides = 'collect') +
  plot_annotation(tag_levels = 'A')

ggsave(paste("ecdf.MCR.HC05NOEC_HC50EC50", dataset.name,Exclusion.CAS,"pdf", sep = "."),width=12, height = 5, units = "in")

############################################################################
# Classical HI-MCR (CEFIC-MIAT DECISSION THREE PLOTS)

# MCR PLOTS In Linear scale

#CHR
SumHI.MCR = list()
Plot.3 = ggplot(data=Data.msPAF, aes(x = HI.HC05.Chronic.NOEC, y = MCR.HC05.Chronic.NOEC)) +
  geom_point() +
  labs(x="HI.HC05.Chronic.NOEC", y="MCR.Chronic.NOEC", title= paste("HI-MCR.Chronic.NOEC", dataset.name,Exclusion.CAS, sep =", "))
# Plot.3
SumHI.MCR[[1]] = Plot.3
# ggsave(paste("SumHI-MCR.HC05.Chronic.NOEC", dataset.name,Exclusion.CAS,"pdf", sep = "."),width=6, height = 5, units = "in")

#Acute
Plot.4 = ggplot(data=Data.msPAF, aes(x = HI.HC50.Acute.EC50, y =MCR.HC50.Acute.EC50)) +
  geom_point() +
  labs(x="SumHI.HC50.Acute.EC50", y="MCR.Acute.EC50", title= paste("HI-MCR.Acute.EC50", dataset.name,Exclusion.CAS, sep =", "))
# Plot.4
SumHI.MCR[[2]] = Plot.4
# ggsave(paste("SumHI-MCR.HC50.Acute.EC50", dataset.name,Exclusion.CAS,"pdf", sep = "."),width=6, height = 5, units = "in")

#Combine
# Arrange the plots using patchworks syntax
SumHI.MCR[[1]] + SumHI.MCR[[2]] +
  # plot_layout(guides = 'collect') +
  plot_annotation(tag_levels = 'A')
ggsave(paste("SumHI-MCR.HC05NOEC_HC05EC50", dataset.name,Exclusion.CAS,"pdf", sep = "."),width=12, height = 5, units = "in")

# MCR PLOTS IN log10 (HI) ###########################################################################

#Define the boundary funtions that needs to be plotted
# MRC = HI #single chemical vs mixtures concenr
# HI = 1 (reference for concern)
# MRC = 2 #mixtures concenr vs no mixtures concer.

fun.1 = function(x) (x)

Plot.3 = ggplot(data=Data.msPAF, aes(x = HI.HC05.Chronic.NOEC, y = MCR.HC05.Chronic.NOEC)) +
  # geom_segment(x = 1, y = 2, xend = 2, yend = 2, colour = "blue") +
  geom_point(alpha=0.3) +
  # geom_vline(xintercept = c(0.1, 0.33, 1), colour = c("red", "red", "blue"), linetype =c(2,2,1)) +
  geom_vline(xintercept = c(1), colour = c("blue"), linetype =c(1)) +
  geom_hline(yintercept = 2, colour = "blue") +
  stat_function(fun = fun.1, colour = "blue") +
  scale_x_log10(limits = c(10^-6, 200)) +
  labs(x="log10(HI.HC05.Chronic.NOEC)", y="MCR.Chronic.NOEC", title= paste("HI-MCR.Chronic.NOEC", dataset.name,Exclusion.CAS, sep =", ")) +
  coord_cartesian(ylim = c(1, 7))
# Plot.3
SumHI.MCR[[3]] = Plot.3
# ggsave(paste("log10SumHI-MCR.HC05.Chronic.NOEC", dataset.name,Exclusion.CAS,"pdf", sep = "."),width=6, height = 5, units = "in")

#Acute
Plot.4 = ggplot(data=Data.msPAF, aes(x = HI.HC50.Acute.EC50, y =MCR.HC50.Acute.EC50)) +
  geom_point(alpha=0.3) +
  # geom_vline(xintercept = c(0.1, 0.33, 1), colour = c("red", "red", "blue"), linetype =c(2,2,1)) +
  geom_vline(xintercept = c(1), colour = c("blue"), linetype =c(1)) +
  geom_hline(yintercept = 2, colour = "blue") +
  stat_function(fun = fun.1, colour = "blue") +
  scale_x_log10(limits = c(10^-6, 200)) +
  coord_cartesian(ylim = c(1, 7)) +
  labs(x="log10(HI.HC50.Acute.EC50)", y="MCR.HC50.Acute.EC50", title= paste("HI-MCR.Acute.EC50", dataset.name,Exclusion.CAS, sep =", "))
# Plot.4
SumHI.MCR[[4]] = Plot.4
# ggsave(paste("log10SumHI-MCR.HC50.Acute.EC50", dataset.name,Exclusion.CAS,"pdf", sep = "."),width=6, height = 5, units = "in")

#Combine
# Arrange the plots using patchworks syntax
SumHI.MCR[[3]] + SumHI.MCR[[4]] +
  # plot_layout(guides = 'collect') +
  plot_annotation(tag_levels = 'A')
ggsave(paste("log10SumHI-MCR.HC05NOEC_HC05EC50", dataset.name,Exclusion.CAS,"pdf", sep = "."),width=12, height = 5, units = "in")

##################################################################################
# PLOTS MCR as a function of N.Measured and N.Detect
MCR.Meas.Det = list()
#CHR
#Measured
Plot.5 = ggplot(data=Data.msPAF, aes(x = N.Measured, y =MCR.HC05.Chronic.NOEC)) +
  geom_point(alpha=0.3,stat = "unique") +
  geom_hline(yintercept = 2, colour = "blue") +
  geom_quantile(method = "rqss", quantiles = c(0.1, 0.5, 0.95), lambda = 1, colour = "red") +
  # facet_wrap(~Study.Type, scales = "fixed") +
  # coord_cartesian(xlim = c(-3, 3)) +
  labs(x="N.Measured", y="MCR.HC05.Chronic.NOEC", title= paste("N.Measured-MCR.HC05.Chronic.NOEC", dataset.name,Exclusion.CAS, sep =", "))
# Plot.5
MCR.Meas.Det[[1]] = Plot.5
# ggsave(paste("N.Measured-MCR.HC05.Chronic.NOEC", dataset.name,Exclusion.CAS,"pdf", sep = "."),width=6, height = 5, units = "in")

#Detected
Plot.6 = ggplot(data=Data.msPAF, aes(x = N.Detected, y =MCR.HC05.Chronic.NOEC)) +
  geom_point(alpha=0.3, stat = "unique") +
  geom_hline(yintercept = 2, colour = "blue") +
  geom_quantile(method = "rqss", quantiles = c(0.1, 0.5, 0.95), colour = "red") +
  # geom_quantile(quantiles = c(0.1, 0.5, 0.95), lambda = 1, colour = "red") +
  # facet_wrap(~Study.Type, scales = "fixed") +
  # coord_cartesian(xlim = c(-3, 3)) +
  labs(x="N.Detected", y="MCR.HC05.Chronic.NOEC", title= paste("N.Detected-MCR.HC05.Chronic.NOEC", dataset.name,Exclusion.CAS, sep =", "))
# Plot.6
MCR.Meas.Det[[2]] = Plot.6
# ggsave(paste("N.Detected-MCR.HC05.Chronic.NOEC", dataset.name,Exclusion.CAS,"pdf", sep = "."),width=6, height = 5, units = "in")

#Acute
Plot.7 = ggplot(data=Data.msPAF, aes(x = N.Measured, y =MCR.HC50.Acute.EC50)) +
  geom_point(alpha=0.3,stat = "unique") +
  geom_hline(yintercept = 2, colour = "blue") +
  geom_quantile(method = "rqss", quantiles = c(0.1, 0.5, 0.95), lambda = 1, colour = "red") +
  # facet_wrap(~Study.Type, scales = "fixed") +
  # coord_cartesian(xlim = c(-3, 3)) +
  labs(x="N.Measured", y="MCR.HC50.Acute.EC50", title= paste("N.Measured-MCR.HC50.Acute.EC50", dataset.name,Exclusion.CAS, sep =", "))
# Plot.7
MCR.Meas.Det[[3]] = Plot.7
# ggsave(paste("N.Measured-MCR.HC50.Acute.EC50", dataset.name,Exclusion.CAS,"pdf", sep = "."),width=6, height = 5, units = "in")


#Detected
Plot.8 = ggplot(data=Data.msPAF, aes(x = N.Detected, y =MCR.HC50.Acute.EC50)) +
  geom_point(alpha=0.3,stat = "unique") +
  geom_hline(yintercept = 2, colour = "blue") +
  geom_quantile(method = "rqss", quantiles = c(0.1, 0.5, 0.95), lambda = 1, colour = "red") +
  # facet_wrap(~Study.Type, scales = "fixed") +
  # coord_cartesian(xlim = c(-3, 3)) +
  labs(x="N.Detected", y="MCR.HC50.Acute.EC50", title= paste("N.Detected-MCR.HC50.Acute.EC50", dataset.name,Exclusion.CAS, sep = ", "))
# Plot.8
MCR.Meas.Det[[4]] = Plot.8
# ggsave(paste("N.Detected-MCR.HC50.Acute.EC50", dataset.name,Exclusion.CAS,"pdf", sep = "."),width=6, height = 5, units = "in")


#Combine
# Arrange the plots using patchworks syntax
MCR.Meas.Det[[1]] + MCR.Meas.Det[[3]] + MCR.Meas.Det[[2]] + MCR.Meas.Det[[4]] +
  # plot_layout(guides = 'collect') +
  plot_layout(ncol = 2) +
  plot_annotation(tag_levels = 'A')
ggsave(paste("N.Measured_N.Detected-MCR.HC05NOEC_HC05EC50", dataset.name,Exclusion.CAS,"pdf", sep = "."),width=12, height = 10, units = "in")

####################################################################################
# PLOTS HI vs N.Measured, N.Detected
SumHI.Meas.Det = list()
#CHR
Plot.9 = ggplot(data=Data.msPAF, aes(x = N.Measured.1, y =HI.HC05.Chronic.NOEC)) +
  geom_point(alpha=0.3,stat = "unique") +
  geom_hline(yintercept = 1, colour = "blue") +
  geom_quantile(method = "rqss", quantiles = c(0.1, 0.5, 0.95), lambda = 1, colour = "red") +
  scale_y_log10() +
  # facet_wrap(~Study.Type, scales = "fixed") +
  # coord_cartesian(xlim = c(-3, 3)) +
  labs(x="N.Measured", y="log10(HI.HC05.Chronic.NOEC)", title= paste("N.Measured-log10(HI.HC05.Chronic.NOEC)", dataset.name,Exclusion.CAS, sep= ", "))
# Plot.9
SumHI.Meas.Det [[1]] = Plot.9

Plot.10 = ggplot(data=Data.msPAF, aes(x = N.Detected.1, y =HI.HC05.Chronic.NOEC)) +
  geom_point(alpha=0.3,stat = "unique") +
  geom_hline(yintercept = 1, colour = "blue") +
  geom_quantile(method = "rqss", quantiles = c(0.1, 0.5, 0.95), lambda = 1, colour = "red") +  # stat_ecdf(pad = F, geom ="step") +
  scale_y_log10() +
  # facet_wrap(~Study.Type, scales = "fixed") +
  # coord_cartesian(xlim = c(-3, 3)) +
  labs(x="N.Detected", y="log10(HI.HC05.Chronic.NOEC)", title= paste("N.Detected-log10(HI.HC05.Chronic.NOEC)", dataset.name,Exclusion.CAS, sep = ", "))
# Plot.10
SumHI.Meas.Det [[2]] = Plot.10

#Acute
Plot.11 = ggplot(data=Data.msPAF, aes(x = N.Measured.1, y =HI.HC50.Acute.EC50)) +
  geom_point(alpha=0.3,stat = "unique") +
  geom_hline(yintercept = 1, colour = "blue") +
  geom_quantile(method = "rqss", quantiles = c(0.1, 0.5, 0.95), lambda = 1, colour = "red") +  # stat_ecdf(pad = F, geom ="step") +
  scale_y_log10() +
  # facet_wrap(~Study.Type, scales = "fixed") +
  # coord_cartesian(xlim = c(-3, 3)) +
  labs(x="N.Measured", y="log10(HI.HC50.Acute.EC50)", title= paste("N.Measured-log10(HI.HC50.Acute.EC50)", dataset.name,Exclusion.CAS, sep=", "))
# Plot.11
SumHI.Meas.Det [[3]] = Plot.11


#Acute
Plot.12 = ggplot(data=Data.msPAF, aes(x = N.Detected.1, y =HI.HC50.Acute.EC50)) +
  geom_point(alpha=0.3,stat = "unique") +
  geom_hline(yintercept = 1, colour = "blue") +
  geom_quantile(method = "rqss", quantiles = c(0.1, 0.5, 0.95), lambda = 1, colour = "red") +  # stat_ecdf(pad = F, geom ="step") +
  scale_y_log10() +
  # facet_wrap(~Study.Type, scales = "fixed") +
  # coord_cartesian(xlim = c(-3, 3)) +
  labs(x="N.Detected", y="log10(HI.HC50.Acute.EC50)", title= paste("N.Detected-log10(HI.HC50.Acute.EC50)", dataset.name,Exclusion.CAS, sep = ", "))
# Plot.12
SumHI.Meas.Det [[4]] = Plot.12


#Combine
# Arrange the plots using patchworks syntax
SumHI.Meas.Det[[1]] + SumHI.Meas.Det[[3]] + SumHI.Meas.Det[[2]] + SumHI.Meas.Det[[4]] +
  # plot_layout(guides = 'collect') +
  plot_layout(ncol = 2) +
  plot_annotation(tag_levels = 'A')
ggsave(paste("N.Measured_N.Detected-SumHI.HC05NOEC_HC05EC50", dataset.name,Exclusion.CAS,"pdf", sep = "."),width=12, height = 10, units = "in")

######################################################################################
# PLOT HI vs MaxHQ
Plot.13 = ggplot(data=Data.msPAF, aes(x = MaxHQ.HC05.Chronic.NOEC, y =HI.HC05.Chronic.NOEC)) +
  geom_point(alpha=0.3,stat = "unique") +
  # geom_hline(yintercept = 2, colour = "blue") +
  # geom_quantile(method = "rqss", quantiles = c(0.1, 0.5, 0.95), lambda = 1, colour = "red") +  # stat_ecdf(pad = F, geom ="step") +
  scale_y_log10() +
  scale_x_log10() +
  # facet_wrap(~Study.Type, scales = "fixed") +
  # coord_cartesian(xlim = c(-3, 3)) +
  labs(x="log10(MaxHQ.HC05.Chronic.NOEC)", y="log10(HI.HC05.Chronic.NOEC)", title= paste("Max.HI vs HI.HC05(Chronic.NOEC)", dataset.name,Exclusion.CAS, sep =", "))
# Plot.13
ggsave(paste("Max.HI vs HI.HC05.Chronic.NOEC", dataset.name,Exclusion.CAS,"pdf", sep = "."),width=6, height = 5, units = "in")


### SUMMARIES, CHEMICAL DETECTION, HQ AND RISK BY CAS ##########################################################
# Reduce dataset to the variables needed
var.set = c("CAS","monitoringSiteIdentifier", "AboveLOQ", "Site.Year", "HQ.Acute", "HQ.Chronic")
Data.AggBySiteID.4 = Data.AggBySiteID.3a[,names(Data.AggBySiteID.3a)%in%var.set]
Data.AggBySiteID.4 = merge(Data.AggBySiteID.4, Data.msPAF[,names(Data.msPAF)%in%c("monitoringSiteIdentifier","Site.Year","HI.HC50.Acute.EC50","HI.HC05.Chronic.NOEC")], by = c("monitoringSiteIdentifier","Site.Year"), all.x = T)
Data.AggBySiteID.4$Perc.Risk.Chr = 100*(Data.AggBySiteID.4$HQ.Chronic/Data.AggBySiteID.4$HI.HC05.Chronic.NOEC)
Data.AggBySiteID.4$Perc.Risk.Acute = 100*(Data.AggBySiteID.4$HQ.Acute/Data.AggBySiteID.4$HI.HC50.Acute.EC50)

# Aggregate by CAS
# N. Measured
# N. Detected
# HQ.CHR Q25, Q50, Q75, Q95
# Risk.Perc.CHR Q25, Q50, Q75, Q95
# HQ.Acute Q25, Q50, Q75, Q95
# Risk.Perc.Acute Q25, Q50, Q75, Q95

Sum.measured =  aggregate(Site.Year  ~ CAS , data=Data.AggBySiteID.4, length)
Sum.detected =  aggregate(HQ.Acute  ~ CAS , data=Data.AggBySiteID.4, function (x) {sum(x>0)})
Sum.Quantiles.HQ.Acute = as.data.frame(as.matrix(aggregate(HQ.Acute  ~ CAS , data=Data.AggBySiteID.4, function (x) {quantile(x, probs = c(0.25,0.5,0.75,0.95))})))
Sum.Quantiles.Risk.Perc.Acute = as.data.frame(as.matrix(aggregate(Perc.Risk.Acute  ~ CAS , data=Data.AggBySiteID.4, function (x) {quantile(x, probs = c(0.25,0.5,0.75,0.95))})))
Sum.Quantiles.HQ.CHR = as.data.frame(as.matrix(aggregate(HQ.Chronic  ~ CAS , data=Data.AggBySiteID.4, function (x) {quantile(x, probs = c(0.25,0.5,0.75,0.95))})))
Sum.Quantiles.Risk.Perc.CHR = as.data.frame(as.matrix(aggregate(Perc.Risk.Chr  ~ CAS , data=Data.AggBySiteID.4, function (x) {quantile(x, probs = c(0.25,0.5,0.75,0.95))})))

# Merge all data
Sum.merged = merge(Sum.measured,Sum.detected, by = c("CAS"))
Sum.merged = merge(Sum.merged,Sum.Quantiles.HQ.CHR, by = c("CAS"))
Sum.merged = merge(Sum.merged,Sum.Quantiles.Risk.Perc.CHR, by = c("CAS"))
Sum.merged = merge(Sum.merged,Sum.Quantiles.HQ.Acute, by = c("CAS"))
Sum.merged = merge(Sum.merged,Sum.Quantiles.Risk.Perc.Acute, by = c("CAS"))

# Names
names(Sum.merged)[c(2,3)] = c("N.Measured", "N.Detected")
names(Sum.merged) = gsub("%", "P", names(Sum.merged))

# From factor to numeric
for (i in 2:19){
  Sum.merged[,i] = as.numeric(as.character(Sum.merged[,i]))
}

# Add chemical name
Sum.merged = merge(Sum.merged, Data.SSD.1[names(Data.SSD.1)%in%c("CAS","CAS.","Substance","PrimaryMoA")], by.x = "CAS", by.y = "CAS.", all.x = T)
Sum.merged = droplevels(Sum.merged)

# Order the dataset (by CHR.RISK.Q75)
Sum.merged = Sum.merged[order(-Sum.merged$HQ.Chronic.50P,Sum.merged$N.Detected),]
Sum.merged = Sum.merged[,c(1,20,21,2:19,22)]

# Nature of the chemical
Sum.merged$CAS.Type = NA
Sum.merged$CAS.Type[Sum.merged$CAS%in%CAS.Metals$CAS] = "Metal"
Sum.merged$CAS.Type[Sum.merged$CAS%in%CAS.Priority$CAS] = "Priority.Non-Metal"
Sum.merged$CAS.Type[!Sum.merged$CAS%in%c(CAS.Priority$CAS,CAS.Metals$CAS)] = "Other"

#####################################################################################
# HI - Measure/Detection plots
Sum.merged.plot = Sum.merged

#Add dummy variable for labeling
#95P
Sum.merged.plot$label.CHR.95P = NA
Sum.merged.plot$label.CHR.95P[Sum.merged.plot$HQ.Chronic.95P>0.1] = as.character(Sum.merged.plot$Substance [Sum.merged.plot$HQ.Chronic.95P>0.1])
Sum.merged.plot$label.Acute.95P = NA
Sum.merged.plot$label.Acute.95P[Sum.merged.plot$HQ.Acute.95P>0.1] = as.character(Sum.merged.plot$Substance [Sum.merged.plot$HQ.Acute.95P>0.1])
#50P
Sum.merged.plot$label.CHR.50P = NA
Sum.merged.plot$label.CHR.50P[Sum.merged.plot$HQ.Chronic.50P>0.1] = as.character(Sum.merged.plot$Substance [Sum.merged.plot$HQ.Chronic.50P>0.1])
Sum.merged.plot$label.Acute.50P = NA
Sum.merged.plot$label.Acute.50P[Sum.merged.plot$HQ.Acute.50P>0.1] = as.character(Sum.merged.plot$Substance [Sum.merged.plot$HQ.Acute.50P>0.1])

#Labels for % Risk
#P95
Sum.merged.plot$label.CHR.95P.Perc = NA
Sum.merged.plot$label.CHR.95P.Perc[Sum.merged.plot$Perc.Risk.Chr.95P>10] = as.character(Sum.merged.plot$Substance [Sum.merged.plot$Perc.Risk.Chr.95P>10])
Sum.merged.plot$label.Acute.95P.Perc = NA
Sum.merged.plot$label.Acute.95P.Perc[Sum.merged.plot$Perc.Risk.Acute.95P>10] = as.character(Sum.merged.plot$Substance [Sum.merged.plot$Perc.Risk.Acute.95P>10])
#P50
Sum.merged.plot$label.CHR.50P.Perc = NA
Sum.merged.plot$label.CHR.50P.Perc[Sum.merged.plot$Perc.Risk.Chr.50P>10] = as.character(Sum.merged.plot$Substance [Sum.merged.plot$Perc.Risk.Chr.50P>10])
Sum.merged.plot$label.Acute.50P.Perc = NA
Sum.merged.plot$label.Acute.50P.Perc[Sum.merged.plot$Perc.Risk.Acute.50P>10] = as.character(Sum.merged.plot$Substance [Sum.merged.plot$Perc.Risk.Acute.50P>10])


# SCATTER PLOTS HQ (MEC/PNEC) ###############################################################

#P95----------------------------------------------------------------------------------------
Scatter.Plots = list()
S.Plot.1<-ggplot(Sum.merged.plot,aes(x=N.Measured,y=HQ.Chronic.95P, label = label.CHR.95P))+
  geom_point(aes(colour = CAS.Type))+
  geom_text(angle = 45, hjust = 0, nudge_x = 0.05, size = 3) +
  # geom_text() +
  # geom_hline(yintercept = c(0.1, 0.01), linetype = c(2,3)) +
  geom_hline(yintercept = c(1, 0.1, 0.001, 0.00001), linetype = c(1,2,2,2)) +
   scale_x_log10()+
  # scale_y_log10()+
  theme_classic()+
  theme(plot.margin = unit(c(0.1,0.1,0.1,0.1), "cm"), plot.title = element_text(size = 10), strip.text.x =element_text(size = 14), rect = element_blank(), panel.border=element_rect(size=2,fill=NA, colour='black'),
        axis.text.y = element_text(size = 9), axis.text.x = element_text(size = 14), axis.title.x=element_text(size=12), axis.title.y=element_text(size=12), legend.text=element_text(size=14), legend.title=element_text(size=14),
        legend.key.size=unit(1, "cm"), legend.position="right", legend.box.background = element_rect(size=1,fill=NA))+
  labs(title= paste(paste0("HQ.Chronic.95P  ","N.CAS = ",nrow(Sum.merged.plot)), dataset.name,Exclusion.CAS, sep = ", "))
Scatter.Plots[[1]] = S.Plot.1
# S.Plot.1
# ggsave(paste("Scatter.CAS.HQ.Chronic.95P-N.Measured", dataset.name,Exclusion.CAS,"pdf", sep = "."),width=6, height = 5, units = "in")

S.Plot.2<-ggplot(Sum.merged.plot,aes(x=N.Detected,y=HQ.Chronic.95P, label = label.CHR.95P))+
  geom_point(aes(colour = CAS.Type))+
  geom_text(angle = 45, hjust = 0, nudge_x = 0.05, size = 3) +
  # geom_text() +
  # geom_hline(yintercept = c(0.1, 0.01), linetype = c(2,3)) +
  geom_hline(yintercept = c(1, 0.1, 0.001, 0.00001), linetype = c(1,2,2,2)) +
  scale_x_log10()+
  # scale_y_log10()+
  theme_classic()+
  theme(plot.margin = unit(c(0.1,0.1,0.1,0.1), "cm"), plot.title = element_text(size = 10), strip.text.x =element_text(size = 14), rect = element_blank(), panel.border=element_rect(size=2,fill=NA, colour='black'),
        axis.text.y = element_text(size = 9), axis.text.x = element_text(size = 14), axis.title.x=element_text(size=12), axis.title.y=element_text(size=12), legend.text=element_text(size=14), legend.title=element_text(size=14),
        legend.key.size=unit(1, "cm"), legend.position="right", legend.box.background = element_rect(size=1,fill=NA))+
  labs(title= paste(paste0("HQ.Chronic.95P  ","N.CAS = ",nrow(Sum.merged.plot)), dataset.name,Exclusion.CAS, sep = ", "))
Scatter.Plots[[2]] = S.Plot.2
# S.Plot.2
# ggsave(paste("Scatter.CAS.HQ.Chronic.95P-N.Detected", dataset.name,Exclusion.CAS,"pdf", sep = "."),width=6, height = 5, units = "in")

S.Plot.3<-ggplot(Sum.merged.plot,aes(x=N.Measured,y=HQ.Acute.95P, label = label.Acute.95P))+
  geom_point(aes(colour = CAS.Type))+
  geom_text(angle = 45, hjust = 0, nudge_x = 0.05, size = 3) +
  # geom_text() +
  # geom_hline(yintercept = c(0.1, 0.01), linetype = c(2,3)) +
  geom_hline(yintercept = c(1, 0.1, 0.001, 0.00001), linetype = c(1,2,2,2)) +
  scale_x_log10()+
  # scale_y_log10()+
  theme_classic()+
  theme(plot.margin = unit(c(0.1,0.1,0.1,0.1), "cm"), plot.title = element_text(size = 10), strip.text.x =element_text(size = 14), rect = element_blank(), panel.border=element_rect(size=2,fill=NA, colour='black'),
        axis.text.y = element_text(size = 9), axis.text.x = element_text(size = 14), axis.title.x=element_text(size=12), axis.title.y=element_text(size=12), legend.text=element_text(size=14), legend.title=element_text(size=14),
        legend.key.size=unit(1, "cm"), legend.position="right", legend.box.background = element_rect(size=1,fill=NA))+
  labs(x="N.Measured",y="HQ.Acute.95P", title= paste(paste0("HQ.Acute.95P  ","N.CAS = ",nrow(Sum.merged.plot)), dataset.name,Exclusion.CAS, sep = ", "))
Scatter.Plots[[3]] = S.Plot.3
# S.Plot.3
# ggsave(paste("Scatter.CAS.HQ.Acute.95P-N.Measured", dataset.name,Exclusion.CAS,"pdf", sep = "."),width=6, height = 5, units = "in")

S.Plot.4<-ggplot(Sum.merged.plot,aes(x=N.Detected,y=HQ.Acute.95P, label = label.Acute.95P))+
  geom_point(aes(colour = CAS.Type))+
  geom_text(angle = 45, hjust = 0, nudge_x = 0.05, size = 3) +
  # geom_text() +
  # geom_hline(yintercept = c(0.1, 0.01), linetype = c(2,3)) +
  geom_hline(yintercept = c(1, 0.1, 0.001, 0.00001), linetype = c(1,2,2,2)) +
  scale_x_log10()+
  # scale_y_log10()+
  theme_classic()+
  theme(plot.margin = unit(c(0.1,0.1,0.1,0.1), "cm"), plot.title = element_text(size = 10), strip.text.x =element_text(size = 14), rect = element_blank(), panel.border=element_rect(size=2,fill=NA, colour='black'),
        axis.text.y = element_text(size = 9), axis.text.x = element_text(size = 14), axis.title.x=element_text(size=12), axis.title.y=element_text(size=12), legend.text=element_text(size=14), legend.title=element_text(size=14),
        legend.key.size=unit(1, "cm"), legend.position="right", legend.box.background = element_rect(size=1,fill=NA))+
  labs(title= paste(paste0("HQ.Acute.95P  ","N.CAS = ",nrow(Sum.merged.plot)), dataset.name,Exclusion.CAS, sep = ", "))
Scatter.Plots[[4]] = S.Plot.4
# S.Plot.4
# ggsave(paste("Scatter.CAS.HQ.Acute.95P-N.Detected", dataset.name,Exclusion.CAS,"pdf", sep = "."),width=6, height = 5, units = "in")

#Combine
# Arrange the plots using patchworks syntax
Scatter.Plots[[2]] + Scatter.Plots[[1]] + Scatter.Plots[[4]] + Scatter.Plots[[3]] +
  plot_layout(guides = 'collect') +
  plot_layout(ncol = 2) +
  plot_annotation(tag_levels = 'A')
ggsave(paste("Scatter.CAS.HQ.95P.CHR.Acute-N.Measure-N.Detected", dataset.name,Exclusion.CAS,"pdf", sep = "."),width=12, height = 10, units = "in")


# 50P -------------------------------------------------------------------
S.Plot.5<-ggplot(Sum.merged.plot,aes(x=N.Measured,y=HQ.Chronic.50P, label = label.CHR.50P))+
  geom_point(aes(colour = CAS.Type))+
  geom_text(angle = 45, hjust = 0, nudge_x = 0.05, size = 3) +
  # geom_text() +
  # geom_hline(yintercept = c(0.1, 0.01), linetype = c(2,3)) +
  geom_hline(yintercept = c(1, 0.1, 0.001, 0.00001), linetype = c(1,2,2,2)) +
  scale_x_log10()+
  # scale_y_log10()+
  theme_classic()+
  theme(plot.margin = unit(c(0.1,0.1,0.1,0.1), "cm"), plot.title = element_text(size = 10), strip.text.x =element_text(size = 14), rect = element_blank(), panel.border=element_rect(size=2,fill=NA, colour='black'),
        axis.text.y = element_text(size = 9), axis.text.x = element_text(size = 14), axis.title.x=element_text(size=12), axis.title.y=element_text(size=12), legend.text=element_text(size=14), legend.title=element_text(size=14),
        legend.key.size=unit(1, "cm"), legend.position="right", legend.box.background = element_rect(size=1,fill=NA))+
  labs(title= paste(paste0("HQ.Chronic.50P  ","N.CAS = ",nrow(Sum.merged.plot)), dataset.name,Exclusion.CAS, sep = ", "))
Scatter.Plots[[5]] = S.Plot.5
# S.Plot.5
# ggsave(paste("Scatter.CAS.HQ.Chronic.50P-N.Measured", dataset.name,Exclusion.CAS,"pdf", sep = "."),width=6, height = 5, units = "in")

S.Plot.6<-ggplot(Sum.merged.plot,aes(x=N.Detected,y=HQ.Chronic.50P, label = label.CHR.50P))+
  geom_point(aes(colour = CAS.Type))+
  geom_text(angle = 45, hjust = 0, nudge_x = 0.05, size = 3) +
  # geom_text() +
  # geom_hline(yintercept = c(0.1, 0.01), linetype = c(2,3)) +
  geom_hline(yintercept = c(1, 0.1, 0.001, 0.00001), linetype = c(1,2,2,2)) +
  scale_x_log10()+
  # scale_y_log10()+
  theme_classic()+
  theme(plot.margin = unit(c(0.1,0.1,0.1,0.1), "cm"), plot.title = element_text(size = 10), strip.text.x =element_text(size = 14), rect = element_blank(), panel.border=element_rect(size=2,fill=NA, colour='black'),
        axis.text.y = element_text(size = 9), axis.text.x = element_text(size = 14), axis.title.x=element_text(size=12), axis.title.y=element_text(size=12), legend.text=element_text(size=14), legend.title=element_text(size=14),
        legend.key.size=unit(1, "cm"), legend.position="right", legend.box.background = element_rect(size=1,fill=NA))+
  labs(title= paste(paste0("HQ.Chronic.50P  ","N.CAS = ",nrow(Sum.merged.plot)), dataset.name,Exclusion.CAS, sep = ", "))
Scatter.Plots[[6]] = S.Plot.6
# S.Plot.6
# ggsave(paste("Scatter.CAS.HQ.Chronic.50P-N.Detected", dataset.name,Exclusion.CAS,"pdf", sep = "."),width=6, height = 5, units = "in")

S.Plot.7<-ggplot(Sum.merged.plot,aes(x=N.Measured,y=HQ.Acute.50P, label = label.Acute.50P))+
  geom_point(aes(colour = CAS.Type))+
  geom_text(angle = 45, hjust = 0, nudge_x = 0.05, size = 3) +
  # geom_text() +
  # geom_hline(yintercept = c(0.1, 0.01), linetype = c(2,3)) +
  geom_hline(yintercept = c(1, 0.1, 0.001, 0.00001), linetype = c(1,2,2,2)) +
  scale_x_log10()+
  # scale_y_log10()+
  theme_classic()+
  theme(plot.margin = unit(c(0.1,0.1,0.1,0.1), "cm"), plot.title = element_text(size = 10), strip.text.x =element_text(size = 14), rect = element_blank(), panel.border=element_rect(size=2,fill=NA, colour='black'),
        axis.text.y = element_text(size = 9), axis.text.x = element_text(size = 14), axis.title.x=element_text(size=12), axis.title.y=element_text(size=12), legend.text=element_text(size=14), legend.title=element_text(size=14),
        legend.key.size=unit(1, "cm"), legend.position="right", legend.box.background = element_rect(size=1,fill=NA))+
  labs(x="N.Measured",y="HQ.Acute.50P", title= paste(paste0("HQ.Acute.50P  ","N.CAS = ",nrow(Sum.merged.plot)), dataset.name,Exclusion.CAS, sep = ", "))
Scatter.Plots[[7]] = S.Plot.7
# S.Plot.7
# ggsave(paste("Scatter.CAS.HQ.Acute.50P-N.Measured", dataset.name,Exclusion.CAS,"pdf", sep = "."),width=6, height = 5, units = "in")

S.Plot.8<-ggplot(Sum.merged.plot,aes(x=N.Detected,y=HQ.Acute.50P, label = label.Acute.50P))+
  geom_point(aes(colour = CAS.Type))+
  geom_text(angle = 45, hjust = 0, nudge_x = 0.05, size = 3) +
  # geom_text() +
  # geom_hline(yintercept = c(0.1, 0.01), linetype = c(2,3)) +
  geom_hline(yintercept = c(1, 0.1, 0.001, 0.00001), linetype = c(1,2,2,2)) +
  scale_x_log10()+
  # scale_y_log10()+
  theme_classic()+
  theme(plot.margin = unit(c(0.1,0.1,0.1,0.1), "cm"), plot.title = element_text(size = 10), strip.text.x =element_text(size = 14), rect = element_blank(), panel.border=element_rect(size=2,fill=NA, colour='black'),
        axis.text.y = element_text(size = 9), axis.text.x = element_text(size = 14), axis.title.x=element_text(size=12), axis.title.y=element_text(size=12), legend.text=element_text(size=14), legend.title=element_text(size=14),
        legend.key.size=unit(1, "cm"), legend.position="right", legend.box.background = element_rect(size=1,fill=NA))+
  labs(title= paste(paste0("HQ.Acute.50P  ","N.CAS = ",nrow(Sum.merged.plot)), dataset.name,Exclusion.CAS, sep = ", "))
Scatter.Plots[[8]] = S.Plot.8
# S.Plot.8
# ggsave(paste("Scatter.CAS.HQ.Acute.50P-N.Detected", dataset.name,Exclusion.CAS,"pdf", sep = "."),width=6, height = 5, units = "in")


#Combine
# Arrange the plots using patchworks syntax
Scatter.Plots[[6]] + Scatter.Plots[[5]] + Scatter.Plots[[8]] + Scatter.Plots[[7]] +
  plot_layout(guides = 'collect') +
  plot_layout(ncol = 2) +
  plot_annotation(tag_levels = 'A')
ggsave(paste("Scatter.CAS.HQ.50P.CHR.Acute-N.Measure-N.Detected", dataset.name,Exclusion.CAS,"pdf", sep = "."),width=12, height = 10, units = "in")

#################################################################
# Hazard Index Dot-plots by CAS
#################################################################
# from wide to long to be able to do dot-plots
# Restrict to relevant variables
# Order by HQ.CHRP95

# Create a new variables name that includes N measured and N detected
Sum.merged.plot$Substance.1 = paste(Sum.merged.plot$Substance,paste(Sum.merged.plot$N.Measured, Sum.merged.plot$N.Detected, sep="|"), sep=" ")

# Order by HQ.CHRP95
Sum.merged.plot = Sum.merged.plot[order(Sum.merged.plot$HQ.Chronic.95P, decreasing = T),]

# Order the levels
Sum.merged.plot$Substance = factor(Sum.merged.plot$Substance, levels = as.character (Sum.merged.plot$Substance))
Sum.merged.plot$Substance.1 = factor(Sum.merged.plot$Substance.1, levels = as.character (Sum.merged.plot$Substance.1))

#save  dataset summary
write.csv(Sum.merged.plot, file = paste("Summary.All.ByCAS.Occurrence.Risk",dataset.name,Exclusion.CAS,"csv", sep="."), row.names = F)



# Restrict to the top 50 chemicals
Sum.merged.plot = Sum.merged.plot[c(1:80),]

# Inverse order for plotting
Sum.merged.plot = Sum.merged.plot[order(Sum.merged.plot$HQ.Chronic.95P, decreasing = F),]

# Order the levels
Sum.merged.plot$Substance = factor(Sum.merged.plot$Substance, levels = as.character (Sum.merged.plot$Substance))
Sum.merged.plot$Substance.1 = factor(Sum.merged.plot$Substance.1, levels = as.character (Sum.merged.plot$Substance.1))


Sum.merged.plot.1 = Sum.merged.plot[,!names(Sum.merged.plot)%in%c("Perc.Risk.Chr.25P" , "Perc.Risk.Chr.50P", "Perc.Risk.Chr.75P",
                                                                  "Perc.Risk.Chr.95P", "Perc.Risk.Acute.25P", "Perc.Risk.Acute.50P", "Perc.Risk.Acute.75P",
                                                                 "Perc.Risk.Acute.95P" , "label.CHR.95P",
                                                                 "label.Acute.95P" ,  "label.CHR.50P" ,  "label.Acute.50P" , "label.CHR.95P.Perc",
                                                                 "label.Acute.95P.Perc", "label.CHR.50P.Perc","label.Acute.50P.Perc")]
Sum.merged.plot.1 = melt(Sum.merged.plot.1, id.vars = c("CAS" ,"CAS.y" , "Substance","Substance.1", "N.Measured","N.Detected", "PrimaryMoA" ,"CAS.Type"), variable.name ="variable", value.name ="HQ")
Sum.merged.plot.1$N.Measured.100 = Sum.merged.plot.1$N.Measured>99
# Set 0 values to a very low number
Sum.merged.plot.1$HQ[Sum.merged.plot.1$HQ==0] = min(Sum.merged.plot.1$HQ[Sum.merged.plot.1$HQ!=0], na.rm = T)

# Order levels
Sum.merged.plot.1$variable = factor(Sum.merged.plot.1$variable, levels = c("HQ.Chronic.95P","HQ.Chronic.75P","HQ.Chronic.50P","HQ.Chronic.25P",
                                                                          "HQ.Acute.95P","HQ.Acute.75P","HQ.Acute.50P","HQ.Acute.25P"))

# Dot plot
Dot.plot.1 = ggplot(subset(Sum.merged.plot.1, variable%in%c("HQ.Chronic.25P", "HQ.Chronic.50P", "HQ.Chronic.75P", "HQ.Chronic.95P")),aes(x=Substance.1, y=HQ)) +
  # geom_point(aes(color = CAS.Type)) +
  geom_point(aes(color = variable), position=position_jitter(h=0.05)) +
  # geom_point() +
  geom_hline(yintercept = c(1, 0.1, 0.01), linetype = c(1,2,2)) +
  scale_y_log10()+
  theme_classic()+
  theme(plot.margin = unit(c(0.1,0.1,0.1,0.1), "cm"), plot.title = element_text(size = 12), strip.text.x =element_text(size = 14), rect = element_blank(), panel.border=element_rect(size=2,fill=NA, colour='black'),
        axis.text.y = element_text(size = 12), axis.text.x = element_text(size = 9), axis.title.x=element_text(size=12), axis.title.y=element_text(size=14), legend.text=element_text(size=14), legend.title=element_text(size=14),
        legend.key.size=unit(1, "cm"), legend.position="right", legend.box.background = element_rect(size=1,fill=NA))+
  labs(x=NULL,y="HQ", title= paste("CAS.Drivers ", dataset.name,Exclusion.CAS, sep=" ")) +
  coord_flip()
Dot.plot.1
ggsave(paste("DotPlot.Risk.Quantiles.CHR", dataset.name,Exclusion.CAS,"pdf", sep = "."),width=14, height = 14, units = "in")



#########################################################################################################
# FRACTION OF HI AND RISK ACCOUNTED BY 1, 2, 3, REST OF COMPONENTS
#######################################################################################################

# Focus only on stations with HI > 1

# CHR
Data.AggBySiteID.5.CHR = Data.AggBySiteID.4[Data.AggBySiteID.4$HI.HC05.Chronic.NOEC> 0.1,]
Data.AggBySiteID.5.CHR = droplevels(Data.AggBySiteID.5.CHR)
# loop over station ID.Year combinations
  # order by HI
  # restrict to 3 rows
  # sum HI, calculate different for "all other components
  # label 1, 2, 3, Other
  # retain CAS
# rbind with previous stations
Data.drivers = NULL
for ( i in 1:length(unique(Data.AggBySiteID.5.CHR$Site.Year))){
  Site.Year_i = unique(Data.AggBySiteID.5.CHR$Site.Year)[i]
  Data_i = Data.AggBySiteID.5.CHR[Data.AggBySiteID.5.CHR$Site.Year==Site.Year_i,]
  Data_i = Data_i[order(Data_i$HQ.Chronic, decreasing = TRUE),]
  nrow_i = nrow(Data_i)
  if (nrow_i>3) {Data_i = Data_i[c(1:3),c(1:4,6,8)]} else{Data_i = Data_i[c(1:nrow_i),c(1:4,6,8)]}
  if (nrow_i==1) {Data_i$Component = 1}
  if (nrow_i==2) {Data_i$Component = c(1,2)}
  if (nrow_i==3) {Data_i$Component = c(1,2,3)}
  if (nrow_i>3) {Data_i = Data_i[c(1:3,3),]; Data_i$Component = c(1,2,3,4);
                                 Data_i[4,5] = Data_i$HI.HC05.Chronic.NOEC[1] - (sum(Data_i$HQ.Chronic)-Data_i$HQ.Chronic[4]);
                                 Data_i[4,3] = NA}
  Data.drivers = rbind(Data.drivers,Data_i)
}

# Arrange factor variables
Data.drivers$Component = as.character(Data.drivers$Component)
Data.drivers$Component[Data.drivers$Component=="4"] = "All.Other"
Data.drivers$Component = factor(Data.drivers$Component, levels = c("1", "2","3","All.Other"))
Data.drivers$Perc.Risk = 100*(Data.drivers$HQ.Chronic/Data.drivers$HI.HC05.Chronic.NOEC)

Perc.Contrib.Top3.HI.1 = t(data.frame(sapply(split(Data.drivers$Perc.Risk,Data.drivers$Component), summary, simplify = T)))

# CAS drivers order by feq
CAS.Drivers = as.data.frame(table(Data.drivers$CAS))
CAS.Drivers$N.Site.Year = length(unique(Data.AggBySiteID.5.CHR$Site.Year))
CAS.Drivers$N.Sites = length(unique(Data.AggBySiteID.5.CHR$monitoringSiteIdentifier))
CAS.Drivers$Freq.Perc = round(100* (CAS.Drivers$Freq/CAS.Drivers$N.Site.Year),1)
names(CAS.Drivers)[1] = "CAS"
CAS.Drivers = merge(CAS.Drivers, Data.SSD[,c(1,2,3)], by.x = "CAS", by.y = "CAS.", all.x = T)
CAS.Drivers = CAS.Drivers[order(CAS.Drivers$Freq, decreasing = F),]
CAS.Drivers$Substance = factor(CAS.Drivers$Substance, levels = as.character(CAS.Drivers$Substance))
CAS.Drivers = CAS.Drivers[order(CAS.Drivers$Freq, decreasing = T),]


head(CAS.Drivers)

# Save CAS drivers
write.csv(CAS.Drivers, file= paste("HI.1.Sites_Top3_CAS.Drivers", dataset.name,Exclusion.CAS,"csv", sep = "."))

#####################################################################
# PLOT BARCHART OF THE HI AND CONTRIBUTIONS OF TOP 3 COMPONENTS
#####################################################################


# Generate ordering
Data_Ave = aggregate(HI.HC05.Chronic.NOEC  ~ Site.Year , data=Data.drivers, max, na.rm=TRUE)
Data_Ave = Data_Ave[order(Data_Ave$HI.HC05.Chronic.NOEC, decreasing =TRUE),]
#apply new levels order
Data.drivers$Site.Year = factor(Data.drivers$Site.Year, levels = Data_Ave$Site.Year)

# Bar stacked plot

# HI values
B.Plots = list()
B.Plot.1<-ggplot(Data.drivers,aes(x=Site.Year, y=HQ.Chronic, fill = Component))+
  geom_bar(stat = "identity", position = position_stack(reverse = TRUE)) +
  geom_hline(yintercept = c(1), linetype = c(2)) +
  # scale_y_log10()+
  theme_classic()+
  theme(plot.margin = unit(c(0.1,0.1,0.1,0.1), "cm"), plot.title = element_text(size = 12), strip.text.x =element_text(size = 14), rect = element_blank(), panel.border=element_rect(size=2,fill=NA, colour='black'),
        axis.text.y = element_text(size = 9), axis.text.x = element_blank(), axis.title.x=element_text(size=12), axis.title.y=element_text(size=12), legend.text=element_text(size=14), legend.title=element_text(size=14),
        legend.key.size=unit(0.5, "cm"), legend.position="right", legend.box.background = element_rect(size=1,fill=NA))+
  labs(x="Sample",y="HI.Chronic", title= paste("HI.Chronic  ", dataset.name,Exclusion.CAS, "N(HI>1)=", CAS.Drivers [1,3], "Sites", sep=" "))
B.Plot.1
B.Plots[[1]] = B.Plot.1
ggsave(paste("BarPlot.HI.ByTopComponents.CHR", dataset.name,Exclusion.CAS,"pdf", sep = "."),width=7, height = 5, units = "in")


# Perc.Risk
B.Plot.2<-ggplot(Data.drivers,aes(x=Site.Year, y=Perc.Risk, fill = Component))+
  geom_bar(stat = "identity", position = position_stack(reverse = TRUE)) +
  geom_hline(yintercept = c(1, 95), linetype = c(2,2)) +
  # scale_y_log10()+
  theme_classic()+
  theme(plot.margin = unit(c(0.1,0.1,0.1,0.1), "cm"), plot.title = element_text(size = 12), strip.text.x =element_text(size = 14), rect = element_blank(), panel.border=element_rect(size=2,fill=NA, colour='black'),
        axis.text.y = element_text(size = 9), axis.text.x = element_blank(), axis.title.x=element_text(size=12), axis.title.y=element_text(size=12), legend.text=element_text(size=14), legend.title=element_text(size=14),
        legend.key.size=unit(0.5, "cm"), legend.position="right", legend.box.background = element_rect(size=1,fill=NA))+
  labs(x="Sample",y="HI.Chronic.Perc.Risk (%)", title= paste("HI.Chronic  ", dataset.name,Exclusion.CAS, "N(HI>1)=", CAS.Drivers [1,3], "Sites", sep=" "))
B.Plot.2
B.Plots[[1]] = B.Plot.1
ggsave(paste("BarPlot.Perc.Risk.ByTopComponents.CHR", dataset.name,Exclusion.CAS,"pdf", sep = "."),width=7, height = 5, units = "in")


######################################################################################################
#  Boxplot contributions by component
######################################################################################################

Box.Plot1<-ggplot(Data.drivers,aes(x=Component,y=Perc.Risk))+
  geom_boxplot(aes(fill=Component))+
  theme_classic()+
  theme(plot.margin = unit(c(0.1,0.1,0.1,0.1), "cm"), plot.title = element_text(size = 12), strip.text.x =element_text(size = 14), rect = element_blank(), panel.border=element_rect(size=2,fill=NA, colour='black'),
        axis.text.y = element_text(size = 9), axis.text.x = element_text(size = 14), axis.title.x=element_text(size=12), axis.title.y=element_text(size=12), legend.text=element_text(size=14), legend.title=element_text(size=14),
        legend.key.size=unit(1, "cm"), legend.position="right", legend.box.background = element_rect(size=1,fill=NA))+
  labs(x="Component",y="Perc.Risk (%)", title= paste("HI.Chronic.Perc.Risk(%)", dataset.name,Exclusion.CAS, "N(HI>1)=", CAS.Drivers [1,3], "Sites", sep=" "))
  # coord_flip(ylim = c(min(Data$log10Q, na.rm = T), max(Data$log10Q, na.rm = T)))
Box.Plot1
ggsave(paste("BoxPlot.Perc.Risk.ByTopComponents.CHR", dataset.name,Exclusion.CAS,"pdf", sep = "."),width=7, height = 5, units = "in")


######################################################################################################
# Bar Plot most frequent CAS drivers
######################################################################################################
Bar.Plot2 = ggplot(subset(CAS.Drivers, Freq.Perc>1),aes(x=Substance, y=Freq.Perc))+
  geom_bar(stat = "identity", fill= "grey75")+
  theme_classic()+
  theme(plot.margin = unit(c(0.1,0.1,0.1,0.1), "cm"), plot.title = element_text(size = 12), strip.text.x =element_text(size = 14), rect = element_blank(), panel.border=element_rect(size=2,fill=NA, colour='black'),
        axis.text.y = element_text(size = 12), axis.text.x = element_text(size = 9), axis.title.x=element_text(size=12), axis.title.y=element_text(size=14), legend.text=element_text(size=14), legend.title=element_text(size=14),
        legend.key.size=unit(1, "cm"), legend.position="right", legend.box.background = element_rect(size=1,fill=NA))+
  labs(x=NULL,y="Freq (%)", title= paste("CAS.Drivers ", dataset.name,Exclusion.CAS, "N(HI>1)=", CAS.Drivers [1,3], "Sites", sep=" ")) +
coord_flip()
Bar.Plot2
ggsave(paste("BarPlot.TopDrivers.CHR", dataset.name,Exclusion.CAS,"pdf", sep = "."),width=14, height = 14, units = "in")

# #Colour by group
# Bar.Plot3 = ggplot(CAS.Drivers,aes(x=Substance, y=Freq.Perc, fill = ))+
#   geom_bar(stat = "identity", fill= "grey75")+
#   theme_classic()+
#   theme(plot.margin = unit(c(0.1,0.1,0.1,0.1), "cm"), plot.title = element_text(size = 12), strip.text.x =element_text(size = 14), rect = element_blank(), panel.border=element_rect(size=2,fill=NA, colour='black'),
#         axis.text.y = element_text(size = 12), axis.text.x = element_text(size = 9), axis.title.x=element_text(size=12), axis.title.y=element_text(size=14), legend.text=element_text(size=14), legend.title=element_text(size=14),
#         legend.key.size=unit(1, "cm"), legend.position="right", legend.box.background = element_rect(size=1,fill=NA))+
#   labs(x=NULL,y="Freq (%)", title= paste("CAS.Drivers ", dataset.name,Exclusion.CAS, "N(HI>1)=", CAS.Drivers [1,3], "Sites", sep=" ")) +
#   coord_flip()
# Bar.Plot2
# ggsave(paste("BarPlot.TopDrivers.CHR", dataset.name,Exclusion.CAS,"pdf", sep = "."),width=14, height = 7, units = "in")


#Save workspace
save.image(paste0("Waterbase_Explore_", dataset.name,"_",Exclusion.CAS,".RData"))

