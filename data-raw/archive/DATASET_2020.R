## code to prepare `DATASET` dataset goes here

#EXPLORE WATERBASE WATERQUALITY DATABASE. COMPUTE MCR AND MAF. PRODUCE PLOTS AND SUMMARY TABLES
#ismaelm.rodeapalomares@bayer.com
#03/26/2021

#cleaning the environemt
rm(list=ls())

#Define and set paths
Dir_Data = "~/s3raw/WB-Zhenglei/"


#Required packages

library(tidyverse)


#####################################################################################################
# LOAD DATASETS
#####################################################################################################

# Load CAS for heavy metals and priority pollutants
CAS.Metals = read.csv(paste0(Dir_Data,"CAS.Metals.csv"))
CAS.Metals$CAS = as.character(CAS.Metals$CAS)
CAS.Priority = read.csv(paste0(Dir_Data,"EU_WFD_45PriorityPollutants.csv"))
CAS.Priority$CAS = gsub("CAS_|-|_", "", CAS.Priority$CAS)
CAS.Priority$CAS = as.character(CAS.Priority$CAS)

# Load Endpoint data
#L.Posthuma SSDs
data(Data.SSD)
Data.SSD <- read.csv(paste0(Dir_Data,"Copy of etc4373-sup-0002-supmat.csv"))

#Load data. Basic exploration of stations information

Stations = read.csv(paste0(Dir_Data,"Waterbase_v2020_1_S_WISE6_SpatialObject_DerivedData.csv")) # v2020

names(Stations)
# names(Stations)[1] = "monitoringSiteIdentifier"

#How many Stations?
length(unique(Stations$monitoringSiteIdentifier))
## note the current reading does not read in as factor but as characters, so changed levels into unique.
#60776
#How many have complete lat/long info?
sum(complete.cases(Stations[,c("lon", "lat")]))
#48563


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

Data.AggBySiteID = read.csv(paste0(Dir_Data,"Waterbase_v2020_1_T_WISE6_AggregatedData.csv")) #2020 #3510775 rows
dim(Data.AggBySiteID) ## [1] 3510775      31
# Data.AggBySiteID = Data.AggByWB.1
# rm(Data.AggByWB.1)
#2021 #3962763 rows


names(Data.AggBySiteID)[1] = "monitoringSiteIdentifier"

# How many of the sites are in the "Sites" dataset?
sum(unique(Data.AggBySiteID$monitoringSiteIdentifier)%in% unique(Stations$monitoringSiteIdentifier))
# 15990 Sites in 2021 out of 25974 unique monitoringSiteIdentifier
# 24229 Sites in 2019
# How many have lat long info?
sum(unique(Data.AggBySiteID$monitoringSiteIdentifier)%in%Stations$monitoringSiteIdentifier[complete.cases(Stations[names(Stations)%in%c("lon", "lat")])])

#21095 Sites have lat long info

head(Data.AggBySiteID)
names(Data.AggBySiteID)
# [1] "monitoringSiteIdentifier"             "monitoringSiteIdentifierScheme"
# [3] "parameterWaterBodyCategory"           "observedPropertyDeterminandCode"
# [5] "observedPropertyDeterminandLabel"     "procedureAnalysedMatrix"
# [7] "resultUom"                            "phenomenonTimeReferenceYear"
# [9] "parameterSamplingPeriod"              "procedureLOQValue"
# [11] "resultNumberOfSamples"                "resultQualityNumberOfSamplesBelowLOQ"
# [13] "resultQualityMinimumBelowLOQ"         "resultMinimumValue"
# [15] "resultQualityMeanBelowLOQ"            "resultMeanValue"
# [17] "resultQualityMaximumBelowLOQ"         "resultMaximumValue"
# [19] "resultQualityMedianBelowLOQ"          "resultMedianValue"
# [21] "resultStandardDeviationValue"         "procedureAnalyticalMethod"
# [23] "parameterSampleDepth"                 "resultObservationStatus"
# [25] "remarks"                              "metadata_versionId"
# [27] "metadata_beginLifeSpanVersion"        "metadata_statusCode"
# [29] "metadata_observationStatus"           "metadata_statements"
# [31] "UID"                                  "CAS"
summary(Data.AggBySiteID$phenomenonTimeReferenceYear)
# Min. 1st Qu.  Median    Mean 3rd Qu.    Max.
# 1931    2006    2010    2008    2012    2018
barplot(table(Data.AggBySiteID$phenomenonTimeReferenceYear))

# How many determinads included in the L.Posthuma SSDs?
# Prepare CAS numbers
Data.AggBySiteID$CAS = gsub("CAS_|-|_", "", Data.AggBySiteID$observedPropertyDeterminandCode)

#How many CAS numbers in SSDs?
sum(unique(Data.AggBySiteID$CAS)%in%as.character(unique(Data.SSD$CAS.)))
# 405 determinands have SSD data

# How many entries from the existing data we will be able to use?
CAS.SSD = unique(Data.AggBySiteID$CAS)[unique(Data.AggBySiteID$CAS)%in%as.character(unique(Data.SSD$CAS.))]
length(Data.AggBySiteID$CAS[Data.AggBySiteID$CAS%in%CAS.SSD])
#2254186 entries out of 3510775
length(Data.AggBySiteID$CAS[Data.AggBySiteID$CAS%in%CAS.SSD])/nrow(Data.AggBySiteID)
# 64 % of the data

#Which are the "CAS" not matched?
CAS.SSD.no = unique(Data.AggBySiteID$CAS)[!unique(Data.AggBySiteID$CAS)%in%as.character(unique(Data.SSD$CAS.))]
table(grepl("EE",CAS.SSD.no)) ## some of them starting with EEAs
# FALSE  TRUE
# 70    65
# 65 determinads have a weird code instead of the CAS number

# Further reduce by the type of sample analyze
table(Data.AggBySiteID$procedureAnalysedMatrix)
# W   W-DIS   W-SPM
# 3331289  179221     265
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
#               A       O
# 3013790  317499       0
Data.AggBySiteID =Data.AggBySiteID[Data.AggBySiteID$resultObservationStatus!="O",]

# metadata_observationStatus,Status of the record regarding its reliability.
# A: Normal record;
# U: Record with lower reliability;
# V: Unvalidated record"
# metadata_statements,Aditional information or statements regarding the reliability of the feature or record.
table(Data.AggBySiteID$metadata_observationStatus)
# A       U
# 3208444  122845
#98587 unreliable records
Data.AggBySiteID =Data.AggBySiteID[Data.AggBySiteID$metadata_observationStatus!="U",]

Data.AggBySiteID = droplevels(Data.AggBySiteID)

#metadata_statements
metadata_statements = unique(Data.AggBySiteID$metadata_statements)
# [1] ""
# [2] "NOTE_LEGACY: Measurement has been confirmed by country to be taken from a highly polluted area "
# [3] "NOTE_LEGACY: Outlier has been confirmed by country as correct value"
# [4] "NOTE_LEGACY: The parameterWaterBodyCategory changed from LW to RW based on the the monitoring site data"
# [5] "NOTE_LEGACY: The parameterWaterBodyCategory changed from RW to LW based on the the monitoring site data"
# [6] "NOTE_WBCAT_CHANGE: The parameterWaterBodyCategory has been changed to match the one officialy reported for the given spatial identifier (old: LW, new: RW)"
# [7] "NOTE_WBCAT_CHANGE: The parameterWaterBodyCategory has been changed to match the one officialy reported for the given spatial identifier (old: LW, new: TW)"table(Data.AggBySiteID$metadata_statements=="NOTE_LEGACY: Measurement has been confirmed by country to be taken from a highly polluted area ")
table(Data.AggBySiteID$metadata_statements!="NOTE_LEGACY: Measurement has been confirmed by country to be taken from a highly polluted area ")
# FALSE    TRUE
# 85    3208359
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
# 322  284808  390337 2523895    8997
Data.AggBySiteID =Data.AggBySiteID[Data.AggBySiteID$parameterWaterBodyCategory%in%c("LW", "RW"),] #2914232 entries

# CAREFULL, THIS SUBSET IS INTERIM, ONLY TO MAKE IT EASY TO MANIPULATE -------

# Subset for the AIs that were matched (Data.AggSiteID.1)
Data.AggBySiteID.1 = Data.AggBySiteID[Data.AggBySiteID$CAS%in%CAS.SSD,]; Data.AggBySiteID.1 = droplevels(Data.AggBySiteID.1)
Data.SSD.1 = Data.SSD[Data.SSD$CAS.%in%CAS.SSD,]; Data.SSD.1 = droplevels(Data.SSD.1)

# Units
table(Data.AggBySiteID.1$resultUom)
# mg/L    mg{NH4}/L   mg{P}/L   mg{S}/L      ug/L
# 95753    102134    184874        10   1374178

#Remove all but mg/L and ug/L and transform all to mg/L
Data.AggBySiteID.1 = Data.AggBySiteID.1[Data.AggBySiteID.1$resultUom%in%c("mg/L", "ug/L"),] #1469931
#transform to ug/L
Data.AggBySiteID.1$resultMinimumValue[Data.AggBySiteID.1$resultUom=="mg/L"] = Data.AggBySiteID.1$resultMinimumValue[Data.AggBySiteID.1$resultUom=="mg/L"]*1000 # to ug/L
Data.AggBySiteID.1$resultMeanValue[Data.AggBySiteID.1$resultUom=="mg/L"] = Data.AggBySiteID.1$resultMeanValue[Data.AggBySiteID.1$resultUom=="mg/L"]*1000 # to ug/L
Data.AggBySiteID.1$resultMedianValue[Data.AggBySiteID.1$resultUom=="mg/L"] = Data.AggBySiteID.1$resultMedianValue[Data.AggBySiteID.1$resultUom=="mg/L"]*1000 # to ug/L
Data.AggBySiteID.1$resultMaximumValue[Data.AggBySiteID.1$resultUom=="mg/L"] = Data.AggBySiteID.1$resultMaximumValue[Data.AggBySiteID.1$resultUom=="mg/L"]*1000 # to ug/L

# Change unit label
Data.AggBySiteID.1$resultUom[Data.AggBySiteID.1$resultUom=="mg/L"] = "ug/L"; Data.AggBySiteID.1 = droplevels(Data.AggBySiteID.1)

# Subset for complete data for Average and Maximun values
Data.AggBySiteID.1 = Data.AggBySiteID.1[!is.na(Data.AggBySiteID.1$resultMeanValue),]
Data.AggBySiteID.1 = Data.AggBySiteID.1[!is.na(Data.AggBySiteID.1$resultMaximumValue),]


###################################################################################
# SSD data quality and curation (origing, and number of taxa per SSD)----------
###################################################################################

table(Data.SSD.1$OrigenQuality.Acute.EC50) %>% View
#Acute
#AllData
#345 ==> I have 400
#ReadAcross
#2
#Potential to extrapolate acute from chronic
#3
#Chronic
table(Data.SSD.1$OrigenQuality.Chronic.NOEC)
#AllData
#330 ==> I have 382
#Potential to extrapolate Chronic NOEC SSD median concentration from poorly represented Acute
#20  ==> I have 23

#Number of taxa per SSD
summary(Data.SSD.1$X.Species.Acute.EC50)
#Acute
# Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's
#    1.00    8.50   16.00   24.15   26.00  210.00       3 # The NA are the ones needing extrapolation
hist(log10(Data.SSD.1$X.Species.Acute.EC50))
hist(Data.SSD.1$X.TaxClass.Acute.EC50)
# How many more than 3 organism
table(Data.SSD.1$X.Species.Acute.EC50>=3)
# FALSE  TRUE
# 20   327  ==> I have 23 and 379
#CHR
summary(Data.SSD.1$X.Species.Chronic.NOEC)
# Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's
#    1.00   10.00   15.00   19.65   24.00  182.00      20
hist(log10(Data.SSD.1$X.Species.Chronic.NOEC))
table(Data.SSD.1$X.Species.Chronic.NOEC>=3)
# FALSE  TRUE
# 5   325

# Remove chem that have less than 3 sp per SSD
Data.SSD.1 = Data.SSD.1[Data.SSD.1$X.Species.Acute.EC50>=3,]
Data.SSD.1 = Data.SSD.1[Data.SSD.1$X.Species.Chronic.NOEC>=3,]
Data.SSD.1 = Data.SSD.1[!is.na(Data.SSD.1$X.Species.Chronic.NOEC),]
Data.SSD.1 = Data.SSD.1[!is.na(Data.SSD.1$X.Species.Acute.EC50),] #325 chemicals


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
table(Data.SSD.1$X.TaxClass.Chronic.NOEC>=3)

# Final interim set of chemicals for analysis
CAS.SSD.1 = CAS.SSD[CAS.SSD%in%Data.SSD.1$CAS.]
Data.SSD.1 = Data.SSD.1[Data.SSD.1$CAS.%in%CAS.SSD.1,]; Data.SSD.1 = droplevels(Data.SSD.1)

# SSD data with Slope data
Data.SSD.1 = Data.SSD.1[!is.na(Data.SSD.1$X10LogSSDSlope.ug.L..SigmaChronic.NOEC),]
Data.SSD.1 = Data.SSD.1[!is.na(Data.SSD.1$X10LogSSDSlope.ug.L..SigmaAcute.EC50),]

#########################################################################
# Subset Water-base rivers based on available SSDs

Data.AggBySiteID.1 = Data.AggBySiteID.1[Data.AggBySiteID.1$CAS%in%CAS.SSD.1,];
Data.AggBySiteID.1 = droplevels(Data.AggBySiteID.1) #1679005 entries

summary(Data.SSD.1$X.Species.Chronic.NOEC)
summary(Data.SSD.1$X.Species.Acute.EC50)

##########################################################################
# Subset the dataset based on the number of measurements per CAS per year

#Number of samples
summary(Data.AggBySiteID.1$resultNumberOfSamples)
#Stations reporting number of samples
# Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's
#   1.000   4.000  10.000   8.703  12.000 366.000     310

# Remove stations do not reporting number of samples
Data.AggBySiteID.1 = Data.AggBySiteID.1[!is.na(Data.AggBySiteID.1$resultNumberOfSamples),]

# Stations reporting more than 5 samples per year to report annual aggregates
Data.AggBySiteID.1 = Data.AggBySiteID.1[Data.AggBySiteID.1$resultNumberOfSamples>=5,]

###########################################################################
# Remove unreliable entries based on missing/unreliable LOQs reporting

# Remove data for which LOQ is not reported
Data.AggBySiteID.1 = Data.AggBySiteID.1[!is.na(Data.AggBySiteID.1$procedureLOQValue),]

# Remove data with LOQ <= 0
Data.AggBySiteID.1 = Data.AggBySiteID.1[Data.AggBySiteID.1$procedureLOQValue>= 0,]

# N Samples below LOQ
summary(Data.AggBySiteID.1$resultQualityNumberOfSamplesBelowLOQ)
Data.AggBySiteID.1 = Data.AggBySiteID.1[!is.na(Data.AggBySiteID.1$resultQualityNumberOfSamplesBelowLOQ),]

#N samples below LOQ
summary(Data.AggBySiteID.1$resultQualityNumberOfSamplesBelowLOQ)
# Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's
#    0.00    6.00   11.00    9.24   12.00  366.00  283332

# Number of samples above LOQ
table(Data.AggBySiteID.1$resultNumberOfSamples > Data.AggBySiteID.1$resultQualityNumberOfSamplesBelowLOQ)
# FALSE   TRUE
# 440361 150429
# FALSE   TRUE
# 454015 160405
100*(440361/(440361+150429)) #74.5% of samples below LOQ; 30% actual measurements
# This is very important to see what to do wit BelowLOQ values!

#if N samples > 1, and N.detects > 2, then max > mean. Therefore, max > mean can be used to separate the samples with detects from those with non-detects
table(Data.AggBySiteID.1$resultMaximumValue>Data.AggBySiteID.1$resultMeanValue)
# FALSE   TRUE
# 577749 335279
# FALSE   TRUE
# 443404 171016
100*(429987/(429987+160803))
# 72.78 below LOQ, 37% avobe LOQ
# The number should be equal to the value in the previous logic


# Convert reported LOQs to ug/L
Data.AggBySiteID.1$procedureLOQValue[Data.AggBySiteID.1$resultUom=="mg/L"] = Data.AggBySiteID.1$procedureLOQValue[Data.AggBySiteID.1$resultUom=="mg/L"]*1000 # to ug/L
# Change unit label
Data.AggBySiteID.1$resultUom[Data.AggBySiteID.1$resultUom=="mg/L"] = "ug/L"; Data.AggBySiteID.1 = droplevels(Data.AggBySiteID.1)
#Remove useless variables from datase
usefull.var = c("monitoringSiteIdentifier","phenomenonTimeReferenceYear", "Site.Year", "resultMeanValue", "resultMaximumValue","CAS","N.Measured","N.Detected","procedureLOQValue", "AboveLOQ", "resultNumberOfSamples", "resultQualityNumberOfSamplesBelowLOQ")
Data.AggBySiteID.1 = Data.AggBySiteID.1[,names(Data.AggBySiteID.1)%in%usefull.var]

###########
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

###########################################################################
## RESTRICT THE DATASET BASED ON THE N CHEMICALS MEASURED PER STATION

# How many determinads measured per station and Year? ################################################
#variable to keep
var = c("monitoringSiteIdentifier","CAS","phenomenonTimeReferenceYear")
DataForNChem = Data.AggBySiteID.1[,names(Data.AggBySiteID.1)%in%c(var)]
measured.chem = aggregate(CAS ~ monitoringSiteIdentifier + phenomenonTimeReferenceYear, data =DataForNChem, length)
summary(measured.chem$CAS)
# Min. 1st Qu.  Median    Mean 3rd Qu.    Max.
# 1.00    2.00   22.00   31.54   45.00  271.00

# How many determinads measured per station and Year?
detected.chem = aggregate(CAS ~ monitoringSiteIdentifier + phenomenonTimeReferenceYear, data =DataForNChem[Data.AggBySiteID.1$resultNumberOfSamples>Data.AggBySiteID.1$resultQualityNumberOfSamplesBelowLOQ,], length)
summary(detected.chem$CAS)
# Min. 1st Qu.  Median    Mean 3rd Qu.    Max.
# 1.000   1.000   5.000   9.637  12.000 125.000

# Will only keep stations measuring at least 10 chemicals the same year
measured.chem = measured.chem[measured.chem$CAS>9,] #11668 StationID - Year combinations
## ==> I have 11745 instead of 11668
summary(measured.chem$CAS)
# Min. 1st Qu.  Median    Mean 3rd Qu.    Max.
# 10.00   18.00   36.00   42.94   53.00  271.00
summary(measured.chem$phenomenonTimeReferenceYear)
#1998 - 2018
length(unique(measured.chem$monitoringSiteIdentifier))
# 4609 Station IDs

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
# 11668 ==> 11745
table(measured.chem$N.Measured>50)
# FALSE  TRUE
# 8098  3570 ==>
# FALSE  TRUE
# 8079  3666
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
#571999 observations  ==> 594180
length(unique(Data.AggBySiteID.2$monitoringSiteIdentifier)) #4609 Site IDs ==> 4654
length(unique(Data.AggBySiteID.2$CAS)) #327 CAS ==> 348 CAS

Data.AggBySiteID.2$AboveLOQ = Data.AggBySiteID.2$resultNumberOfSamples > Data.AggBySiteID.2$resultQualityNumberOfSamplesBelowLOQ
summary(Data.AggBySiteID.2$AboveLOQ)
# Mode   FALSE    TRUE
# logical  432024  139975

#Remove useless variables from datase
usefull.var = c("monitoringSiteIdentifier","phenomenonTimeReferenceYear", "Site.Year", "resultMeanValue", "resultMaximumValue","CAS","N.Measured","N.Detected","procedureLOQValue", "AboveLOQ", "resultNumberOfSamples", "resultQualityNumberOfSamplesBelowLOQ")
Data.AggBySiteID.2 = Data.AggBySiteID.2[,names(Data.AggBySiteID.2)%in%usefull.var]
# v2021: 619600     12

stations <- Stations[Stations$monitoringSiteIdentifier %in% unique(Data.AggBySiteID.2$monitoringSiteIdentifier),]
usethis::use_data(stations,overwrite = T)
all(complete.cases(stations[,c("lon","lat")]))



usethis::use_data(Data.AggBySiteID.2,overwrite = T)
rm(Data.AggBySiteID)
rm(Data.AggBySiteID.1)
