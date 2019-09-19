library(data.table)
library(lubridate)

df <- read.csv("Data/data_16-9.csv")
df.img <- read.csv("Data/imaging_16-9.csv")

###First ct
df.img <- df.img[df.img$Imaging.Timepoint=="CT Early",] #CT Early
df.img <- data.frame(data.table(df.img)[,.(first(Imaging.SubduralHematomaSubacuteChronic),
                                first(Imaging.TraumaticSubarachnoidHemorrhage),
                                first(Imaging.EpiduralHematoma),
                                first(Imaging.SubduralHematomaAcute),
                                first(Imaging.SkullFracture),
                                first(Imaging.SubduralCollectionMixedDensity),
                                first(Imaging.CisternalCompression),
                                first(Imaging.MidlineShift),
                                first(Imaging.IntraventricularHemorrhage),
                                first(Imaging.MassLesion)), by=gupi]) #take first one
colnames(df.img) <- c("gupi", "sdmchron", "tsah", "edh", "sdhacute", "skullfrx","sdcolmixdens", "cistern", "midline", "intravhm", "mass")
df <- merge(df, df.img, by="gupi", all.x=TRUE)

##categorize
facvars<- colnames(df)[which(colnames(df)%in%c("InjuryHx.PresEmergencyServicePolice","InjuryHx.PresEmergencyServiceHelicopter",
                                               "InjuryHx.PresEmergencyServiceAmbuBasic", "InjuryHx.PresEmergencyServiceFirefighter",
                                               "InjuryHx.PresEmergencyServiceAmbuSpec", "InjuryHx.PresEmergencyServiceNone",
                                               "InjuryHx.PresTBIRef"  ,"InjuryHx.PresArrivalMethod",
                                               "InjuryHx.PresEmergencyCare","Subject.DateTimeInjReliable"  ,
                                               "InjuryHx.PresEmergencyCareSuppOxygen" , "InjuryHx.PresCirculationTreatmentCPR" ,
                                               "InjuryHx.PresCirculationTreatmentIVFluids" , "InjuryHx.PresEmergencyCareIntubation" ,
                                               "InjuryHx.PresCirculationTreatmentUnknown" ,"InjuryHx.PresCirculationTreatmentNone",
                                               "InjuryHx.PresEmergencyCareVentilation"  ,"InjuryHx.PresFirstOnSceneDepartUnknownTime",
                                               "InjuryHx.PresFirstOnSceneUnknownTime", "InjuryHx.PresERIntracranialSurg",
                                               "InjuryHx.PresIntubation", "InjuryHx.PresCTBrain",
                                                "InjuryHx.PresERExtracranialSurg","Subject.Sex",
                                               "Subject.PatientType", "Subject.SiteCode" ,
                                                 "InjuryHx.InjType","InjuryHx.InjPlace",
                                                "InjuryHx.InjCause"   , "InjuryHx.InjArea",                          
                                               "InjuryHx.EmergSurgInterventionsExtraCran",   "InjuryHx.EmergSurgInterventionsIntraCran",  
                                               "InjuryHx.PupilsBaselineDerived",  "InjuryHx.InjMech"  ,                        
                                                "sdmchron","tsah" ,"edh", "sdhacute","skullfrx" ,
                                               "sdcolmixdens" ,"cistern","midline" ,  "intravhm" ,"mass"))]

for(i in facvars){
  df[,i] <- factor(df[,i])
}

### replace 88/99 with NA
for(i in facvars){
  if("88"%in%levels(df[,i])|"99"%in%levels(df[,i])|""%in%levels(df[,i])){
    indices <- which(levels(df[,i])%in%c("88","99", ""))
    levels(df[,i])[indices] <- NA
  }
}

levels(df$InjuryHx.PresTBIRef) <- c("primary", "secondary")
levels(df$InjuryHx.PresArrivalMethod) <- c("ambulance", "helicopter", "mmt", "walk in")
levels(df$InjuryHx.PresEmergencyCare) <- c("none", "bystander", "bystander", "bystander", "paramedic", "nurse", "physician", "medical rescue team", NA)
levels(df$Subject.PatientType) <- c("ER", "ward", "ICU")
levels(df$InjuryHx.InjType) <- c("closed", "blast", "crush", "penetrating", "penetrating", "penetrating", "closed")
levels(df$InjuryHx.InjPlace) <- c("street", "home", "work/school", "sport", "military", "public location")
levels(df$InjuryHx.InjCause) <- c("RTI", "fall", NA, "violence", "violence", "suicide")
levels(df$InjuryHx.InjArea) <- c("urban", "rural")
levels(df$InjuryHx.InjMech) <- c(NA, "high", "high", "high", "high", "high", "high", "high", "high", "high",  
                                 "high", "high", NA, NA, "low", "low", "low", "high", NA, NA, 
                                 "low", "high", "low", "high", "high", "high", NA, "high", "high", "high",
                                 "high", "high", "high", "high", "high", "high", "high", "high", "high", "high",
                                 "high", "high", NA, NA, "low", "high", NA, "low", "low", "high",
                                 NA, NA, "low", "high", "low", "high", NA, "low", "high", "low",
                                 "high", "high") #if 1 or 7, then high, if 6 low, not any of these three missing

########## GCS recoding
levels(df$InjuryHx.GCSEDArrMotor) <- c(NA, 1:6, NA, NA,NA)
df$InjuryHx.GCSEDArrMotor <- as.numeric(df$InjuryHx.GCSEDArrMotor)
levels(df$InjuryHx.GCSEDArrScore) <- c(10:15, 3:9, NA,NA)
df$InjuryHx.GCSEDArrScore <- as.numeric(df$InjuryHx.GCSEDArrScore)

levels(df$InjuryHx.GCSPreHospBestMotor) <- c(NA, 1:6, NA, NA,NA)
df$InjuryHx.GCSPreHospBestMotor <- as.numeric(df$InjuryHx.GCSPreHospBestMotor)
levels(df$InjuryHx.GCSPreHospBestScore) <- c(10:15, 3:9, NA,NA)
df$InjuryHx.GCSPreHospBestScore <- as.numeric(df$InjuryHx.GCSPreHospBestScore)

save(df, file = "Data/cleaned.RData")

