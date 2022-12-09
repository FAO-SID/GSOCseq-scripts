############################################
#                                          #                                                                           
#                                          # 
#   GSOCseq: Script 3 Miami Model          #
#   NPP                                    #
#                                          #  
#   Isabel Luotto                          #
#   Contact: Isabel.Luotto@fao.org         #
#                                          # 
#                                          # 
############################################

#Initial setup -----------------------------------------------------------------
rm(list=ls())
gc()

# Working directory
#wd <- 'C:/Users/luottoi/Documents/GSOCseq'
wd <- 'C:/Users/hp/Documents/FAO/GSOCseq/OCB/GSOCseq_TM/'
setwd(wd)


# Load Packages 
library(terra)

# File paths
AOI_path <- 'INPUTS/AOI_POLYGON/NP.shp'

SOC_path <- 'INPUTS/SOC_MAP/THA_NP_SOC.tif'


# Open Anual Precipitation (mm) and Mean Anual Temperature (grades C) stacks

Temp<-rast("INPUTS/TERRA_CLIMATE/AverageTemperature_1981-2001.tif")
Prec<-rast("INPUTS/TERRA_CLIMATE/Precipitation_1981-2001.tif")

AOI<-vect(AOI_path)


# Temperature Annual Mean 

k<-1
TempList<-list()
#######loop for starts#########
for (i in 1:(dim(Temp)[3]/12)){

Temp1<-mean(Temp[[k:(k+11)]])
TempList[i]<-Temp1

k<-k+12
}
#######loop for ends##########
TempStack<-rast(TempList)
TempStack<-TempStack*0.1 # rescale to °C

#Annual Precipitation

k<-1
PrecList<-list()
########loop for starts#######
for (i in 1:20){

Prec1<-sum(Prec[[k:(k+11)]])
PrecList[i]<-Prec1

k<-k+12
}
########loop for ends#######
PrecStack<-rast(PrecList)

# Calculate eq 1 from MIAMI MODEL (g DM/m2/day)

NPP_Prec<-3000*(1-exp(-0.000664*PrecStack))

# Calculate eq 2 from MIAMI MODEL (g DM/m2/day)

NPP_temp<-3000/(1+exp(1.315-0.119*TempStack))

# Calculate eq 3 from MIAMI MODEL (g DM/m2/day)

NPP_MIAMI_List<-list()

########loop for starts#######
for (i in 1:20){
NPP_MIAMI_List[i]<-min(NPP_Prec[[i]],NPP_temp[[i]])
}
########loop for ends#######

NPP_MIAMI<-rast(NPP_MIAMI_List)

#NPP_MIAMI gDM/m2/year To tn DM/ha/year

NPP_MIAMI_tnDM_Ha_Year<-NPP_MIAMI*(1/100)

#NPP_MIAMI tn DM/ha/year To tn C/ha/year

NPP_MIAMI_tnC_Ha_Year<-NPP_MIAMI_tnDM_Ha_Year*0.5

# Save WORLD NPP MIAMI MODEL tnC/ha/year



writeRaster(NPP_MIAMI_tnC_Ha_Year,filename="INPUTS/NPP_TERRA/NPP_MIAMI_tnC_Ha_Year_STACK_81-00.tif",overwrite=TRUE)

#NPP_MIAMI_tnC_Ha_Year<-stack("NPP_MIAMI_tnC_Ha_Year_STACK_81-00.tif")

# NPP MEAN

NPP_MIAMI_MEAN_81_00<-mean(NPP_MIAMI_tnC_Ha_Year)


#Open FAO GSOC MAP 



SOC_MAP_AOI<-rast(SOC_path)

# Crop & mask



NPP_MIAMI_MEAN_81_00_AOI<-crop(NPP_MIAMI_MEAN_81_00,AOI)
NPP_MIAMI_MEAN_81_00_AOI<-resample(NPP_MIAMI_MEAN_81_00_AOI,SOC_MAP_AOI)
NPP_MIAMI_MEAN_81_00_AOI<-mask(NPP_MIAMI_MEAN_81_00_AOI,AOI)

writeRaster(NPP_MIAMI_MEAN_81_00_AOI,filename="INPUTS/NPP_TERRA/NPP_MIAMI_MEAN_81-00_AOI.tif",overwrite=TRUE)
writeRaster(NPP_MIAMI_MEAN_81_00,filename="INPUTS/NPP_TERRA/NPP_MIAMI_MEAN_81-00.tif",overwrite=TRUE)


#UNCERTAINTIES MINIMUM TEMP , PREC

Temp_min<-Temp*1.02
Prec_min<-Prec*0.95

# Temperature Annual Mean 

k<-1
TempList<-list()
########loop for starts#######
for (i in 1:20){

Temp1<-mean(Temp_min[[k:(k+11)]])
TempList[i]<-Temp1

k<-k+12
}
########loop for ends#######

TempStack<-rast(TempList)
TempStack<-TempStack*0.1 # rescale to °C

#Annual Precipitation

k<-1
PrecList<-list()

########loop for starts#######
for (i in 1:20){

Prec1<-sum(Prec_min[[k:(k+11)]])
PrecList[i]<-Prec1

k<-k+12
}
########loop for ends#######

PrecStack<-rast(PrecList)

# Calculate eq 1 from MIAMI MODEL (g DM/m2/day)

NPP_Prec<-3000*(1-exp(-0.000664*PrecStack))

# Calculate eq 2 from MIAMI MODEL (g DM/m2/day)

NPP_temp<-3000/(1+exp(1.315-0.119*TempStack))

# Calculate eq 3 from MIAMI MODEL (g DM/m2/day)

NPP_MIAMI_List<-list()

########loop for starts#######
for (i in 1:20){
NPP_MIAMI_List[i]<-min(NPP_Prec[[i]],NPP_temp[[i]])
}
########loop for ends#######

NPP_MIAMI<-rast(NPP_MIAMI_List)

#NPP_MIAMI gDM/m2/year To tn DM/ha/year

NPP_MIAMI_tnDM_Ha_Year<-NPP_MIAMI*(1/100)

#NPP_MIAMI tn DM/ha/year To tn C/ha/year

NPP_MIAMI_tnC_Ha_Year<-NPP_MIAMI_tnDM_Ha_Year*0.5

# Save WORLD NPP MIAMI MODEL tnC/ha/year



writeRaster(NPP_MIAMI_tnC_Ha_Year,filename="NPP_MIAMI_tnC_Ha_Year_STACK_81-00_MIN.tif",overwrite=TRUE)

# NPP MEAN

NPP_MIAMI_MEAN_81_00<-mean(NPP_MIAMI_tnC_Ha_Year)

# Crop & and mask



NPP_MIAMI_MEAN_81_00_AOI<-crop(NPP_MIAMI_MEAN_81_00,AOI)
NPP_MIAMI_MEAN_81_00_AOI<-resample(NPP_MIAMI_MEAN_81_00_AOI,SOC_MAP_AOI)
NPP_MIAMI_MEAN_81_00_AOI<-mask(NPP_MIAMI_MEAN_81_00_AOI,AOI)

writeRaster(NPP_MIAMI_MEAN_81_00_AOI,filename="INPUTS/NPP_TERRA/NPP_MIAMI_MEAN_81-00_AOI_MIN.tif",overwrite=TRUE)
writeRaster(NPP_MIAMI_MEAN_81_00,filename="INPUTS/NPP_TERRA/NPP_MIAMI_MEAN_81-00_MIN.tif",overwrite=TRUE)


#UNCERTAINTIES MAXIMUM TEMP , PREC

# Open Anual Precipitation (mm) and Mean Anual Temperature (grades C) stacks

Temp_max<-Temp*0.98
Prec_max<-Prec*1.05

# Temperature Annual Mean 

k<-1
TempList<-list()

########loop for starts#######
for (i in 1:20){

Temp1<-mean(Temp_max[[k:(k+11)]])
TempList[i]<-Temp1

k<-k+12
}
########loop for ends#######

TempStack<-rast(TempList)
TempStack<-TempStack*0.1 # rescale to °C

#Annual Precipitation

k<-1
PrecList<-list()

########loop for starts#######
for (i in 1:20){

Prec1<-sum(Prec_max[[k:(k+11)]])
PrecList[i]<-Prec1

k<-k+12
}
########loop for ends#######

PrecStack<-rast(PrecList)

# Calculate eq 1 from MIAMI MODEL (g DM/m2/day)

NPP_Prec<-3000*(1-exp(-0.000664*PrecStack))

# Calculate eq 2 from MIAMI MODEL (g DM/m2/day)

NPP_temp<-3000/(1+exp(1.315-0.119*TempStack))

# Calculate eq 3 from MIAMI MODEL (g DM/m2/day)

NPP_MIAMI_List<-list()

########loop for starts#######
for (i in 1:20){
NPP_MIAMI_List[i]<-min(NPP_Prec[[i]],NPP_temp[[i]])
}
########loop for ends#######


NPP_MIAMI<-rast(NPP_MIAMI_List)

#NPP_MIAMI gDM/m2/year To tn DM/ha/year

NPP_MIAMI_tnDM_Ha_Year<-NPP_MIAMI*(1/100)

#NPP_MIAMI tn DM/ha/year To tn C/ha/year

NPP_MIAMI_tnC_Ha_Year<-NPP_MIAMI_tnDM_Ha_Year*0.5

# Save NPP MIAMI MODEL tnC/ha/year



writeRaster(NPP_MIAMI_tnC_Ha_Year,filename="INPUTS/NPP_TERRA/NPP_MIAMI_tnC_Ha_Year_STACK_81-00_MAX.tif",overwrite=TRUE)

# NPP MEAN

NPP_MIAMI_MEAN_81_00<-mean(NPP_MIAMI_tnC_Ha_Year)

# Crop & and mask



NPP_MIAMI_MEAN_81_00_AOI<-crop(NPP_MIAMI_MEAN_81_00,AOI)
NPP_MIAMI_MEAN_81_00_AOI<-resample(NPP_MIAMI_MEAN_81_00_AOI,SOC_MAP_AOI)
NPP_MIAMI_MEAN_81_00_AOI<-mask(NPP_MIAMI_MEAN_81_00_AOI,AOI)

writeRaster(NPP_MIAMI_MEAN_81_00_AOI,filename="INPUTS/NPP_TERRA/NPP_MIAMI_MEAN_81-00_AOI_MAX.tif",overwrite=TRUE)
writeRaster(NPP_MIAMI_MEAN_81_00,filename="INPUTS/NPP_TERRA/NPP_MIAMI_MEAN_81-00_MAX.tif",overwrite=TRUE)





