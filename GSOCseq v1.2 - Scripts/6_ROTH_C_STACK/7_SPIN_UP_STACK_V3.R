############################################
#                                          #                                                                           
#                                          # 
#   GSOCseq: Script 7 Spin up stack        #
#                                          #
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

# 3-digit Country ISO-code
ISO <-'THA_NP'
# Load Packages 
library(terra)


path_AOI<-("INPUTS/AOI_POLYGON/NP.shp")

path_SOC<-("INPUTS/SOC_MAP/THA_NP_SOC.tif")

path_CLAY<-("INPUTS/CLAY/THA_NP_Clay_WA.tif")

path_PREC<-("INPUTS/TERRA_CLIMATE/Prec_Stack_81-00_TC.tif")
path_TEMP<-("INPUTS/TERRA_CLIMATE/Temp_Stack_81-00_TC.tif")
path_PET<-("INPUTS/TERRA_CLIMATE/PET_Stack_81-00_TC.tif")

path_LU<-("INPUTS/LAND_USE/THA_NP_ESA_Land_Cover_13clases_FAO_2020.tif")

path_COV<-("INPUTS/COV/THA_NP_Cov_stack.tif")


# Open the shapefile of the region/country
AOI<-vect(path_AOI)

#Open SOC MAP FAO
SOC_MAP_AOI<-rast(path_SOC)

# Open Clay layer

Clay_WA_AOI<-rast(path_CLAY)

#Open Precipitation layer 
PREC<-rast(path_PREC)

PREC<-crop(PREC,AOI)
PREC<-resample(PREC,SOC_MAP_AOI)
PREC<-mask(PREC,AOI)


#Open Temperatures layer 

TEMP<-rast(path_TEMP)

TEMP<-crop(TEMP,AOI)
TEMP<-resample(TEMP,SOC_MAP_AOI)
TEMP<-mask(TEMP,AOI)

#Open Potential Evapotranspiration layer 

PET<-rast(path_PET)

PET<-crop(PET,AOI)
PET<-resample(PET,SOC_MAP_AOI)
PET<-mask(PET,AOI)


# OPen Land Use layer reclassify to FAO classes 

# 0	No Data
# 1 Artificial
# 2 Croplands
# 3 Grassland
# 4 Tree Covered
# 5 Shrubs Covered
# 6 Herbaceous vegetation flooded
# 7 Mangroves
# 8 Sparse Vegetation
# 9 Baresoil
# 10 Snow and Glaciers
# 11 Waterbodies
# 12 TreeCrops
# 13 Paddy fields
LU_AOI<-rast(path_LU)

# Open Vegetation Cover layer 

Cov_AOI<-rast(path_COV)

# Use Land use layer to convert it to DR layer 

#DPM/RPM (decomplosable vs resistant plant material)
#(1) Most agricultural crops and improved grassland and tree crops 1.44 
#(2) Unimproved grassland and schrub 0.67
#(3) Deciduous and tropical woodland 0.25    

DR<-(LU_AOI==2 | LU_AOI==12| LU_AOI==13)*1.44+ (LU_AOI==4)*0.25 + (LU_AOI==3 | LU_AOI==5 | LU_AOI==6 | LU_AOI==8)*0.67

plot(LU_AOI[[1]],DR[[1]] , pch=3, cex=3, ylab="DPM/RPM", xlab="Land Use classes")

# STACK all layers

Stack_Set_AOI<-c(SOC_MAP_AOI,Clay_WA_AOI,TEMP,PREC,PET,DR,LU_AOI,Cov_AOI)

writeRaster(Stack_Set_AOI,filename=(paste0("INPUTS/STACK/",ISO,"_Stack_Set_SPIN_UP.tif")),overwrite=TRUE)


