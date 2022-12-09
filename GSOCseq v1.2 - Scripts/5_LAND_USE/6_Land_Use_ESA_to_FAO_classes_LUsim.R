############################################
#                                          #                                                                           
#                                          # 
#   GSOCseq: Script 6 Land use/cover       #
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
# File paths
AOI_path <- 'INPUTS/AOI_POLYGON/NP.shp'
SOC_path <- 'INPUTS/SOC_MAP/THA_NP_SOC.tif'
Lu_raw_path <- 'INPUTS/LAND_USE/Raw CCI Land cover/THA_C3S-LC-L4-LCCS-Map-300m_2016-2020.tif'
# Load Packages 
library(terra)
# Open the shapefile of the region/country
AOI <- vect(AOI_path)

# Open Land Use Layer (ESA)

ESA_LU<-rast(Lu_raw_path)
plot(ESA_LU[[1]])

# Cut the LU layer by the country polygon
ESA_LU<-crop(ESA_LU,AOI)
ESA_LU<-mask(ESA_LU,AOI)
plot(ESA_LU[[1:4]])

# Reclassify ESA LAND USE to FAO LAND USE classes

#   0 = 0	  No Data
#	190 = 1 Artificial
#	10 11 30 40 = 2 Croplands
#	130 = 3 Grassland
#	50 60 61 62 70 71 72 80 81 82 90 100 110 = 4 Tree Covered
#	120 121 122= 5 Shrubs Covered
#	160 180 = 6 Herbaceous vegetation flooded
#	170 = 7 Mangroves
#	150 151 152 153= 8 Sparse Vegetation
#	200 201 202 = 9 Baresoil
#	220 = 10 Snow and Glaciers
#	210 = 11 Waterbodies
#	12  = 12 Treecrops
# 20 = 13 Paddy fields(rice/ flooded crops)

# Create a reclassification matrix. "Is" to "become"
is<-c(0,190,10,11,30,40,130,50,60,61,62,70,71,72,80,81,82,90,100,110,120,121,122,160,180,
170,150,151,152,153,200,201,202,220,210,12,20)

become<-c(0,1,2,2,2,2,3,4,4,4,4,4,4,4,4,4,4,4,4,4,5,5,5,6,6,7,8,8,8,8,9,9,9,10,11,12,13)

recMat<-matrix(c(is,become),ncol=2,nrow=37)

# Reclassify
FAO_LU <- classify(ESA_LU, recMat)

# Resample to SOC map layer extent and resolution
SOC_MAP_AOI<-rast(SOC_path) # change for your own SOC MAP

FAO_LU<-resample(FAO_LU,SOC_MAP_AOI,method='near') 
FAO_LU<-mask(FAO_LU,SOC_MAP_AOI) 

# Save Land Use raster
writeRaster(FAO_LU,filename=paste0("INPUTS/LAND_USE/",ISO,"ESA_Land_Cover_13clases_FAO_Stack.tif"),overwrite=TRUE)

# We save separately the land use from 2020 to perform the target´s points creation
writeRaster(FAO_LU[[length(FAO_LU)]],filename=paste0("INPUTS/LAND_USE/",ISO,
                                                     "_ESA_Land_Cover_13clases_FAO_2020.tif"),overwrite=TRUE)


