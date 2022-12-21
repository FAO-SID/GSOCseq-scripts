############################################
#                                          #                                                                           
#                                          # 
#   GSOCseq: Script 5 Clay                 #
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
# Working directory
#wd <- 'C:/Users/luottoi/Documents/GSOCseq'
wd <- 'C:/Users/hp/Documents/FAO/GSOCseq/OCB/GSOCseq_TM/'
setwd(wd)

# 3-digit Country ISO-code
ISO <-'THA_NP'
# File paths
AOI_path <- 'INPUTS/AOI_POLYGON/NP.shp'
SOC_path <- 'INPUTS/SOC_MAP/THA_NP_SOC.tif'

# Load Packages 
library(terra)


# Open the shapefile of the region/country
AOI<-vect(AOI_path)

#Open FAO AOI GSOCmap 
SOC_MAP<-rast(SOC_path)#DATE: 12-11-2020

# Open Clay layers  (ISRIC)
files <- list.files(path='INPUTS/CLAY/', pattern='.tif', full.names = T)
files <- files[grepl('CLYPPT_M' ,files)]

clay <- rast(files)

clay<-crop(clay,AOI)
clay<-mask(clay,AOI)

# Weighted Average of four depths 

WeightedAverage<-function(r){return(r[[1]]*(1/30)+r[[2]]*(4/30)+r[[3]]*(10/30)+r[[4]]*(15/30))}

clay<-WeightedAverage(clay)


writeRaster(clay,filename=paste0("INPUTS/CLAY/",ISO,"_Clay_WA.tif"),overwrite=TRUE)

