############################################
#                                          #                                                                           
#                                          # 
#   GSOCseq: Script 10 Target Points       #
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

# 3-digit Country ISO-code
ISO <-'THA_NP'
# Load Packages 
library(raster)
library(terra)

LU<-raster("INPUTS/LAND_USE/THA_NP_ESA_Land_Cover_13clases_FAO_2020.tif")

points<-vect(rasterToPoints(LU,fun=function(x){x==2|x==3|x==5|x==12|x==13},sp=TRUE))
names(points)[1] <- 'FID'
points$FID <- 1:nrow(points)
writeVector(points, paste0("INPUTS/TARGET_POINTS/",ISO,"_Target_points.shp"),overwrite=TRUE) 
