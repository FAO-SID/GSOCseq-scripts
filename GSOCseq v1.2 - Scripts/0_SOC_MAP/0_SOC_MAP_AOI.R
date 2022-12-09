############################################
#                                          #                                                                           
#                                          # 
#   GSOCseq: Script 0 SOC Map              #
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

SOC_path <- 'INPUTS/SOC_MAP/GSOCmap_1.6.1.tif'

# Load Packages 
library(terra)


# Open the shapefile of the region/country
AOI<-vect(AOI_path)

#Open FAO GSOC MAP , crop it and masked by the aoi. Then save it. 

SOC_MAP<-rast(SOC_path)
SOC_MAP<-crop(SOC_MAP,AOI)
SOC_MAP<-mask(SOC_MAP,AOI)

writeRaster(SOC_MAP, paste0( 'INPUTS/SOC_MAP/',ISO,'_SOC.tif'),overwrite=TRUE)

