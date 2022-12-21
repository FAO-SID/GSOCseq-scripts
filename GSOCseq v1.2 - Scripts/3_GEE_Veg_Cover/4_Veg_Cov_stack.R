############################################
#                                          #                                                                           
#                                          # 
#   GSOCseq: Script 4 Vegetation           #
#   Cover Factor                           #  
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
SOC_MAP<-rast(SOC_path)


# Open Vegetation Cover layer based only in proportion of NDVI pixels grater than 0.6 
files <- list.files(path='INPUTS/COV/', pattern='.tif', full.names = T)
files <- files[grepl('_prop_gt' ,files)]

Rlist =list()
for (i in 1:length(files)){

Cov1<-rast(files[i])
Cov1[is.na(Cov1)] <- 0
Cov1<-crop(Cov1,AOI)
Cov1<-mask(Cov1,AOI)
Cov1<-resample(Cov1,SOC_MAP,method='bilinear') 
Rlist[[i]] <-Cov1
}

covs <-rast(Rlist)

# rescale values to 1 if it is bare soil and 0.6 if it is vegetated.

covs_f<-((covs)*(-0.4))+1

plot(covs_f[[4]],covs[[4]])

writeRaster(covs_f,filename=paste0('INPUTS/COV/',ISO,'_Cov_stack.tif'),overwrite=TRUE)



