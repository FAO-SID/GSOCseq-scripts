#### Script to merge rasters######
# Isabel Luotto
# 07/05/21

#Empty environment
rm(list =ls())

#Load packages
library(raster)
library(rgdal)
#library(tidyverse)


#User defined variables
#path to working directory
wd <- "C:/Users/hp/Documents/FAO/GSOCseq/Ethiopia/OUTPUTS/4_MAPS/"

#path to zones
z1 <-paste0(wd, "z1")
z2 <-paste0(wd, "z2")
#z3 <-paste0(wd, "/z3")
#z4 <-paste0(wd, "/z4")
#z5 <-paste0(wd, "/z5")
#z6 <- paste0(wd, "/z6")

# zerovalues folder
#zerovalues <- paste0(wd, "/zerovalues")
#output folder
outputdir <- paste0(wd, "combined")

#set working directory
setwd(wd)

#create list of product names

#The commented lines show how I created it from the training material
# below the list of files already part of this script
# product <-list.files(path = '.', pattern = ".tif",
#                      full.names = TRUE)
# product <- str_remove(product, "./Pergamino_GSOCseq")

product <-c("_AbsDiff_BAU_Map030.tif" ,"_AbsDiff_SSM1_Map030.tif",
            "_AbsDiff_SSM2_Map030.tif"    ,    "_AbsDiff_SSM3_Map030.tif"       
            ,"_ASR_BAU_Map030.tif" ,"_ASR_BAU_UncertaintyMap030.tif"
            , "_ASR_SSM1_Map030.tif","_ASR_SSM1_UncertaintyMap030.tif"
            , "_ASR_SSM2_Map030.tif" ,"_ASR_SSM2_UncertaintyMap030.tif"
            , "_ASR_SSM3_Map030.tif"  ,"_ASR_SSM3_UncertaintyMap030.tif"
            , "seq_BAU_UncertaintyMap030.tif","_finalSOC_BAU_Map030.tif"
            , "_finalSOC_SSM1_Map030.tif"  ,"_finalSOC_SSM2_Map030.tif"
            , "_finalSOC_SSM3_Map030.tif","_RelDiff_SSM1_Map030.tif"
            , "_RelDiff_SSM2_Map030.tif"  ,"_RelDiff_SSM3_Map030.tif"
            , "_RSR_SSM1_Map030.tif","_RSR_SSM1_UncertaintyMap030.tif"
            , "_RSR_SSM2_Map030.tif" ,"_RSR_SSM2_UncertaintyMap030.tif"
            , "_RSR_SSM3_Map030.tif"  ,"_RSR_SSM3_UncertaintyMap030.tif"
            , "_SSM_UncertaintyMap030.tif","_T0_Map030.tif"
            , "_T0_UncertaintyMap030.tif"
            )

#Loop over each product type
for(i in product) {

#Search for files that correspond to the product name 
#in each zone folder
 
files <- c(list.files(path = z1, pattern =i,
                             full.names = TRUE),list.files(path = z2, pattern = i,
                                                           full.names = TRUE)
           # ,list.files(path = z3, pattern = i,
           #            full.names = TRUE),
           # list.files(path = z4, pattern = i,
           #            full.names = TRUE),
           # list.files(path = z5, pattern = i,
           #            full.names = TRUE),
           # list.files(path = z6, pattern = i,
           #            full.names = TRUE)
          )


#Stack and merge the files
rz1 <- raster(grep("z1", files, value = TRUE))
rz2 <- raster(grep("z2", files, value = TRUE))


rzones <- merge(rz1,rz2)

#Save the merged raster in the output folder
writeRaster(rzones, paste0(outputdir, "/ETH_GSOCseq", i), overwrite =TRUE)
print(i)
}



