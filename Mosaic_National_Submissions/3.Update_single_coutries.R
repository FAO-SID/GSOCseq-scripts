library(raster)
library(rgdal)
#library(terra)
library(stringr)
library(googlesheets4)
library(data.table)
library(stringr)
rm(list=ls())

#Before using this script make sure that the new layers are correctly linked and timestamped
#in the submission overview folder
#for now I manually dowload them


#Fix and merge all layers with the gap-fill layer
gsocseq_dir  <- "C:/Users/hp/Documents/FAO/GSOCseq/GSOCseq_V1.0.0/sub_and_gap/"
raw_dir <-"C:/Users/hp/Documents/FAO/GSOCseq/National_submissions/"
out_dir  <- "C:/Users/hp/Documents/FAO/GSOCseq/GSOCseq_V1.0.0/sub_and_gap/Version_1.0.1"
#1) Mask global product to update new layers
#2) Add new layers


## Select country(ies) with ISO
ISO <- c("ARG", "BTN","ISR")
## Specify products to be updated
products <- ".tif"

# #Mask global product
# #Load UN country boarders (can be found on gdrive)
# map <- readOGR("C:/Users/hp/Documents/FAO/data/un_maps/Official UN Map/UN_Map_v2020/UNmap0_shp/BNDA_CTY.shp")
# map <-  map[map$ISO3CD %in%ISO,]
# 
# #Create a spatraster to use as a mask layer
# map <- vect(map)
# 
# #GSOCseq layers
# GSOCseq <- list.files(path= gsocseq_dir,pattern=products,full.names=T)
 arg <- list.files(path=paste0(raw_dir,ISO[1]),pattern=products,full.names=T)
 btn <- list.files(path=paste0(raw_dir,ISO[2]),pattern=products,full.names=T)
# r <-rast(GSOCseq[1])
# map <- rasterize(map, r)
# 
# #Mask all gap-fill layers
# for (i in 1:length(GSOCseq)){
#   g <-rast(GSOCseq[i])
#   g <- mask(g, map,inverse=T)
#   filename <- sub('.*/', '', GSOCseq[i])
#   writeRaster(g, paste0(out_dir, '/', filename),overwrite=TRUE)
#   print(paste("Masked",i))
# }


GSOCseq <- list.files(path= out_dir,pattern=products,full.names=T)
for (i in 1:length(GSOCseq)){
  
  g <-raster(GSOCseq[i])
  ARG <- raster(arg[i])
  BTN <- raster(btn[i])
  

  s <- merge(ARG,BTN)
  
  g <- merge(g,s)
  
  
  filename <- sub('.*/', '', GSOCseq[i])
  
  writeRaster(g, paste0(out_dir, '/', filename),overwrite=TRUE)
  print(paste("Masked and Mosaicked:",GSOCseq[i]))
}
