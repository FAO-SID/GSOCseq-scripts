library(raster)
library(rgdal)
library(terra)
library(stringr)
library(googlesheets4)
library(data.table)
library(stringr)
rm(list=ls())

#Fix and merge all layers with the gap-fill layer
sub_dir  <- "C:/Users/hp/Documents/FAO/GSOCseq/GSOCseq_V1.0.0/sub_and_gap/"
out_dir <- "C:/Users/hp/Documents/FAO/GSOCseq/GSOCseq_V1.0.0/sub_and_gap/no_ITA_MAR/"
MAR_dir <-"C:/Users/hp/Documents/FAO/GSOCseq/National_submissions/MAR/"
#Mask out country(ies) to be updated
exclude <- c("ITA", "MAR")

map <- readOGR("C:/Users/hp/Documents/FAO/data/un_maps/Official UN Map/UN_Map_v2020/UNmap0_shp/BNDA_CTY.shp")
map <-  map[!(map$ISO3CD %in% exclude),]
map <- vect(map)

SUBS <- list.files(path= sub_dir,pattern='.tif',full.names=T)

g <-rast(SUBS[1])
map <- rasterize(map, g)
plot(map)
rm(g)
#map <-rast("C:/Users/hp/Documents/FAO/GSOCseq/GSOCseq_V1.0.0/blank_sub_mask_2308.tif")
SUBS <- list.files(path= sub_dir,pattern='.tif',full.names=F)
#Mask all gap-fill layers
for (i in 1:length(SUBS)){
  s <-rast(paste0(sub_dir, SUBS[i]))
  s <- mask(s, map)
  
  writeRaster(s, paste0(out_dir, SUBS[i]),overwrite=TRUE)
  print(paste("Masked:",SUBS[i]))
} 

#map <-rast("C:/Users/hp/Documents/FAO/GSOCseq/GSOCseq_V1.0.0/blank_sub_mask_2308.tif")
SUBS <- list.files(path= out_dir,pattern='.tif',full.names=F)
MAR <- list.files(path= MAR_dir,pattern='.tif',full.names=F)
#Mask all gap-fill layers
for (i in 1:length(SUBS)){
  s <-raster(paste0(sub_dir, SUBS[i]))
  mar <-raster(paste0(sub_dir, SUBS[i]))
  
  mos <- merge(s,mar)
  writeRaster(mos, paste0(out_dir, SUBS[i]),overwrite=TRUE)
  print(paste("Masked:",SUBS[i]))
} 
