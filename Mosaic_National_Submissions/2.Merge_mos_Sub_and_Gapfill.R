# This merges the outputs from script 1 with the global gap fill layer
# The Gapfill layer is masked by submitted layers and countries that want to be left blank
# 29 mosaicked GSOCseq layers are outputted


# Isabel Luotto 24/08/21


#Empty environment
rm(list=ls())

# Load libraries
library(raster)
library(rgdal)
library(terra)#This is used to mask the gapfill layer
library(stringr)
library(googlesheets4)
library(data.table)
library(stringr)


#Define folders
gapfill_dir <- "C:/Users/hp/Documents/FAO/GSOCseq/GSOCseq_V1.0.0/GapFill_220821/"
mask_gap_dir <- "C:/Users/hp/Documents/FAO/GSOCseq/GSOCseq_V1.0.0/GapFill_220821/sub_masked/"
sub_dir<- "C:/Users/hp/Documents/FAO/GSOCseq/National_submissions/GSOCseq_V1.0.0/combined/"
out_sub_gap  <- "C:/Users/hp/Documents/FAO/GSOCseq/GSOCseq_V1.0.0/sub_and_gap/"

# Create mask layer based on submissions and blanks

#List of blank countries (needs to be updated manually)
blanks <- c("AUS","AUT","BEL",
            "LVA","NLD","NZL","NOR", "ITA")


# download National Submissions
sheet_url <- "https://docs.google.com/spreadsheets/d/1B9qukBJehe7p0T4TkwR8A4tUMLs8LkP5m6nlM9ae4ZM/edit#gid=2110860500"
##sheet of interest for the map
gs4_deauth()
sheet <- "Master"
gsheet <- read_sheet(sheet_url, sheet =sheet)
gsheet <- as.data.table(gsheet)

gsheet$date <- substr(gsheet$Timestamp, 1, 10)
gsheet$date <- as.Date(gsheet$date)

gsheet <-gsheet[, c("ISO", "Country") := tstrsplit(Country, "; ", fixed=TRUE)]

#Select unique ISOs
gsheet  <- gsheet[!duplicated(gsheet$ISO),]

exclude <- c(blanks, gsheet$ISO)

#Load UN country boarders (can be found on gdrive)
map <- readOGR("C:/Users/hp/Documents/FAO/data/un_maps/Official UN Map/UN_Map_v2020/UNmap0_shp/BNDA_CTY.shp")
map <-  map[!(map$ISO3CD %in% exclude),]

#Create a spatraster to use as a mask layer 
map <- vect(map)

#Gapfill layer files
GAPS <- list.files(path= gapfill_dir,pattern='.tif',full.names=T)

g <-rast(GAPS[1])
map <- rasterize(map, g)

#Update date in the name
writeRaster(map, "C:/Users/hp/Documents/FAO/GSOCseq/GSOCseq_V1.0.0/blank_sub_mask_2308.tif", overwrite=TRUE)
#map <-rast("C:/Users/hp/Documents/FAO/GSOCseq/GSOCseq_V1.0.0/blank_sub_mask_2308.tif")
GAPS <- list.files(path= gapfill_dir,pattern='.tif',full.names=F)

#Mask all gap-fill layers
for (i in 1:length(GAPS)){
  g <-rast(paste0(gapfill_dir, GAPS[i]))
  g <- mask(g, map)

  writeRaster(g, paste0(mask_gap_dir, GAPS[i]),overwrite=TRUE)
  print(paste("Masked:",GAPS[i]))
}



## Absdiff
products <-c("*AbsDiff_*")
subs <- list.files(path= sub_dir,pattern=products,full.names=F)
gaps <- list.files(path= mask_gap_dir,pattern=products,full.names=F)

for (i in 1:length(subs)){
  s <- raster(paste0(sub_dir,subs[i]))
  g <-raster(paste0(mask_gap_dir,gaps[i]))
  mos <- merge(s,g)
  writeRaster(mos,paste0(out_sub_gap, subs[i]),format="GTiff",
              NAflag=-999,options=c("COMPRESS=DEFLATE","PREDICTOR=2","ZLEVEL=9"),overwrite=TRUE)
 
  
}

# T0
products <-c("*GSOCseq_T0_Map030*")
subs <- list.files(path= sub_dir,pattern=products,full.names=F)
gaps <- list.files(path= mask_gap_dir,pattern=products,full.names=F)

for (i in 1:length(subs)){
  s <- raster(paste0(sub_dir,subs[i]))
  g <-raster(paste0(mask_gap_dir,gaps[i]))
  mos <- merge(s,g)
  writeRaster(mos,paste0(out_sub_gap, subs[i]),format="GTiff",
              NAflag=-999,options=c("COMPRESS=DEFLATE","PREDICTOR=2","ZLEVEL=9"),overwrite=TRUE)
  
}


#Final BAU SOC
products <-c("*GSOCseq_finalSOC*")
subs <- list.files(path= sub_dir,pattern=products,full.names=F)
gaps <- list.files(path= mask_gap_dir,pattern=products,full.names=F)

for (i in 1:length(subs)){
  s <- raster(paste0(sub_dir,subs[i]))
  g <-raster(paste0(mask_gap_dir,gaps[i]))
  mos <- merge(s,g)
  writeRaster(mos,paste0(out_sub_gap, subs[i]),format="GTiff",
              NAflag=-999,options=c("COMPRESS=DEFLATE","PREDICTOR=2","ZLEVEL=9"),overwrite=TRUE) 
  
}



#Merge Absolute Differences BAU raster layers.
products <-c("*GSOCseq_AbsDiff*")
subs <- list.files(path= sub_dir,pattern=products,full.names=F)
gaps <- list.files(path= mask_gap_dir,pattern=products,full.names=F)

for (i in 1:length(subs)){
  s <- raster(paste0(sub_dir,subs[i]))
  g <-raster(paste0(mask_gap_dir,gaps[i]))
  mos <- merge(s,g)
  writeRaster(mos,paste0(out_sub_gap, subs[i]),format="GTiff",
              NAflag=-999,options=c("COMPRESS=DEFLATE","PREDICTOR=2","ZLEVEL=9"),overwrite=TRUE)
  
}



#Merge Relative Differences SSM1 raster layers.
products <-c("*GSOCseq_RelDiff*")
subs <- list.files(path= sub_dir,pattern=products,full.names=F)
gaps <- list.files(path= mask_gap_dir,pattern=products,full.names=F)

for (i in 1:length(subs)){
  s <- raster(paste0(sub_dir,subs[i]))
  g <-raster(paste0(mask_gap_dir,gaps[i]))
  mos <- merge(s,g)
  writeRaster(mos,paste0(out_sub_gap, subs[i]),format="GTiff",
              NAflag=-999,options=c("COMPRESS=DEFLATE","PREDICTOR=2","ZLEVEL=9"),overwrite=TRUE)
  
}



#Merge Absolute sequestration 
products <-c("*GSOCseq_ASR*")
subs <- list.files(path= sub_dir,pattern=products,full.names=F)
gaps <- list.files(path= mask_gap_dir,pattern=products,full.names=F)

for (i in 1:length(subs)){
  s <- raster(paste0(sub_dir,subs[i]))
  g <-raster(paste0(mask_gap_dir,gaps[i]))
  mos <- merge(s,g)
  writeRaster(mos,paste0(out_sub_gap, subs[i]),format="GTiff",
              NAflag=-999,options=c("COMPRESS=DEFLATE","PREDICTOR=2","ZLEVEL=9"),overwrite=TRUE)
  
}


#Merge Relative sequestration Rates SSM1 raster layers.
products <-c("*GSOCseq_RSR*")
subs <- list.files(path= sub_dir,pattern=products,full.names=F)
gaps <- list.files(path= mask_gap_dir,pattern=products,full.names=F)

for (i in 1:length(subs)){
  s <- raster(paste0(sub_dir,subs[i]))
  g <-raster(paste0(mask_gap_dir,gaps[i]))
  mos <- merge(s,g)
  writeRaster(mos,paste0(out_sub_gap, subs[i]),format="GTiff",
              NAflag=-999,options=c("COMPRESS=DEFLATE","PREDICTOR=2","ZLEVEL=9"),overwrite=TRUE)
  
}

#Fix and merge all layers with the gap-fill layer
gapfill_dir <- "C:/Users/hp/Documents/FAO/GSOCseq/GSOCseq_Results_data/GSOCseq/WORLD_GSOCseq_AbsDiff_BAU_Map030_Corr_1km_t0mask_incmask.tif"
sub_dir <- "C:/Users/hp/Documents/FAO/GSOCseq/National_submissions/GSOCseq_V1.0.0/combined"


#Merge Uncertainties
products <-c("*Uncer*")
subs <- list.files(path= sub_dir,pattern=products,full.names=F)
gaps <- list.files(path= mask_gap_dir,pattern=products,full.names=F)

for (i in 1:length(subs)){
  s <- raster(paste0(sub_dir,subs[i]))
  g <-raster(paste0(mask_gap_dir,gaps[i]))
  mos <- merge(s,g)
  writeRaster(mos,paste0(out_sub_gap, subs[i]),format="GTiff",
              NAflag=-999,options=c("COMPRESS=DEFLATE","PREDICTOR=2","ZLEVEL=9"),overwrite=TRUE) 
  
}




