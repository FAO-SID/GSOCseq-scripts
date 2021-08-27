# This script downloads and unzips the National GSOCseq Submissions from gdrive.
# Out of range values get automatically masked 
# 29 mosaicked GSOCseq layers are outputted
# Important* this version of the script only unzips zip files (other formats e.g. 
# rar) need to be extracted manually
# In order for the script to work the folder structure should as follows: ISO/layers*.tif
# Layers inside subfolders need to be copied over manually.

# Isabel Luotto 24/08/21


#Empty environment
rm(list=ls())

#Load libraries
library(raster)
library(rgdal)
library(googledrive)
library(googlesheets4)
library(data.table)
library(stringr)

#Set working directory
wd <- "C:/Users/hp/Documents/FAO/GSOCseq/National_submissions"
setwd(wd)

#Function to check if it's a zipped file
is.zip <- function(filepath){
  result <- tryCatch({
    unzip(filepath, list = TRUE)
    return(TRUE)
  }, error = function(e){
    return(FALSE)
  })
  return(result)
}


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

#Select unique latest observation (files from the latest submission will be downloaded)
gsheet <-unique(gsheet[order(date)], by="ISO", fromLast=TRUE)
gsheet <- gsheet[,c("ISO", "Country",  "National GSOCseq Layers")]

drive_auth(email ="isa.luotto@gmail.com")

for (i in unique(gsheet$ISO)){
setwd(wd)
link <- as.character(gsheet[ISO == i, "National GSOCseq Layers"])

dir.create(as.character(gsheet[ISO==i,"ISO"]))

setwd(paste0(wd,"/",as.character(gsheet[ISO==i,"ISO"])))
drive_download(link,overwrite = TRUE)

file <- list.files(path=paste0(wd,"/",as.character(gsheet[ISO==i,"ISO"])), pattern =".zip")


if (is.zip(file)){
  unzip(file)

}
else{

 print(paste0("Unzip manually ", i))

}}

#########**IMPORTANT**  Fix folder structures before proceeding######################################


#Copy all files outside of the individual ISO folders

setwd("C:/Users/hp/Documents/FAO/GSOCseq/National_submissions")
# Merge National Submissions
#Get list of countries
ISOs <- str_sub(list.dirs(recursive=F),-3,-1)
#Product list
product <-c(
  "*AbsDiff_BAU_Map030*" ,"*AbsDiff_SSM1_Map*",
  "*AbsDiff_SSM2_Map030*"    ,    "*AbsDiff_SSM3_Map030*"
  ,"*ASR_BAU_Map030*" ,
  "*ASR_SSM1_Map030*","*ASR_SSM1_Unce*"
  , "*ASR_SSM2_Map030*" ,"*ASR_SSM2_Unce*"
  , "*ASR_SSM3_Map030*"  ,"*ASR_SSM3_Unce*"
  , "*ASR_BAU_UncertaintyMap030*","*finalSOC_BAU_Map030*"
  , "*finalSOC_SSM1_Map030*"  ,"*finalSOC_SSM2_Map030*"
  , "*finalSOC_SSM3_Map030*",
  "*RelDiff_SSM1_Map030*"
  , "*RelDiff_SSM2_Map030*"  ,"*RelDiff_SSM3_Map030*"
  , "*RSR_SSM1_Map030*",
  "*RSR_SSM1_Unce*"
  , "*RSR_SSM2_Map030*" ,"*RSR_SSM2_Unce*"
  , "*RSR_SSM3_Map030*"  ,"*RSR_SSM3_Unce*"
  , "*SSM_UncertaintyMap030*","*T0_Map030*"
  , "*T0_UncertaintyMap030*",
  "*GSOCseq_BAU_UncertaintyMap030*"
)


for (i in unique(ISOs)){
  
  files <- list.files(path=paste0(wd,"/",i), pattern =".tif"
                      ,full.names = TRUE,recursive=T)
  files <-files[!grepl(".aux", files)|!grepl(".ovr", files)]
  for (p in unique(product)){
    
    if( !(TRUE%in% grepl(p, files, fixed=F)))
    {print(paste("Product name:",p, "not correct for",i))}
  }
    
  if(length(files)>=29){
  GSOCseq <-"C:/Users/hp/Documents/FAO/GSOCseq/National_submissions/GSOCseq_V1.0.0/"

  file.copy(from=files,
            to=GSOCseq,
            overwrite = TRUE, recursive = TRUE,
            copy.mode = TRUE)}
  else{
    print(paste("Check", i))
    GSOCseq <-"C:/Users/hp/Documents/FAO/GSOCseq/National_submissions/GSOCseq_V1.0.0/"
    
    file.copy(from=files,
              to=GSOCseq,
              overwrite = TRUE, recursive = FALSE,
              copy.mode = TRUE)
  }

}

WD_files<-"C:/Users/hp/Documents/FAO/GSOCseq/National_submissions/GSOCseq_V1.0.0/"
setwd(WD_files)

#Create a folder to save mosaicked layers
#dir.create("combined")

outputs<-"C:/Users/hp/Documents/FAO/GSOCseq/National_submissions/GSOCseq_V1.0.0/combined"


# Fix and combine layers
#Load SOC map as reference layer
soc <- raster("C:/TRAINING_MATERIALS_GSOCseq_MAPS_12-11-2020/INPUTS/SOC_MAP/GSOCmap_1.6.1.tif")

# #Mask Russia to UN boarders (ZAF already submitted map without LSO)  
# library(terra)
# RUS <-list.files(pattern="RUS",full.names=TRUE)
# map <- vect("C:/Users/hp/Documents/FAO/data/un_maps/Official UN Map/UN_Map_v2020/RUS.shp")
# map <- rasterize(map, rast(RUS[20]))
# 
# for (i in 1:length(RUS)){
#   rus <- rast(RUS[i])
#   rus <- mask(rus,map)
#   writeRaster(rus,RUS[i], overwrite=T)
#   print(paste("Cropped",RUS[i],i))
# }
# detach("package:terra", unload=TRUE)

#Check that all layers get picked up 
 #Number of submissions
num_sub <- length(unique(gsheet$ISO)) -1#(-USA)
for(p in product) {

  sub_list<-list.files(pattern=p,full.names=TRUE)
  if(length(sub_list)!=num_sub){
    print(paste("Check:", p,length(sub_list) ))
    
  }
  
}
 
 
#Loop over each product type
for(p in product) {
# Open the t0  raster layer
  setwd(WD_files)
T0_list<-list.files(pattern=p,full.names=TRUE)

#Loop to fix layers

for(i in 1:NROW(T0_list)){
  r<-raster(str_sub(T0_list[i],3))
  if(grepl("AbsDiff",p)) {
    r[r>=80] <-NA
    r[r<=(-80)] <-NA
  } else if (grepl("T0_Map030",p)|grepl("final",p)) {
    r[r<0]<-NA
    r[r>800]<-NA
  } else if (grepl("RSR",p)&!grepl("Unce",p)) {
    r[r<0]<-NA
    r[r>4]<-NA
  } else if (grepl("ASR",p)&!grepl("Unce",p)) {
    r[r<=(-5)]<-NA
    r[r>5]<-NA 
  } else if (grepl("RelDiff",p)) {
    r[r<=0]<-NA
    r[r>=80]<-NA
  } else  if(grepl("Unce",p)) {
    r[r<0]<-NA
    r[r>200]<-NA  }
  
 if(as.character(r@crs) !="+proj=longlat +datum=WGS84 +no_defs"){
   wgs ="+proj=longlat +datum=WGS84 +no_defs"
   r <- projectRaster(r,crs=wgs)
   print(paste("Reprojected", i))
   writeRaster(r, str_sub(T0_list[i],3), overwrite=TRUE)
 }
  else{
  
  if(round(as.numeric(res(r)[1]),8)!= 0.00833333){
    t <- crop(soc,r)
    r <- resample(r,t ,method="bilinear")
  
  print(paste("Resampled", i))
  writeRaster(r, str_sub(T0_list[i],3), overwrite=TRUE)
  }
    else{
  
 writeRaster(r, str_sub(T0_list[i],3), overwrite=TRUE)

    }
  
  }
  writeRaster(r, str_sub(T0_list[i],3), overwrite=TRUE)
  print(i)
  }



R_list<-list()
for(j in 1:NROW(T0_list)){
  r<-raster(str_sub(T0_list[j],3))
  R_list[[j]]<-r
}


Mos<-do.call(mosaic,c(R_list,fun=mean,tolerance=0.5))#11,19

setwd(outputs)
writeRaster(Mos,filename=paste0("GSOCseq_",gsub("*\\*", "", p)),format='GTiff', overwrite=TRUE)

}





