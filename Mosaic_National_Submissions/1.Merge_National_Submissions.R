# Isabel Luotto 24/08/21

# This script downloads and unzips the National GSOCseq Submissions from gdrive.
# Out of range values get automatically masked 
# 29 mosaicked GSOCseq layers are outputted
# Important* this version of the script only unzips zip files (other formats e.g. 
# rar) need to be extracted manually
# Updates are based on the change log outputted from this script
# the change log contains a date. Only maps after a given date are updated 

#Protocol of the script
# 1)Download, unzip and create copies of the layers
##1.1) Select gdrive download links based on latest submission and log date
##1.2) Download and unzip layers
##1.3) Create a copy of all files and paste them in one folder (to not modify original files)
##1.4) Check for completeness and correct names (the loop outputs comments to check)
##1.5) Fix names manually and copy over files again
# 2) Generate mosaic of the submitted layers
##2.1)Fix projection 
##2.2)resolution
##2.3) Mask out of range values
##2.4)mosaic layers 
##2.5)save intermediate update layers 
# 3) Update GSOCseq layers based on new maps
##3.1) Create mask shapefile out of the UN borders
##3.2) Mask GSOCseq layers
##3.3) Mosaic GSOCseq layers with intermediate maps
##3.4) Save new GSOCseq layers and delete intermediate layers
##3.5) Generate change log
###############################################################################

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
wd <- "C:/Users/hp/Documents/FAO/GSOCseq/GSOCseq_mosaic_subs"
setwd(wd)
#Folder where the latest GSOCseq layers are located
gseqv_dir <-"C:/Users/hp/Documents/FAO/GSOCseq/GSOCseq_V1.0.0/sub_and_gap"

#Output folder for the new version
GSOCseq_output_dir <-"C:/Users/hp/Documents/FAO/GSOCseq/GSOCseq_V1.1.0"
outputs<-"C:/Users/hp/Documents/FAO/GSOCseq/GSOCseq_mosaic_subs/intermediate/combined"
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


########################################################################################
# 1) Download and unzip layers (layers are saved in individual ISO folders)
## 1.1) Select gdrive download links based on latest submission and log date

#Read log file to select only layers after a certain timestamp 
log <- fread('2021-08-26_GSOCseq_log.csv')
date_log <-gsub("/", "-", log$Date)


#Read Submission overview google sheet
sheet_url <- "https://docs.google.com/spreadsheets/d/1B9qukBJehe7p0T4TkwR8A4tUMLs8LkP5m6nlM9ae4ZM/edit#gid=2110860500"
gs4_deauth()
sheet <- "Master"
gsheet <- read_sheet(sheet_url, sheet =sheet)
gsheet <- as.data.table(gsheet)

#Select drive links based on latest submission 
gsheet$date <- substr(gsheet$Timestamp, 1, 10)
gsheet$date <- format(as.Date(gsheet$date),"%m-%d-%Y")
gsheet <-gsheet[, c("ISO", "Country") := tstrsplit(Country, "; ", fixed=TRUE)]
gsheet <-unique(gsheet[order(date)], by="ISO", fromLast=TRUE)
gsheet <- gsheet[,c('date',"ISO", "Country",  "National GSOCseq Layers")]

# To update the GSOCseq select only countries after the last log date
gsheet <-with(gsheet, gsheet[(date >=date_log), ])


##1.2) Download and unzip layers
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

##1.3) Create a copy of all files and paste them in one folder (to not modify original files)
setwd("C:/Users/hp/Documents/FAO/GSOCseq/GSOCseq_mosaic_subs")

#Get list of countries
ISOs <- str_sub(list.dirs(recursive=F),-3,-1)
ISOs <-ISOs[ISOs!='ate']
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
  GSOCseq <-"intermediate/"

  file.copy(from=files,
            to=GSOCseq,
            overwrite = TRUE, recursive = TRUE,
            copy.mode = TRUE)}
  else{
    print(paste("Check", i))
    GSOCseq <-"intermediate/"
    
    file.copy(from=files,
              to=GSOCseq,
              overwrite = TRUE, recursive = FALSE,
              copy.mode = TRUE)
  }

}

##1.5) Fix names manually and copy over files again (repeat the previous loop) if no file names need to be fixed proceed


# 2) Generate mosaic of the submitted layers
##2.1)Fix projection 
##2.2)resolution
##2.3) Mask out of range values
##2.4)mosaic layers 
##2.5)save intermediate update layers

#Set the working directory to the files were the intermediate layers will be stored
interm_dir<-"C:/Users/hp/Documents/FAO/GSOCseq/GSOCseq_mosaic_subs/intermediate"
setwd(interm_dir)

# Fix and combine layers
#Load SOC map as reference layer
soc <- raster("C:/TRAINING_MATERIALS_GSOCseq_MAPS_12-11-2020/INPUTS/SOC_MAP/GSOCmap_1.6.1.tif")

#Check that the number of layers is correct 
num_sub <- length(unique(gsheet$ISO)) 
for(p in product) {

  sub_list<-list.files(pattern=p,full.names=TRUE)
  if(length(sub_list)!=num_sub){
    print(paste("Check:", p,length(sub_list) ))
    
  }
  
}
 
 
#Loop over each product type
for(p in product) {
# Open the t0  raster layer
  setwd(interm_dir)
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


# 3) Update GSOCseq layers based on new maps

##3.1) Create mask shapefile out of the UN borders with the ISOs
#Mask previous GSOCseq version
#Load UN country boarders (can be found on gdrive)
library(terra)

map <- readOGR("C:/Users/hp/Documents/FAO/data/un_maps/Official UN Map/UN_Map_v2020/UNmap0_shp/BNDA_CTY.shp")
map <- vect(map[map$ISO3CD %in%ISOs,])


##3.2) Mask GSOCseq layers

#list GSOCseq layers from the previous version
gseqv <- list.files(path=gseqv_dir, pattern =".tif",full.names=T)

g <-rast(gseqv[1])
map <- rasterize(map, g)

for (i in 1:length(gseqv)){
  g <-rast(gseqv[i])
  g <- mask(g, map,inverse=T)
  print(paste("Masked",i))

  filename <- sub('.*/', '', gseqv[i])
  writeRaster(g, paste0(GSOCseq_output_dir, '/',filename),overwrite=TRUE)
  print(paste(filename,i))
  
}
#Better restart R at this point if the terra package causes problems

##3.3) Mosaic GSOCseq layers with intermediate maps
##3.4) Save new GSOCseq layers and overwrite intermediate layers
intermd<- list.files(path= outputs,pattern=".tif",full.names=T)
gseqv <- list.files(path=gseqv_dir, pattern =".tif",full.names=T)

for (i in 1:length(gseqv)){
  g <-raster(gseqv[i])
  intm <- raster(intermd[i])
 mos <- merge(g,intm)
 
  filename <- sub('.*/', '', gseqv[i])
  writeRaster(mos, paste0(GSOCseq_output_dir, '/',filename),overwrite=TRUE, NAflag=-999)
  print(paste(filename,i))
}


##3.5) Generate change log
#get date and time
ts <- Sys.time()
date <-format(Sys.Date(),"%m-%d-%Y")
#get countries that were updated 
ISOs <- as.data.frame(gsheet$ISO)
ISOs <- as.data.frame(ISOs[1:50,])
colnames(ISOs)<-'ISO'
ISOs <-ISOs %>%
  dplyr::summarise(ISO = paste(ISO, collapse = ","))
#Comments
comments <- "The GSOCseq v1.0.0 is launched."

#Get table
table  <- data.frame(Date=date,Timestamp=ts, Countries=ISOs, Comments=comments)

#Save log file 
fwrite(table, paste0(date,'_GSOCseq_log.csv'), sep=';')


