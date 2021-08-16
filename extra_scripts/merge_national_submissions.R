# All files should be in the same folder called (WD_files)
# The outputs will be saved to "outputs"
# Outliers will be masked out applying NA values to them 
rm(list=ls())

library(raster)
library(rgdal)
library(googledrive)
library(googlesheets4)
library(data.table)
library(stringr)

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

#Select unique latest observation
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

setwd("C:/Users/hp/Documents/FAO/GSOCseq/National_submissions")
# Merge National Submissions
#Get list of countries
ISOs <- str_sub(list.dirs(recursive=F),-3,-1)


for (i in unique(ISOs)){
  files <- list.files(path=paste0(wd,"/",as.character(gsheet[ISO==i,"ISO"])), pattern =".tif"
                      ,full.names = TRUE)
  GSOCseq <-"C:/Users/hp/Documents/FAO/GSOCseq/National_submissions/GSOCseq_V1.0.0/"

  file.copy(from=files,
            to=GSOCseq,
            overwrite = TRUE, recursive = FALSE,
            copy.mode = TRUE)

}

WD_files<-"C:/Users/hp/Documents/FAO/GSOCseq/National_submissions/GSOCseq_V1.0.0/"
setwd(WD_files)
outputs<-"C:/Users/hp/Documents/FAO/GSOCseq/National_submissions/GSOCseq_V1.0.0/combined"

### RUN ###
soc <- raster("C:/TRAINING_MATERIALS_GSOCseq_MAPS_12-11-2020/INPUTS/SOC_MAP/GSOCmap_1.6.1.tif")


product <-c("*AbsDiff_BAU_Map030" ,"*AbsDiff_SSM1_Map030",
            "*AbsDiff_SSM2_Map030"    ,    "*AbsDiff_SSM3_Map030"       
            ,"*ASR_BAU_Map030" ,"*ASR_BAU_UncertaintyMap030"
            , "*ASR_SSM1_Map030","*ASR_SSM1_UncertaintyMap030"
            , "*ASR_SSM2_Map030" ,"*ASR_SSM2_UncertaintyMap030"
            , "*ASR_SSM3_Map030"  ,"*ASR_SSM3_UncertaintyMap030"
            , "*BAU_UncertaintyMap030","*finalSOC_BAU_Map030"
            , "*finalSOC_SSM1_Map030"  ,"*finalSOC_SSM2_Map030"
            , "*finalSOC_SSM3_Map030","*RelDiff_SSM1_Map030"
            , "*RelDiff_SSM2_Map030"  ,"*RelDiff_SSM3_Map030"
            , "*RSR_SSM1_Map030","*RSR_SSM1_UncertaintyMap030"
            , "*RSR_SSM2_Map030" ,"*RSR_SSM2_UncertaintyMap030"
            , "*RSR_SSM3_Map030"  ,"*RSR_SSM3_UncertaintyMap030"
            , "*SSM_UncertaintyMap030","*T0_Map030"
            , "*T0_UncertaintyMap030"
)

#Loop over each product type
for(p in product) {
# Open the t0  raster layer
  setwd(WD_files)
T0_list<-list.files(pattern=p,full.names=TRUE)

#Loop to fix layers

R_list<-list()
for(i in 1:NROW(T0_list)){
  r<-raster(T0_list[i])
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
  
  #name<-paste0(names(r),'_Corr.tif')
  r[r<0]<-NA
  r[r>800]<-NA
  
  writeRaster(r, str_sub(T0_list[i],3), overwrite=TRUE)
  print(i)
    }
    }
  }



R_list<-list()
for(j in 1:NROW(T0_list)){
  r<-raster(T0_list[j])
  #name<-paste0(names(r),'_Corr.tif')
  #plot(r)
  #boxplot(values(r))
  #boxplot(values(r),outline=FALSE)
  #writeRaster(r,filename=name,format="GTiff")
  R_list[[j]]<-r
}

Mos<-do.call(mosaic,c(R_list,fun=mean,tolerance=0.5))#11,19

setwd(outputs)
writeRaster(Mos,filename=paste0("GSOCseq_",str_sub(p,2)),format='GTiff', overwrite=TRUE)

}
