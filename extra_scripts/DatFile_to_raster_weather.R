### This script opens the dat files and converts to a tif file (mosaic)

## Run this script once for each year and each variable. 


library(raster)
library(rgdal)


#remove all variables and make sure you have enough memory.
#If not try processing less files
rm(list=ls()) 

setwd("C:/Users/lucia/Downloads/JAPAN/")
#select the directory to one year data
WD_files<-("C:/Users/lucia/Downloads/JAPAN/weather/1990")

#change the pattern to open temperature files
#precipitation files
#file_list<-list.files(path=WD_files,pattern=glob2rx(paste0("MSPRM*.dat")),full.names=TRUE)

#temperature files
file_list<-list.files(path=WD_files,pattern=glob2rx(paste0("MSTMM*.dat")),full.names=TRUE)

#Define the name of the variable
#variable<-'pre_'
variable<-'temp_'

# select the number of files to process
#file_list<-file_list[201:298]

spdf_o_list<-list()
spdf_centroid_list<-list()
raster_list<-list()
year<-testdata[1,2]
filename<-paste0(variable,'_',year)

for (i in 1:NROW(file_list)){  
testdata<-read.table(file_list[i],header=FALSE)

names(testdata)[1]<-'1km_gridCode'
names(testdata)[2]<-'year'
names(testdata)[3]<-'Lat'
names(testdata)[4]<-'Lon'

for (j in 5:16){
  n<-(j-4)
  names(testdata)[j]<-paste0(variable,n)
}

testdata$glat<-as.numeric(substr(testdata$`1km_gridCode`,1,2))
testdata$mlat<-as.numeric(substr(testdata$`1km_gridCode`,5,5))
testdata$slat<-as.numeric(substr(testdata$`1km_gridCode`,7,7))

testdata$glon<-as.numeric(substr(testdata$`1km_gridCode`,3,4))
testdata$mlon<-as.numeric(substr(testdata$`1km_gridCode`,6,6))
testdata$slon<-as.numeric(substr(testdata$`1km_gridCode`,8,8))


testdata$Lat<-testdata$glat/1.5+testdata$mlat*(5/60)+(testdata$slat)*(30/3600)
testdata$Lon<-100+testdata$glon+testdata$mlon*(7.5/60)+(testdata$slon)*(45/3600)


testdata<-testdata[1:16]

# Create spatial points data 
coords<-cbind(testdata$Lon,testdata$Lat)
sp<-SpatialPoints(coords)
spdf <-SpatialPointsDataFrame(sp, testdata)
proj4string(spdf)  <- CRS('+proj=longlat +datum=WGS84')
#spdf_o_list[i]<-spdf

#writeOGR(spdf, ".",paste0(filename,'_originals'), driver="ESRI Shapefile",overwrite=TRUE)

# Create a raster from the original points adding an extra pixel to it .
ext<-extent(spdf)
r<-raster(xmn=-ext[1], xmx=ext[2]+0.0125, ymn=ext[3], ymx=ext[4]+0.008333333,resolution=c(0.0125,0.008333333))
crs(r)=crs(spdf)

#shift spdf data so now the points are in the centroid of the pixels

ncoords<-coords
ncoords[,1]<-coords[,1]+(0.0125/2)  
ncoords[,2]<-coords[,2]+(0.008333333/2)

sp<-SpatialPoints(ncoords)
spdf <-SpatialPointsDataFrame(sp, testdata)
proj4string(spdf)  <- CRS('+proj=longlat +datum=WGS84')
#spdf_centroid_list[i]<-spdf

#writeOGR(spdf, ".",paste0(filename,'_centroids'), driver="ESRI Shapefile",overwrite=TRUE)

# Rasterize the spdf points
testRaster<-rasterize(spdf,r,spdf@data[5:16])
testRaster<-stack(testRaster)

raster_list[i]<-testRaster
#writeRaster(testRaster,filename=filename,format='GTiff',overwrite=TRUE)

}

#spdf_o <- do.call(bind, spdf_o_list) 
#writeOGR(spdf_o, ".",paste0(filename,'_originals'), driver="ESRI Shapefile",overwrite=TRUE)
#spdf_ce <- do.call(bind, spdf_centroid_list) 
#writeOGR(spdf_ce, ".",paste0(filename,'_centroids'), driver="ESRI Shapefile",overwrite=TRUE)


rasterMos<-do.call(mosaic,c(raster_list,fun=mean,tolerance=0.005,na.rm=TRUE))

writeRaster(rasterMos,filename=paste0(filename),format='GTiff',overwrite=TRUE)
