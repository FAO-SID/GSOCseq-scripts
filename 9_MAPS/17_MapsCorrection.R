# Luciano E Di Paolo & Guillermo Peralta

# Clean the Gsoc Seq products

library(raster)
library(rgdal)

# MASK ALL THE LAYERS WITH THE T0 RASTER LAYER. 

# Define directory

WD_files<-("D:/TRAINING_MATERIALS_GSOCseq_MAPS_12-11-2020/OUTPUTS/4_MAPS2")
setwd(WD_files)

# Set the ISO code of the country
ISO="WORLD"

### RUN ###

# Open the t0  raster layer
name=paste0(ISO,'_GSOCseq_T0_Map030.tif')

T0_map<-raster(name)

T0_map[T0_map<0]<-NA
T0_map[T0_map>800]<-NA

plot(T0_map)

boxplot(values(T0_map))
boxplot(values(T0_map),outline=FALSE)
name=paste0(ISO,'_5km_GSOCseq_T0_Map030')
nameOut=paste0(name,'_Corr.tif')
writeRaster(T0_map,filename=nameOut,format="GTiff")

#Open Final SOC raster layers.

SOC_list<-list.files(path=WD_files,pattern=glob2rx(paste0(ISO,"*_GSOCseq_finalSOC*_Map030.tif")),full.names=TRUE)

for(i in 1:NROW(SOC_list)){
  r<-raster(SOC_list[i])
  name<-paste0(names(r),'_Corr.tif')
  r[r<0]<-NA
  r[r>800]<-NA
  plot(r)
  boxplot(values(r))
  boxplot(values(r),outline=FALSE)
  writeRaster(r,filename=name,format="GTiff")
}

# Absolute Differences in SOC stocks

AbsDiff_list<-list.files(path=WD_files,pattern=glob2rx(paste0(ISO,"*_GSOCseq_AbsDiff*_Map030.tif")),full.names=TRUE)

for(i in 1:NROW(AbsDiff_list)){
  r<-raster(AbsDiff_list[i])
  name<-paste0(names(r),'_Corr.tif')
  r[r<=-80]<-NA
  r[r>=80]<-NA
  plot(r)
  boxplot(values(r))
  boxplot(values(r),outline=FALSE)
  writeRaster(r,filename=name,format="GTiff")
}

# Relative differences in SOC stocks

RelDiff_list<-list.files(path=WD_files,pattern=glob2rx(paste0(ISO,"*_GSOCseq_RelDiff*_Map030.tif")),full.names=TRUE)

for(i in 1:NROW(RelDiff_list)){
  r<-raster(RelDiff_list[i])
  name<-paste0(names(r),'_Corr.tif')
  r[r<0]<-NA
  r[r>=80]<-NA
  plot(r)
  boxplot(values(r))
  boxplot(values(r),outline=FALSE)
  writeRaster(r,filename=name,format="GTiff")
}

#Absolute Sequestration Rates

ASR_list<-list.files(path=WD_files,pattern=glob2rx(paste0(ISO,"*_GSOCseq_ASR*_Map030.tif")),full.names=TRUE)

for(i in 1:NROW(ASR_list)){
  r<-raster(ASR_list[i])
  name<-paste0(names(r),'_Corr.tif')
  r[r<=-4]<-NA
  r[r>4]<-NA
  plot(r)
  boxplot(values(r))
  boxplot(values(r),outline=FALSE)
  writeRaster(r,filename=name,format="GTiff")
}

# Relative Sequestration Rates

RSR_list<-list.files(path=WD_files,pattern=glob2rx(paste0(ISO,"*_GSOCseq_RSR*_Map030.tif")),full.names=TRUE)

for(i in 1:NROW(RSR_list)){
  r<-raster(RSR_list[i])
  name<-paste0(names(r),'_Corr.tif')
  r[r<0]<-NA
  r[r>4]<-NA
  plot(r)
  boxplot(values(r))
  boxplot(values(r),outline=FALSE)
  writeRaster(r,filename=name,format="GTiff")
}

# Uncertainties

UNC_list<-list.files(path=WD_files,pattern=glob2rx(paste0(ISO,"*UncertaintyMap030.tif")),full.names=TRUE)

for(i in 1:NROW(UNC_list)){
  r<-raster(UNC_list[i])
  name<-paste0(names(r),'_Corr.tif')
  r[r<0]<-NA
  r[r>200]<-NA
  plot(r)
  boxplot(values(r))
  boxplot(values(r),outline=FALSE)
  writeRaster(r,filename=name,format="GTiff")
}
