############################################
#                                          #                                                                           
#                                          # 
#   GSOCseq: Script 15  Exclude Impossible #
#   Values                                 #
#                                          #  
#   Isabel Luotto                          #
#   Contact: Isabel.Luotto@fao.org         #
#                                          # 
#                                          # 
############################################

#Initial setup -----------------------------------------------------------------
rm(list=ls())
gc()

# Working directory
#wd <- 'C:/Users/luottoi/Documents/GSOCseq'
wd <- 'C:/Users/hp/Documents/FAO/GSOCseq/Lebanon/OUTPUTS/4_MAPS'
setwd(wd)

# 3-digit Country ISO-code
ISO <-'LBN'

#Define path for where the final maps will be outputted 
tif_map_path <- 'corr/'
# Load Packages 
library(terra)

### RUN ###

# Open the t0  rast layer
name=paste0(ISO,'_GSOCseq_T0_Map030.tif')

T0_map<-rast(name)

T0_map[T0_map<0]<-NA
T0_map[T0_map>800]<-NA

plot(T0_map)

boxplot(values(T0_map))
boxplot(values(T0_map),outline=FALSE)
name=paste0(ISO,'_GSOCseq_T0_Map030')
nameOut=paste0(tif_map_path,name,'.tif')
writeRaster(T0_map,filename=nameOut, overwrite=T)

#Open Final SOC rast layers

SOC_list<-list.files(pattern=glob2rx(paste0(ISO,"*_GSOCseq_finalSOC*_Map030.tif")))

for(i in 1:NROW(SOC_list)){
  r<-rast(SOC_list[i])
  name<-paste0(tif_map_path,SOC_list[i])
  r[r<0]<-NA
  r[r>800]<-NA
  plot(r)
  boxplot(values(r))
  boxplot(values(r),outline=FALSE)
  writeRaster(r,filename=name, overwrite=T)
}

# Absolute Differences in SOC stocks

AbsDiff_list<-list.files(pattern=glob2rx(paste0(ISO,"*_GSOCseq_AbsDiff*_Map030.tif")))

for(i in 1:NROW(AbsDiff_list)){
  r<-rast(AbsDiff_list[i])
  name<-paste0(tif_map_path,AbsDiff_list[i])
  r[r<=-80]<-NA
  r[r>=80]<-NA
  plot(r)
  boxplot(values(r))
  boxplot(values(r),outline=FALSE)
  writeRaster(r,filename=name, overwrite=T)
}

# Relative differences in SOC stocks

RelDiff_list<-list.files(pattern=glob2rx(paste0(ISO,"*_GSOCseq_RelDiff*_Map030.tif")))

for(i in 1:NROW(RelDiff_list)){
  r<-rast(RelDiff_list[i])
  name<-paste0(tif_map_path,RelDiff_list[i])
  r[r<0]<-NA
  r[r>=80]<-NA
  plot(r)
  boxplot(values(r))
  boxplot(values(r),outline=FALSE)
  writeRaster(r,filename=name, overwrite=T)
}

#Absolute Sequestration Rates

ASR_list<-list.files(pattern=glob2rx(paste0(ISO,"*_GSOCseq_ASR*_Map030.tif")))

for(i in 1:NROW(ASR_list)){
  r<-rast(ASR_list[i])
  name<-paste0(tif_map_path,ASR_list[i])
  r[r<=-4]<-NA
  r[r>4]<-NA
  plot(r)
  boxplot(values(r))
  boxplot(values(r),outline=FALSE)
  writeRaster(r,filename=name, overwrite=T)
}

# Relative Sequestration Rates

RSR_list<-list.files(pattern=glob2rx(paste0(ISO,"*_GSOCseq_RSR*_Map030.tif")))

for(i in 1:NROW(RSR_list)){
  r<-rast(RSR_list[i])
  name<-paste0(tif_map_path,RSR_list[i])
  r[r<0]<-NA
  r[r>4]<-NA
  plot(r)
  boxplot(values(r))
  boxplot(values(r),outline=FALSE)
  writeRaster(r,filename=name, overwrite=T)
}

# Uncertainties

UNC_list<-list.files(pattern=glob2rx(paste0(ISO,"*UncertaintyMap030.tif")))

for(i in 1:NROW(UNC_list)){
  r<-rast(UNC_list[i])
  name<-paste0(tif_map_path,UNC_list[i])
  r[r<0]<-NA
  r[r>200]<-NA
  plot(r)
  boxplot(values(r))
  boxplot(values(r),outline=FALSE)
  writeRaster(r,filename=name, overwrite=T)
}
