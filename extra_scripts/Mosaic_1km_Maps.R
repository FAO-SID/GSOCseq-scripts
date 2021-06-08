# Mosaic the SOC maps

library(rgdal)
library(raster)

WD_files<-("D:/TRAINING_MATERIALS_GSOCseq_MAPS_12-11-2020/OUTPUTS/5_GLOBAL_MAPS/CORRECTED/Downscale_V8")
setwd(WD_files)

# Set the ISO code of the country
ISO="WORLD"


SOC_list<-list.files(path=WD_files,pattern=glob2rx("*T0_Map030_Corr_1km.tif"),full.names=TRUE)

Rrlist<-list()

for(i in 1:NROW(SOC_list)){
  r<-raster(SOC_list[i])
  #name<-paste0(names(r),'_Corr_1km.tif')
  r[r<0]<-NA
  r[r>800]<-NA
  #plot(r)
  #boxplot(values(r))
  #boxplot(values(r),outline=FALSE)
  Rrlist[i]<-r
  #writeRaster(r,filename=name,format="GTiff")
}

t0_Mos<-do.call(mosaic,c(Rrlist,fun=mean,tolerance=0.5))

writeRaster(t0_Mos,("WORLD_GSOCseq_T0_Map030_Corr_1km.tif"),format="GTiff")

###

SOC_list<-list.files(path=WD_files,pattern=glob2rx("*_BAU_Map030_Corr_1km.tif"),full.names=TRUE)

Rrlist<-list()

for(i in 1:NROW(SOC_list)){
  r<-raster(SOC_list[i])
  #name<-paste0(names(r),'_Corr_1km.tif')
  r[r<0]<-NA
  r[r>800]<-NA
  #plot(r)
  #boxplot(values(r))
  #boxplot(values(r),outline=FALSE)
  Rrlist[i]<-r
  #writeRaster(r,filename=name,format="GTiff")
}

Bau_Mos<-do.call(mosaic,c(Rrlist,fun=mean,tolerance=0.5))

writeRaster(Bau_Mos,("WORLD_GSOCseq_BAU_Map030_Corr_1km.tif"),format="GTiff")

###

SOC_list<-list.files(path=WD_files,pattern=glob2rx("*_SSM1_Map030_Corr_1km.tif"),full.names=TRUE)

Rrlist<-list()

for(i in 1:NROW(SOC_list)){
  r<-raster(SOC_list[i])
  #name<-paste0(names(r),'_Corr_1km.tif')
  r[r<0]<-NA
  r[r>800]<-NA
  #plot(r)
  #boxplot(values(r))
  #boxplot(values(r),outline=FALSE)
  Rrlist[i]<-r
  #writeRaster(r,filename=name,format="GTiff")
}

SSM1<-do.call(mosaic,c(Rrlist,fun=mean,tolerance=0.5))


writeRaster(SSM1,("WORLD_GSOCseq_fincalSOC_SSM1_Map030_Corr_1km.tif"),format="GTiff")

###

SOC_list<-list.files(path=WD_files,pattern=glob2rx("*_SSM2_Map030_Corr_1km.tif"),full.names=TRUE)

Rrlist<-list()

for(i in 1:NROW(SOC_list)){
  r<-raster(SOC_list[i])
  #name<-paste0(names(r),'_Corr_1km.tif')
  r[r<0]<-NA
  r[r>800]<-NA
  #plot(r)
  #boxplot(values(r))
  #boxplot(values(r),outline=FALSE)
  Rrlist[i]<-r
  #writeRaster(r,filename=name,format="GTiff")
}

SSM2<-do.call(mosaic,c(Rrlist,fun=mean,tolerance=0.5))


writeRaster(SSM2,("WORLD_GSOCseq_finalSOC_SSM2_Map030_Corr_1km.tif"),format="GTiff")

###

SOC_list<-list.files(path=WD_files,pattern=glob2rx("*_SSM3_Map030_Corr_1km.tif"),full.names=TRUE)

Rrlist<-list()

for(i in 1:NROW(SOC_list)){
  r<-raster(SOC_list[i])
  #name<-paste0(names(r),'_Corr_1km.tif')
  r[r<0]<-NA
  r[r>800]<-NA
  #plot(r)
  #boxplot(values(r))
  #boxplot(values(r),outline=FALSE)
  Rrlist[i]<-r
  #writeRaster(r,filename=name,format="GTiff")
}

SSM3<-do.call(mosaic,c(Rrlist,fun=mean,tolerance=0.5))


writeRaster(SSM3,("WORLD_GSOCseq_finalSOC_SSM3_Map030_Corr_1km.tif"),format="GTiff")

###########################################################################
rm(list=ls())
###########################################################################
WD_files<-("D:/TRAINING_MATERIALS_GSOCseq_MAPS_12-11-2020/OUTPUTS/5_GLOBAL_MAPS/CORRECTED/Downscale_V8")
setwd(WD_files)

### 
#Absolute Differences
###

t0_Mos<-raster("WORLD_GSOCseq_T0_Map030_Corr_1km.tif")
Bau_Mos<-raster("WORLD_GSOCseq_BAU_Map030_Corr_1km.tif")
SSM1<-raster("WORLD_GSOCseq_finalSOC_SSM1_Map030_Corr_1km.tif")
SSM2<-raster("WORLD_GSOCseq_finalSOC_SSM2_Map030_Corr_1km.tif")
SSM3<-raster("WORLD_GSOCseq_finalSOC_SSM3_Map030_Corr_1km.tif")

t0_Mos_n <- crop(extend(t0_Mos, Bau_Mos), Bau_Mos)

Bau_Mos<-mask(Bau_Mos,t0_Mos_n)

funDif<-function(r1,r2){return(r1-r2)}

AbsDiffBAU<-overlay(Bau_Mos,t0_Mos_n,fun=funDif)

AbsDiffBAU[AbsDiffBAU<=-80]<-NA
AbsDiffBAU[AbsDiffBAU>=80]<-NA

writeRaster(AbsDiffBAU,("WORLD_GSOCseq_AbsDiff_BAU_Map030_Corr_1km.tif"),format="GTiff")

AbsDiffSSM1<-overlay(SSM1,t0_Mos_n,fun=funDif)

AbsDiffSSM1[AbsDiffSSM1<=-80]<-NA
AbsDiffSSM1[AbsDiffSSM1>=80]<-NA

writeRaster(AbsDiffSSM1,("WORLD_GSOCseq_AbsDiff_SMM1_Map030_Corr_1km.tif"),format="GTiff")

AbsDiffSSM2<-overlay(SSM2,t0_Mos_n,fun=funDif)

AbsDiffSSM2[AbsDiffSSM2<=-80]<-NA
AbsDiffSSM2[AbsDiffSSM2>=80]<-NA

writeRaster(AbsDiffSSM2,("WORLD_GSOCseq_AbsDiff_SMM2_Map030_Corr_1km.tif"),format="GTiff")

AbsDiffSSM3<-overlay(SSM3,t0_Mos_n,fun=funDif)

AbsDiffSSM3[AbsDiffSSM3<=-80]<-NA
AbsDiffSSM3[AbsDiffSSM3>=80]<-NA

writeRaster(AbsDiffSSM3,("WORLD_GSOCseq_AbsDiff_SSM3_Map030_Corr_1km.tif"),format="GTiff")

###
#Relative Differences
###

RelDiffSSM1<-overlay(SSM1,Bau_Mos,fun=funDif)  

RelDiffSSM1[RelDiffSSM1<0]<-NA
RelDiffSSM1[RelDiffSSM1>=80]<-NA

writeRaster(RelDiffSSM1,("WORLD_GSOCseq_RelDiff_SMM1_Map030_Corr_1km.tif"),format="GTiff")

RelDiffSSM2<-overlay(SSM2,Bau_Mos,fun=funDif)

RelDiffSSM2[RelDiffSSM2<0]<-NA
RelDiffSSM2[RelDiffSSM2>=80]<-NA
writeRaster(RelDiffSSM2,("WORLD_GSOCseq_RelDiff_SMM2_Map030_Corr_1km.tif"),format="GTiff")

RelDiffSSM3<-overlay(SSM3,Bau_Mos,fun=funDif)

RelDiffSSM3[RelDiffSSM3<0]<-NA
RelDiffSSM3[RelDiffSSM3>=80]<-NA
writeRaster(RelDiffSSM3,("WORLD_GSOCseq_RelDiff_SSM3_Map030_Corr_1km.tif"),format="GTiff")


###
# Absolute Sequestration Rates
###

funRate<-function(x){x/20}

ASRBAU<-calc(AbsDiffBAU,funRate)

ASRBAU[ASRBAU<=-4]<-NA
ASRBAU[ASRBAU>4]<-NA
writeRaster(ASRBAU,("WORLD_GSOCseq_ASR_BAU_Map030_Corr_1km.tif"),format="GTiff")


WD_files<-("D:/TRAINING_MATERIALS_GSOCseq_MAPS_12-11-2020/OUTPUTS/5_GLOBAL_MAPS/CORRECTED/Downscale_V8")
setwd(WD_files)

AbsDiffSSM1<-raster("WORLD_GSOCseq_AbsDiff_SMM1_Map030_Corr_1km.tif")
AbsDiffSSM2<-raster("WORLD_GSOCseq_AbsDiff_SMM2_Map030_Corr_1km.tif")
AbsDiffSSM3<-raster("WORLD_GSOCseq_AbsDiff_SSM3_Map030_Corr_1km.tif")

RelDiffSSM1<-raster("WORLD_GSOCseq_RelDiff_SMM1_Map030_Corr_1km.tif")
RelDiffSSM2<-raster("WORLD_GSOCseq_RelDiff_SMM2_Map030_Corr_1km.tif")
RelDiffSSM3<-raster("WORLD_GSOCseq_RelDiff_SSM3_Map030_Corr_1km.tif")



ASRSSM1<-calc(AbsDiffSSM1,funRate)
ASRSSM1[ASRSSM1<=-4]<-NA
ASRSSM1[ASRSSM1>4]<-NA

writeRaster(ASRSSM1,("WORLD_GSOCseq_ASR_SSM1_Map030_Corr_1km.tif"),format="GTiff")

ASRSSM2<-calc(AbsDiffSSM2,funRate)
ASRSSM2[ASRSSM2<=-4]<-NA
ASRSSM2[ASRSSM2>4]<-NA
writeRaster(ASRSSM2,("WORLD_GSOCseq_ASR_SSM2_Map030_Corr_1km.tif"),format="GTiff")

ASRSSM3<-calc(AbsDiffSSM3,funRate)
ASRSSM3[ASRSSM3<=-4]<-NA
ASRSSM3[ASRSSM3>4]<-NA
writeRaster(ASRSSM3,("WORLD_GSOCseq_ASR_SSM3_Map030_Corr_1km.tif"),format="GTiff")

###
#Relative Sequestration Rate
###

RSRSSM1<-calc(RelDiffSSM1,funRate)
RSRSSM1[RSRSSM1<0]<-NA
RSRSSM1[RSRSSM1>4]<-NA
writeRaster(RSRSSM1,("WORLD_GSOCseq_RSR_SSM1_Map030_Corr_1km.tif"),format="GTiff")

RSRSSM2<-calc(RelDiffSSM2,funRate)
RSRSSM2[RSRSSM2<0]<-NA
RSRSSM2[RSRSSM2>4]<-NA
writeRaster(RSRSSM2,("WORLD_GSOCseq_RSR_SSM2_Map030_Corr_1km.tif"),format="GTiff")

RSRSSM3<-calc(RelDiffSSM3,funRate)
RSRSSM3[RSRSSM3<0]<-NA
RSRSSM3[RSRSSM3>4]<-NA
writeRaster(RSRSSM3,("WORLD_GSOCseq_RSR_SSM3_Map030_Corr_1km.tif"),format="GTiff")

