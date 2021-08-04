# Mask values with T0 out of range. 
library(rgdal)
library(raster)
WD_files<-("D:/TRAINING_MATERIALS_GSOCseq_MAPS_12-11-2020/OUTPUTS/5_GLOBAL_MAPS/CORRECTED/Downscale_v8/1km")
setwd(WD_files)

# Open the t0  raster layer
name=paste0('WORLD_GSOCseq_T0_Map030_Corr_1km.tif')

T0_map<-raster(name)

Map_list<-list.files(path=WD_files,pattern=glob2rx(paste0("*.tif")),full.names=TRUE)
setwd("D:/TRAINING_MATERIALS_GSOCseq_MAPS_12-11-2020/OUTPUTS/5_GLOBAL_MAPS/CORRECTED/Downscale_v8/1km/t0mask")
for(i in 13:NROW(Map_list)){
  r<-raster(Map_list[i])
  filename=names(r)
  filename=paste0(filename,'_t0mask')
  r<-resample(r,T0_map)
  r<-mask(r,T0_map,updatevalue=-999)
  writeRaster(r,filename=filename,format='GTiff')
}
