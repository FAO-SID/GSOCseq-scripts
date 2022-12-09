# Cut the Gsoc Seq products by country

library(raster)
library(rgdal)

WD_files<-("D:/TRAINING_MATERIALS_GSOCseq_MAPS_12-11-2020/OUTPUTS/5_GLOBAL_MAPS/CORRECTED/Downscale_v8/1km")
setwd(WD_files)
# Create a list of the layers

Map_list<-list.files(path=WD_files,pattern=glob2rx(paste0("*.tif")),full.names=TRUE)

# Open shapefile with countries to clip with. 

Countries<-readOGR("D:/TRAINING_MATERIALS_GSOCseq_MAPS_12-11-2020/INPUTS/AOI_POLYGON/prueba_3_paises.shp")


for(i in 1:NROW(Map_list)){
  r<-raster(Map_list[i])
for (j in 1:length(Countries)){
  
rc<-crop(r,Countries[j,])
rc<-mask(rc,Countries[j,])
# define the column with the iso code or name of the country
ISO=Countries@data$nam[j]
name<-paste0(ISO,'_',names(r),'.tif')
writeRaster(rc,filename=name,format="GTiff")
}}