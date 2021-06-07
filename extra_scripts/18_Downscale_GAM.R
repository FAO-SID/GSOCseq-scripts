# Downscale of the 5 km GSOC SEQ MAP
# using DISSEVER package. 
# https://cran.r-project.org/web/packages/dissever/vignettes/dissever-demo.html

# https://www.sciencedirect.com/science/article/pii/S0098300411002895?via%3Dihub

library(raster)
library(spatialEco)
library(rgdal)
library(dissever)

library(viridis)
library(dplyr)
library(magrittr)

library("writexl")

# Open covariables

#SOC<-raster("D:/TRAINING_MATERIALS_GSOCseq_MAPS_12-11-2020/INPUTS/SOC_MAP/GSOCmap_1.6.1.tif")
#Clay<-raster("D:/TRAINING_MATERIALS_GSOCseq_MAPS_12-11-2020/INPUTS/CLAY/Clay_WA_AOI_1km_World.tif")
#LU<-raster("D:/TRAINING_MATERIALS_GSOCseq_MAPS_12-11-2020/INPUTS/LAND_USE/ESA_Land_Cover_12clases_FAO_World_1km.tif")

#Clay<-crop(Clay,SOC)
#Clay<-resample(Clay,SOC,method='bilinear')
#Clay[Clay<0]<-NA

#CovStack<-stack(SOC,Clay,LU)

#writeRaster(CovStack, filename="D:/TRAINING_MATERIALS_GSOCseq_MAPS_12-11-2020/INPUTS/STACK/CovSTACK_downscale_1km.tif",format="GTiff")

CovStack<-stack("D:/TRAINING_MATERIALS_GSOCseq_MAPS_12-11-2020/INPUTS/STACK/CovSTACK_downscale_1km.tif")

LU_mask<-raster("D:/TRAINING_MATERIALS_GSOCseq_MAPS_12-11-2020/INPUTS/LAND_USE/LandUse_mask2351213.tif")


WD_files<-("D:/TRAINING_MATERIALS_GSOCseq_MAPS_12-11-2020/OUTPUTS/5_GLOBAL_MAPS/CORRECTED/INPUTS_Downscale")
ISO="WORLD"
setwd(WD_files)
SOC_list<-list.files(path=WD_files,pattern=glob2rx(paste0(ISO,"*_Corr.tif")),full.names=TRUE)

#AOI<-readOGR("D:/TRAINING_MATERIALS_GSOCseq_MAPS_12-11-2020/INPUTS/AOI_POLYGON/Regiones_v3.shp")

Grid<-readOGR("D:/TRAINING_MATERIALS_GSOCseq_MAPS_12-11-2020/INPUTS/AOI_POLYGON/Regiones_v8.shp")

setwd("D:/TRAINING_MATERIALS_GSOCseq_MAPS_12-11-2020/OUTPUTS/5_GLOBAL_MAPS/CORRECTED/Downscale_V8")


for (j in 1:nrow(Grid@data)){

id<- Grid@data[j,]
CovStackGrid<-crop(CovStack,Grid[j,])
m1<-crop(LU_mask,Grid[j,])
CovStackGrid<-mask(CovStackGrid,m1,maskvalue=0)
CovStackGrid<-mask(CovStackGrid,Grid[j,])
plot(CovStackGrid)
CovStackGrid<-stack(CovStackGrid)

#CovStack<-crop(CovStack,AOI[12,])# cambiar numero de continente


## Using dissver
# The fine data must be a stack layer


min_iter <- 5 # Minimum number of iterations
max_iter <- 10 # Maximum number of iterations
p_train <- 0.0025 # Subsampling of the initial data



rsquetelist<-data.frame()

for (i in 1:NROW(SOC_list)){
  id<-paste0(i,'_',j)
  r<-raster(SOC_list[i])
  r1<-crop(r,Grid[j,]) # cambiar numero de continente
  r1<-mask(r1,Grid[j,])
  name<-paste0(names(r),id,'_Corr.tif')
  r1<-focal(r1, w = matrix(1,5,5), fun= mean,  na.rm = TRUE, NAonly=TRUE , pad=TRUE)

  res_GAM <- dissever(
    coarse = r1, # stack of fine resolution covariates
    fine = CovStackGrid, # coarse resolution raster
    method ="gamSpline", # regression method used for disseveration
    p = p_train, # proportion of pixels sampled for training regression model
    min_iter = min_iter, # minimum iterations
    max_iter = max_iter # maximum iterations
  )
  preds <- extractPrediction(list(res_GAM$fit))
  
      perf <- preds %>% 
      summarise(
        rsq = cor(obs, pred)^2, 
        rmse = sqrt(mean((pred - obs)^2))
      )
    perf
    
    rsquetelist[i,1:2]<-perf
    rsquetelist[i,3]<-name
    #p<-plotObsVsPred(preds)
    #p
    #dev.copy2pdf(file = paste0(id,name,'_graph.pdf'))
    
    name<-paste0(id,names(r),'_1km.tif')
    writeRaster(res_GAM$map, filename=name,format="GTiff")
    #dev.off()
    
}
 name<-paste0(id,'.xlsx')
    write_xlsx(rsquetelist,name)
}
