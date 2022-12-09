# Mario Guevara & Luciano Di Paolo
#script to erase the artifacts

# install packages
install.packages(c("landmap","geoR","plotKML","glmnet","xgboost","kernlab","deepnet","mlr","RStoolbox","spdep"))

#remove all objects from working folder
rm(list=ls())
#set path to working folder
path <- "C:/Users/lucia/Downloads/GSOCseq_Russia-20210717T210451Z-001/GSOCseq_Russia"
#set working folder
setwd(path)

#load requiered libraries
library(landmap)
library(rgdal)
library(geoR)
library(plotKML)
library(raster)
library(glmnet)
library(xgboost)
library(kernlab)
library(deepnet)
library(mlr)
library(RStoolbox)
library(spdep)

#Open the map with artifacts
target_ras <- raster('A/RUS_GSOCseq_AbsDiff_BAU_Map030_Corr.tif')

nlay<-names(target_ras)
#Open a vector of the area of interest
aoi<-readOGR("Test_subset.shp")
plot(aoi)
#crop the map with artifacts with the aoi
target_ras<-crop(target_ras,aoi)
#in case the map is too large or you are short of memory, reduce the number of pixels.
target_ras1<-aggregate(target_ras,fact=c(2,1),fun=mean)

#visualize the artifacts
plot(target_ras, zlim=c(-3, 3))

# IMPORTANT Reproject to metric coordinates 

target_ras<-projectRaster(target_ras, crs="+init=EPSG:3576")

#plot by quantiles
#qnt<- cut(target , breaks=quantile(target),  include.lowest=TRUE)
#visualize the differences
#plot(qnt, zlim=c(-3, 3))
#target <- crop(target, drawExtent())

#converto map to a spatial points data
target_all <- as(target_ras, 'SpatialPointsDataFrame')

#change the name of the layer in order to reuse these portion of the script
names(target_all)<-'layer'
#all points
target_all <- target_all[target_all$layer> -80.0000,]
target_all <- target_all[target_all$layer< 80.0000,]
#names(target_all)<-nlay
#set a seed for random numbers
set.seed(100)
#select 100 random points
target <- target_all[sample(nrow(target_all), 1500), ]

#read the covariates or prediction factors
covar<- raster('F/RUS_GSOCseq_T0_Map030_Corr.tif')
# crop the covariable
covar<-crop(covar,aoi)
# reduce the number of pixels 
covar<-aggregate(covar,fact=c(2,1),fun=mean)
# reproject to metric coordinates
covar<-projectRaster(covar, crs="+init=EPSG:3576")
#plot(covar)
covar_cr<-as(covar,'SpatialPixelsDataFrame')
#covar_cr[is.na(covar_cr)] <- -999

#crop to extent
ex <- extent(target)
#convert to spatial polygons
ex <- as(ex, 'SpatialPolygons')
#define projection system
proj4string(ex) <- CRS(projection(target))
#reproject to covar CRS
sh_cr <- spTransform(ex, CRS(projection(covar_cr)))

#reproject train to CRS of covar
train <- spTransform(target, CRS(projection(covar)))
#plot to verify spatial integrity 
plot(extent(covar_cr))
plot(train,add=TRUE)

#plot(extent(covar))
#plot(train, add=TRUE)
#cr <- crop(covar, drawExtent())
#datcr <- crop(train, cr)

#puedes cambiar a spc = TRUE si quieres que transforme a PCAs las covariables 
#(yo le puse false porque ya estaban transformadas) y 
#poner oblique.coords = TRUE para generar coordenadas rotadas 
#Read methodology  from Moller et al 
#https://soil.copernicus.org/articles/6/269/2020/ aqui 
#remove garbage
gc()
#train a model for downscaling the dataset
modelArt <- train.spLearner(train["layer"], covariates=covar_cr, parallel="multicore",spc=TRUE,oblique.coords=TRUE, lambda=1)

#remove some variables and garbage, we need all memory
rm(covar)
rm(covar_cr)
gc()

# Predict
modelArt_p <- predict(modelArt)


#revise the accuracy of the model
summary(modelArt@spModel$learner.model$super.model$learner.model)

#extract the digital soil maps
maps <- trim(stack(modelArt_p$pred))
#visualize the maps
plot(maps)
#
#qnt<- cut(maps$response , breaks=quantile(maps$response), include.lowest=TRUE)
#rescale to original data values
#x <-  rescaleImage(maps$response, ymin = min(target_all$layer), ymax = max(target_all$layer))
#make two panels
#par(mfrow=c(1,2))
#plot the original raster
#target_ras[target_ras == -999.0000] <- NA
#antes de downscaling
spplot(target_ras, main='original_output')

#target_ras

#despues de downscaling
spplot(x, main='downscaled_output')
#r<-projectRaster(x, crs="+init=EPSG:4326")
r2<-projectRaster(maps$response, crs="+init=EPSG:4326")

#Open the original layer, and resample the output to it.
target_ras <- raster('A/RUS_GSOCseq_AbsDiff_BAU_Map030_Corr.tif')

r2<-disaggregate(r2,fact=c(3,1),method='bilinear')

r2<-raster::resample(r2,target_ras)

#You can change the suffix
#save the result
writeRaster(r2, paste0(nlay,'_nArt.tif'),overwrite=TRUE)




