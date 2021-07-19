##########################
#                        #
# GSOCseq Stats          #
# Stats per different    #
# categories             #
##########################

#empty global environment
rm(list = ls())

#User defined variables

#Path to the GSOCseq layer
dir = "C:/Users/hp/Documents/FAO/GSOCseq/World_5km"
#GSOCseq layer names (RSR, SSM1-3)
fn1 <- "C:/Users/hp/Documents/FAO/GSOCseq/World_5km/WORLD_KM5_GSOCseq_RSR_SSM1_Map030.tif"
fn2 <- "C:/Users/hp/Documents/FAO/GSOCseq/World_5km/WORLD_KM5_GSOCseq_RSR_SSM2_Map030.tif"
fn3 <- "C:/Users/hp/Documents/FAO/GSOCseq/World_5km/WORLD_KM5_GSOCseq_RSR_SSM3_Map030.tif"
#Define factor layer tile name
fact = "clim"
#output factor tile location
dir_out_fact = "C:/Users/hp/Documents/FAO/GSOCseq/GSOCseq_technical_report/data/clim/clim_tiles/"

#Load packages
library(raster)
library(rgdal)
library(data.table)
library(parallel)
library(doParallel)


#Get stats per land use 

#Divide the RSR layer into different tiles. No tiles are actually saved, this allows you to use a tif
#file like a database


#rsr <- readGDAL("C:/Users/hp/Documents/FAO/GSOCseq/World_1km/WORLD_GSOCseq_RSR_SSM3_Map030_Corr_1km.tif")
obj <- GDALinfo(fn1)

#Divide raster into tiles
tiles <- GSIF::getSpatialTiles(obj, block.x=50, return.SpatialPolygons = FALSE)

#This is if you want to look at the grid
#tiles.pol <- GSIF::getSpatialTiles(obj, block.x=50, return.SpatialPolygons = TRUE)
#tile.pol  <- SpatialPolygonsDataFrame(tiles.pol, tiles)
# #plot(raster(rsr), col=bpy.colors(20))
#lines(tile.pol, lwd=2)


#Write function to tile the factor layer, this can be done once
#the output tifs will be used later to run in parallel

#Land use layer
#lu <- raster(paste0(esa_folder,"ESA_Land_Cover_12clases_FAO_World_2015_1km.tif"))
#

# Climate
clim <- raster("C:/users/hp/documents/FAO/GSOCseq/GSOCseq_technical_report/data/clim/IPCC_CLIMATE_2919.tif")

#create a function that tiles the factor layer based on the GSOCseq tiles
fun_fact_tiles <-function(i, tiles, dir,fact_name, layer ,dir_out_fact,threshold=190,fn){
  out.tif = paste0(dir, "T", i, ".tif")
  if(!file.exists(out.tif)){
    x = raster(readGDAL(fn, offset=unlist(tiles[i,c("offset.y","offset.x")]),
                        region.dim=unlist(tiles[i,c("region.dim.y","region.dim.x")]),
                        output.dim=unlist(tiles[i,c("region.dim.y","region.dim.x")]),
                        silent = TRUE))
    lux <- crop(layer, x)
    lux <- resample(lux,x, method= "ngb")
    writeRaster(lux, paste0(dir_out_fact, fact, i, ".tif"), overwrite= TRUE)
    
  }
}
#Run in parallel
# mclapply(1:nrow(tiles), FUN=fun_fact_tiles, tiles=tiles, fact_name=fact, layer=lu) #this works with linux

no_cores <- detectCores(logical = TRUE)
cl <- makeCluster(no_cores-1)
registerDoParallel(cl)


seq_id_all<- seq_along(1:nrow(tiles))
clusterExport(cl,list('fun_fact_tiles','tiles', 'fact'))

clusterEvalQ(cl, {library(raster)
  library(rgdal)})

#Create factor layer tiles
system.time(
  results<- c(parLapply(cl,seq_id_all,fn=fn1,fun=fun_fact_tiles,tiles=tiles,fact=fact, dir=dir, layer=clim, dir_out_fact=dir_out_fact))
)

stopCluster(cl)

datalist =list()
fun_zonal_tiles <- function(i, tiles, dir,fact, threshold=190, fn1,fn2,fn3){
  out.tif = paste0(dir, "T", i, ".tif")
  if(!file.exists(out.tif)){
    #use the tif raster as a database and onu load the tile i
    x = raster(readGDAL(fn1, offset=unlist(tiles[i,c("offset.y","offset.x")]),
                        region.dim=unlist(tiles[i,c("region.dim.y","region.dim.x")]), 
                        output.dim=unlist(tiles[i,c("region.dim.y","region.dim.x")]),
                        silent = TRUE))
    x2 = raster(readGDAL(fn2, offset=unlist(tiles[i,c("offset.y","offset.x")]),
                         region.dim=unlist(tiles[i,c("region.dim.y","region.dim.x")]), 
                         output.dim=unlist(tiles[i,c("region.dim.y","region.dim.x")]),
                         silent = TRUE))
    x3 = raster(readGDAL(fn3, offset=unlist(tiles[i,c("offset.y","offset.x")]),
                         region.dim=unlist(tiles[i,c("region.dim.y","region.dim.x")]), 
                         output.dim=unlist(tiles[i,c("region.dim.y","region.dim.x")]),
                         silent = TRUE))
    
    x <- stack(x,x2,x3)
    
    lu = raster(paste0(dir_out_fact, fact, i, ".tif"))
    lu <- as.factor(lu)
    #correct pixel area and ha to km2
    x <- x*area(x)*100
    
    #calculate sum,mean and sd
    stats_sum <- as.data.frame(zonal(x,lu, fun ="sum",na.rm = TRUE))
    colnames(stats_sum) <- c("zone", "SSM1_sum","SSM2_sum","SSM3_sum")
    
    stats_mean <- as.data.frame(zonal(x,lu, fun ="mean",na.rm = TRUE))
    colnames(stats_mean) <- c("zone", "SSM1_mean","SSM2_mean","SSM3_mean")
    
    stats_sd <- as.data.frame(zonal(x,lu, fun ="sd",na.rm = TRUE))
    colnames(stats_sd) <- c("zone", "SSM1_sd","SSM2_sd","SSM3_sd")
    
    #stats_n <- as.data.frame(zonal(x,lu, fun ="count",na.rm = TRUE))
    
    stats <- merge(stats_sum,stats_mean, by= "zone", all.x= TRUE)
    stats <- merge(stats,stats_sd, by= "zone", all.x= TRUE)
    #stats <- merge(stats,stats_n, by= "zone", all.x= TRUE)
    
    #write.csv(stats, paste0(dir, "T", i, ".csv"))
    datalist[[i]] <- stats # add it to your list
  }
}


no_cores <- detectCores(logical = TRUE)
cl <- makeCluster(no_cores-1)  
registerDoParallel(cl)


seq_id_all<- seq_along(1:nrow(tiles))
clusterExport(cl,list('fun_zonal_tiles','tiles', 'fn1','fn2','fn2','dir','dir_out_fact', 'datalist'))

clusterEvalQ(cl, {library(raster)
  library(rgdal)})


system.time(
  results<- c(parLapply(cl,seq_id_all,fun=fun_zonal_tiles,tiles=tiles,fact=fact, fn1=fn1, 
                        fn2=fn2,fn3=fn3,dir=dir))
)

stopCluster(cl)


## Merge output
# files <-list.files(path = "C:/Users/hp/Documents/FAO/GSOCseq/World_1km/", pattern = ".csv")
# 
# dir <-"C:/Users/hp/Documents/FAO/GSOCseq/World_1km/"
# 
# df3 <- df_bind[, lapply(.SD, sum), by = .(zone)]
# 
# 
# 
# datalist = list()
# 
# for (i in files) {
#     dat <- fread(paste0(dir,i),select = c("zone", "sum"))
#     
#     datalist[[i]] <- dat # add it to your list
# }
#df_bind <- rbindlist(datalist)

df_bind <- rbindlist(results)

df_bind <- df_bind[df_bind$SSM1_sum >0,]

#Merge all to one
data <- df_bind[, .(SSM1_sum=sum(SSM1_sum),
                    SSM2_sum=sum(SSM2_sum),
                    SSM3_sum=sum(SSM3_sum),
                    SSM1_mean=mean(SSM1_mean),
                    SSM2_mean=mean(SSM2_mean),
                    SSM3_mean=mean(SSM3_mean),
                    SSM1_sd=sqrt(sum(SSM1_sd=SSM1_sd^2)),
                    SSM2_sd=sqrt(sum(SSM2_sd=SSM2_sd^2)),
                    SSM3_sd=sqrt(sum(SSM3_sd=SSM3_sd^2))),
                by = .(zone)]

write.csv(data, paste0("C:/Users/hp/Documents/FAO/GSOCseq/GSOCseq_technical_report/data/",
                       fact, "_GSOCseq_stats_allscenarios.csv"))
