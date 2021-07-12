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
dir = "C:/Users/hp/Documents/FAO/GSOCseq/GSOCseq_technical_report/data/GSOCseq/"
#GSOCseq layer name
fn <- "C:/Users/hp/Documents/FAO/GSOCseq/GSOCseq_technical_report/data/GSOCseq/WORLD_GSOCseq_RSR_SSM3_Map030_Corr_1km.tif"
#Define factor layer tile name
fact = "LU"
#output factor tile location
dir_out_fact = "C:/Users/hp/Documents/FAO/GSOCseq/GSOCseq_technical_report/data/LU/LU_tiles/"

#Load packages
library(raster)
library(rgdal)
library(data.table)
library(parallel)
library(doParallel)


###########################
#                         #
# Figure 1: RSR Land use  #
#                         #
###########################

#Divide the RSR layer into different tiles. No tiles are actually saved, this allows you to use a tif
#file like a database


#rsr <- readGDAL("C:/Users/hp/Documents/FAO/GSOCseq/World_1km/WORLD_GSOCseq_RSR_SSM3_Map030_Corr_1km.tif")
obj <- GDALinfo(fn)

#Divide raster into tiles
tiles <- GSIF::getSpatialTiles(obj, block.x=50, return.SpatialPolygons = FALSE)

#This is if you want to look at the grid
# tiles.pol <- GSIF::getSpatialTiles(obj, block.x=50, return.SpatialPolygons = TRUE)
# tile.pol  <- SpatialPolygonsDataFrame(tiles.pol, tiles)
# #plot(raster(rsr), col=bpy.colors(20))
# #lines(tile.pol, lwd=2)


#Write function to tile the factor layer, this can be done once
#the output tifs will be used later to run in parallel

# #Land use layer
#lu <- raster(paste0(esa_folder,"ESA_Land_Cover_12clases_FAO_World_2015_1km.tif"))
# 

# 
# #create a function that tiles the factor layer based on the GSOCseq tiles
# fun_fact_tiles <-function(i, tiles, dir,fact_name, layer ,dir_out_fact,threshold=190,fn){
#   out.tif = paste0(dir, "T", i, ".tif")
#   if(!file.exists(out.tif)){
#     x = raster(readGDAL(fn, offset=unlist(tiles[i,c("offset.y","offset.x")]),
#                         region.dim=unlist(tiles[i,c("region.dim.y","region.dim.x")]),
#                         output.dim=unlist(tiles[i,c("region.dim.y","region.dim.x")]),
#                         silent = TRUE))
#     lux <- crop(layer, x)
#     writeRaster(lux, paste0(dir_out_fact, fact, i, ".tif"), overwrite= TRUE)
# 
# }
# }
# #Run in parallel 
# # mclapply(1:nrow(tiles), FUN=fun_fact_tiles, tiles=tiles, fact_name=fact, layer=lu) #this works with linux
# 
# no_cores <- detectCores(logical = TRUE)
# cl <- makeCluster(no_cores-1)  
# registerDoParallel(cl)
# 
# 
# seq_id_all<- seq_along(1:nrow(tiles))
# clusterExport(cl,list('fun_fact_tiles','tiles', 'fact'))
# 
# clusterEvalQ(cl, {library(raster)
#   library(rgdal)})
# 
# #Create factor layer tiles
# system.time(
#   results<- c(parLapply(cl,seq_id_all,fn=fn,fun=fun_fact_tiles,tiles=tiles,fact=fact, dir=dir, layer=lu, dir_out_fact=dir_out_fact))
# )
# 
# stopCluster(cl)

datalist =list()
fun_zonal_tiles <- function(i, tiles, dir,fact, threshold=190, fn){
  out.tif = paste0(dir, "T", i, ".tif")
  if(!file.exists(out.tif)){
    #use the tif raster as a database and onu load the tile i
    x = raster(readGDAL(fn, offset=unlist(tiles[i,c("offset.y","offset.x")]),
                        region.dim=unlist(tiles[i,c("region.dim.y","region.dim.x")]), 
                        output.dim=unlist(tiles[i,c("region.dim.y","region.dim.x")]),
                        silent = TRUE))
    
    lu = raster(paste0(dir_out_fact, fact, i, ".tif"))
    lu <- as.factor(lu)
    #correct pixel area and ha to km2
    x <- x*area(x)*100
    #calculate sum,mean and sd
    stats_sum <- as.data.frame(zonal(x,lu, fun ="sum",na.rm = TRUE))
    stats_mean <- as.data.frame(zonal(x,lu, fun ="mean",na.rm = TRUE))
    stats_sd <- as.data.frame(zonal(x,lu, fun ="sd",na.rm = TRUE))
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
clusterExport(cl,list('fun_zonal_tiles','tiles', 'fn','dir','dir_out_fact', 'datalist'))

clusterEvalQ(cl, {library(raster)
  library(rgdal)})


system.time(
  results<- c(parLapply(cl,seq_id_all,fun=fun_zonal_tiles,tiles=tiles,fact=fact, fn=fn))
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

#Merge all to one
data <- df_bind[, .(sum=sum(sum),mean=mean(mean), sd=sqrt(sum(sd=sd^2))), by = .(zone)]


