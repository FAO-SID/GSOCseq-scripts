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

#GSOCseq layer names
files <- list.files(path=dir, pattern ="RSR", full.names=TRUE)

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
obj <- GDALinfo(files[1])

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
# clim <- raster("C:/users/hp/documents/FAO/GSOCseq/GSOCseq_technical_report/data/clim/IPCC_CLIMATE_2919.tif")
# 
# #create a function that tiles the factor layer based on the GSOCseq tiles
#  fun_fact_tiles <-function(i, tiles,fact_name, layer ,dir_out_fact,threshold=190,files){
#    out.tif = paste0(dir, "T", i, ".tif")
#    if(!file.exists(out.tif)){
#      x = raster(readGDAL(files[1], offset=unlist(tiles[i,c("offset.y","offset.x")]),
#                          region.dim=unlist(tiles[i,c("region.dim.y","region.dim.x")]),
#                          output.dim=unlist(tiles[i,c("region.dim.y","region.dim.x")]),
#                          silent = TRUE))
#      lux <- crop(layer, x)
#      lux <- resample(lux,x, method= "ngb")
#      writeRaster(lux, paste0(dir_out_fact, fact, i, ".tif"), overwrite= TRUE)
# 
#  }
#  }
#  #Run in parallel
#  # mclapply(1:nrow(tiles), FUN=fun_fact_tiles, tiles=tiles, fact_name=fact, layer=lu) #this works with linux
# 
#  no_cores <- detectCores(logical = TRUE)
#  cl <- makeCluster(no_cores-1)
#  registerDoParallel(cl)
# 
# 
#  seq_id_all<- seq_along(1:nrow(tiles))
#  clusterExport(cl,list('fun_fact_tiles','tiles', 'fact'))
# 
#  clusterEvalQ(cl, {library(raster)
#    library(rgdal)})
# 
#  #Create factor layer tiles
#  system.time(
#    results<- c(parLapply(cl,seq_id_all,fn=fn1,fun=fun_fact_tiles,tiles=tiles,fact=fact, dir=dir, layer=clim, dir_out_fact=dir_out_fact))
#  )
# 
#  stopCluster(cl)

datalist =list()
stats_all = list()
fun_zonal_tiles <- function(i, tiles, dir,fact, threshold=190,files){
  for (j in seq_along(files)){
    x = raster(readGDAL(files[j], offset=unlist(tiles[i,c("offset.y","offset.x")]),
                        region.dim=unlist(tiles[i,c("region.dim.y","region.dim.x")]), 
                        output.dim=unlist(tiles[i,c("region.dim.y","region.dim.x")]),
                        silent = TRUE))
    #Product name
    product <- ifelse(grepl("Rel",files[j]), "reldiff", 
                      ifelse(grepl("Abs",files[j]), "absdiff",
                             ifelse(grepl("RSR",files[j]), "RSR",
                                    ifelse(grepl("ASR",files[j]), "ASR",
                                           ifelse(grepl("final",files[j]), "final", 
                                                  ifelse(grepl("_T0",files[j]), "T0","NA" ))))))
    scenario <- ifelse(grepl("SSM1",files[j]), "SSM1", 
                       ifelse(grepl("SSM2",files[j]), "SSM2",
                              ifelse(grepl("SSM3",files[j]), "SSM3",
                                     ifelse(grepl("BAU",files[j]), "BAU",
                                            ifelse(grepl("_T0",files[j]), "T0","NA" )))))
    
    lu = raster(paste0(dir_out_fact, fact, i, ".tif"))
    lu <- as.factor(lu)
    
    #calculate sum,mean and sd
    stats_mean <- as.data.frame(zonal(x,lu, fun ="mean",na.rm = TRUE))
    stats_mean$product <- paste0(product, "_", scenario)
    
    #correct pixel area and ha to km2
    x <- x*area(x)*100
    
    
    stats_sum <- as.data.frame(zonal(x,lu, fun ="sum",na.rm = TRUE))
    stats_sum$product <- paste0(product, "_", scenario)
    
    
    
    stats_sd <- as.data.frame(zonal(x,lu, fun ="sd",na.rm = TRUE))
    stats_sd$product <- paste0(product, "_", scenario)
    
    #stats_n <- as.data.frame(zonal(x,lu, fun ="count",na.rm = TRUE))
    
    stats <- merge(stats_sum,stats_mean, by= c("zone", "product"), all.x= TRUE)
    stats <- merge(stats,stats_sd, by= c("zone", "product"), all.x= TRUE)
    #stats <- merge(stats,stats_n, by= "zone", all.x= TRUE)
    
    #write.csv(stats, paste0(dir, "T", i, ".csv"))
    stats_all[[j]] <- stats # add it to your list
    stats <- rbindlist(stats_all)
  }
  datalist[[i]] <- stats
}


no_cores <- detectCores(logical = TRUE)
cl <- makeCluster(no_cores-1)  
registerDoParallel(cl)


seq_id_all<- seq_along(1:nrow(tiles))
clusterExport(cl,list('fun_zonal_tiles','tiles', 'files','dir_out_fact', 'datalist', 'stats_all'))

clusterEvalQ(cl, {library(raster)
  library(rgdal)
  library(data.table)})


system.time(
  results<- c(parLapply(cl,seq_id_all,fun=fun_zonal_tiles,tiles=tiles,fact=fact, files=files))
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

df_bind <- df_bind[df_bind$sum >0,]

#Merge all to one
#Merge all to one
data <- df_bind[, .(sum=sum(sum),
                    mean=mean(mean),
                    sd=sqrt(sum(sd))), by=.(zone,product)]

write.csv(data, paste0("C:/Users/hp/Documents/FAO/GSOCseq/GSOCseq_technical_report/data/",
                       fact, "_GSOCseq_stats_allscenarios.csv"))
