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
fact = "Lu"
#output factor tile location
dir_out_fact = "C:/Users/hp/Documents/FAO/GSOCseq/GSOCseq_technical_report/data/LU/LU_tiles/"

#Load packages
library(raster)
library(rgdal)
library(data.table)
library(parallel)
library(doParallel)


#Divide the GSOCseq layer into different tiles. No tiles are actually saved, this allows you to use a tif
#file like a database

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


#Create a function to get zonal stats per tile
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
    
    #Load factor tile
    lu = raster(paste0(dir_out_fact, fact, i, ".tif"))
    
    
    
    #calculate sum,mean and sd
    stats_mean <-ifelse(all(is.na(values(lu))), "No_data", list(zonal(x,lu, fun ="mean",na.rm = TRUE)))
    stats_mean <- as.data.frame(stats_mean)
    #correct pixel area and ha to km2
    x <- x*area(x)*100
    
    
    stats_sum <- ifelse(all(is.na(values(lu))), "No_data", list(zonal(x,lu, fun ="sum",na.rm = TRUE)))
    stats_sum <- as.data.frame(stats_sum)
    stats_sd <- ifelse(all(is.na(values(lu))), "No_data", list(zonal(x,lu, fun ="sd",na.rm = TRUE)))
    stats_sd <- as.data.frame(stats_sd)
    #stats_n <- as.data.frame(zonal(x,lu, fun ="count",na.rm = TRUE))
    
    stats <- merge(stats_sum,stats_mean, by= c("zone"), all.x= TRUE)
    
    stats <-merge(stats,stats_sd, by= c("zone"), all.x= TRUE)
    
    stats$product <-paste0(product, "_", scenario)
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

df_bind <- rbindlist(results)

df_bind <- df_bind[df_bind$sum >0,]


#Merge all to one
data <- df_bind[, .(sum=sum(sum),
                    mean=mean(mean),
                    sd=sqrt(sum(sd))), by=.(zone,product)]

write.csv(data, paste0("C:/Users/hp/Documents/FAO/GSOCseq/GSOCseq_technical_report/data/",
                       fact, "_GSOCseq_stats_allscenarios.csv"))

