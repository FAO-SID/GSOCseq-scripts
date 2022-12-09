############################################
#                                          #                                                                           
#                                          # 
#   GSOCseq: Script 14 Points to Raster    #
#                                          #
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
wd <- 'C:/Users/hp/Documents/FAO/GSOCseq/OCB/GSOCseq_TM/'
setwd(wd)

# 3-digit Country ISO-code
ISO <-'THA_NP'
# File paths
AOI_path <- 'INPUTS/AOI_POLYGON/NP.shp'

SOC_path <- 'INPUTS/SOC_MAP/THA_NP_SOC.tif'

#Define path for where the final maps will be outputted 
tif_map_path <- 'OUTPUTS/4_MAPS'
# Load Packages 
library(terra)


# Open the shapefile of the region/country
AOI<-vect(AOI_path)

#Open FAO GSOC MAP , crop it and masked by the aoi. Then save it. 

SOC_MAP<-rast(SOC_path)

# Open the output shapefile from the Forward phase
FORWARD<-vect("OUTPUTS/3_FORWARD/THA_NP_FORWARD.shp")


#Creates emtpy raster 

empty_raster<-SOC_MAP*0


# Cut the raster with the country vector
Country_raster<-crop(empty_raster,AOI)

# Replace Na values for zero values
FORWARD[is.na(FORWARD)] <- -999

# Points to Raster BAU

setwd(tif_map_path)

Country_BAU_2040_Map<-rasterize(FORWARD, Country_raster ,FORWARD$SOC_BAU_20, updateValue='all')
writeRaster(Country_BAU_2040_Map,filename=paste0(ISO,"_GSOCseq_finalSOC_BAU_Map030.tif"),overwrite=TRUE)

# Points to Raster Low Scenario
Country_Lwr_2040_Map<-rasterize(FORWARD, Country_raster ,FORWARD$Lw_Sc, updateValue='all')
writeRaster(Country_Lwr_2040_Map,filename=paste0(ISO,"_GSOCseq_finalSOC_SSM1_Map030.tif"),overwrite=TRUE)

# Points to Raster Med Scenario
Country_Med_2040_Map<-rasterize(FORWARD, Country_raster ,FORWARD$Md_Sc, updateValue='all')
writeRaster(Country_Med_2040_Map,filename=paste0(ISO,"_GSOCseq_finalSOC_SSM2_Map030.tif"),overwrite=TRUE)

# Points to Raster High Scenario
Country_Hgh_2040_Map<-rasterize(FORWARD, Country_raster ,FORWARD$Hgh_S, updateValue='all')
writeRaster(Country_Hgh_2040_Map,filename=paste0(ISO,"_GSOCseq_finalSOC_SSM3_Map030.tif"),overwrite=TRUE)

# Points to Raster initial SOC (t0) 2018/2020
Country_SOC_2018_Map<-rasterize(FORWARD, Country_raster ,FORWARD$SOC_t0, updateValue='all')
writeRaster(Country_SOC_2018_Map,filename=paste0(ISO,"_GSOCseq_T0_Map030.tif"),overwrite=TRUE)

# Difference BAU 2040 - SOC 2018

Diff_BAU_SOC_2018<-Country_BAU_2040_Map-Country_SOC_2018_Map
writeRaster(Diff_BAU_SOC_2018,filename=paste0(ISO,"_GSOCseq_AbsDiff_BAU_Map030.tif"),overwrite=TRUE)
writeRaster(Diff_BAU_SOC_2018/20,filename=paste0(ISO,"_GSOCseq_ASR_BAU_Map030.tif"),overwrite=TRUE)

# Difference Low Scenario - SOC 2018

Diff_Lw_SOC_2018<-Country_Lwr_2040_Map-Country_SOC_2018_Map
writeRaster(Diff_Lw_SOC_2018,filename=paste0(ISO,"_GSOCseq_AbsDiff_SSM1_Map030.tif"),overwrite=TRUE)
writeRaster(Diff_Lw_SOC_2018/20,filename=paste0(ISO,"_GSOCseq_ASR_SSM1_Map030.tif"),overwrite=TRUE)

# Difference Med Scenario - SOC 2018

Diff_Md_SOC_2018<-Country_Med_2040_Map-Country_SOC_2018_Map
writeRaster(Diff_Md_SOC_2018,filename=paste0(ISO,"_GSOCseq_AbsDiff_SSM2_Map030.tif"),overwrite=TRUE)
writeRaster(Diff_Md_SOC_2018/20,filename=paste0(ISO,"_GSOCseq_ASR_SSM2_Map030.tif"),overwrite=TRUE)

# Difference High Scenario - SOC 2018

Diff_Hg_SOC_2018<-Country_Hgh_2040_Map-Country_SOC_2018_Map
writeRaster(Diff_Hg_SOC_2018,filename=paste0(ISO,"_GSOCseq_AbsDiff_SSM3_Map030.tif"),overwrite=TRUE)
writeRaster(Diff_Hg_SOC_2018/20,filename=paste0(ISO,"_GSOCseq_ASR_SSM3_Map030.tif"),overwrite=TRUE)

# Difference Low Scenario - BAU 2040

Diff_Lw_BAU_2040<-Country_Lwr_2040_Map-Country_BAU_2040_Map
writeRaster(Diff_Lw_BAU_2040,filename=paste0(ISO,"_GSOCseq_RelDiff_SSM1_Map030.tif"),overwrite=TRUE)
writeRaster(Diff_Lw_BAU_2040/20,filename=paste0(ISO,"_GSOCseq_RSR_SSM1_Map030.tif"),overwrite=TRUE)

# Difference Med Scenario - BAU 2040

Diff_Md_BAU_2040<-Country_Med_2040_Map-Country_BAU_2040_Map
writeRaster(Diff_Md_BAU_2040,filename=paste0(ISO,"_GSOCseq_RelDiff_SSM2_Map030.tif"),overwrite=TRUE)
writeRaster(Diff_Md_BAU_2040/20,filename=paste0(ISO,"_GSOCseq_RSR_SSM2_Map030.tif"),overwrite=TRUE)

# Difference High Scenario - BAU 2040

Diff_Hg_BAU_2040<-Country_Hgh_2040_Map-Country_BAU_2040_Map
writeRaster(Diff_Hg_BAU_2040,filename=paste0(ISO,"_GSOCseq_RelDiff_SSM3_Map030.tif"),overwrite=TRUE)
writeRaster(Diff_Hg_BAU_2040/20,filename=paste0(ISO,"_GSOCseq_RSR_SSM3_Map030.tif"),overwrite=TRUE)

# Uncertainties SOC 2018

UNC_2018<-rasterize(FORWARD, Country_raster ,FORWARD$UNC_0, updateValue='all')
writeRaster(UNC_2018,filename=paste0(ISO,"_GSOCseq_T0_UncertaintyMap030.tif"),overwrite=TRUE)

# Uncertainties SOC BAU 2038

UNC_BAU<-rasterize(FORWARD, Country_raster ,FORWARD$UNC_B, updateValue='all')
writeRaster(UNC_BAU,filename=paste0(ISO,"_GSOCseq_BAU_UncertaintyMap030.tif"),overwrite=TRUE)

# Uncertainties SOC SSM 

UNC_SSM<-rasterize(FORWARD, Country_raster ,FORWARD$UNC_S, updateValue='all')
writeRaster(UNC_SSM,filename=paste0(ISO,"_GSOCseq_SSM_UncertaintyMap030.tif"),overwrite=TRUE)

# Uncertainties for the Absolute difference SSM_ - SOC2018

UNC_abs_rate_BAU<- sqrt((FORWARD$UNC_B*FORWARD$SOC_BAU_20)^2 + (FORWARD$UNC_0*FORWARD$SOC_t0)^2)/abs(FORWARD$SOC_t0+FORWARD$SOC_BAU_20)
UNC_abs_rate_BAU_Map<-rasterize(FORWARD, Country_raster ,UNC_abs_rate_BAU, updateValue='all')
writeRaster(UNC_abs_rate_BAU_Map,filename=paste0(ISO,"_GSOCseq_ASR_BAU_UncertaintyMap030.tif"),overwrite=TRUE)

UNC_abs_rate_Lw<- sqrt((FORWARD$UNC_S*FORWARD$Lw_Sc)^2 + (FORWARD$UNC_0*FORWARD$SOC_t0)^2)/abs(FORWARD$SOC_t0+FORWARD$Lw_Sc)
UNC_abs_rate_Lw_Map<-rasterize(FORWARD, Country_raster ,UNC_abs_rate_Lw, updateValue='all')
writeRaster(UNC_abs_rate_Lw_Map,filename=paste0(ISO,"_GSOCseq_ASR_SSM1_UncertaintyMap030.tif"),overwrite=TRUE)

UNC_abs_rate_Md<- sqrt((FORWARD$UNC_S*FORWARD$Md_Sc)^2 + (FORWARD$UNC_0*FORWARD$SOC_t0)^2)/abs(FORWARD$SOC_t0+FORWARD$Md_Sc)
UNC_abs_rate_Md_Map<-rasterize(FORWARD, Country_raster ,UNC_abs_rate_Md, updateValue='all')
writeRaster(UNC_abs_rate_Md_Map,filename=paste0(ISO,"_GSOCseq_ASR_SSM2_UncertaintyMap030.tif"),overwrite=TRUE)

UNC_abs_rate_Hg<- sqrt((FORWARD$UNC_S*FORWARD$Hgh_S)^2 + (FORWARD$UNC_0*FORWARD$SOC_t0)^2)/abs(FORWARD$SOC_t0+FORWARD$Hgh_S)
UNC_abs_rate_Hg_Map<-rasterize(FORWARD, Country_raster ,UNC_abs_rate_Hg, updateValue='all')
writeRaster(UNC_abs_rate_Hg_Map,filename=paste0(ISO,"_GSOCseq_ASR_SSM3_UncertaintyMap030.tif"),overwrite=TRUE)

# Uncertainties for the Relative difference  SSM_ - SOCBAU

UNC_Rel_rate_Lw<- sqrt((FORWARD$UNC_S*FORWARD$Lw_Sc)^2 + (FORWARD$UNC_B*FORWARD$SOC_BAU_20)^2)/abs(FORWARD$SOC_BAU_20+FORWARD$Lw_Sc)
UNC_Rel_rate_Lw_Map<-rasterize(FORWARD, Country_raster ,UNC_Rel_rate_Lw, updateValue='all')
writeRaster(UNC_Rel_rate_Lw_Map,filename=paste0(ISO,"_GSOCseq_RSR_SSM1_UncertaintyMap030.tif"),overwrite=TRUE)

UNC_Rel_rate_Md<- sqrt((FORWARD$UNC_S*FORWARD$Md_Sc)^2 + (FORWARD$UNC_B*FORWARD$SOC_BAU_20)^2)/abs(FORWARD$SOC_BAU_20+FORWARD$Md_Sc)
UNC_Rel_rate_Md_Map<-rasterize(FORWARD, Country_raster ,UNC_Rel_rate_Md, updateValue='all')
writeRaster(UNC_Rel_rate_Md_Map,filename=paste0(ISO,"_GSOCseq_RSR_SSM2_UncertaintyMap030.tif"),overwrite=TRUE)

UNC_Rel_rate_Hg<- sqrt((FORWARD$UNC_S*FORWARD$Hgh_S)^2 + (FORWARD$UNC_B*FORWARD$SOC_BAU_20)^2)/abs(FORWARD$SOC_BAU_20+FORWARD$Hgh_S)
UNC_Rel_rate_Hg_Map<-rasterize(FORWARD, Country_raster ,UNC_Rel_rate_Hg, updateValue='all')
writeRaster(UNC_Rel_rate_Hg_Map,filename=paste0(ISO,"_GSOCseq_RSR_SSM3_UncertaintyMap030.tif"),overwrite=TRUE)






