#++++++++++++++++++++++++++++++++++++#
#                                    # 
# Mitigation potential of soils      #
# based on FAOSTAT total             #
# Agricultural emissions             #
# Isabel Luotto 24/05/21             #
#                                    #
#++++++++++++++++++++++++++++++++++++#

# Empty the environment 
rm(list = ls())

# Load libraries
library(FAOSTAT)
library(countrycode)
library(raster)
library(sp)
library(plotly)

# User defined variables

## Working directory (wd). It should end with a "/"
#wd <- "C:/Users/hp/Documents/FAO/GSOCseq/"
wd <- "C:/Users/luottoi/Documents/GSOCseq/"


## GSOCseq output maps directory. The paste0 function will combine
## your working directory wd with the folder name that contains
## your output maps
GSOCseq_folder <- paste0(wd,"Cuba/maps/")


## Create a folder called FAOSTAT in your working directory
## you can do it manually or by using the dir.create function 
## remember to comment the dir.function command after running it once

#dir.create(paste0(wd, "/FAOSTAT/"))
## Define a path to the FAOSTAT folder
data_folder <- paste0(wd,"FAOSTAT/")

## Country of interest (specified by the 3-digit ISO code)
ISO <- "CUB"
country <- "Cuba"

## Carbon ton to CO2 conversion
CO2conv <- 3.67

# Set the working directory
setwd(wd)

# Download the total agricultural emissions from FAOSTAT
# use the link below to download the data once by using the
# download_faostat_bulk function
# after downloading comment lines 56-57

#all_em <- "http://fenixservices.fao.org/faostat/static/bulkdownloads/Emissions_Agriculture_Agriculture_total_E_All_Data.zip"
#download_faostat_bulk(all_em, data_folder)
data <- read_faostat_bulk(paste0(data_folder,"Emissions_Agriculture_Agriculture_total_E_All_Data.zip")) 

# Extract the most recent year
data <- data[, c("area_code","area", "item","y2018","element", "unit")]

# Convert FAO country codes to ISO codes
data$ISO <- countrycode(data$area_code, origin = 'fao', destination = 'iso3c')
data <- data[data$ISO == ISO & data$item == "Agriculture total",]

# Extract CO2 equivalent
data <-data[data$element ==  "Emissions (CO2eq)",]
data <- na.omit(data)

# Calculate total agricultural emissions in Gigagrams
# this sums total agricultural emissions in CO2 eq and emissions from soils
totagrem <- round(sum(data$y2018),2)

# Load Output Maps: Relative Sequestration Rates (SSM-BAU)
# paste0 will combine the path to the outputs maps contained in
# GSOCseq_folder and the ISO variable 
# if you get an error message, make sure that paste0 is creating a correct
# path
SSM1 <- raster(paste0(GSOCseq_folder, country,"_GSOCseq_RSR_SSM1_Map030.tif"))
SSM2 <- raster(paste0(GSOCseq_folder,country, "_GSOCseq_RSR_SSM2_Map030.tif"))
SSM3 <- raster(paste0(GSOCseq_folder,country, "_GSOCseq_RSR_SSM3_Map030.tif"))

# Correct RSR values based on pixel area
SSM1 <- SSM1 * area(SSM1)
SSM2 <- SSM2 * area(SSM2)
SSM3 <- SSM3 * area(SSM3)


# Sum over every pixel to get total sequestration potential in gigagrams
totssm1 <- round(((sum(unique(SSM1[!is.na(SSM1)]))*CO2conv*100)/1000),2)
totssm2 <- round(((sum(unique(SSM2[!is.na(SSM2)]))*CO2conv*100)/1000),2)
totssm3 <- round(((sum(unique(SSM3[!is.na(SSM3)]))*CO2conv*100)/1000),2)

# Estimate mitigation potential 
SSM1 <-round(((totssm1/totagrem) *100),2)
SSM2 <-round(((totssm2/totagrem) *100),2)
SSM3 <-round(((totssm3/totagrem) *100),2)

# Create table 
table <- data.frame(Total_Agricultural_Emissions =c("CO2eq gigagrams/year", totagrem, totagrem, totagrem),
                    Scenario = c("/", "SSM1", "SSM2", "SSM3"),
                    C_sequestered =c("CO2eq gigagrams/year",totssm1,totssm2, totssm3),
                    Mitigation_share =c("%",SSM1,SSM2,SSM3))

table
# Display the table in the console
data <-data.frame(Total_Agricultural_Emissions =c(totagrem, totagrem, totagrem),
                  Scenario = c("SSM1", "SSM2", "SSM3"),
                  C_sequestered =c(totssm1,totssm2, totssm3),
                  Mitigation_share =c(SSM1,SSM2,SSM3))


###################
#
# Waterfall chart
# Potential CO2 mitigation
#
######################

# Create the input dataframe
x= list("Total Agr. emissions", "SSM1", "SSM2", "SSM3", "Final emissions")
measure= c("relative", "relative", "relative", "relative", "total")

## Calculate final emissions [tot agr. em - CO2 eq seq under SSM3]
final <- totagrem - totssm3

## Calculate additional CO2 seq for every scenario
change_ssm1 <- totssm1
change_ssm2 <- totssm2 -totssm1
change_ssm3 <- totssm3 -totssm2 

##Create labels
text= c(paste0("+",totagrem), 
        paste0("-", change_ssm1), 
        paste0("-", change_ssm2),
        paste0("-", change_ssm3),
        paste0("+",final ))

y= c(totagrem, -change_ssm1 , -change_ssm2, -change_ssm3,0)

##Create final data frame for plotly
data = data.frame(x=factor(x,levels=x),measure,text,y)



fig <- plot_ly(
  data, name = "20", type = "waterfall", measure = ~measure,
  decreasing = list(marker = list(color = "Green")),
  increasing = (marker = list(color = "orange")),
  totals = list(marker = list(color = "orange")),
  x = ~x, textposition = "outside", y= ~y, text =~text,
  connector = list(line = list(color= "rgb(63, 63, 63)"))) 
fig <- fig %>%
  config(displayModeBar = FALSE) %>%
  layout(title = "Potential CO<sub>2</sub> mitigation",
         xaxis = list(title = ""),
         yaxis = list(title = "CO<sub>2</sub>-eq gigagrams yr<sup>-1</sup> "),
         autosize = TRUE,
         showlegend = TRUE)%>% 
  layout(plot_bgcolor='transparent') %>% 
  layout(paper_bgcolor='transparent',
         shapes = list(
           list(type = "rect",
                fillcolor = "red", line = list(color = "red"), opacity = 1,
                x0 = -0.4, x1 = 0.4, xref = "x",
                y0 = 0.0, y1 = totagrem, yref = "y")))





fig


