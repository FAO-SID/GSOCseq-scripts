library(rmarkdown)
library(stringr)

rm(list= ls())

#Working directory
wd <- "C:/Users/hp/Documents/FAO/GSOCseq/Joint Maps"
setwd(wd)

#Load country names
countries <- read.csv("data/metadata_ISO.csv", sep = ";")
countries <- countries[countries$ROMNAM!="Hala'ib Triangle",]
countries <- countries[complete.cases(countries),]

ISOs <- read.csv('C:/Users/hp/Documents/FAO/data/un_maps/country boundary/attribute_table.csv')
ISOs <- unique(ISOs$ISO3CD)
#Get list of countries




for (i in unique(ISOs)){
  country <- countries[countries$ISO == i,"ROMNAM"]
  render("GSOCseq_layers.rmd",output_file = paste0(i ,'.pdf'))  
  print(paste0(i, " done!"))
}
