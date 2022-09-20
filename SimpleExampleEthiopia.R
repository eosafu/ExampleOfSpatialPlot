# A simple code for ploting spatial estimates using ggplot.
# This example used the Ethiopia shp file.
require(rgdal)
require(ggplot2)
# Download shape file
download.file("https://africaopendata.org/dataset/76683f42-e659-4c57-8d4f-bc1debdf2720/resource/47564350-00ed-4863-8a96-98fba25d24d5/download/ethiopiaregion.zip", fn)
# Determine the location of the shape file in your hard drive
fn <- file.path(tempdir(), "ethiopiaregion.zip", fsep = "\\")
# Unzip
utils::unzip(fn, exdir = tempdir())
# Load into R. "Eth_Region_2013.shp" is among the unzipped folder.
shp <- readOGR(dsn = file.path(tempdir(), "Eth_Region_2013.shp"), stringsAsFactors = F)
names(shp)
# Fortify
shp.map <- fortify(shp, region = "REGIONNAME")
# Get listed regions
region <- unique(shp.map$group )
# For each state, determine your estimate (eg. prevalence). 
########### For example ##############
aux= rbinom(length(region),10,prob = 0.6)
prevalence = aux/length(region)
est_data <- data.frame(region=region,est=prevalence)
head(est_data)
######################################
# Function to fill all the rows in the fortified shp file
Infil <- function(region,est){
  shp.map$prev[which(shp.map$group==as.character(region))] <- est
  return(shp.map)
}

# Iterate over the firtified shape file to insert all the estimates in the file
shp.map$prev=0
for (i in 1:nrow(est_data)) {
  shp.map <-Infil(est_data[i,1],est_data[i,2]) 
}
# Plot 
ggplot(shp.map, aes(x = long, y = lat, group = group, fill = prev)) +
  geom_polygon(colour = "black", size = 0.5, aes(group = group)) +
  theme_minimal()+ scale_fill_gradient(name="prevalence",high='green', low='red')
