# Install function for packages (automatic)
packages <- function(x){
  x <- as.character(match.call()[[2]])
  if (!require(x,character.only=TRUE)){
    install.packages(pkgs=x,repos="http://cran.r-project.org")
    require(x,character.only=TRUE)
  }
}


# Required packages
packages(plyr)
packages(ggmap)

# set working directory
setwd("~/Repositories/Webscraper")

location <- read.table("cleaned_data_20160114.csv", header=TRUE, sep=",", fileEncoding="UTF-8")

# get location data
location_vector <- location$hirdetesek_cim

my_data <- NULL
my_row <- NULL

#set up loop
from <- 1 
#to <- length(location_vector)
to <- 2000

for (current_ID in from:to) {

  print(paste("getting location ", current_ID))
  
  current_location <- location_vector[current_ID]

  geocoded_location <- geocode(as.character(current_location))
  
  my_row$lat <- geocoded_location$lat
  my_row$lng <- geocoded_location$lon
  my_row$ID <- current_ID
    
  my_data <- rbind(my_data, my_row)
  
  write.csv(my_data, file = "data_1_2000.csv")

}

# location <- cbind(location, my_data)

# write.csv(location, file = "geocoded_data.csv")

print("FINISHED!")