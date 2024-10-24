
"Case Study 5"
"Finding the hottest country on each continent"
  
"Install Packages for Raster-Vector Data"

#install.packages("terra")             
#install.packages("ncdf4")
#install.packages('leaflet')
#install.packages("ColorBrewer")

library(terra)
library(sf)
library(dplyr)
library(tidyverse)
library(spData)
library(leaflet)
library(RColorBrewer)

#Prepare Climate Data

  library(ncdf4)
  download.file("https://crudata.uea.ac.uk/cru/data/temperature/absolute.nc","crudata.nc", method = "curl")
  
  #Read in data using rast() function from terra package
  tmean <- rast("crudata.nc")
  plot(tmean)

#Calculate the maximum temperature in each country 
  
  #calculate annual max for each pixel
   max_temp <- tmean %>%
                 max()
  plot(max_temp)        
  
  #Identify maximum temperature data points, overlay country polygons
      #within each country, calculating hottest pixel of hottest month
  data(world)
    
  #If want to remove Antarctica form dataset
  #world %>% filter(continent != "Antarctica") %>% st_as_sf()
    
  max_temp_by_country <-
    terra::extract(x = max_temp, 
                   y = world, 
                   fun = max,    #takes max value within each country
                   na.rm = TRUE, 
                   small = TRUE) #ensures small countries are in analysis
  
  plot(max_temp_by_country)  
  
  #Bind temp data column to world dataset 
    #Need world dataset before temp data so maintain geometry
  world_clim <- bind_cols(world, max_temp_by_country)
   
#Plot temperature data by country
  
  Map <- 
    ggplot(world_clim) +
    geom_sf(aes(fill = max)) +
    scale_fill_viridis_c(name="Maximum\nTemperature (C)") +   
    theme(legend.position = 'bottom') +
    labs(title = "Hottest Country in each Continent",
         legend = "Maximum Temperature (C)")
 
   ggsave(Map, filename = "Hottest_Country_Map.png", width = 5, height = 3)
  
  #Prepare a table of hottest countries
  hottest_continents <-
    world_clim %>%
    group_by(continent) %>%
    top_n(1) %>%
    select(name_long, continent, max) %>%
    st_set_geometry(NULL) %>%
    arrange(desc(max))

  
  
#Create a Leaflet Map (Still working on...)
  #create a vector of values to color by
  #color_values <- (max_temp_by_country)
  #palette <- colorNumeric("magma", domain = color_values)
  #leaflet(world_clim) %>% addTiles() %>% addPolygons(fillColor = palette(color_values), weight = 1)

    
