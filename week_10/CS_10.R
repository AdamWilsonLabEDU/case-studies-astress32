#' ---
#' title: Satellite Remote Sensing
#' subtitle: Analyze Satellite Data
#' week: 10
#' type: Case Study
#' reading:
#' tasks:
#'    - Extract a timeseries from a single location in a netcdf file (part 1)
#'    - Calculate a monthly climatology from a weekely timeseries (part 2)
#'    - Summarize Land Surface Temperature by Land Cover (part 3)
#' ---
#' 

#' 
#' # Tasks
#' 

#' 
#' 
#install.packages("ggmap")
#install.packages("rasterVis")
#install.packages("stringr")
#' 
#' 
#' ### Libraries
library(terra)
library(rasterVis)
library(ggmap)
library(tidyverse)
library(knitr)
library(sf)
library(stringr)
library(ggplot2)

# New Packages
library(ncdf4) # to import data from netcdf format

# Data Set up

#' * Land Surface Temperature (`lst`): MOD11A2
#' * Land Cover (`lc`): MCD12Q1

## # Create a folder to hold the downloaded data
dir.create("data",showWarnings = F) #create a folder to hold the data

lulc_url="https://github.com/adammwilson/DataScienceData/blob/master/inst/extdata/appeears/MCD12Q1.051_aid0001.nc?raw=true"
lst_url="https://github.com/adammwilson/DataScienceData/blob/master/inst/extdata/appeears/MOD11A2.006_aid0001.nc?raw=true"

## # download them
download.file(lulc_url,destfile="data/MCD12Q1.051_aid0001.nc", mode="wb")
download.file(lst_url,destfile="data/MOD11A2.006_aid0001.nc", mode="wb")


#' ## Load data into R

lulc=rast("data/MCD12Q1.051_aid0001.nc",subds="Land_Cover_Type_1")
lst=rast("data/MOD11A2.006_aid0001.nc",subds="LST_Day_1km")


#' ## Explore LULC data
plot(lulc)

lulc=lulc[[13]]
plot(lulc)


#' ### Process landcover data
  Land_Cover_Type_1 = c(
    Water = 0, 
    `Evergreen Needleleaf forest` = 1, 
    `Evergreen Broadleaf forest` = 2,
    `Deciduous Needleleaf forest` = 3, 
    `Deciduous Broadleaf forest` = 4,
    `Mixed forest` = 5, 
    `Closed shrublands` = 6,
    `Open shrublands` = 7,
    `Woody savannas` = 8, 
    Savannas = 9,
    Grasslands = 10,
    `Permanent wetlands` = 11, 
    Croplands = 12,
    `Urban & built-up` = 13,
    `Cropland/Natural vegetation mosaic` = 14, 
    `Snow & ice` = 15,
    `Barren/Sparsely vegetated` = 16, 
    Unclassified = 254,
    NoDataFill = 255)

lcd=data.frame(
  ID=Land_Cover_Type_1,
  landcover=names(Land_Cover_Type_1),
  col=c("#000080","#008000","#00FF00", "#99CC00","#99FF99", "#339966", "#993366", "#FFCC99", 
        "#CCFFCC", "#FFCC00", "#FF9900", "#006699", "#FFFF00", "#FF0000", "#999966", "#FFFFFF", 
        "#808080", "#000000", "#000000"),
  stringsAsFactors = F)
      # colors from https://lpdaac.usgs.gov/about/news_archive/modisterra_land_cover_types_yearly_l3_global_005deg_cmg_mod12c1
kable(head(lcd))
https://www.nyserda.ny.gov/All-Programs/Clean-Energy-Standard#:~:text=%2C%20which%20sets%20goals%20for%20achieving,jobs%20to%20New%20York%20State.

#' Convert LULC raster into a 'factor' (categorical) raster.  This requires building the Raster Attribute Table (RAT).  
  # convert to raster
lulc=as.factor(lulc)

# update the RAT with a left join
levels(lulc)=left_join(levels(lulc)[[1]],lcd)[-1,]
activeCat(lulc)=1

# plot it
gplot(lulc)+
  geom_raster(aes(fill=as.factor(value)))+
  scale_fill_manual(values=setNames(lcd$col,lcd$ID),
                    labels=lcd$landcover,
                    breaks=lcd$ID,
                    name="Landcover Type")+
  coord_equal()+
  theme(legend.position = "right")+
  guides(fill=guide_legend(ncol=1,byrow=TRUE))


#' # Land Surface Temperature
plot(lst[[1:12]])


#' ## Convert LST to Degrees C 
#' You can convert LST from Degrees Kelvin (K) to Celcius (C) with `scoff()`.
scoff(lst)=cbind(0.02,-273.15)
plot(lst[[1:10]])


#' ## Part 1: Extract timeseries for a point
  
  #Extract LST values for a single point and plot them.
    
#define a new sf point
lw = data.frame(x= -78.791547,y=43.007211) %>% 
  st_as_sf(coords=c("x","y"),crs=4326) 
    
#Transform the point to the projection of the raster using `st_transform()`.  You'll need to get the projection of the raster with st_crs().
st_transform(lw, crs = st_crs(lst))

#Extract the LST data for that location `
lst_extract <- terra::extract(lst, lw, buffer = 1000, fun = mean, na.rm = T) #use terra
temperature <- t(lst_extract[-1])

#Extract the dates for each layer 
dates <- time(lst)


#Combine the dates and transposed raster values into a data.frame
lst_dates <- data.frame(temperature, dates)

#Plot it with `ggplot()` including points for the raw data and a smooth version as a line.

ggplot(data = lst_dates, 
       aes(x = dates, y = temperature)) + 
  geom_point() +
  geom_smooth(span = 0.05, n = 1000) +
  labs(x = "date", y = "Monthly Mean Land Surface Temeprature")

#' Part 2: Summarize weekly data to monthly climatologies

lst_month <- tapp(lst, fun = "mean", index = 'month', na.rm = TRUE)

  #view raster data 
  structure(lst_month)

  #Set names of layers to months
  names(lst_month) = month.name[as.numeric(str_replace(names(lst_month),"m_",""))]
  
  structure(lst_month)
  
  #Plot map for each month
  gplot(lst_month) + 
    geom_raster(aes(fill = value)) + 
    facet_wrap(~variable)
  
  #Calculate the monthly mean for the entire image with `
  monthly_mean <- 
    global(lst_month, mean , na.rm=T)
    
    
#' ## Part 3: Summarize Land Surface Temperature by Land Cover

#Resample `lulc` to `lst` grid
lulc2 <- resample(lulc, lst, method="near")

#Extract the values from `lst_month` and `lulc2` into a data.frame as follows:
 lcds1=cbind.data.frame(
  values(lst_month),
  ID=values(lulc2[[1]])) %>%
  na.omit()
 
 colnames(lcds1)[13] = "Land_Cover"

 
#Gather the data into a 'tidy' format using 
 lulc2 <- 
   lcds1 %>%
   gather(key='month',value='value',-Land_Cover) %>%
   mutate(ID=as.numeric(Land_Cover)) %>%
   mutate(month = factor(month,levels = month.name, ordered = TRUE)) %>%
  left_join(lcd) %>%
   filter(landcover%in%c("Urban & built-up","Deciduous Broadleaf forest"))
          
#Plot to illustrate the monthly variability in LST between the two land cover types.  The exact form of plot is up to you.  Experiment with different geometries, etc.
ggplot(lulc2, aes(y=value,x=month))+
  facet_wrap(~landcover)+
  geom_point(alpha=.5,position="jitter")+
  geom_violin(alpha=.5,col="red",scale = "width",position="dodge")+
  theme(axis.text.x=element_text(angle=90, hjust=1))+
  ylab("Monthly Mean Land Surface Temperature (C)")+
  xlab("Month")+
  ggtitle("Land Surface Temperature in Urban and Forest areas in Buffalo, NY")

