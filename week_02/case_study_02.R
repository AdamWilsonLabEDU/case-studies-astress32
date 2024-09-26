#Case Study 2 _ Temperature Data

#Install and Call Library
library(tidyverse)
temp<-read.csv("station.csv") #put file into working directory

dataurl="https://data.giss.nasa.gov/tmp/gistemp/STATIONS_v4/tmp_USW00014733_14_0_1/station.csv"
read_table(dataurl) #downloads and imports table from website

#Tell NASA site to create temporary file
httr::GET("https://data.giss.nasa.gov/cgi-bin/gistemp/stdata_show_v4.cgi?id=USW00014733&ds=14&dt=1")

temp<-read_csv(dataurl, na="999.90", skip=1, col_names = c("YEAR","JAN", "FEB", "MAR", "APR","MAY","JUN","JUL", "AUG","SEP","OCT","NOV",  
                                                           "DEC","DJF","MAM","JJA",  
                                                           "SON","metANN"))
View(temp)


#Visualize Data with GGPlot
A <- ggplot(
  data = temp, 
  mapping = aes(x = YEAR, y = JJA)) +
  geom_line() +
  geom_smooth(colour = "red") +
  labs(title = "Mean Summer Temperatures in Buffalo, NY", 
       subtitle = "June, July, and August Data from Global Historical Climate Network", #use \n to add another line
       caption = "Red line is a LOESS smooth",
       x = "Year", y = "Mean Summer Temperature (C)")

#Save Plot as a specific file (.png,.pdf.jpeg...) and define size
ggsave(A, filename = "CS2_Mean_Temp_Plot.png", width = 6, height = 6)

