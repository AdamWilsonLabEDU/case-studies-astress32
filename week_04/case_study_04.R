#Case Study 4: Farthest Airport from New York City

  #Objective: What is the full name (not the three letter code) of the destination airport 
  #farthest from any of the NYC airports in the flights table?

#install.packages("nycflights13")
library(nycflights13)
library(tidyverse)
library(dplyr)

data("flights")
  View(flights)
data("airports")
glimpse(airports)


#Find Airport code that is farthest from NYC airports
  farthest_airport_code <- flights |>
    arrange(desc(distance)) |> 
    slice(1)
    

#Join to dataset with full airport names
  ?left_join()    #how to join datasets when don't share a variable?
                  #Link dest to faa (set equal to)
  
  farthest_airport <- farthest_airport_code %>%
    left_join(airports, by = c("dest" = "faa")) %>%
    select(destination = name)
  
  farthest_airport_final <- as.character(farthest_airport$destination)
  #Honolulu International 
  
