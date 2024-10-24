#find.package("ggplot2") - not found, need to install

#install.packages("ggplot2")
#install.packages("pak")
#install.packages("gapminder")


library(ggplot2)
library(gapminder)
library(dplyr)      #need this to read pipes ( |>, %>%)

#Prepare data for Plot#1

data(gapminder)
#View(gapminder)

#Filter out Kuwait from dataset
#gapminder |> filter(country != "Kuwait")
data_filtered <- gapminder %>%
  filter(country != "Kuwait")

#Plot 1

Plot_1 <- ggplot(
  data = data_filtered,
  mapping = aes(x=lifeExp, y=gdpPercap, color=continent, size = pop/100000)
) + 
  theme_bw() + 
  geom_point() + 
  scale_y_continuous(trans = "sqrt") +
  facet_wrap(~year,nrow=1) +           #facet_grid, facet_wrap helps show data with progression/timeseries
  labs(x="Life Expectancy", y="GDP Per Capita", size="Population (100K)", title = "Wealth and Life Expectancy through Time")    
#Use labs() function to label x and y axes

#save plot as .png
ggsave(filename = "Plot_1.png", width = 15, height = 6)


#Prepare data for Plot #2

#data_continent <- data_filtered |> group_by(continent)
#data_by_year <- data_filtered |> group_by(year)
data_grouped <- data_filtered |> group_by(continent, year)


#calculate data for black continent average line
#use grouped dataset
gapminder_continent <- summarize(data_grouped, gdpPercapweighted = weighted.mean(x = gdpPercap, w = pop), 
                                 pop = sum(as.numeric(pop)))

#Plot 2

#Use new summarized dataset to show continent average
#Define dataset and aes for each geom plot since using 2 datasets

#fix this to connect points linearly - using group=country
ggplot()+
  geom_line(data=data_filtered, aes(x=year, y=gdpPercap, color=continent, group=country)) + #aes() is link between data and how its visualized
  geom_point(data=data_filtered, aes(x=year, y=gdpPercap, color=continent, group=country, size = pop/100000)) +
  geom_line(data = gapminder_continent, aes(x=year, y=gdpPercapweighted), color="black") +
  geom_point(data = gapminder_continent, aes(x=year, y=gdpPercapweighted, size = pop/100000)) +
  facet_wrap(~continent, nrow = 1) +
  theme_bw() +
  labs(x="Year", y="GDP Per Capita", size="Population (100K)", color="Continent", title = "Life Expectancy")
#add title and size for exported png
#check the weighted average for size of dots

#save plot as .png
ggsave(filename = "Plot_2.png", width = 15, height = 6)


