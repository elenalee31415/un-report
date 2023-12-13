library(tidyverse)

gapminder_1997 <- read_csv("gapminder_1997.csv")
View(gapminder_1997)

name <- "Ben"
name

age <- 26
age

name_character <- "Gandalf"
name_character

test <- read_csv("gapminder_1997.csv")
Sys.Date() #outputs current date in year-month-day (0000-00-00) format, used for knowing when I last ran code
Sys.time() 
Sys.timezone()

getwd() #get working/current directory
sum(5,6)

round(3.1415926535897932384, 2)
read_csv(file = 'gapminder_1997.csv')

ggplot(data = gapminder_1997) + # + sign connects ggplot and aes and label functions so they're all on the same graph
  aes(x = gdpPercap) +  #good practice to start new line for each new feature you add to the graph
  labs(x = "GDP Per Capita") +
  aes(y = lifeExp) + 
  labs(y = "Life Expectancy") +
  geom_point() + #could change size, shape, color, etc. of points here if you wanted to
  labs(title = "Do people in wealthy countries live longer?") +
  aes (color = continent) + 
  scale_color_brewer(palette = "Set1") + #changes color palette
  aes(size = pop/1000000) + #makes point size proportional to population of country 
  labs(size = "Population (in millions)") + #changes title of legend for population point size
  aes(shape = continent)

ggplot(data = gapminder_1997) + #doing same thing as above more efficiently
  aes(x = gdpPercap, y = lifeExp, color = continent, shape = continent, size = pop/100000) + 
  geom_point() + 
  scale_color_brewer(palette = "Set1") + 
  labs(x = "GDP Per Capita", y = "Life Expectancy", 
       title = "Do people in wealthy countries live longer?", size = "Population (in millions)")

gapminder_data <- read_csv("gapminder_data.csv")
View(gapminder_data)
dim(gapminder_data) #tells us dimensions of data: # rows, # columns
head(gapminder_data) #first 6 rows of data from dataset

ggplot(data = gapminder_data) + #year as x axis, life expectancy as y axis, continent to define color, use points 
  aes(x = year, y = lifeExp, color = continent, shape = continent, size = pop/100000) +
  geom_point() + 
  scale_color_brewer(palette = "Set2") + 
  labs(x = "Year", y = "Life Expectancy", 
       title = "How does life expectancy vary by year?", size = "Population (in millions")
