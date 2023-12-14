library (tidyverse)

gapminder_data <- read_csv("data/gapminder_data.csv")
View(gapminder_data)

summarize(gapminder_data, averagelifeExp = mean(lifeExp))

gapminder_data %>% summarize(averagelifeExp = mean (lifeExp)) #pipe function, shift + command + M = keyboard shortcut 

gapminder_data %>%  #nicer way to write piping function 
  summarize(averagelifeExp = mean(lifeExp)) #pipe function sends data set (gapminder_data) into summarize function automatically
# you can send the output from pipe function into another function in a chain

#data frame = set of rows and columns with your data in it, like an Excel spreadsheet 
#tibble: tidyverse form of rows and columns w/ your data but a little more persnickety; need data uploaded very correctly

gapminder_data_summarized <- gapminder_data %>% summarize(averagelifeExp = mean(lifeExp))

#danger! don't do this, it'll erase existing dataset and replace with a single number, average life expectancy
# gapminder_data <- gapminder_data %>% summarize(averagelifeExp = mean(lifeExp))

gapminder_data %>% 
  summarize(recent_year = max(year))

gapminder_data %>%
  filter(year == 2007) %>%  
  summarize (average = mean(lifeExp)) #gets average life expectancy but only for 2007

#what is the average GDP per capita for the first year in the dataset?
gapminder_data %>% 
  summarize(first_year = min(year))

gapminder_data %>% 
  filter(year == 1952) %>% 
  summarize (average_GDP = mean(gdpPercap))

#more effiicent way to do it
gapminder_data %>% 
  filter(year == min(year)) %>% 
  summarize (average_GDP = mean(gdpPercap))

#another way to do it:
my_object = gapminder_data %>% 
  summarize(first_year = min(year))

gapminder_data %>%  
  filter(year == pull(my_object)) %>% 
  summarize(average = mean(gdpPercap))

#group_by command

gapminder_data %>% 
  group_by(year) %>% 
  summarize(average = mean(lifeExp))

#calculate avg. life expectancy by continent
gapminder_data %>% 
  group_by(continent) %>% 
  summarize (average = mean(lifeExp)) #makes column "average" 

gapminder_data %>% 
  group_by(continent) %>% 
  summarize(average = mean(lifeExp), min=min(lifeExp)) # min = min(lifeExp) creates another column w/minimum life expectancy sorted by continent

#mutate ()

gapminder_data %>% 
  mutate(gdp = pop * gdpPercap) #make new column gdp

gapminder_data %>% 
  mutate(gdp = pop*gdpPercap, pop_in_millions = pop/1000000)

# select()

gapminder_data %>%  #pull out just the columns you're interested in looking at
  select(pop,year)

gapminder_data %>% #remove a column that you don't want
  select(-continent)

#produce a data frame that has only the columns country, continent, year, and life expectancy

gapminder_data %>% 
  select(-gdpPercap, -pop)

#more about select helper functions (see help page for more)
gapminder_data %>% 
  select(year, starts_with('c'))

gapminder_data %>% 
  select(ends_with('p'))

#changing shape of data
gapminder_data %>% #get separate column for every year we want to look at
  select(country, continent, year, lifeExp) %>% 
  pivot_wider(names_from = year, values_from = lifeExp)

#create dataset with just year 2007 data from Americas
gapminder_data_2007 <- read_csv("data/gapminder_data.csv") %>% 
  filter(year == 2007 & continent == "Americas") %>% 
  select(-year, -continent)

#cleaning up data
temp <- read_csv("data/co2-un-data.csv")

read_csv("data/co2-un-data.csv", skip = 1) #skip 1st row of data 

#skip first 2 rows of data and give it new column names 
co2_emissions_dirty <- read_csv("data/co2-un-data.csv", skip = 2, 
                                col_names = c("Region", "country", "Year", "Series", "Value", "Footnotes", "Source"))

#rename()
read_csv("data/co2-un-data.csv", skip =1) %>% 
  rename(country =...2) #renames column called "...2" as "country"

co2_emissions_dirty %>% 
  select(country, Year, Series, Value)

#change what Series values say to make them shorter using recode function
co2_emissions_dirty %>%  
  select(country, Year, Series, Value) %>% 
  mutate(Series = recode(Series, "Emissions (thousand metric tons of carbon dioxide)" = "total_emissions", "Emissions per capita (metric tons of carbon dioxide)" = "per_capita_emissions"))

#split total_emissions and per_capita emissions into 2 separate columns
co2_emissions_dirty %>%  
  select(country, Year, Series, Value) %>% 
  mutate(Series = recode(Series, "Emissions (thousand metric tons of carbon dioxide)" = "total_emissions", "Emissions per capita (metric tons of carbon dioxide)" = "per_capita_emissions")) %>% 
  pivot_wider(names_from = Series, values_from = Value)

#count() tells you how many data entries you have for a given entry (like the year 2005)
co2_emissions_dirty %>%  
  select(country, Year, Series, Value) %>% 
  mutate(Series = recode(Series, "Emissions (thousand metric tons of carbon dioxide)" = "total_emissions", "Emissions per capita (metric tons of carbon dioxide)" = "per_capita_emissions")) %>% 
  pivot_wider(names_from = Series, values_from = Value) %>% 
  count(Year)

#filter out data for just 2005, drop out year column

co2_emissions_dirty %>%  
  select(country, Year, Series, Value) %>% 
  mutate(Series = recode(Series, "Emissions (thousand metric tons of carbon dioxide)" = "total_emissions", "Emissions per capita (metric tons of carbon dioxide)" = "per_capita_emissions")) %>% 
  pivot_wider(names_from = Series, values_from = Value) %>% 
  filter(Year == 2005) %>% 
  select(-Year)

#create new object 
co2_emissions <- co2_emissions_dirty %>%  
  select(country, Year, Series, Value) %>% 
  mutate(Series = recode(Series, "Emissions (thousand metric tons of carbon dioxide)" = "total_emissions", "Emissions per capita (metric tons of carbon dioxide)" = "per_capita_emissions")) %>% 
  pivot_wider(names_from = Series, values_from = Value) %>% 
  filter(Year == 2005) %>% 
  select(-Year)

#to merge data sets co2_emissions and gapminder_data_2007: use "country" as key b/c they share that column in common
#innerjoin(): new data frame will only have the rows where the key is found for both data frames being combined
inner_join(gapminder_data, co2_emissions)
inner_join(gapminder_data, co2_emissions, by="country")

gapminder_co2 <- inner_join(gapminder_data_2007, co2_emissions, by = "country") #by can take multiple arguments, separated by comma

ggplot(gapminder_co2, aes(x = gdpPercap, y = per_capita_emissions)) +
  geom_point() +
  labs(x = "GDP (per capita)", y = "CO2 emitted (per capita)", 
       title = "Association between a nation's GDP and CO2 production")

ggsave("figures/gdppercap_vs_CO2.png")
  


