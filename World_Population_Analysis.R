# Loading the required packages ####
library(tidyverse)
library(knitr)

# Importing the dataset ####
world_population <- read.csv("Data/world_population.csv")
head(world_population)
colSums(is.na(world_population))

# convert continent to factor for easy grouping
world_population$Continent <- as.factor(world_population$Continent)
levels(world_population$Continent)


# Highest and Least Populated Countries ####
# 1. World ====
# (a) top 10 most populated countries in the World ----
world_top_10 <- world_population %>%
  arrange(desc(`X2022.Population`))
head(world_top_10, 10)
#Visualize this in a chart
ggplot(data = head(world_top_10, 10)) +
  geom_col(mapping = aes(x=reorder(Country.Territory, -X2022.Population), 
                         y = X2022.Population),
           fill='#ff8c8c') +
  ggtitle("Most Populated Countries in the World") +
  labs(x = 'Countries', y = 'Population') +
  scale_y_continuous(labels = scales::number_format()) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# (b) top 10 least populated countries in the World ----
world_bottom_10 <- world_population %>% 
  arrange(`X2020.Population`)
head(world_bottom_10,10)
#Visualize this in a chart
ggplot(data = head(world_bottom_10,10)) +
  geom_col(mapping = aes(x=reorder(Country.Territory, -X2022.Population), 
                         y = X2022.Population),
           fill='#ff8c8c') +
  ggtitle("Least Populated Countries in the World") +
  labs(x = 'Countries', y = 'Population') +
  scale_y_continuous(labels = scales::number_format()) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# 2. Africa ====
# (a) top 5 most populated countries in Africa ----
africa_top_5 <- world_population[
  world_population$Continent == "Africa", ] %>%
  arrange(desc(`X2022.Population`))
head(africa_top_5, 5)
#Visualize this in a chart
ggplot(data = head(africa_top_5, 5)) +
  geom_col(mapping = aes(x=reorder(Country.Territory, -X2022.Population), 
                         y = X2022.Population),
           fill='darkblue') +
  ggtitle("Most Populated Countries in Africa") +
  labs(x = 'Countries', y = 'Population') +
  scale_y_continuous(labels = scales::number_format())

# (b) top 5 least populated countries in Africa ----
africa_bottom_5 <- world_population[
  world_population$Continent == "Africa", ] %>%
  arrange(`X2022.Population`)
head(africa_bottom_5, 5)
#Visualize this in a chart
ggplot(data = head(africa_bottom_5, 5)) +
  geom_col(mapping = aes(x=reorder(Country.Territory, -X2022.Population), 
                         y = X2022.Population),
           fill='darkblue') +
  ggtitle("Least Populated Countries in Africa") +
  labs(x = 'Countries', y = 'Population') +
  scale_y_continuous(labels = scales::number_format())

# Percentage Increase Between 1970 and 2022 ####
# 1. World Percentage increase =====
new_column <- mutate(world_population, 
                     percent_change = round(((`X2022.Population` - `X1970.Population`)/ `X1970.Population`) * 100))
per_increase <- new_column %>% 
  select(`Country.Territory`, Continent, `X2022.Population`, `X1970.Population`, percent_change) %>%
  arrange(desc(percent_change))
head(per_increase,5)
#Visualize this in a chart
ggplot(data = head(per_increase,5)) +
  geom_col(mapping = aes(x=reorder(Country.Territory, -percent_change), 
                         y = percent_change),
           fill='darkblue') +
  ggtitle("World Percentage Increase in Population",
          subtitle = '(1970 to 2022)') +
  labs(x = 'Country', y = 'Percentage Increase') +
  scale_y_continuous(labels = scales::number_format())

# 2. Africa Percentage increase =====
africa_per_increase <- new_column %>%
  select(`Country.Territory`, Continent,`X2022.Population`, `X1970.Population`, percent_change) %>%
  filter(Continent=='Africa')%>%
  arrange(desc(percent_change))
head(africa_per_increase, 5)
#Visualize this in a chart
ggplot(data = head(africa_per_increase, 5)) +
  geom_col(mapping = aes(x=reorder(Country.Territory, -percent_change), 
                         y = percent_change),
           fill='darkblue') +
  ggtitle("Africa Percentage Increase in Population",
          subtitle = '(1970 to 2022)') +
  labs(x = 'Country', y = 'Percentage Increase') +
  scale_y_continuous(labels = scales::number_format())

# Total Population by Continent ####
total_population <- world_population %>% 
  group_by(Continent) %>% summarise(
    sum_2022 = sum(`X2022.Population`),
    sum_2020 = sum(`X2020.Population`),
    sum_2015 = sum(`X2015.Population`),
    sum_2010 = sum(`X2010.Population`),
    sum_2000 = sum(`X2000.Population`),
    sum_1990 = sum(`X1990.Population`),
    sum_1980 = sum(`X1980.Population`),
    sum_1970 = sum(`X1970.Population`)
  )
# print the result
print(total_population)
#Visualize this in a chart
ggplot(data = total_population) +
  geom_col(mapping = aes(x=reorder(Continent, -sum_2022), y = sum_2022),
           fill='darkblue') +
  ggtitle("Total World Population by Continent - 2022") +
  labs(x = 'Continents', y = 'Population') +
  scale_y_continuous(labels = scales::number_format())

# Growth rate by continents (1970-2022) ####
growth_rate <- total_population %>%
  mutate(percent_change = ((sum_2022 - sum_1970)
                           /sum_1970)*100) %>%
  arrange(desc(percent_change))
# print the result
print(growth_rate)
# Visualization
ggplot(data = growth_rate) +
  geom_col(mapping = aes(x=reorder(Continent, -percent_change), y=percent_change),
           fill='darkblue') +
  ggtitle("Growth Rate by Continents from 1970-2022",
          subtitle = 'in percentage') +
  labs(x='Continents', y='Percent Change')

# Nigeria ####
# time series plot for Nigeria
# select Nigeria
nigeria_data <- world_population[world_population$Country.Territory == "Nigeria", ]
# Reshape the data to long format for easy plotting
nigeria_data_long <- pivot_longer(nigeria_data, cols = starts_with("X"), names_to = "Year", values_to = "Population")
nigeria_data_long$Year <- as.numeric(gsub("X|\\.Population", "", nigeria_data_long$Year))
# Use ggplot2 to create a time-series plot
ggplot(nigeria_data_long, aes(x = Year, y = Population)) +
  geom_line() +
  labs(title = "Population Growth of Nigeria (1970-2022)",
       x = "Year",
       y = "Population") +
  scale_y_continuous(labels = scales::number_format()) +
  theme_minimal()
