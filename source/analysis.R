library(tidyverse)
library(dplyr)
library(ggplot2)

library(stringr)

# The functions might be useful for A4
source("../source/a4-helpers.R")

# Load the data
incarceration_trends <- read.csv("C:/Users/netra/Documents/info201/data/incarceration_trends.csv")


## Test queries ----
#----------------------------------------------------------------------------#
# Simple queries for basic testing
#----------------------------------------------------------------------------#
# Return a simple string
test_query1 <- function() {
  return ("Hello world")
}

# Return a vector of numbers
test_query2 <- function(num=6) {
  v <- seq(1:num)
  return(v)
}

## Section 2  ---- 
#----------------------------------------------------------------------------#
# Summary of data surrounding black jail populations in the United States

#Where is the total black jail population the highest?
highest_black_jail_pop <- incarceration_trends %>%
  group_by(state) %>%
  summarize(total_black_jail_pop = sum(as.integer(na.omit((black_jail_pop)))))%>%
  filter(total_black_jail_pop == max(total_black_jail_pop)) %>%
  pull(state) 
state_highest_black_jail_pop <- state.name[grep(highest_black_jail_pop, state.abb)] 

#Where is the total black jail population the lowest?
lowest_black_jail_pop <- incarceration_trends %>%
  group_by(state) %>%
  summarize(total_black_jail_pop = sum(as.integer(na.omit((black_jail_pop)))))%>%
  filter(total_black_jail_pop == min(total_black_jail_pop)) %>%
  pull(state)
state_lowest_black_jail_pop <- state.name[grep(lowest_black_jail_pop, state.abb)]


#How much has the total black jail population changed between 1988 and 2018?
pop_1988 <- incarceration_trends %>%
  filter(year == "1988") %>%
  summarize(pop = sum(as.integer(na.omit(black_jail_pop))))%>%
  pull(pop)

pop_2018 <- incarceration_trends %>%
  filter(year == "2018") %>%
  summarize(pop = sum(as.integer(na.omit(black_jail_pop))))%>%
  pull(pop)

total_change <- pop_2018 - pop_1988

#----------------------------------------------------------------------------#

## Section 3  ---- 
#----------------------------------------------------------------------------#
# Growth of the U.S. Prison Population
#----------------------------------------------------------------------------#
# This function summarizes total jail population per year, in the function 'get_year_jail_pop'
get_year_jail_pop <- function() {
    incarceration_trends %>%
    group_by(year) %>%
    summarize(pop_year = total_jail_pop) %>%
return(pop_year)
}

get_year_jail_pop()

# This function calls the 'get_year_jail_pop' function, and plots its data into a bar chart
plot_jail_pop_for_us <- function()  {
  options(scipen=999)
  plot <- ggplot(data=get_year_jail_pop(), aes(x=year, y=pop_year))+
    geom_bar(stat="identity") +
    ylab("Total Jail Population") +
    xlab("Year") +
    ggtitle("Increase of Jail Population in U.S. (1970-2018)")
  return(plot)   
} 

plot_jail_pop_for_us()


## Section 4  ---- 
#----------------------------------------------------------------------------#
# Growth of Prison Population by State 

# This function returns the total jail population per state, per year, 
# given the states that are called as parameters
states <- incarceration_trends$state

get_jail_pop_by_states <- function(states) {
    incarceration_trends %>%
    filter(state %in% states) %>%
    group_by(year, state)%>%
    summarize(prison = sum(na.omit(as.integer(total_jail_from_prison))))%>%
  return(prison)
}


# This function plots the the jail population per year for the states that were
# called as parameters, in a line chart with points
plot_jail_pop_by_states <- function(states) {
  chart <- ggplot(data = get_jail_pop_by_states(states),
                  aes(x=year, y=prison, color=state)) +
          geom_point()+
          geom_line()+
          ggtitle("Growth of U.S. Prison Population by State (1970-2018)")+
          labs(x = "Year", y = "Total U.S. Prison Population", color = "State")
  return(chart)
}

get_jail_pop_by_states(c("CA","OR","AL", "WA"))
plot_jail_pop_by_states(c("CA","OR","AL", "WA"))

# See Canvas
#----------------------------------------------------------------------------#


## Section 5  ---- 
#----------------------------------------------------------------------------#
# <variable comparison that reveals potential patterns of inequality>

# This function returns the proportion of black jail population to total jail
# population, and the white jail population to total jail population per state
black_state_jail_pop <- function() {
  incarceration_trends %>%
    group_by(state)%>%
    summarize(
      black_state_proportion = sum(na.omit(as.integer(black_jail_pop)))/sum(na.omit(as.integer(total_jail_pop))),
      white_state_proportion = sum(na.omit(as.integer(white_jail_pop)))/sum(na.omit(as.integer(total_jail_pop))))%>%
  return(black_state, white_state)
}

black_state_jail_pop()

# This function plots the comparison between the proportions of black and white 
#jail populations in a scatterplot
plot_black_state_jail_pop <- function() {
  scatterplot <- ggplot(data=black_state_jail_pop(), 
                        aes(x=black_state_proportion, 
                        y=white_state_proportion)) +
    geom_point()+
    labs(x= "Proportion of black jail population per state",
         y= "Proportion of white jail population per state")+
    ggtitle("Comparison of the Proportion of White and Black Jail Population Per State")
  return(scatterplot)
}

plot_black_state_jail_pop()
# See Canvas
#----------------------------------------------------------------------------#

## Section 6  ---- 
#----------------------------------------------------------------------------#
# <a map shows potential patterns of inequality that vary geographically>

# Load packages
library(maps)

# Load US map dataset
us_map <- map_data("state")

# Convert State Abbreviations to State names
incarceration_trends$state <- state.name[match(incarceration_trends$state, state.abb)]

# Convert to lowercase
incarceration_trends <- incarceration_trends %>% mutate(state = tolower(state))


# Plot US Map
ggplot() + 
  geom_polygon(data = us_map, aes(x=long, y=lat, group=group),
               color="black", fill = "blue")

# Find proportion of Black jail population to state population
# for each state in 2018
black_jail_2018 <- incarceration_trends%>%
  filter(year == 2018)%>%
  group_by(county_name)%>%
  mutate(black_jail_proportion = black_jail_pop/total_jail_pop)%>%
  select(state, county_name, black_jail_proportion)

# Change NA values to 0
black_jail_2018["black_jail_proportion"][is.na(black_jail_2018["black_jail_proportion"])] <- 0

# Find sum of total Black jail population proportion across counties in each state
sum_black_jail_2018 <- black_jail_2018%>%
  group_by(state)%>%
  summarize(black_jail_proportion = sum(black_jail_proportion))

# Merge US map data and Black jail popluation proportion data
merged_states <- merge(us_map, sum_black_jail_2018, by.x = "region", by.y = "state")


# Plot the merged data
plotMap <- ggplot()
plotMap <- plotMap + geom_polygon(data = merged_states, 
                      aes(x=long, y=lat, group=group, fill = black_jail_proportion), 
                      color="white", size = 0.2) +
  ggtitle("Proportion of Incarcerated Black People in 2018")+
  labs(x = "Longitude", y = "Latitude")+
  scale_fill_continuous(name="Percentage of Black People in Jails", 
                        low = "lightblue", high = "darkblue",limits = c(0,80), 
                        breaks=c(10, 20, 30, 40, 50, 60, 70, 80), na.value = "grey")

plotMap


# See Canvas
#----------------------------------------------------------------------------#

## Load data frame ---- 


