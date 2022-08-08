# Script corresponding to
#2-dplyr-mapview-tidycensus.Rmd

library(tidyverse)
library(tidycensus)
library(mapview)
library(sf)
library(viridis) #used to change color palettes
library(scales) #Used to help with the ggplot date axis below.

# Re-generate the NYT COVID-19 dataset we were working with before.--------
nyt_state_url = "https://raw.githubusercontent.com/nytimes/covid-19-data/master/us-states.csv"
nyt_state_data = nyt_state_url %>% 
  url() %>% 
  read_csv()
library(lubridate)
nyt_state_wrangle = nyt_state_data %>% 
  group_by(state) %>% 
  arrange(date) %>% 
  rename(
    cases_cumul = cases,
    deaths_cumul = deaths
  ) %>% 
  mutate(
    cases_cumul_day_before = lag(cases_cumul),
    cases_incident = cases_cumul - cases_cumul_day_before,
    
    deaths_cumul_day_before = lag(deaths_cumul),
    deaths_incident = deaths_cumul - deaths_cumul_day_before,
    year = year(date),
    month = month(date),
    week = week(date)
  ) %>% 
  ungroup() %>% 
  arrange(state, date)

nyt_state_wrangle %>% 
  dplyr::select(date, year, month)

#View(nyt_state_wrangle)
# Load state-level census data using tidycensus-------
nyt_state_data
install.packages("sf")
install.packages("tidycensus")
library(tidycensus)
tidycensus::census_api_key("key")
census_api_key("your_census_key_here", install=TRUE) #Note I've done this in a separate file.

## Load state-level population data---------

#Run this to save the data locally for speed.
options(tigris_use_cache = TRUE) 

#One way I keep track of whether an object has geometry 
#associated with it to add a _geo suffix to the name.
pop_by_state_geo =tidycensus::get_acs(
  year=2019,
  variables = "B01001_001", #total population
  geography = "state",
  geometry = TRUE #to grab geometry
)

library(here)
setwd(here("data-processed"))
save(pop_by_state_geo, file = "pop_by_state_geo.RData")
#A convenient way to search through the census variables is to use the `load_variables` function, 
#as described here: https://walker-data.com/tidycensus/articles/basic-usage.html.
vars_acs_2019 = load_variables(2019, "acs5", cache = TRUE)
#View(vars_acs_2019)

#Confirm the data are an sf object (simple feature) and explore the variable names.
class(pop_by_state_geo)
pop_by_state_geo

## A quick mapview------
#Another essential package in the **sf** workflow is **mapview** 
#(https://r-spatial.github.io/mapview/index.html), 
#which creates quick interactive maps. In just one line, we can interactively map the ACS state-level population data we just loaded. 

#The `zcol` argument colors the polygons (states, here) based on the value of the variable, 
#"estimate", which corresponds to the state's population, as obtained from tidycensus. 
#The default color palette for the visualization is [viridis]
#(https://cran.r-project.org/web/packages/viridis/vignettes/intro-to-viridis.html).

mapview(pop_by_state_geo, zcol = "estimate")

pop_by_state_geo %>% #the object name and then the pipe
  filter(estimate>30000000) %>% 
#  filter(NAME == "Texas") %>% 
  mapview(zcol = "estimate" )#the column to be visualized

## Wrangle the census data further to prep for the join with the NYT state-level COVID-19 data.--------
names(pop_by_state_geo)
names(nyt_state_wrangle)
pop_by_state_wrangle_geo = pop_by_state_geo %>%
  rename(
    fips_2digit = GEOID,
    state = NAME,
    population = estimate
  ) %>% 
  dplyr::select(-moe, -variable) %>% 
  #To simplify some of the maps below, create an indicator variable corresponding to the Continental 48.
  #Do the same type of operation using two different types of syntax, the  %in% operator and the or operator.
  mutate(
    continental_48 = case_when(
      state %in% c("Alaska", "Hawaii", "Northern Mariana Islands", "Guam", "Virgin Islands", "Puerto Rico") ~ 0,
      TRUE ~ 1)    
  ) 

names(pop_by_state_wrangle_geo)

## Use mapview to visualize population.------
pop_by_state_wrangle_geo %>%
  filter(continental_48==1) %>%   #Limit to continental 48 so the default zoom is more zoomed in.
  mapview(zcol = "population" ) #Note the object argument is implied by the pipe operator.

# Link the NYT COVID-19 data with the census data------
## Summarize NYT COVID-19 data and link with census data.-------
nyt_by_state_w_pop_geo = nyt_state_wrangle %>% 
  group_by(state) %>% 
  summarise(
    cases_cumul = max(cases_cumul, na.rm=TRUE),
    deaths_cumul = max(deaths_cumul, na.rm=TRUE)
  ) %>% 
  #Recall, after we use group_by and summarise, the data go from a very big dataset 
  #with a row for each day-state combination to a smaller dataset with just a row for every state.
  left_join(pop_by_state_wrangle_geo, by = "state") %>% #This contains the geometry
  st_as_sf() %>% 
  mutate(
    cases_cumul_per_pop = cases_cumul/population,
    deaths_cumul_per_pop = deaths_cumul/population
  )

class(nyt_by_state_w_pop_geo)
View(nyt_by_state_w_pop_geo)

nyt_by_state_w_pop_geo
### Visualize cumulative incidence per population using mapview-------
nyt_by_state_w_pop_geo %>%
  #Limit to continental 48 so the default zoom is more zoomed in.
  filter(continental_48==1) %>% 
  mapview(
    zcol = "cases_cumul_per_pop")

### Exercise 1: Calculate cases per person-year for each state.--------

#Calculate the number of incident cases per person year for each state.

nyt_by_state_w_pop_geo %>%
  filter(continental_48==1) %>%   #Limit to continental 48 so the default zoom is more zoomed in.
  mapview(
    zcol = "cases_cumul_per_pop",
    layer.name = "Cumulative incidence per pop",
    #The palette function is from the viridis package.
    col.regions = viridis_pal(alpha = 1, begin = 0, end = 1, direction = -1, option = "B")
  )



### Visualize using a ggplot2 histogram--------
nyt_by_state_w_pop_geo %>%
  filter(continental_48==1) %>%   #Limit to continental 48 so the default zoom is more zoomed in.
  ggplot(aes(x=cases_cumul_per_pop))+
  geom_histogram() 

## Link the daily NYT COVID-19 data with the population data to analyze temporal trends in incidence by state---------
pop_by_state_wrangle_nogeo = pop_by_state_wrangle_geo %>% 
  st_set_geometry(NULL) %>% 
  as_tibble()

class(pop_by_state_wrangle_geo)
class(pop_by_state_wrangle_nogeo)

pop_by_state_wrangle_nogeo

nrow(nyt_state_wrangle)
nrow(pop_by_state_wrangle_nogeo)
nyt_state_pop_wrangle = nyt_state_wrangle %>% 
  left_join(pop_by_state_wrangle_nogeo, by = "state") %>% 
  mutate(
    cases_cumul_per_pop = cases_cumul/population,
    deaths_cumul_per_pop = deaths_cumul/population,
    cases_incident_per_pop = cases_incident/population,
    deaths_incident_per_pop = deaths_incident/population 
  )

names(nyt_state_pop_wrangle)

nyt_state_pop_wrangle %>% 
  dplyr::select(date, state, contains("cumul"), contains("incident"))


### Visualize daily cumulative incidence per population for the continental 48 states using ggplot2.-------
nyt_state_pop_wrangle %>% 
  filter(continental_48==1) %>%   
  ggplot( aes(x=date, y=cases_cumul_per_pop))+
  geom_line(aes(color = state)) +
  ylab("Cumulative COVID-19\n incidence\n per person") +
  xlab("Date")  +
  theme_bw() +
  theme(legend.position="bottom") +
  scale_x_date(date_breaks = "months" , date_labels = "%b-%y") +
  theme(legend.position="bottom") +
  theme(axis.text.x = element_text(angle = 90))

### Visualize daily incidence rate cases for the continental 48 states.---------
nyt_state_pop_wrangle %>% 
  filter(continental_48==1) %>%   
  #To  make it easier
  ggplot( aes(x=date, y=cases_incident_per_pop))+
  geom_line(aes(color = state)) +
  scale_x_date(date_breaks = "months" , date_labels = "%b-%y") +
  ylab("COVID-19\n incidence rate\n per person") +
  xlab("Date")  +
  theme_bw() +
  theme(legend.position="bottom") +
  theme(axis.text.x = element_text(angle = 90))

nyt_state_pop_wrangle %>% 
  mutate(
    great_lakes = case_when(
      state == "Illinois" |
        state == "Indiana" |
        state == "Michigan" |
        state == "Minnesota" |
        state == "New York" |
        state == "Ohio" |
        state == "Pennsylvania" |
        state == "Wisconsin" ~ 1,
      TRUE ~ 0)) %>% 
  filter(great_lakes==1) %>% 
  filter(year == 2021) %>% 
  #Pipe the tibble into the ggplot function:
  ggplot( aes(x=date, y=cases_incident_per_pop))+
  geom_line(aes(color = state)) +
  ylab("COVID-19\n incidence rate\n per population") +
  xlab("Date")  +
  theme_bw() +
  scale_x_date(date_breaks = "months" , date_labels = "%b-%y") +
  theme(legend.position="bottom") +
  theme(axis.text.x = element_text(angle = 90))

# Exercises------------
## Exercise 1: Calculate cases per person-year for each state.-------


# Exercise: Calculate the number of incident cases per person year for each state.

## Exercise 2: Use facet_wrap to separate plots into groups.-----------

#Exercise: Divide the states into regions
#(e.g., using the classification here,
#and create line charts of cumulative incidence and 
#incidence rate by state by region.

## Exercise 3: Calculate and graph weekly incidence rate------

# Exercise: 
#   Weekly incidence rate is probably a more stable measure. 
# How would we calculate that? Calculate weekly incidence rate and graph both 
#overall (contiguous 48) and by state.


# Solutions to exercises-------------
## Exercise 1-------
nyt_state_pop_wrangle_year = nyt_state_pop_wrangle %>% 
  group_by(state, year) %>% 
  #group by with summarise here
  summarise(
    cases_cumul_year = max(cases_cumul, na.rm=TRUE),
    deaths_cumul_year = max(deaths_cumul, na.rm=TRUE)
  ) %>% 
  ungroup() %>% #just to be sure
  group_by(state) %>%  
  #note we use group_by() without summarise here
  arrange(year) %>% 
  mutate(
    cases_cumul_year_before = lag(cases_cumul_year),
    cases_incident_year = cases_cumul_year - cases_cumul_year_before,
    deaths_cumul_year_before = lag(deaths_cumul_year),
    deaths_incident_year = deaths_cumul_year - deaths_cumul_year_before
  ) %>% 
  ungroup() %>% 
  #link in the population data
  left_join(pop_by_state_wrangle_nogeo, by = "state") %>% 
  #calculate weekly cumulative incidence per population 
  #up until that point in time in that week and the weekly incidence rate
  mutate(
    cases_cumul_per_pop_year = cases_cumul_year/population,
    deaths_cumul_per_pop_year = deaths_cumul_year/population,
    cases_incident_per_pop_year = cases_incident_year/population,
    deaths_incident_per_pop_year = deaths_incident_year/population 
  ) 

nyt_state_pop_wrangle_year
View(nyt_state_pop_wrangle_year)
## Exercise 2------
#not done yet

## Exercise 3-------
names(nyt_state_pop_wrangle)
#Create a look up table for date and week so that we can left-join
#it and visualize using the date itself on the x-axis again
lookup_year_date_week_end = nyt_state_pop_wrangle %>% 
  group_by(year, week) %>% 
  summarise(date_max = max(date)) %>% 
  ungroup()

lookup_year_date_week_end

nyt_state_pop_wrangle_weekly = nyt_state_pop_wrangle %>% 
  group_by(state, year, week) %>% 
  #group by with summarise here
  summarise(
    #an average might also work here...possibly more stable
    #than the max within the week showing that in a comment:
    # cases_cumul_week = mean(cases_cumul, na.rm=TRUE),
    # deaths_cumul_week = mean(deaths_cumul, na.rm=TRUE)
    cases_cumul_week = max(cases_cumul, na.rm=TRUE),
    deaths_cumul_week = max(deaths_cumul, na.rm=TRUE)
  ) %>% 
  ungroup() %>% #just to be sure
  group_by(state) %>%  
  #note we use group_by() without summarise here
  arrange(year, week) %>% 
  mutate(
    cases_cumul_week_before = lag(cases_cumul_week),
    cases_incident_week = cases_cumul_week - cases_cumul_week_before,
    deaths_cumul_week_before = lag(deaths_cumul_week),
    deaths_incident_week = deaths_cumul_week - deaths_cumul_week_before
  ) %>% 
  ungroup() %>% 
  #link in the population data
  left_join(pop_by_state_wrangle_nogeo, by = "state") %>% 
  #calculate weekly cumulative incidence per population 
  #up until that point in time in that week and the weekly incidence rate
  mutate(
    cases_cumul_per_pop_week = cases_cumul_week/population,
    deaths_cumul_per_pop_week = deaths_cumul_week/population,
    cases_incident_per_pop_week = cases_incident_week/population,
    deaths_incident_per_pop_week = deaths_incident_week/population 
  ) %>% 
  #look up the date corresponding to the
  #end of the week for the chart
  left_join(lookup_year_date_week_end, by = c("year", "week"))


#Explore the missing values in particular. Are they as we'd expect?
#For a given state, the "week-before" value should be missing
#for the state's first observation in the data, but not missing otherwise
View(nyt_state_pop_wrangle_weekly)
names(nyt_state_pop_wrangle_weekly)

#cumulative covid incidence at week end
nyt_state_pop_wrangle_weekly %>% 
  filter(continental_48==1) %>%   
  ggplot( aes(x=date_max, y=cases_cumul_per_pop_week))+
  geom_line(aes(color = state)) +
  ylab("Cumulative COVID-19\n incidence\n per person \n at end of week") +
  xlab("Date")  +
  theme_bw() +
  labs(color = "State")+ #label legend
  theme(legend.position="bottom") +
  scale_x_date(date_breaks = "month" , date_labels = "%b-%y") +
  theme(legend.position="bottom") +
  theme(axis.text.x = element_text(angle = 90))

#weekly incidence rate by week
nyt_state_pop_wrangle_weekly %>% 
  filter(continental_48==1) %>%   
  ggplot( aes(x=date_max, y=cases_incident_per_pop_week))+
  geom_line(aes(color = state)) +
  ylab("Incidence rate of COVID-19\n per person-week") +
  xlab("Date")  +
  #https://ggplot2.tidyverse.org/reference/labs.html
  labs(color = "State")+ #label legend
  theme_bw() +
  theme(legend.position="bottom") +
  scale_x_date(date_breaks = "month" , date_labels = "%b-%y") +
  theme(legend.position="bottom") +
  theme(axis.text.x = element_text(angle = 90))



