#An R script corresponding to 
#~teach-r/docs/1-dplyr-nyt-covid.Rmd

#August 2, 2022

# Install packages and download the data-------
install.packages("tidyverse")
install.packages("here")
library(tidyverse)
library(here)

## Load the COVID-19 data from *The New York Times--------

### Load the data directly from their Github repository------
nyt_state_url = "https://raw.githubusercontent.com/nytimes/covid-19-data/master/us-states.csv"

nyt_state_data = nyt_state_url %>% 
  url() %>% 
  readr::read_csv()


nyt_state_data
#what's the max date?
max(nyt_state_data$date) 

### Download the data I saved and load using `read_csv()`--------
#save as csv
setwd(here("data-processed"))
write_csv(nyt_state_data, "nyt_state_data_downloaded.csv")
#I manually moved it to data-input as well, so confirm that this works:

setwd(here("data-input"))
getwd()

nyt_state_data = readr::read_csv("nyt_state_data_downloaded.csv")


### Further explore the data using View() before manipulation with dplyr verbs.
View(nyt_state_data)
nrow(nyt_state_data) #How many rows?
ncol(nyt_state_data) #How many columns?
dim(nyt_state_data) #Get the table's dimensions, i.e., number of rows and columns.
names(nyt_state_data) #Return the names of the variables.

#Returns information about each variable's type (i.e., character vs numeric)
str(nyt_state_data) 

# Explore the NYT COVID-19 data using dplyr------
## Essential dplyr verbs------
## mutate-------
nyt_state_data_2 = mutate(nyt_state_data, cases_plus_1 = cases+1)
class(nyt_state_data$cases)

nyt_state_data_2_pipe = nyt_state_data %>% 
  mutate(cases_plus_1 = cases+1) %>% 
  mutate(fifty = 50)

nyt_state_data_2
names(nyt_state_data_2)
nyt_state_data_2_pipe
names(nyt_state_data_2_pipe)

nyt_state_data_2vars  = nyt_state_data %>% 
  mutate(
    cases_plus_1 = cases+1,
    cases_plus_2 = cases+2
  )
nyt_state_data_2vars

## filter------
nyt_state_data_mi = filter(nyt_state_data, state == "Michigan")

nyt_state_data_mi_pipe = nyt_state_data %>% 
  filter(state == "Michigan")


nrow(nyt_state_data_mi)
nrow(nyt_state_data_mi_pipe)
nrow(nyt_state_data_mi)/nrow(nyt_state_data)

## select------
names(nyt_state_data)
date = dplyr::select(nyt_state_data, date)
more_cols = nyt_state_data %>% 
  dplyr::select(date, state)
nyt_state_data_state = nyt_state_data %>% 
  dplyr::select(starts_with("state"))
nyt_state_data_state                

nyt_state_data_nodeaths = nyt_state_data %>% 
  dplyr::select(everything(), -deaths)
names(nyt_state_data)
names(nyt_state_data_nodeaths)

## arrange--------

## group_by-------
nyt_state_wrangle = nyt_state_data %>% 
  
  #To calculate incident deaths, we will subtract the number of cumulative deaths on one day from the day before.
  #So let's group the data by state and then sort by date. Note it will default sort to ascending, meaning the earliest days will be on top.
  group_by(state) %>% 
  arrange(date) %>% 
  #Then calculate the number of incident deaths and incident cases on that day by subtracting the corresponding value from the day before.
  #Calculate the value from the day before using the lag() function, which grabs the value of one row up the data.
  #(We could have equivalently sorted descending and then used the lead() function.)
  #To explicitly differentiate between cumulative and incident deaths, let's rename the cases and deaths variables first. 
  rename(
    cases_cumul = cases,
    deaths_cumul = deaths
  ) %>% 
  mutate(
    cases_cumul_day_before = lag(cases_cumul),
    cases_incident = cases_cumul - cases_cumul_day_before,
    
    deaths_cumul_day_before = lag(deaths_cumul),
    deaths_incident = deaths_cumul - deaths_cumul_day_before
  ) %>% 
  ungroup() %>% 
  #For visual convenience, now sort the data by state by date
  arrange(state, date)


### group_by (continued)-------
nyt_last_day_by_state = nyt_state_wrangle %>% 
  arrange(desc(date)) %>% #Use desc() to sort it by date descending,
  #meaning the most recent day will be on the top of the dataset.
  group_by(state) %>% 
  slice(1) #The number 1 grabs the top row

nyt_last_day_by_state

nrow(nyt_last_day_by_state)

nyt_last_day  = nyt_state_wrangle %>% 
  arrange(desc(date)) %>% 
  #  group_by(state) %>% #This comments out the code so this part doesn't run.
  slice(1) #The number 1 grabs the top row

nrow(nyt_last_day)

nyt_last_day
### group_by and summarise----------
nyt_cases_summary = nyt_state_wrangle %>% 
  group_by(state) %>% 
  summarise(
    cases_incident_average = mean(cases_incident, na.rm=TRUE),
    cases_cumul_way1 = sum(cases_incident, na.rm=TRUE),
    cases_cumul_way2 = max(cases_cumul, na.rm=TRUE)
  )

dim(nyt_cases_summary)
nyt_cases_summary

### group_by and summarise (continued)
nyt_cases_overall = nyt_state_wrangle %>% 
  group_by(state) %>% 
  summarise(cases_cumul = max(cases_cumul, na.rm=TRUE)) %>% 
  ungroup() %>% 
  mutate(dummy=1) %>%
  group_by(dummy) %>% 
  summarise(cases_cumul = sum(cases_cumul, na.rm=TRUE))

nyt_cases_overall

## All together now in an example using `case_when()`--------
x=c(1,2,3,4,5)
5%in%x
6%in%x

deaths_cumul_by_state_greatlakes = nyt_state_wrangle %>% 
  mutate(
    #If the condition is met, set the value to 1. If not -- confusingly expressed by TRUE -- set it to 0.
    great_lakes = case_when(
      state == "Illinois" |
        state == "Indiana" |
        state == "Michigan" |
        state == "Minnesota" |
        state == "New York" |
        state == "Ohio" |
        state == "Pennsylvania" |
        state == "Wisconsin" ~ 1,
      TRUE ~ 0),  #The parentheses closes this case_when function.
    #The comma outside the parentheses implies we have another variable to be created in the mutate function
    
    #Alternative way using the %in% operator:A more concise way to state that may arguably be less readable is to use %in%    
    #The case_when function assigns a 1 wherever it's true.
    great_lakes_alt = case_when(
      state %in% c(
        "Illinois", "Indiana", "Michigan", 
        "Minnesota", "New York", "Ohio", 
        "Pennsylvania", "Wisconsin"
      ) ~ 1,
      TRUE ~0)
  ) %>% 
  
  filter(great_lakes==1) %>% 
  group_by(state) %>% 
  summarise(
    deaths_cumul = max(deaths_cumul, na.rm=TRUE)
  )

deaths_cumul_by_state_greatlakes