#Script code corresponding to
#~teach-r/docs/0-pre-reqs.Rmd
library(tidyverse)
library(here)

setwd(here("data-processed"))
getwd()
here()

library(tidyverse)
lake_huron_raw = datasets::LakeHuron #Define new object called lake_huron_raw.
lake_huron_raw #Take a look at it.

lake_huron_transformed = lake_huron_raw %>% #use the object defined above
  as_tibble() %>% 
  rename(level_ft =x) %>% #rename variable to "level_ft"
  mutate(year = row_number()-1+1875) #add a column for year

setwd(here("data-processed"))
getwd() #check to confirm
save(lake_huron_transformed, file = "lake_huron_transformed.RData")
