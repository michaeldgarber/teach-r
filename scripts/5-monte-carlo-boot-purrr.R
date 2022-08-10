#Working script corresponding to
#~docs/5-monte-carlo-boot-purrr.Rmd

## Monte carlo simulation - one dataset--------
library(tidyverse)
library(truncnorm)
set.seed(11)#set the seed so this dataset is always the same.
monte_carlo_sim_frozen = 1:100 %>% 
  as_tibble() %>% 
  rename(county_id = value) %>% 
  mutate(
    state = 1,
    pop = rpois(n=n(), lambda=10000), #poisson dist. mean = var=1000
    dem = rbinom(n=n(), size=1, prob =.5), #binomial dist.
    prev = rtruncnorm( #truncated normal so prevalence is bounded by 0,1
      n=n(), 
      a=0, 
      b=1,
      mean=0.05+(pop/50000)+(dem/10),
      sd=0.05 +(pop/200000)+(dem/40)),
    n_cases = as.integer(pop*prev),
  )

monte_carlo_sim_frozen

#assess correlation
cor(monte_carlo_sim_frozen$pop,
    monte_carlo_sim_frozen$prev)

#What is the mean and standard deviation number of cases per county in the "frozen" dataset?
monte_carlo_sim_frozen %>% 
  group_by(state) %>% 
  summarise(
    n_cases_mean = mean(n_cases),
    n_cases_sd = sd(n_cases)) %>% 
  ungroup()

monte_carlo_sim_frozen %>% 
  ggplot(aes(n_cases))+
  geom_histogram()

## Define a function for replications--------
monte_carlo_sim_fun = function(rep_id_val){
  monte_carlo_sim_df = 1:100 %>% 
    as_tibble() %>% 
    rename(county_id = value) %>% 
    mutate(
      state = 1,
      pop = rpois(n=n(), lambda=10000), #mean = var=1000
      dem = rbinom(n=n(), size=1, prob =.5),
      prev = rtruncnorm(
        n=n(), 
        a=0, 
        b=1,
        mean=0.05+(pop/50000)+(dem/10),
        sd=0.05 +(pop/200000)+(dem/40)),
      n_cases = as.integer(pop*prev),
      rep_id = rep_id_val  #add replication ID
    )
}

#run one iteration
monte_carlo_sim_once = monte_carlo_sim_fun(rep_id_val = 5)
monte_carlo_sim_once
nrow(monte_carlo_sim_once)

#run function lots of times

rep_id_val_list = 1:500
rep_id_val_list
class(rep_id_val_list)

library(purrr) #make sure it's attached
set.seed(NULL) #allow it to vary differently each time.
monte_carlo_sim_lots  = rep_id_val_list %>% 
  map_dfr(monte_carlo_sim_fun) 

##look at results ---------
monte_carlo_sim_lots
nrow(monte_carlo_sim_lots)
n_distinct(monte_carlo_sim_lots$rep_id)

## summarize results------
monte_carlo_sim_lots %>% 
  group_by(rep_id,state) %>% 
  summarise(
    n_cases_mean = mean(n_cases),
    n_cases_sd = sd(n_cases)) %>% 
  ggplot(aes(n_cases_mean))+
  geom_histogram()

monte_carlo_sim_lots_summary = monte_carlo_sim_lots %>% 
  group_by(rep_id,state) %>% #group by both rep id and state
  summarise(
    n_cases_mean = mean(n_cases),
    n_cases_sd = sd(n_cases)) %>% 
  group_by(state) %>% #now collapse over rep id
  summarise(
    n_cases_mean_ll = quantile(n_cases_mean, probs = 0.025, na.rm=TRUE),
    n_cases_mean_med = quantile(n_cases_mean, probs = 0.5, na.rm=TRUE),
    n_cases_mean_ul = quantile(n_cases_mean, probs = 0.975, na.rm=TRUE)
  ) %>% 
  ungroup() %>% 
  mutate(simulation_type = "monte-carlo")

monte_carlo_sim_lots_summary

# Bootstrap-------

## Take one sample
monte_carlo_sim_one_boot = monte_carlo_sim_frozen %>% 
  slice_sample(prop=1,replace=TRUE)
monte_carlo_sim_one_boot

nrow(monte_carlo_sim_one_boot)
n_distinct(monte_carlo_sim_one_boot$county_id)

## Define a function to take lots of samples
boot_fun = function(rep_id_val){
  monte_carlo_sim_boot = monte_carlo_sim_frozen %>% 
    slice_sample(prop=1,replace=TRUE) %>% 
    mutate(rep_id = rep_id_val) #to keep track of reps
}

boot_lots  = rep_id_val_list %>% 
  map_dfr(boot_fun) 

## summarize results---
boot_lots

boot_lots %>% 
  group_by(rep_id,state) %>% 
  summarise(
    n_cases_mean = mean(n_cases),
    n_cases_sd = sd(n_cases)) %>% 
  ggplot(aes( n_cases_mean))+
  geom_histogram()

boot_lots_summary = boot_lots %>% 
  group_by(rep_id,state) %>% #group by both rep id and state
  summarise(
    n_cases_mean = mean(n_cases),
    n_cases_sd = sd(n_cases)) %>% 
  group_by(state) %>% #now collapse over rep id
  summarise(
    n_cases_mean_ll = quantile(n_cases_mean, probs = 0.025, na.rm=TRUE),
    n_cases_mean_med = quantile(n_cases_mean, probs = 0.5, na.rm=TRUE),
    n_cases_mean_ul = quantile(n_cases_mean, probs = 0.975, na.rm=TRUE)
  ) %>% 
  ungroup() %>% 
  mutate(simulation_type = "bootstrap")

boot_lots_summary

#How well did bootstrapping do?
monte_carlo_sim_lots_summary %>% 
  bind_rows(boot_lots_summary)