library(tidyverse)# load the tidyverse

#let's remove them all again(
#and run just df2's code
df1 = c(1:10) %>% 
  as_tibble() %>%
  mutate(
    x_std_normal=rnorm(n=n(), mean=0, sd=1),
    x_binomial = rbinom(n=n(), size=1, prob = .5))

df2 = c(20:50) %>%
  as_tibble() %>%
  mutate(
    x_std_normal=rnorm(n=n(), mean=0, sd=1),
    x_binomial = rbinom(n=n(), size=1, prob = .5))

#only df2 appears
df3 = 100:200 %>% 
  as_tibble() %>%
  mutate(
    x_std_normal=rnorm(n=n(), mean=0, sd=1),
    x_binomial = rbinom(n=n(), size=1, prob = .5))

  