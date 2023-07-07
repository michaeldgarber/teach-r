#Simulate some data with missingness and fit a linear regression model
#July 7, 2023
install.packages("tidyverse")#install the tidyverse
library(tidyverse) #load all of the tidyverse packages

#Create a simulated dataset with some missingness------
data=1:100 %>% 
  as_tibble() %>% 
  rename(row_id=value) %>% #call this row id
  mutate(
    #simulate some data
    x=rnorm(n=n(),mean=1,sd=3),
    a=rnorm(n=n(),mean=10+1*x,sd=3*x),
    y=rnorm(n=n(),mean=50+5*x+10*a,sd=5+2*a),
    p=rnorm(n=n(),mean=100+10*x,sd=10*x),#e.g., population
    #create a 1/0 indicator for missing using the binomial distribution
    #missing prevalence is 20% randomly distributed
    missing_x=rbinom(n=n(),size=1,prob=.2),
    missing_y=rbinom(n=n(),size=1,prob=.2),
    missing_a=rbinom(n=n(),size=1,prob=.2),
    missing_p=rbinom(n=n(),size=1,prob=.2),
    
    #if that missing indicator is 1, set x, y, and a to missing
    x=case_when(
      missing_x==1~NA_real_,
      TRUE~x
    ),
    y=case_when(
      missing_y==1~NA_real_,
      TRUE~y
    ),
    p=case_when(
      missing_a==1~NA_real_,
      TRUE~p
  ),
  
  #By the way you could create a variable that is one divided by the other like this
  rate=y/p
  )

data

# Fit a linear regression model in this data-------


linear_regression_model= glm(
  formula=  rate~ #  the outcome variable
    x+a ,#independent variables (predictor variables)

  family="gaussian", 
  
  data=data ,
  na.action = na.exclude#exclude missings
)
linear_regression_model

#Check out model coefficients
linear_regression_model$coefficients
