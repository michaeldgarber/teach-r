#Poisson regression for rate data
#Revised June 27, 2023


# Intro text------

#Poisson regression is used to model rates.
#A rate is defined as the number of events divided by some denominator that measures the at-risk experience
#giving rise to those events. In epidemiology, it is often expressed as number of events per person time.
#Note this is different from a "risk", because a risk can be viewed as a probability bounded
#by 0 and 1, whereas

#We can use poisson regression to model those rates as a function of predictor variables.
#Specifically, we model a Poisson distributed variable (integer 0, 1, 2, 3, ...)
#https://en.wikipedia.org/wiki/Poisson_distribution

#and we include the denominator of the rate as an "offset."

#Technically, we model the logarithm of the rate as a linear function of predictor variables.

#As described in video 9.9 below, the model takes the following functional form:
#ln(rate)=ln(y/t) = b0+b1*x1+b2*x2 +...
#where y is the number of events (or a count), t is follow-up time (the rate's denominator),
#b0 is the intercept, b1 is the coefficient for variable x1, b2 is the cofficient for
#independent variable x2, and so on.

#Then, because of the properties of logarithms, we can rewrite the log(rate) as:
#ln(y/t)=ln(y)-ln(t)
#so the full model can be expressed as
#ln(rate)=ln(y/t) =ln(y)-ln(t)= b0+b1*x1+b2*x2  

#Now we can move ln(t) over to the other side
#ln(y)-ln(t)= b0+b1*x1+b2*x2 
#ln(y)= b0+b1*x1+b2*x2 +ln(t)

#This ln(t) is what is known as the "offset" when fitting the Poisson model for rate data
#in software.

#I recommend this series of YouTube videos by MarinStatsLectures-R Programming, which
#discuss theory and application of the Poisson model

#9.9 Poisson Regression: The Model For Rate Data (what is an offset?)
#https://www.youtube.com/watch?v=5pZbER_mR4k&list=RDCMUCaNIxVagLhqupvUiDK01Mgg&index=4

#9.10 Poisson Regression in R: Fitting a Model To Rate Data (with offset) in R
#https://www.youtube.com/watch?v=QP4F98ysrEA

#9.11 Poisson Regression: Model Assumptions
#https://www.youtube.com/watch?v=r492fF9XZl8&t=2s

#The videos use the British doctor's data, a famous study on lung cancer and smoking
#https://en.wikipedia.org/wiki/British_Doctors_Study

# Load British Doctor's data-------
#To download British doctor's data, we can use this package:
#https://rdrr.io/cran/ACSWR/man/bs1.html
install.packages("ACSWR") #This installs the package containing the data
library(ACSWR) #This loads the package containing the data
data(bs1) #This brings data into the working environment.
bs1 #Take a look at the data.
#I like to work in the tidyverse, so let's make it a tibble
#(a data frame with some nice formatting.)
#Please see here for an intro to data wrangling in the tidyverse
#https://michaeldgarber.github.io/teach-r/dplyr-1-nyt-covid.html
library(tidyverse)
bs1_tibble=bs1 %>% as_tibble()
bs1_tibble
#In looking at the data, We can see that "Deaths" in this dataset are the Poisson variable 
#(counts greater than 0)
#And the denominator for the rate is "Person_Years".
#So the rate is rate=(Deaths/Person_Years)

# Fit a Poisson regression to model the rate------

#Following video 9.10 above, let's model the rate as a function of smoking (yes/no) 
#and the age category.
#We use the glm() function to fit this model
names(bs1_tibble)#get var names to make sure we type them correctly
pois_model = glm(
  formula=  Deaths~ #  #the outcome variable is Deaths, a count variable
    Smoke_Ind+Age_Group+ #the two predictor variables
    
    offset(log(Person_Years)),#here is where we specify the offset, i.e., the denominator of the rate
  
  family="poisson", #Poisson family for Poisson regression. Note lowercase. R is case-sensitive.
  
  data=bs1_tibble #the data in which the model is fit
           )

#Note this can all be done on fewer lines, as long as arguments are separated by commas.
#I put on multiple lines so I could annotate each.
pois_model = glm(formula=  Deaths~ Smoke_Ind+Age_Group+ offset(log(Person_Years)),  
                 family="poisson", data=bs1_tibble)

#Check out the model results by printing the object corresponding to the model
pois_model

#The rate ratio corresponding to each predictor can be found by exponentiating the coefficient
#The coefficients can be accessed by the syntax [model$coefficients]
pois_model_coefficients=pois_model$coefficients
rate_ratios=exp(pois_model_coefficients)
rate_ratios
#So adjusting for age category, smokers have a 1.4-times higher rate of lung cancer death

#Use the model to predict values-------
#We can use this model to predict expected values based on values of age category, smoking,
#and the person-years
#We use predict.glm() to do this
predicted_values=predict.glm(
  pois_model,#the model,
  #we can use any dataset here as long as it has the variables included in the model.
  #Here, we're simply going to use the original data in which the model was fit.
  #We can then compare the true values in the dataset with the model's predicted values
  #for that observation.
  newdata = bs1_tibble,
  type="response"
) %>% 
  as_tibble()#I'm converting it to a tibble for easier data wrangling

#We can see here that we have just one column of data. These are the expected values
#corresponding to each row in the original dataset.
predicted_values

#We can append these predicted values to the original dataset using the dplyr function
#bind_cols()

bs1_tibble_w_pred_values=bs1_tibble %>% 
  bind_cols(predicted_values) %>% 
  #The predicted values are called "value". Let's change that to 
  #Deaths_expected
  rename(Deaths_expected=value) %>% 
  #We could calculate a residual
  #Residual=actual value - predicted value
  mutate(residual=Deaths-Deaths_expected)

bs1_tibble_w_pred_values

#We can examine the actual number of deaths and the expected number based on the model to see how
#well the model performed.
#Here I'm using dplyr::select() to just select the two deaths variables and then will print them
#in the console so I can compare the two.
bs1_tibble_w_pred_values %>% 
  dplyr::select(Deaths,Deaths_expected)

#How are the residuals distributed?
bs1_tibble_w_pred_values %>% 
  ggplot()+
  geom_histogram(
    aes(residual),#the variable to be visualized with the histogram
    bins = 5#customize the histogram's bin width
    )

