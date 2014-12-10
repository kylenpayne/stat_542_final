#misc.R
# this file contains some misc. helper functions for our project


library(xtable)
#compare loss function metrics, place into latex table

# this function takes a list of model predictions ran of test sets
# and calculates the loss function across all models in 
# the list. 

# I'm pretty sure this makes sense, that we can extract the original

#compare.loss <- function()
  
  
#fill-in missing values in the categorical hashed variables
train_subset_2014.11.21<- apply(train_subset, MARGIN=1, function(x){
  ifelse(x=="", "miss", x)
})


#count the proportion of missingness for any vector
prop_missing <- function(vec){
  if(is.factor(vec)){
    return(mean(vec == ""))
  }
  if(is.numeric(vec)){
    return(mean(is.na(vec)))
  }
}




