#data_mgmt.R
# This file takes care of some of the general 
# data management on the training set (possibly the test set)
# such as imputation, subsetting, etc...

library(dummy)
library(splines)
#source("misc.R");


#plugs in a simple missing level for the categorical variables
simple_impute_cat <- function(vec){
    #if the number of levels are 
    if(any(levels(vec) == "")){
      #set the level that is "missing" equal to 
      # "miss"
      levels(vec)[which(levels(vec)=="")] <- "miss"
      return(vec)
    }
    else{
      return(vec)
    }
  
}




#simple imputation of numeric vectors
simple_impute_numeric <- function(vec, type="Median"){
  
  #vector of acceptable type strings
  types <- c("med", "median", "Median", "mu", "ave", "mean", "Mean")
  #checks to see if the column vector is numeric
  if(is.numeric(vec)){
    if(type %in% types[1:3]){

      #median imputation
      
      obs<- vec[!is.na(vec)]
      med.obs <- median(obs)
      print(paste("The median of the observed values is", 
                  as.character(med.obs)))
      vec[is.na(vec)] <- med.obs
      
      return(vec)
      
    }
    
    if(type %in% types[4:7]){
      #mean imputation
      
      obs<- vec[!is.na(vec)]
      mean.obs <- mean(obs)
      vec[is.na(vec)] <- mean.obs
      return(vec)
    }
       
  }
}

train_subset_imputed_numeric <- apply(train_subset[,1:14], MARGIN=2, FUN =
                                        function(x){
                                          return(simple_impute_numeric(x))
                                        })

train_subset_imputed_cat <- apply(train_subset[,15:40], MARGIN=2, FUN =
                                    function(x){
                                      return(simple_impute_cat(x))
                                    })


#fuck-it let's create a dummy coded version of all of the categorical
# variables.

cat_vars_dummy_coded <- dummy(train_subset[,15:40], p = 10)

#the newest incarnation of the training set
train_subset_12.6.14<-data.frame(cbind(train_subset_imputed_numeric, 
                                       cat_vars_dummy_coded))


#sample 10,000 rows.
train_subset_test <- train_subset_12.6.14[sample(1:10^6, size=10^4),]

#create a fake test set.
fake_test_set <- train_subset_12.6.14[sample(1:10^6, 10^4),]


#some results from the exploratory.R indicate that a b-spline basis
#with 3 df's may produce useful predictors

#cbinds list of cubic smooths together
b_spline_matrix<-do.call(cbind, lapply(train_subset_test[,2:14], FUN=function(x){
  return(bs(x,3))
}))

#combine the training set to the b_splines
train_subset_test_12.8.14 <- cbind(train_subset_test,data.frame(b_spline_matrix))


