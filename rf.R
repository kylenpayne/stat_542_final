# Random Forest code
library(bigrf)

#lol
# this will be run on the entire data-set on amazon
#big.data <- big.matrix(train_data)  

features <- train_subset_test_12.8.14[,-1]
y <- factor(train_subset_test_12.8.14[,1])


bigrf_model_1 <- bigrfc(features, y, ntrees=50L, trace=1)

#fake test set sample predictions
predictions <- predict(bigrf_model_1, features, 
                       y, trace=1)

#training set prediction
llfun(y, predictions)