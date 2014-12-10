#exploratory.R

source("misc.R");


library(ggplot2)
library(reshape2)


summary(train_subset)
# return simple vector with degree of missingness
prop_missing_train_subset <- sapply(train_subset,
                                      FUN = prop_missing)




long.y <- rep(train_subset_test[,1], 13)

#melt the covariates
features <- melt(train_subset_test[,2:14])

#combine the long response with the melted features
eda_plot_set <- cbind(long.y, features)


# Create some plots of the target vs. the training subset
#for varying levels of the df's of the natural cubic smoothing
#spline

plot.response <- ggplot(eda_plot_set, aes(value, long.y)) + geom_point() + 
  ylim(-0.25, 1.25) + stat_smooth(method="gam",formula=y~bs(x,3)) 
plot.response + facet_grid(. ~ variable, scales="free")









