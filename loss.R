#loss.R

#loss function implementation


llfun <- function(actual, prediction) {
  epsilon <- .000000000000001
  yhat <- pmin(pmax(prediction, epsilon), 1-epsilon)
  logloss <- -mean(actual*log(yhat)
                   + (1-actual)*log(1 - yhat))
  return(logloss)
}

com_loss <- 'this function requires the usage of 
 two vectors, a prediction vector, as 
 well as the vector of the true responses'

comment(llfun) <- com_loss



