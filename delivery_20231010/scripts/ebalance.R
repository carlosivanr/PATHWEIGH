library(ebal)

# create toy data: treatment indicator and three covariates X1-3
treatment <- c(rep(0,50),rep(1,30))
X <- rbind(replicate(3,rnorm(50,0)),replicate(3,rnorm(30,.5)))
colnames(X) <- paste("x",1:3,sep="")

# entropy balancing
# Takes as input two objects
# 1. A binary vector of the group that the observation belongs to
# 2. A matrix (X) of all of the covariates to utilize
eb.out <- ebalance(Treatment=treatment, X=X)

# eb.out
# Contains W which are the weights for the control group
# 


# means in treatment group data
# Returns a 1 x 3 matrix of the 
apply(X[treatment==1,],2,mean)

# means in reweighted control group data
apply(X[treatment==0,],2,weighted.mean,w=eb.out$w)

# means in raw data control group data
apply(X[treatment==0,],2,mean)


# Make a data frame and add the weights
test <- cbind(as.data.frame(treatment), as.data.frame(X)) %>%
  mutate(weight = 1)

test$weight[treatment == 0] <- eb.out$w

# Weights can then be included into the statistical model

# Could also try weight it package
library(WeightIt)
library(cobalt)

# Estimand ATT Agerate treatment effect on the treated is equivalent to ebal default
(W1 <- weightit(treatment ~ x1 + x2 + x3, 
                data = test,
                method = "ebal", 
                estimand = "ATT"))
summary(W1)
bal.tab(W1)
W1$weights

# Compare the mean by WeightIt vs ebal()
apply(X[treatment==0,],2,weighted.mean,w=W1$weights[1:50])


