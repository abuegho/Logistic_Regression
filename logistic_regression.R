## Regression with binary outcomes


## Exercise: logistic regression

## Convert Unknown marital status to NA
NatHealth2011$r_maritl = factor(NatHealth2011$r_maritl, 
                                levels = c("0 Under 14 years", "1 Married - spouse in household",
                                           "2 Married - spouse not in household", "4 Widowed", 
                                           "5 Divorced", "6 Separated", "7 Never married", 
                                           "8 Living with partner"))

## Collapse inapplicable values in everwrk to NA's
NatHealth2011$everwrk = factor(NatHealth2011$everwrk,
                               levels = c("1 Yes", "2 No"))

##   1. Use glm to conduct a logistic regression to predict ever worked
##      (everwrk) using age (age_p) and marital status (r_maritl).

Age_Marital = glm(everwrk ~ age_p + r_maritl, data = NatHealth2011,
                  family = binomial)
Age_Marital_sum = coef(summary(Age_Marital))
Age_Marital_sum[, "Estimate"] = exp(coef(Age_Marital))

##   2. Predict the probability of working for each level of marital
##      status.

allEffects(Age_Marital)

plot(allEffects(Age_Marital_sum), rotx = 45)

##   Note that the data is not perfectly clean and ready to be modeled. You
##   will need to clean up at least some of the variables before fitting
##   the model.
