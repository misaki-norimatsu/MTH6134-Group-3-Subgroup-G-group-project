#Consider a binary outcome in the data and fit a model for it.
install.packages("arm")
library(arm)

source("code/load_data.R")

#The variable injured is binomial, with 1 injured and 0 not injured, so a binomial glm is appropriate.

glm<-glm(injured ~ age + minutes_played + bmi + training_load + prev_injuries, data = league, family = "binomial")
summary(glm)

#Age, and previous injuries have very low p values so is strongly significant predictor of injuries. Training load is also a significant predictor. As these are positive coefficients estimates, this suggests that as age, training load, and previous injuries increase, the probability of injuries increase. 
#Minutes played, and BMI have p values greater than 5% so are not significant predictors of injury.
#The residual deviance is much smaller than the null deviance so the explanatory variables improved the fit of the model more than the model with only the intercept.

exp(summary(glm)$coef[2,1]) #1 is the estimate column 2 is age row.
#The odds for having injuries for players that are older increase by 11.1%.
exp(summary(glm)$coef[5,1])
#The odds for having injuries for players that have a higher training load increase by 35.4%.
exp(summary(glm)$coef[6,1])
#The odds for having injuries for players that have previous injuries increase by 67.1%.

confint.default(glm)
#Minutes played and bmi include 0 in the Confidence Interval which means it could be 0 and have no significant effect on probability of injury.


plot(fitted(glm),resid(glm))

#The residual vs fitted plot have two curved lines of points which is expected for a binary response variable. The symmetric residuals are two smooth curves.

binnedplot(x = fitted(glm),
           y = resid(glm),
           xlab = "Fitted probability")

#Most points are within the bands, suggesting an acceptable fit, however they are not very centered around the zero line, and 6 points are outside which may indicate some lack of fit, with the model not capturing the relationship between injuries and other explanatory variables. 

glm_2<-glm(injured ~ age * minutes_played, data = league, family = "binomial")
summary(glm_2)

#All the terms in this model are not significant. The difference between ages and minutes played is not significant. This suggests age, minutes played and their interaction are not good predictors of injuries by themselves.
#The residual deviance(577.17) is higher than the previous model(538.55), so may be a worse fit.

binnedplot(x = fitted(glm_2),
           y = resid(glm_2),
           xlab = "Fitted probability")

#More of  the points are within the bands, and less concentrated around lower probabilities, however the significance and deviance of the model suggests a bad fit.

par(mfrow=c(1,2))
plot(fitted(glm), resid(glm, type="deviance"),
     xlab="Fitted", ylab="Deviance Residual", main="Linear fit")
plot(fitted(glm), resid(glm, type="pearson"),
     xlab="Fitted", ylab="Pearson Residual", main="Linear fit")
par(mfrow=c(1,1))
