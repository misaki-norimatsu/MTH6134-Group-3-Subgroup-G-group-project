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

par(mfrow=c(1,2))
plot(fitted(glm), resid(glm, type="deviance"),
     xlab="Fitted", ylab="Deviance Residual", main="Linear fit")
plot(fitted(glm), resid(glm, type="pearson"),
     xlab="Fitted", ylab="Pearson Residual", main="Linear fit")
par(mfrow=c(1,1))

#The residual vs fitted plot have two curved lines of points which is expected for a binary response variable. The symmetric residuals are two smooth curves, which suggests linear is not appropriate. The pearson and deviance residual plots look similar, however pearson have more curvature. This could suggest a misfit, or the model is not capturing all of the variance. Could test interaction, transformation or polynomial terms.

binnedplot(x = fitted(glm),
           y = resid(glm),
           xlab = "Fitted probability")

#Most points are within the bands, suggesting an acceptable fit, however they are not very centered around the zero line, and 6 points are outside which may indicate some lack of fit, with the model not capturing the relationship between injuries and other explanatory variables. 

glm_2<-glm(injured ~ age * minutes_played, data = league, family = "binomial")
summary(glm_2)

#All the terms in this model are not significant. The difference between ages and minutes played is not significant. This suggests age, minutes played and their interaction are not good predictors of injuries by themselves.
#The residual deviance(577.17) is higher than the previous model(538.55), so may be a worse fit.

glm_3<-glm(injured ~ age * training_load, data = league, family = "binomial")
summary(glm_3)

#All the terms in this model are significant. The difference between ages and training load is significant as their interaction is significant, may be necessary to include interaction in model.
#The residual deviance(559.01) is higher than the model with no interaction(538.55), so may be a worse fit.



glm_full_interaction<-glm(injured ~ age + minutes_played + bmi + training_load + prev_injuries +  age * training_load, data = league, family = "binomial")
summary(glm_full_interaction)

#With the interaction added to the full model, the residual deviance (525.05) is lower than without interaction (538.55) so it may be a better fit.
#The effect of age on log odds of injuries is -2.560e-01, when training load is not considered. With the interaction term (1.100e-01) this change in log odds of injuries per unit increase in training load. The interaction is p=0.000359 so is strongly statistically significant.

anova(glm, glm_full_interaction, test= "Chisq")
#The p values is 0.0002387 so adding the interaction did significantly improve the fit.

r1<-residuals(glm)
r2<-residuals(glm_full_interaction)
common_ylim <- range(c(r1, r2))

par(mfrow=c(1,2))
plot(fitted(glm_full_interaction), resid(glm_full_interaction, type="deviance"),
     xlab="Fitted", ylab="Deviance Residual", ylim= common_ylim, main="Linear fit")
plot(fitted(glm_full_interaction), resid(glm_full_interaction, type="pearson"),
     xlab="Fitted", ylab="Pearson Residual", ylim= c(-1,5), main="Linear fit")
par(mfrow=c(1,1))
#The model with interaction has wider range of probabilities up to 0.8, and has a changed density of points.

binnedplot(x = fitted(glm_full_interaction),
           y = resid(glm_full_interaction),
           xlab = "Fitted probability")
#More of the points are within the bands, suggesting an acceptable fit, however they are not very centered around the zero line with most underneath, and 6 points are outside which may indicate some lack of fit, with the model not capturing the relationship between injuries and other explanatory variables. 

glm_4<-glm(injured ~ age * prev_injuries, data = league, family = "binomial")
summary(glm_2)

#The interaction is not significant.

glm_5<-glm(injured ~ age * bmi, data = league, family = "binomial")
summary(glm_5)

#The interaction is slightly significant, however it is less significant then training_load so have focused on training_load. May revisit this interaction if using this model.

glm_6<-glm(injured ~ training_load * prev_injuries, data = league, family = "binomial")
summary(glm_6)

#The interaction is not significant.

#When analysing the significant variables, the only interaction that is significant is between age and training load.


##Overall, the binomial model has a moderate fit, even with interaction added, because the binned plot is not centered around zero with some outliers, although the residual deviance decrease, it does not explain all of the variance or relationship between the variables.