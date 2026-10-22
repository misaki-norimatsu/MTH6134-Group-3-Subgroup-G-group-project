boxplot(bmi ~ pst, data = data,
        xlab = "pst",
        ylab = "bmi",
        main = "BMI by Playing Position",
        col = "lightblue")
model5=lm(bmi~pst,data = data)
summary(model5)
anova(model5)
model6=lm(bmi ~ pst, data = data)
par(mfrow = c(2, 2))
plot(model6)

#Residuals VS   Fitted(topleft)
#There is no obvious evidence of a systematic pattern in the residuals.
#No obvious curved pattern
#The four clusters are expected because position is categorical.

#Normal Q-Q plot(Top Right)
#The residuals appear approximately normally distributed, 
#although there may be minor departures from normality in the tails.
#deviations seem relatively small

#scale location plot(bottom left)
#There is no strong visual evidence of heteroscedasticity,
#suggesting the constant variance assumption is reasonably satisfied.
#The spread of residuaks appears reasonably similar across groups

#Residuals vs Leverage(bottom right)
#There's no obvious evidence of highly inluential observations,
#although Cook's distance should be examined numerically
#No clearly extreme influential observations are apparent from the displayed Cook's distance 
