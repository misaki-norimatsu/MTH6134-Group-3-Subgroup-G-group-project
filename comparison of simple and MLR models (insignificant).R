model1=lm(minutespld~trnload,data=data)
anova(model1,model)
summary(model1)$adj.r.squared
summary(model)$adj.r.squared
#An analysis of variance (ANOVA) was conducted to compare a simple linear
#regression model containing training load with a multiple linear regression model
#containing age, BMI and triaing load.The results inidicate that including age and
#bmi didn't significantly improve model fit,(F(2,616)=0.3637),(p=0.6952).Therefore,
#there was insufficient evidence to reject the null hypothesis that the coefficients of age and bmi
#were jointly equal to zero.Consequently,the simpler model containing only training load
#was preffered. However, its relatively low adjusted R-squared suggests that training load alone
#explains only a small proportion of the variation in minutes played
