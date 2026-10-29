model=lm(minutespld~age+bmi+trnload, data = data)
age=(data$age)
bmi=(data$bmi)
summary(model)
par(mfrow = c(2, 2))
plot(model)
par(mfrow = c(1, 1))
#multiple linear regression was conducted to investigate the relationship 
#between minutes played and three explanatory varibales:age, BMI and traing load
#The overall regression model was stastically significant, F(3,616)=9.978, P<0.001
#however the model explained only 4.63% variation in minutes palyed
#Diagnostic plots indicated that the residuals were broadly centred around zero
#allthough deviations from normality were visible in the q-q plot.The scale location
#plot suggested relatively consistent resiudal variance, while the leverage plot showed a small number
#of observations that may warrant further investigation
#overall, the findings suggest that training load is positively associated with minutes played,
#while age and BMI provide limited additional explanatory information