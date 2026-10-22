plot(age,bmi,xlab = "Age (years)",ylab = "BMI",main = "Age vs BMI", pch = 19)
model12=lm(bmi~age,data = data)
summary(model12)
anova(model12)
plot(age,bmi,xlab = "Age (years)",ylab = "BMI",main = "Age vs BMI")
abline(model12,col("orange"),lwd=2)
qqnorm(rstudent(model12),main=null)
qqline(rstudent(model12),lwd=2)
#A simple linear regression model was fitted to investigate the relationship 
#between age and BMI in the league. The results show a very weak negative association
#with a regression coefficeint of -0.00764, indictaing that each additional
#year of agee was associated with an average decrease of approxiamtely 0.008 BMI units
#overall, the results suggest that age alone isn't alone a useful predictor of BMI
#among the players in league.