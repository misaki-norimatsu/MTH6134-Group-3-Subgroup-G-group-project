plot(trnload,bmi,
xlab="trnload",
ylab="bmi",
main="trnload vs bmi",
pch=19)
boxplot(bmi~trnload,
xlab="trnload",
ylab="bmi",)
model10=lm(bmi~trnload,data = data)
summary(model10)
anova(model10)
#A simple linear regression model was fitted to investigate the relationship between
#traing load and BMI among the league.The results revealed a weak negative association,
#with a regression coefficient of -0.06267, suggesting that each one-unit increase in training
#load is associated with an average decrease of approximately of 0.063 BMI units
#However this realtionship was not significant(P=0.0299)