age=(data$age)
plot(age, minutespld,
     xlab = "age (years)",
     ylab = "minutespld",
     main = "age vs minutespld",
     pch = 19)
model4=lm(minutespld~age,data = data)
summary(model4)
anova(model4)
plot(age,minutespld,
     xlab = "age",
     ylab = "minutespld",
     main = "age vs minutespld")

abline(model4, col = "red", lwd = 2)

#The scatter plot shows little evidence of a linear relationship between age and minutes played
#The regression line is almost horizontal,suggesting that age has minimal association with playing time
#The regression analysis suggests there's considerable variation in minutes played among players
#of similar ages, a very low R-squared value (0.00027) and a statistically insignificant
#p-value (0.682). Therefore, age doesn't appear to be a useful predicctor of minutes played in the dataset.