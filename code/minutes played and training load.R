bmi=(data$bmi)
trnload=(data$training_load)
age=(data$age)
model1=lm(minutespld~trnload,data = data)
summary(model1)
plot(model1$fitted, rstudent(model1),xlab="Fitted values", ylab="Studentised residuals")
m_linear <- lm(minutespld ~ trnload, data = data)
m_factor <- lm(minutespld ~ factor(trnload), data = data)
anova(m_linear, m_factor)
cor.test(minutespld,trnload, method = "spearman", exact = FALSE)
#A Spearman's rank correlation test was conducted to investigate the relationship
#between training load and minutes played.The results showed a weak positive
#correlation(p=0.195),suggesting that players with higher traiing loads tend to 
#play more minutes.The relationship was statistically significant(p<0.001),
#providing strong evidence of association between two variables.However, 
#the relatively small correlation soefficient indictaes that the relationship is
#weak, suggesting that other factors may also influence playing time.
