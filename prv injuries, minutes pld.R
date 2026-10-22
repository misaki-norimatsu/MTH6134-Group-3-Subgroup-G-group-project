plot(prvinj,minutespld,
xlab = "Number of Previous Injuries",
ylab = "Minutes Played",
main = "Previous Injuries vs Minutes Played",
pch = 19)
model2=lm(minutespld~prvinj,data = data)
summary(model2)
anova(model2)
plot(prvinj,minutespld,
xlab = "prvinj",
ylab = "minutespld",
main = "Prvinj,minutespld")
abline(model2, col = "orange", lwd = 2)
#The scatterplot examining previouus injuries against minutes played shows no clear
#linear relationship between two variables.The regression line is almost horizontal,
#indicating that previous injury history has very little association with playing time.