source("code/load_data.R")

#number of rows and columns in the data
dim(players)
dim(matches)
#players- 620 rows, 8 columns
#matches- 46 rows, 4 columns

#structure of the data
str(players)
str(matches)

#first few rows of the data
head(players)
head(matches)

#summary of the data
summary(players)
summary(matches)

#exact column names
names(players)
names(matches)
#injured contains 0s and 1s, so it's a binary response.
#minutes_played measures playing time and is numeric
#goals_scored and goals_conceded are counts.
#position and home_away are categorical variables.
#prev_injuries is a count of previous injuries.
#age and bmi are numerical variables
#training_load is coded from 1 to 5, so it is an ordered rating or score.

# Convert categorical variables into factors
players$team <- factor(players$team)
players$position <- factor(players$position)
matches$home_away <- factor(matches$home_away)

# Check the categories
table(players$position)
table(players$injured)
table(matches$home_away)


#Identify response and explanatory variables

#Example 1: Player injury status
#Response variable: injured
#Explanatory variables: age, bmi, training_load, prev_injuries
#Outcome type: Binary (0/1)
#Potential model: Logistic regression

#model for example 1
model_injury <- glm(
  injured ~ age + bmi + training_load + prev_injuries,
  family = binomial(link = "logit"),
  data = players
)
#the results
summary(model_injury)
#Convert coefficients to odds ratios
exp(coef(model_injury))
#Calculate confidence intervals for odds ratios
exp(confint(model_injury))

#Analysis of example 1:

#From the summary of the model, we can see:
#For age, the estimated coefficient is positive, suggesting that the older players tend to have higher odds of injury when BMI, training load and previous injuries are held constant. Since the p-value is below 0.05, there is statistical evidence of an association between age and injury status in this model.
#For BMI, the estimate is negative, suggesting slightly lower injury odds for players with higher BMI, holding the other variables constant. However, the p-value is much larger than 0.05, which means the model doesn't provide clear statistical evidence of an association between BMI and injury status after adjusting for the other predictors. It suggests that the data may not provide enough evidence to establish a relationship between BMI and injury.
#For training load, the coefficient is positive, suggesting that higher training load ratings are associated with higher injury odds after accounting for age, BMI and previous injuries. The p-value is below 0.05, so the model provides statistical evidence of an association.
#For previous injuries, the coefficient is positive, and the p-value is very small, which provides strong statistical evidence that players with more previous injuries tend to have higher odds of being recorded as injured, controlling for other variables.

#From the exp() function, the exponential of each coefficient is calculated, converting coefficients into odd ratios, and we can see that:
#Each additional year of age is associated with 11.1% higher odds of injury, where BMI, training load and previous injuries remain constant.
#Around 4.5% lower odds per BMI unit.
#For training load, around 37.2% higher odds per one-unit increase.
#For each additional previous injury, around 67% higher odds.

#The null deviance for this model is 594.71, representing the lack of fit of an intercept-only logistic regression model that did not include any explanatory variables.
#After including age, BMI, training load and previous injuries, the residual deviance decreased to 538.86, suggesting that the explanatory variables collectively improved the model's fit compared with the intercept-only model.
#The fitted logistic regression model had an Akaike Information Criterion (AIC) of 548.86. AIC balances model fit against model complexity and is useful for comparing alternative models fitted to the same response data. However, this value cannot be interpreted as a standalone measure of model quality. Further comparisons with plausible alternative models, alongside diagnostic checks and assessment of data quality, would be needed to evaluate the model more fully.

# Calculate odds ratios
OR <- exp(coef(model_injury))
# Calculate 95% confidence intervals
CI <- exp(confint(model_injury))
# Remove the intercept: focus on the predictors
OR <- OR[-1]
CI <- CI[-1, ]
# Leave space on the left for predictor names
par(mar = c(5, 9, 4, 2))
# Plot odds ratios against their predictor positions
plot(
  OR,
  seq_along(OR),
  xlim = range(CI),
  yaxt = "n",
  ylab = "",
  xlab = "Odds ratio (95% confidence interval)",
  pch = 19,
  main = "Factors associated with injury status"
)
# Add the confidence intervals as horizontal lines
segments(
  CI[, 1], seq_along(OR),
  CI[, 2], seq_along(OR)
)
# Add predictor names to the vertical axis
axis(
  2,
  at = seq_along(OR),
  labels = names(OR),
  las = 1
)
# Add the reference line for an odds ratio of 1
abline(v = 1, lty = 2, col = "grey40")


#Example 2: Player playing time
#Response variable: minutes_played
#Explanatory variables: age, position, training_load
#Outcome type: Numerical
#Potential model: Linear regression

#model for example 2
model_minutes <- lm(
  minutes_played ~ age + position + training_load,
  data = players
)
#the results
summary(model_minutes)
#Diagnostic plots
par(mfrow = c(2, 2))
plot(model_minutes)
par(mfrow = c(1, 1))


#Example 3: Goals scored per match
#Response variable: goals_scored
#Explanatory variables: home_away, opponent_rank
#Outcome type: Count
#Potential model: Poisson regression

#model for example 3
model_goals <- glm(
  goals_scored ~ home_away + opponent_rank,
  family = poisson(link = "log"),
  data = matches
)
#the results
summary(model_goals)
#Convert coefficients to expected-goal ratios
exp(coef(model_goals))
#Check residual deviance relative to residual degrees of freedom
deviance(model_goals) / df.residual(model_goals)


#Example 4: Goals conceded per match
#Response variable: goals_conceded
#Explanatory variables: home_away, opponent_rank
#Outcome type: Count
#Potential model: Poisson regression

#model for example 4
model_conceded <- glm(
  goals_conceded ~ home_away + opponent_rank,
  family = poisson(link = "log"),
  data = matches
)
#the results
summary(model_conceded)
#Convert coefficients to ratios
exp(coef(model_conceded))
#Rough overdispersion check
deviance(model_conceded) / df.residual(model_conceded)


#Example 5: Previous player injuries
#Response: prev_injuries
#Explanatory variables: age, bmi, training_load
#Outcome type: Count
#Potential model: Poisson regression?

#model for example 5
model_prev_injuries <- glm(
  prev_injuries ~ age + bmi + training_load,
  family = poisson(link = "log"),
  data = players
)
#the results
summary(model_prev_injuries)
#Convert coefficients to ratios
exp(coef(model_prev_injuries))
#Rough overdispersion check
deviance(model_prev_injuries) /
  df.residual(model_prev_injuries)


# Compare model summaries
AIC(model_injury)
AIC(model_minutes)
AIC(model_goals)
AIC(model_conceded)
AIC(model_prev_injuries)