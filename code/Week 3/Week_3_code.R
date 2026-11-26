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

#residuals and diagnostics for example 1:

# Calculate predicted probabilities of injury
predicted_prob <- fitted(model_injury)
# Calculate raw residuals
raw_residuals <- players$injured - predicted_prob
# Look at the first six players
head(data.frame(
  observed = players$injured,
  predicted_probability = predicted_prob,
  residual = raw_residuals
))
# Calculate Pearson residuals
pearson_residuals <- residuals(
  model_injury,
  type = "pearson"
)
# Plot residuals against predicted probabilities
plot(
  fitted(model_injury),
  pearson_residuals,
  xlab = "Predicted probability of injury",
  ylab = "Pearson residuals",
  main = "Residuals vs fitted probabilities",
  pch = 19
)
# Add a reference line at zero
abline(h = 0, lty = 2)
# Calculate deviance residuals
dev_res <- residuals(model_injury, type = "deviance")
# Plot their distribution
hist(
  dev_res,
  main = "Distribution of deviance residuals",
  xlab = "Deviance residual",
  breaks = 20
)
# Identify the five largest absolute residuals
head(
  sort(abs(dev_res), decreasing = TRUE),
  5
)
# Calculate Cook's distances
cook <- cooks.distance(model_injury)
# Plot Cook's distances
plot(
  cook,
  type = "h",
  xlab = "Player observation",
  ylab = "Cook's distance",
  main = "Influence of individual players"
)
# Add a rough reference threshold
abline(h = 4 / nrow(players), lty = 2, col = "red")
# Identify the five largest values
head(sort(cook, decreasing = TRUE), 5)
par(mfrow = c(2, 2))
plot(model_injury, which = 1)
plot(model_injury, which = 2)
plot(model_injury, which = 4)
plot(model_injury, which = 5)
par(mfrow = c(1, 1))
# Reset the plotting layout
par(mfrow = c(1, 1))
# Reset the margins
par(mar = c(4, 4, 2, 1))
# Display one diagnostic plot
plot(model_injury, which = 1)
# Set a 2 by 2 plotting layout
par(mfrow = c(2, 2))
# Reduce the margins
par(mar = c(3, 3, 2, 1))
# Diagnostic plots
plot(model_injury, which = 1)
plot(model_injury, which = 2)
plot(model_injury, which = 4)
plot(model_injury, which = 5)
# Reset plotting layout
par(mfrow = c(1, 1))

#Analysis from residuals and diagnostics:
#the residual vs fitted plot showed two curved band of points which is expected for a binary response variable.
#the smooth trend line remained relatively close to zero, with no strong pattern apparent.
#q-q plot showed points moving further away from the reference line indicating that the deviance residuals were not normally distributed, but normality of residuals is not a required assumption of logistic regression so this plot shouldn't be treated as a decisive indication of poor model fit.
#Cook's distance and residuals-versus-leverage plots identified several potentially influential observations. The five largest Cook's distances ranged from approximately 0.0205 to 0.0265, exceeding the rough screening threshold of (4/620 = 0.00645). Observations 238, 101, 169, 546 and 281 were therefore selected for further investigation. Several of these observations also had relatively large deviance residuals.
#Overall, the diagnostic plots suggest that the model is a reasonable starting point, although some individual observations may have a relatively large influence on the estimated coefficients. These observations should be checked for data-entry errors or unusual characteristics before deciding whether any further modelling adjustments are necessary.

players[c(238, 101, 169, 546, 281), ]
#Further investigation of the five observations with the largest Cook's distances showed that all five players had an injury recorded. 
#Four had no previous injuries, and three had the lowest training-load rating of 1. 
#These characteristics may help explain why these observations were relatively influential, since the fitted model estimated higher injury odds for players with greater training loads and more previous injuries. 
#However, no obviously impossible values were identified in the records. The observations were therefore retained rather than removed, as there was no clear evidence that they were data errors.


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

#Analysis of example 2:

#Residuals and diagnostics for example 2:
#four diagnostic plots
par(mfrow = c(2, 2))
par(mar = c(4, 4, 2, 1))
plot(model_minutes, which = 1)
plot(model_minutes, which = 2)
plot(model_minutes, which = 4)
plot(model_minutes, which = 5)
par(mfrow = c(1, 1))
#calculate residuals
predicted_minutes <- fitted(model_minutes)
raw_residuals_minutes <- residuals(model_minutes)
head(data.frame(
  observed = players$minutes_played,
  predicted = predicted_minutes,
  residual = raw_residuals_minutes
))
# Largest absolute residuals
head(sort(abs(rstudent(model_minutes)), decreasing = TRUE), 5)
# Cook's distances
cook_minutes <- cooks.distance(model_minutes)
head(sort(cook_minutes, decreasing = TRUE), 5)
# Rough screening threshold
4 / nrow(players)

#Analysis from residuals and diagnostics:

players[c(531, 43, 169, 40, 466), ]
players[c(531, 43, 480, 65, 53), ]


#Example 3: Goals scored per match
#Response variable: goals_scored
#Explanatory variables: home_away, opponent_rank
#Outcome type: Count
#Potential model: Poisson regression

#model for example 3
model_scored <- glm(
  goals_scored ~ home_away + opponent_rank,
  family = poisson(link = "log"),
  data = matches
)
#the results
summary(model_scored)
#Convert coefficients to expected-goal ratios
exp(coef(model_scored))
#Check residual deviance relative to residual degrees of freedom
deviance(model_scored) / df.residual(model_scored)

#Analysis for example 3:

#Residuals and diagnostics for example 3:
#produce diagnostic plots
par(mfrow = c(2, 2))
par(mar = c(4, 4, 2, 1))
plot(model_scored, which = 1)
plot(model_scored, which = 2)
plot(model_scored, which = 4)
plot(model_scored, which = 5)
par(mfrow = c(1, 1))
#compare observed and predicted goals
predicted_scored <- fitted(model_scored)
pearson_scored <- residuals(
  model_scored,
  type = "pearson"
)
head(data.frame(
  observed = matches$goals_scored,
  predicted = predicted_scored,
  residual = pearson_scored
))
#check for overdispersion
dispersion_scored <- sum(pearson_scored^2) /
  df.residual(model_scored)
dispersion_scored
#deviance ratio
deviance(model_scored) / df.residual(model_scored)
#check influential matches
cook_scored <- cooks.distance(model_scored)
head(sort(cook_scored, decreasing = TRUE), 5)
# Rough screening threshold
4 / nrow(matches)
top_scored <- order(cook_scored, decreasing = TRUE)[1:5]
matches[top_scored, ]

#Analysis from residuals and diagnostics:

matches[8, ]
data.frame(
  match = 8,
  actual_goals = matches$goals_scored[8],
  predicted_goals = fitted(model_scored)[8],
  pearson_residual = residuals(model_scored, type = "pearson")[8],
  cooks_distance = cooks.distance(model_scored)[8]
)


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

#Analysis for example 4:

#Residuals and diagnostics for example 4:
#diagnostic plots
par(mfrow = c(2, 2))
par(mar = c(4, 4, 2, 1))
plot(model_conceded, which = 1)
plot(model_conceded, which = 2)
plot(model_conceded, which = 4)
plot(model_conceded, which = 5)
par(mfrow = c(1, 1))
#calculate residuals
predicted_conceded <- fitted(model_conceded)
pearson_conceded <- residuals(
  model_conceded,
  type = "pearson"
)
head(data.frame(
  observed = matches$goals_conceded,
  predicted = predicted_conceded,
  residual = pearson_conceded
))
#check overdispersion
dispersion_conceded <- sum(pearson_conceded^2) /
  df.residual(model_conceded)
dispersion_conceded
deviance(model_conceded) / df.residual(model_conceded)
#identify influential matches
cook_conceded <- cooks.distance(model_conceded)
head(sort(cook_conceded, decreasing = TRUE), 5)
# Inspect the five most influential matches
top_conceded <- order(cook_conceded, decreasing = TRUE)[1:5]
matches[top_conceded, ]

#Analysis from residuals and diagnostics:


#Example 5: Previous player injuries
#Response: prev_injuries
#Explanatory variables: age, bmi, training_load
#Outcome type: Count
#Potential model: Poisson regression?

#model for example 5
model_prev <- glm(
  prev_injuries ~ age + bmi + training_load,
  family = poisson(link = "log"),
  data = players
)
#the results
summary(model_prev)
#Convert coefficients to ratios
exp(coef(model_prev))
#Rough overdispersion check
deviance(model_prev) /
  df.residual(model_prev)

#Analysis for example 5:

#Residuals and diagnostics for example 5:
#diagnostic plot
par(mfrow = c(2, 2))
par(mar = c(4, 4, 2, 1))
plot(model_prev, which = 1)
plot(model_prev, which = 2)
plot(model_prev, which = 4)
plot(model_prev, which = 5)
par(mfrow = c(1, 1))
#calculate residuals
predicted_prev <- fitted(model_prev)
pearson_prev <- residuals(
  model_prev,
  type = "pearson"
)
head(data.frame(
  observed = players$prev_injuries,
  predicted = predicted_prev,
  residual = pearson_prev
))
#check overdispersion
dispersion_prev <- sum(pearson_prev^2) /
  df.residual(model_prev)
dispersion_prev
deviance(model_prev) / df.residual(model_prev)
#identify influential players
cook_prev <- cooks.distance(model_prev)
head(sort(cook_prev, decreasing = TRUE), 5)
# Rough screening threshold
4 / nrow(players)
top_prev <- order(cook_prev, decreasing = TRUE)[1:5]
players[top_prev, ]

#Analysis from residuals and diagnostics:


# Compare model summaries
AIC(model_injury)
AIC(model_minutes)
AIC(model_goals)
AIC(model_conceded)
AIC(model_prev_injuries)

#Example 1 question: What factors are associated with player injury status? (binary logistic model)
#Example 3 question: How are hom advantage and opponent ranking associated with goals scored? (Poisson count model)
#Example 2 question: How are age, position, and training load associated with playing time? (Continuous linear model)