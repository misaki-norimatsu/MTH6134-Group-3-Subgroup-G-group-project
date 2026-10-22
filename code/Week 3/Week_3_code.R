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