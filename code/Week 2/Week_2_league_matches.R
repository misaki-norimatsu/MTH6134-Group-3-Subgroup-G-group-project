source("code/load_data.R") #loading in data

# Summary statistics ----
#raw data
head(matches)
str(matches)
#overall summary
summary(matches)
#scoring
table(matches$goals_scored) 
table(matches$goals_conceded)
#home vs away
tapply(matches$goals_scored, matches$home_away, mean) 
tapply(matches$goals_conceded, matches$home_away, mean)
tapply(matches$goals_scored, matches$home_away, sd)
tapply(matches$goals_conceded, matches$home_away, sd)
#opponent rank
table(matches$opponent_rank) #Ashford finished 9th
#outlier match
matches[matches$goals_scored == 8,] #8-0 home win vs bottom team
mean(matches$goals_scored[matches$goals_scored<8]) #mean without it

# Data contains Ashford United's 46 league matches (23 home, 23 away); rank 9 is absent therefore Ashford finished 9th. 
# Ashford scored a mean of 1.30 goals per game, and conceded 0.83. Home performance was notably stronger; mean goals scored 1.74 at home compared to 0.87 away. Also with 0.57 conceded against 1.09 respectively.
# One outlier match: 8-0 home win against bottom-ranked side, far outside the rest of the scoring distribution and flagged as a possible influential point

# Plots ----
#goals scored vs opponent rank
plot(matches$opponent_rank, jitter(matches$goals_scored, amount = 0.1),
  xlab = "Opponent rank", ylab = "Goals scored",
  main = "Goals scored vs opponent rank", pch = 19,
  col = c("black","red")[as.factor(matches$home_away)])
legend("topleft", legend = c("Away","Home"),
       col = c("black","red"),pch = 19)

cor(matches$opponent_rank, matches$goals_scored) #0.34

#goals conceded vs opponent rank
plot(matches$opponent_rank, jiter(matches$goals_conceded, amount = 0.1),
     xlab = "Opponent rank", ylab = "Goals Conceded",
     main = "Goals conceded vs opponent rank", pch = 19,
     col = c("black","red")[as.factor(matches$home_away)])

legend("topleft", legend = c("Away","Home"),
       col = c("black","red"), pch = 19)

cor(matches$opponent_rank, matches$goals_conceded) #-0.38

# Goals scored vs opponent rank shows a weak positive relationship (r=0.34). Ashford tend to score more against lower-placed teams
# Goals conceded vs opponent rank shows negative relationship (r=-0.38)
# Home matches generally lie above away matches for goals scored 
# As goals are whole numbers, the point sit in row on the plots with many matches with 0. Therefore, a straight line model may not fit perfectly
# The 8-0 win against the bottom teams stands out clearly from the other matches

# Linear Model ----
#goals scored vs opponent rank
fit_scored <- lm(goals_scored ~ opponent_rank, data = matches)
summary(fit_scored)

plot(matches$opponent_rank, jitter(matches$goals_scored, amount = 0.1),
     xlab = "Opponent rank", ylab = "Goals scored",
     main = "Goals scored vs opponent rank", pch = 19)
abline(fit_scored, col = "red", lwd = 2)
#goals conceded vs opponent rank
fit_conceded <- lm(goals_conceded ~ opponent_rank, data = matches)
summary(fit_conceded)

plot(matches$opponent_rank, jitter(matches$goals_conceded, amount = 0.1),
     xlab = "Opponent rank", ylab = "Goals conceded",
     main = "Goals conceded vs opponent rank", pch = 19)
abline(fit_conceded, col = "red", lwd = 2)

# Goals scored model: Expected(goals scored)=0.443+0.068*opponent rank. Each place lower an opponent is, Ashford score 0.068 more goals on average, p=0.02 significant.
# Expected goals vs top team: about 0.51, vs bottom team: about 2.08
# Opponent needs to be 15 places lower in order for Ashford to score 1 more goal
# R-squared=0.12
# Goals conceded model: Expected(goals conceded)=1.359-0.042*opponent rank. Each place lower an opponent is, Ashford concede 0.042 fewer goals on avergage, p=0.01 significant
# Expected goals conceded vs top team: about 1.32, vs bottom team: about 0.35
# R-squared=0.14
# Intercept has no real meaning in either model as rank 0 does not exist

#Home/away and interaction ----
#goals scored
full_scored <- lm(goals_scored ~ opponent_rank * home_away, data = matches)
summary(full_scored)
anova(full_scored)

add_scored <- update(full_scored, . ~ . - opponent_rank:home_away)
summary(add_scored)

ranks <- 1:24
new_away <- data.frame(opponent_rank = ranks, home_away = "Away")
new_home <- data.frame(opponent_rank = ranks, home_away = "Home")

ci_away <- predict(add_scored, newdata = new_away, interval = "confidence")
ci_home <- predict(add_scored, newdata = new_home, interval = "confidence")
pi_away <- predict(add_scored, newdata = new_away, interval = "prediction")
pi_home <- predict(add_scored, newdata = new_home, interval = "prediction")

plot(matches$opponent_rank, jitter(matches$goals_scored, amount = 0.1),
     xlab = "Opponent rank", ylab = "Goals scored",
     main = "Goals scored: additive model", pch = 19, ylim = c(-3,9),
     col = c("black", "red")[as.factor(matches$home_away)])

matlines(ranks, ci_away, lty = c(1,2,2), col = "black")
matlines(ranks, ci_home, lty=c(1,2,2), col = "red")
matlines(ranks, pi_away[, 2:3], lty = 3, col = "black")
matlines(ranks, pi_home[, 2:3], lty = 3, col = "red")
abline(h = 0, col = "grey")

legend("topleft", legend = c("Away", "Home"), col = c("black", "red"), pch = 19)

#goals conceded
full_conceded <- lm(goals_conceded ~ opponent_rank * home_away, data = matches)
summary(full_conceded)
anova(full_conceded)

add_conceded <- update(full_conceded, . ~ . - opponent_rank:home_away)
summary(add_conceded)

ci_away <- predict(add_conceded, newdata = new_away, interval = "confidence")
ci_home <- predict(add_conceded, newdata = new_home, interval = "confidence")
pi_away <- predict(add_conceded, newdata = new_away, interval = "prediction")
pi_home <- predict(add_conceded, newdata = new_home, interval = "prediction")

plot(matches$opponent_rank, jitter(matches$goals_conceded, amount = 0.1),
     xlab = "Opponent rank", ylab = "Goals conceded",
     main = "Goals conceded: additive model", pch = 19, ylim = c(-2,4),
     col = c("black", "red")[as.factor(matches$home_away)])

matlines(ranks, ci_away, lty = c(1,2,2), col = "black")
matlines(ranks, ci_home, lty=c(1,2,2), col = "red")
matlines(ranks, pi_away[, 2:3], lty = 3, col = "black")
matlines(ranks, pi_home[, 2:3], lty = 3, col = "red")
abline(h = 0, col = "grey")

legend("topright", legend = c("Away", "Home"), col = c("black", "red"), pch = 19)

# Interaction between opponent rank and home/away not significant for goals scored (p=0.188), so dropped
# Additive model: Away: Expected(goals scored) = 0.008 + 0.068 x opponent rank
#                 Home: Expected(goals scored) = 0.878 + 0.068 x opponent rank
# Ashford score 0.87 more goals at home than away against the same opponent (p=0.026, significant)
# Opponent rank still significant (p=0.015), same effect at home and away (parallel lines)
# R-squared increased from 0.12 to 0.21 after adding home/away
# Only the 8-0 match lies outside the prediction intervals
# Prediction intervals go below 0, which is impossible for goals, so the linear model is not ideal for count data
# Interaction not significant for goals conceded either (p=0.186), so dropped
# Additive model: Away: Expected(goals conceded) = 1.620 - 0.042 x opponent rank
#                 Home: Expected(goals conceded) = 1.098 - 0.042 x opponent rank
# Ashford concede 0.52 fewer goals at home (p=0.016), opponent rank still significant (p=0.007)
# R-squared increased from 0.14 to 0.25 after adding home/away
# Prediction intervals for goals conceded also go below 0
# p-values should be treated with caution until model assumptions are checked (step 5)

# Diagnostics ----
#goals scored
plot(add_scored$fitted, rstudent(add_scored),
     xlab = "Fitted values", ylab = "Studentised residuals",
     main = "Residuals vs fitted: goals scored")
abline(h = 0, lwd = 2)
which_large <- (1:nrow(matches))[abs(rstudent(add_scored)) > 3]
text(add_scored$fitted[which_large], rstudent(add_scored)[which_large],
     labels = which_large, pos = 4)

qqnorm(rstudent(add_scored), main = NULL)
qqline(rstudent(add_scored), lwd = 2)

plot(add_scored, 4)
abline(h = 8/(46 - 2*3), lty = 2)

add_scored_no8 <- update(add_scored, data = matches[-8, ])
summary(add_scored_no8)

#goals conceded
plot(add_conceded$fitted, rstudent(add_conceded),
     xlab = "Fitted values", ylab = "Studentised residuals",
     main = "Residuals vs fitted: goals conceded")
abline(h = 0, lwd = 2)
which_large <- (1:nrow(matches))[abs(rstudent(add_conceded)) > 3]
text(add_conceded$fitted[which_large], rstudent(add_conceded)[which_large],
     labels = which_large, pos = 4)

qqnorm(rstudent(add_conceded), main = NULL)
qqline(rstudent(add_conceded), lwd = 2)

plot(add_conceded, 4)
abline(h = 8/(46 - 2*3), lty = 2)


# Goals scored diagnostics:
# Residuals vs fitted show diagonal stripes as goals are whole numbers, not random scatter
# Negative residuals squashed at low fitted values since goals can't go below 0
# Match 8 (8-0 home win vs bottom team) has studentised residual of 6.2, far beyond 3
# QQ plot: points roughly follow the line but in steps, match 8 far above the line
# Cook's distance threshold = 8/(46-2*3) = 0.2, match 8 has Cook's distance 0.76, highly influential
# Without match 8, opponent rank effect falls from 0.068 to 0.038 and is no longer significant (p=0.071)
# Home advantage still significant without match 8 (0.60 goals, p=0.037)
# So the opponent rank conclusion depends on one match, home advantage is more reliable
# Box-Cox can't be used as goals scored contains zeros (response must be positive)
# Goals conceded diagnostics:
# Same diagonal stripes in residuals vs fitted, but no residuals beyond 3
# QQ plot follows the line reasonably well
# No Cook's distances above 0.2 (largest 0.16, match 29)
# Goals conceded model reasonably acceptable apart from discreteness
# Overall: linear model is a useful first look but goals are counts, Poisson regression (week 6) more appropriate
# Box-Cox can't be used for either model as both responses contain zeros (response must be positive)
