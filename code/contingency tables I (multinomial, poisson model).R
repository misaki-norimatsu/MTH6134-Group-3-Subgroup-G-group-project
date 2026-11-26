source("code/load_data.R")

library(nnet)
library(MASS)

attach(players)
#For contingency tables, they must be in contingency tables related to each other.
#2 way interaction:
table(position, injured)
table(team, position)
table(team, injured)

#check independence of relationships between these factors.
chisq.test(table(position, injured))
chisq.test(table(team, position))
chisq.test(table(team, injured))
##There is no significant relationship between injury and position as the p value is high at 0.2233, and between team and position (p=0.11), and team and injury (p=0.31). For tests involving team the approximation may not hold as expected counts below 5.

#Must make a weight as frequency is not in table.
#Minutes played is too large and will cause AIC to keep all terms, and multinomial model to not converge. Choose rescaled to make weight the number of players.
weight<- minutes_played / mean (minutes_played)
pos.mult <-multinom (injured~ team * position, weight= weight)
stepAIC(pos.mult)
#AIC has dropped all terms for final AIC of 605.246, with an intercept only model so this shows team and position are not predictors of injury.

#intercept is -1.44882, so odds of injury is 0.2348, this is without any other factors. 
exp(-1.44882)

###Contingency table analysis found no evidence of relationship between injury and each category position, and team, with stepwise AIC removing both from the multinomial model.


##Matches contingency tables
attach(matches)
table(home_away, goals_scored)
table(home_away, goals_conceded)
table(goals_scored, goals_conceded)

#Model with Poisson for count data.
model_Q<- glm(goals_scored ~ home_away +opponent_rank, family= poisson)
summary(model_Q)
#The model has residual deviance 49.785, and null deviance of 64.844, shows the model is reasonable.
#Both predictors are significant, with opponent rank high significant so both have an effect on goals scored.
exp(0.69315) #Shows odds of  scoring goals are twice that at home than away.
exp(0.05435) #Shows odds of scoring with a higher rank is less than odds of opponents with lower rank.

model_y<- glm(goals_conceded ~ home_away +opponent_rank, family= poisson)
summary(model_y)
#The model has residual deviance 32.797, and null deviance of 41.566, shows the model is reasonable.
<<<<<<< HEAD
#Opponent rank is significant, with opponent rank significant at 0.0293 so has an effect on goals conceded, although home_away is the lowest significance with p-value 0.0558.
=======
#Opponent rank is significant, with opponent rank significant at 0.0293 so has an effect on goals conceded, although home_away is the lowest significance with p-value 0.558.
>>>>>>> 83c5ce26c69ac7918ec1fac504f20f300c8d9cf1
