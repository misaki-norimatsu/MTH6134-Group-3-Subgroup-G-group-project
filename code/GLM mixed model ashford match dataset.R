ashford <- read_excel(file.choose())
View(ashford)
table(ashford$home_away)
win <- ifelse(ashford$goals_scored > ashford$goals_conceded, 1, 0)
ashford$home_away <- as.factor(ashford$home_away)
glm_model <- glm(win ~ home_away + opponent_rank,data = ashford,family = binomial)
summary(glm_model)
#The binomial GLM demonstrates that home advantage is a statistically significant predictor of Ashford's match success
#opponent rank also exhibits a positive relationship with the probability of winning, although it isn't statistically 
#significant at the 5% level. Therefore, there is stronger statistical evidence supporting home advantage asa predictor 
#of match outcomes
library(lme4)
ashford$opponent=factor(ashford$opponent_rank)
glmm_model=glmer(win~home_away+opponent_rank+(1|opponent),data = ashford,family=binomial)
summary(glmm_model)
#A generalised linear mixed model was fitted to account for repeated matches against
#individual opponents.However, the estimated random effect variance was approximately 
#zero, resulting in a singular fit.Furthermore, the GLMM produced a higher AIC of 62.409
#compared with 60.409 for the GLM. Therefore, the simpler binomial GLM was preferred. as including 
#opponent-level random effects didn't improve model fit. Home advantage remianed a statistically
#significant predictor of match success

AIC(glm_model,glmm_model)
#The GLM produced a lower AIC of 60.409 compared with 62.409 for the GLMM. This
#suggests that incorporating opponent-level random effects doen't improve model 
#performance sufficiently to justify the additional complexity. Therefore, the simpler GLM was selected for further 
#analysis.

VarCorr(glmm_model)
isSingular(glmm_model)
ranef(glmm_model)
#The estimated opponent-level random effects were negligible, with a standard deviation of approximately zero
#The singularity test confirmed a singular fit, indicating that the random effect didn't meaningfully improve the model
#Combined with the lower AIC of the GLM, these findings support selecting the simpler binomial GLM for further analysis
#However, with only 2 matches per opponent, the ability to detect opponent-level variation is limited


null_model=glm(win ~ 1,data = ashford,family = binomial)
anova(null_model, glm_model, test = "Chisq")
#The analysis of deviance revealed a significant reduction in residual deviance from 63.683
#to 54.409(p=0.00969).Therefore, including home advantage and opponent rank significantly improves
#model fit compared with the intercept-only model, supporting their inclusion in the analysis



library(ggplot2)
packageVersion("ggplot2")
newdata=expand.grid(home_away = c("Home", "Away"),opponent_rank = sort(unique(ashford$opponent_rank)))

newdata$probability=predict( glm_model,newdata = newdata,type = "response")
ggplot(newdata, aes(x = opponent_rank,y = probability,colour = home_away)) +geom_line(linewidth = 1.2) +labs(title = "Predicted Probability of Winning",x = "Opponent Rank",y = "Probability of Winning",colour = "Match Venue")
#The graph illustrates how home advantage and opponent ranking affect Ashford's predicted probability of winning.
#The predicted probability plot demonstrates a clear home advantage across all opponent rankings.Ashford's predicted probability
#of winning increases against lower-ranked opponents, both at home and away.However, home advantage is the only statistically significant
#predictor at the 5% level, suggesting that much venue provides stronger statistical evidence of an association
#with match success.

 