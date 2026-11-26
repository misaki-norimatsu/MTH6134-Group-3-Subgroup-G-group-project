#Logistic GLM with player characteristics
injuryglm=glm(injured~age+min900+trnload+prvinj+bmi+pst,data=data,family = binomial("logit"))
#Logistic GLMM with random intercept for team
injuryglmm=glmer(injured~age+min900+trnload+prvinj+bmi+pst+(1|team),data=data,family=binomial(link = "logit"))
summary(injuryglm)
#The binomial logistic regression identified age, training load and previous injuries as statistically significant predictors of inury
#at the 5% significance level. Previous injuries exhibited the strongest statistical evidence of an association,
#with each additional previous injury associated with a 66.1% increase in injury odds.Minutes played, BMI and individual
#playing position coefficients were insignicant after controlling for the other explanatory variables.Minutes played was rescaled by dividing
#by 900, rperesenting the equivalent of ten full matches.This allows the coefficient to be interpreted in more meaniningful units of playingtime
summary(injuryglmm)
#A binomial glmm was fitted by including team as a randoom intercept to account for potential within-team similarities in injury risk.
#However, the estimated random-effect variance was approximately 0, indicating negligible additional variation between teams. Furtehrmore,
#the glmm produced a higher AIC of 556.22 compared with 554.22 for the glm. Therefore, the simpler GLM was preffered, wiht ag, training load 
#and previous injuries remaining statistically signinficant predictors of injury.


#Comparer fit and inspect the estimated varation between teams
AIC(injuryglm,injuryglmm)
#The GLM is preffered due to its lower AIC of 554.224 compared with 556.224 for the GLMM.Although teh difference is relatively small, the negligible random-effect
#variance provides further justification for retaining the simpler GLM for subsequent injury analysis
VarCorr(injuryglmm)
#The estimated standard deviarion of the team random effect was approximately 0
#suggesting that team membership contributes negligible additional variation in injury
#odds.This further supports retaining the simpler GLM,consistent with the AIC comparison
isSingular(injuryglmm)
#The singularity test returned true, indicating that the inclusion as a random effect provides negligible additional explanatory value.
#Combined with the higher AIC of the GLMM, this further supports selecting the simple GLM as the preffered model



#Adjusted odds ratios with approximate 95% confidence intervals 
estimates=coef(summary(injuryglmm))
round(exp(cbind(OR = estimates[, "Estimate"],
lower_95 = estimates[, "Estimate"] - 1.96 * estimates[, "Std. Error"],
upper_95 = estimates[, "Estimate"] + 1.96 * estimates[, "Std. Error"])), 2)
#The confidence intervals for age, traing load and previous injuries exclude 1, confirming their statistical signficance at the 5% level. Previous 
#injuries provide the strongest statistical evidence of an association with injury, with each additional previous injury associated with a 66% increase in injuey odds.
#Although goalkeepers have higher estimated injury odds than defenders, the wide confidence interval indicates considerable uncertainty.

profiles <- data.frame(age = c(25, 25),min900 = c(2700, 2700) / 900, trnload = c(3, 3),prvinj = c(0, 2),  bmi = c(23.5, 23.5),pst = factor(c("DEF", "DEF"), levels = levels(data$pst)))




profiles$probability <- predict(injuryglm,newdata = profiles,type = "response", re.form=NA)

profiles[,c("prvinj","probability")]
injuryglm$xlevels$pst
profiles$pst <- factor(
  rep(injuryglm$xlevels$pst[1], 2),
  levels = injuryglm$xlevels$pst
)

profiles$probability <- predict(injuryglm,
                                newdata = profiles,
                                type = "response")

profiles[, c("prvinj", "probability")]

#The predicted probability of injury increased from 9.00% to 21.43% when pevious injuries
#increased from 0 to 2, holding all other explanatory variables constant, this demonstrates the imoprtance of injury history as a predictor
#of injury risk among football players.