#### STRUCTURAL EQUATIONS ####

## Summary statistics ----
aeq <- df3 %>% select(
  aeq_total,
  cor,
  #ntrials,
  age
  #nationality,
  #sex
)

descriptive_aeq <- as.data.frame(psych::describe(aeq))

descriptive_aeq <- dplyr::select(descriptive_aeq, 
                                 n,
                                 mean,
                                 sd,
                                 median,
                                 min,
                                 max,
                                 skew,
                                 kurtosis)

descriptive_aeq


aeq_cov <- cov(aeq, use='pairwise.complete.obs')
aeq_cor <- cov2cor(aeq_cov)

corrplot::corrplot(aeq_cor, 
                   is.corr = FALSE,       # whether is a correlation matrix 
                   method = "circle",     # magnitude of covariances as circles 
                   type = "upper",        # remove the bottom of the covariance matrix
                   addCoef.col = "black"  # add to the plot the coefficients
)



model_aeq_mediation <-'
## Judgement
judgement =~ cor

## AEQ
aeq =~ aeq_total

## Direct effect
judgement ~ c*age

## Mediator
aeq ~ a*age
judgement ~ b*aeq

## Indirect effect
ab := a*b

## Total effect
total := c + (a*b)
'

fit_aeq_mediation <- cfa(model_aeq_mediation, data=df3)

summary(fit_aeq_mimic, standardized=TRUE)



model_aeq <- '
aesthetic_experience =~ emotional + cultural + perceptual + understanding + `flow-proximal` + `flow-experience`
'

fit_model_aeq <- cfa(model_aeq, data=df3)

summary(fit_model_aeq)




# MIMIC MODEL -------------------------------------------------------------
aeq_mimic <- '
## Latent variable
aesthetic_experience =~ emotional + cultural + perceptual + understanding + `flow-proximal` + `flow-experience`

## Add effect on aes experience
aesthetic_experience ~ sex + age + nationality

## Add effects on judgement
cor ~ aesthetic_experience + sex + age + nationality
'


fit_aeq_mimic <- cfa(aeq_mimic, data=df3.2)
graph_sem(fit_aeq_mimic, standardized=TRUE)
summary(fit_aeq_mimic, standardized=TRUE)


res <- model.matrix(~nationality, data=df3)
head(res[, -1])
