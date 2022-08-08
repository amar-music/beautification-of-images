#### Statistics ####
library("apaTables")



# Quickpsy fit ------------------------------------------------------------

## Split positive and negative ----
qps_fit.p <- quickpsy(dfap, alpha, nCor, count, guess=TRUE, lapses = TRUE)
qps_fit.n <- quickpsy(dfan, alpha, nCor, count, guess=TRUE, lapses = TRUE)

## Combined with grouping ----
qps_fit <- quickpsy(df.cont, alpha, cor, grouping=('positive'), guess=TRUE, lapses=TRUE)

## Fit parameters ----
qps_fit.p$par    
qps_fit.n$par

## Thresholds ----
plotthresholds(qps_fit.sym)
plotthresholds(qps_fit.asym)

## Quick visual inspection of fit ----
plot(qps_fit.sym)
plot(qps_fit.asym)

qps_fit.sym$aic
qps_fit.asym$aic

qps_fit.sym$thresholds
qps_fit.asym$thresholds

qps_fit.sym$par # p1= position, p2= scale/slope, p3= guess, p4= lapse
qps_fit.asym$par

qps_fit.asym$parcomparisons




# Polynomial regression ---------------------------------------------------

## Simplified dataframe ----
dfa.short <- dfa %>%
  subset(select = c("acc", "alpha", "positive")) %>%
  mutate_at('positive', as.factor) %>%
  mutate(alpha = abs(alpha))


## Quadratic regression ----
quad_reg <- lm(acc ~ poly(alpha, 2, raw = TRUE) + positive, data = dfa.short)
summary(quad_reg)

## Cubic regression ----
cubic_reg <- lm(acc ~ poly(alpha, 3, raw = TRUE) + positive, data = dfa.short)
summary(cubic_reg)

## Model comparison ----
anova(quad_reg, cubic_reg)

## APA tables ----
apa.reg.table(quad_reg)
apa.reg.table(cubic_reg)


## Find optimal polynomial number (cross-validation) ----
df.shuffled <- dfa.short[sample(nrow(dfa.short)),]
k_folds <- 10
p_degrees <- 5

folds <- cut(seq(1,nrow(df.shuffled)),breaks=k_folds,labels=FALSE)
mse = matrix(data=NA,nrow=k_folds,ncol=p_degrees)


for(i in 1:k_folds){
  
  #define training and testing data
  testIndexes <- which(folds==i,arr.ind=TRUE)
  testData <- df.shuffled[testIndexes, ]
  trainData <- df.shuffled[-testIndexes, ]
  
  #use k-fold cv to evaluate models
  for (j in 1:p_degrees){
    fit.train = lm(acc ~ poly(abs(alpha), j) + positive, data=trainData)
    fit.test = predict(fit.train, newdata=testData)
    mse[i,j] = mean((fit.test-testData$acc)^2) 
  }
}

colMeans(mse)





# Confounding factors -----------------------------------------------------
df.anova <- df.cont %>% 
  group_by(label_2, alpha, c_key) %>%  
  summarize(acc = mean(cor))


aov_test <- aov(acc ~ label_2 + c_key, df.anova)
summary(aov_test)



