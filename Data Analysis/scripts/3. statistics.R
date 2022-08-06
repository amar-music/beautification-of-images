#### Statistics ####
library("apaTables")
# IDEA
# Split the dataframe in positive and negative alphas
# Take abs() of negative df
# Significant difference between negative and positive alphas?


# Quickpsy fit ------------------------------------------------------------
qps_fit.p <- quickpsy(dfap, alpha, nCor, count, guess=TRUE, lapses = TRUE)
qps_fit.n <- quickpsy(dfan, alpha, nCor, count, guess=TRUE, lapses = TRUE)

qps_fit <- quickpsy(df.cont, alpha, cor, grouping=('positive'), guess=TRUE, lapses=TRUE)

## Fit parameters ----
qps_fit.p$par    
qps_fit.n$par

plotthresholds(qps_fit.sym)
plotthresholds(qps_fit.asym)

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

plot(colMeans(mse), type='h', lwd=10)

ggplot(dfa.short, aes(x=alpha, y=acc, col=positive)) +
  geom_point()


##

barplot(by(data=df$cor, INDICES = df$age, FUN = mean))


ggplot(dfa.long, aes(label_2, acc, group=label_2, col=as.factor(label_2))) +
  geom_point() +
  stat_smooth(method="lm", formula = y ~ x)




qps_fit1 <- quickpsy(df.cont, alpha, cor, grouping=.('age'), guess=TRUE, lapses=TRUE, bootstrap='none')

ggplot(df.cont, aes(x=alpha, y=cor, group=age, col=age)) +
  facet_wrap(. ~ age) +
  #geom_point(data = dfa.cont, aes(x=alpha, y=acc)) +
  geom_line(data = qps_fit1$curves, aes(x = x, y = y)) +
  ylim(c(0, 1)) +
  labs(
    x = "Difference Between Stimuli",
    y = "Proportion Agreement with GANalyze"
  ) +
  geom_vline(xintercept=0, linetype=2) +
  theme_apa()


qps_fit2 <- quickpsy(df.cont, alpha, cor, grouping=.('label_2'), guess=TRUE, lapses=TRUE, bootstrap='none')

ggplot(df.cont, aes(x=alpha, y=cor, group=label_2, col=label_2)) +
  #facet_wrap(. ~ nationality) +
  #geom_point(data = dfa.cont, aes(x=alpha, y=acc)) +
  geom_line(data = qps_fit2$curves, aes(x = x, y = y)) +
  ylim(c(0, 1)) +
  labs(
    x = "Difference Between Stimuli",
    y = "Proportion Agreement with GANalyze"
  ) +
  geom_vline(xintercept=0, linetype=2) +
  theme_apa()

mean(df.cont$aeq_total)
hist(df.cont$aeq_total)



# Confounding factors -----------------------------------------------------
df.anova <- df.cont %>% 
  group_by(label_2, alpha, c_key, trial) %>%  
  summarize(acc = mean(cor))


aov_test <- aov(acc ~ label_2 + c_key + trial, df.anova)
summary(aov_test)




ggplot(df.anova, aes(label_2)) +
  geom_histogram()


ggplot(df.anova, aes(label_2, acc)) +
  geom_point() +
  ylim(0, 1)




