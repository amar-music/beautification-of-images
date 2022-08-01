#### Statistics ####

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



# Planned contrasts -------------------------------------------------------
aov1 <- aov(acc ~ alpha, dfan)
Anova(aov1, type="III")
plot(dfa$acc ~ abs(dfa$alpha))

model <- lm(acc ~ abs(alpha), data = dfa)

ggplot(dfa, aes(abs(alpha), acc, group=positive, col=as.factor(positive))) +
  geom_point() +
  stat_smooth(method="lm", formula = y ~ poly(x, 2, raw=TRUE))

poly_reg <- lm(acc ~ poly(abs(alpha), 2, raw = TRUE) + positive, data = dfa)
summary(poly_reg)

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




