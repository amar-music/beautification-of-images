#### SCRAP ####


# Plots -------------------------------------------------------------------

## Plot for each participant ----
ggplot(diff_correct.indiv, aes(diff, prop, group=sub)) +
  geom_point(size=2) + geom_line(size=1) + geom_hline(yintercept=c(0.5, 1)) +
  facet_wrap(~sub) +
  ylab("Prop. of correct responses") +
  theme_apa() +
  theme(legend.position = "none")


## Total plot ----
ggplot(dfd, aes(x = diff, y = acc)) +
  geom_line() +
  geom_point() +
  geom_errorbar(aes(x = diff, ymin = lower_ci, ymax = upper_ci), width=0.0025) +
  geom_hline(yintercept=c(0.5, 1)) +
  ylab("Prop. of correct responses") +
  xlab("Difference") +
  theme_apa()


## GLM fits ----
ggplot(df, aes(alpha, cor, group=positive, col=positive)) +
  #geom_point(alpha = 0.2) +
  geom_smooth(method = "glm", method.args = list(family = binomial(link = "logit"))) +
  labs(
    title = "Logistic Regression for All Alphas",
    x = "Alpha",
    y = "Proportion Agreement with GANalyze"
  ) +
  theme_apa()


## Continuous GLM fit ----
ggplot(df.cont, aes(alpha, cor)) +
  #geom_point(alpha = 0.2) +
  geom_smooth(method = "glm", method.args = list(family = binomial(link = "logit"))) +
  labs(
    title = "Logistic Regression for All Alphas",
    x = "Alpha",
    y = "Proportion Agreement with GANalyze"
  ) +
  theme_apa()
