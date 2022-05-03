#### PLOTS ####

# Psychometric functions --------------------------------------------------

## Plot for each participant ----
# ggplot(diff_correct.indiv, aes(diff, prop, group=sub)) +
#   geom_point(size=2) + geom_line(size=1) + geom_hline(yintercept=c(0.5, 1)) +
#   facet_wrap(~sub) + 
#   ylab("Prop. of correct responses") +
#   theme_apa() +
#   theme(legend.position = "none")
  

## Total plot
ggplot(diff_correct.total, aes(diff, acc)) +
  geom_line(size=1.5) +
  geom_point(size=3) + 
  geom_hline(yintercept=c(0.5, 1)) +
  ylab("Prop. of correct responses") +
  theme_apa()

ggplot(alpha_correct.total, aes(alpha, acc)) +
  geom_line(size=1.5) +
  geom_point(size=3) + 
  geom_hline(yintercept=c(0.5, 1)) +
  ylab("Prop. of correct responses") +
  theme_apa()

plot()
#





# Evaluate participants ---------------------------------------------------
## Plot indiviual L/R responses
par(mfrow = c(2, 3))
for (subject in subs) {
  plot((jitter(u_key, 0.25)) ~ trial, data = df[df$sub == subject, ])
  title(paste(subject, "- left/right"))
}

## Plot correct responses over trials
par(mfrow = c(2, 3))
for (subject in subs) {
  plot((jitter(cor, 0.25)) ~ trial, data = df[df$sub == subject, ])
  title(paste(subject, "- correct/incorrect"))
}

## Mean RT over trials
ggplot(df[df$rt < 15000,], aes(x = trial, y = rt)) +
  geom_point(aes(color = sub)) +
  xlab("Trial number") + ylab("Mean RT (s)") +
  theme_apa() +
  theme(legend.position = "none")

## Time elapsed
ggplot(df, aes(x = trial, y = time_elapsed)) +
  geom_point(aes(color = sub)) +
  xlab("Trial number") + ylab("Mean RT (s)") +
  theme_apa() +
  theme(legend.position = "none")


