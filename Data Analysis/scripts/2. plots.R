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
ggplot(diff_correct.total, aes(x = diff, y = acc)) +
  geom_line() +
  geom_point() + 
  geom_errorbar(aes(x = diff, ymin = lower_ci, ymax = upper_ci), width=0.0025) + 
  geom_hline(yintercept=c(0.5, 1)) +
  ylab("Prop. of correct responses") +
  xlab("Difference") +
  theme_apa()


ggplot(alpha_correct.total, aes(x = alpha, y = acc)) +
  geom_line() +
  geom_point() + 
  geom_errorbar(aes(x = alpha, ymin = lower_ci, ymax = upper_ci), width=0.0025) + 
  geom_hline(yintercept=c(0.5, 1)) +
  ylab("Prop. of correct responses") +
  xlab("Alpha") +
  theme_apa()




linear_model <- lm(acc ~ diff, data = diff_correct.indiv)
summary(linear_model)



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




# World Map ---------------------------------------------------------------
ggplot(nationalities) +
  geom_map(aes(map_id = Var1, fill = Freq), map = world_map) +
  geom_polygon(data = world_map, aes(x = long, y = lat, group = group), colour = 'black', fill = NA) +
  expand_limits(x = world_map$long, y = world_map$lat) +
  #scale_fill_viridis(option='magma') +
  scale_fill_distiller(type='seq', direction=1, palette='YlOrRd') +
  theme_void()

