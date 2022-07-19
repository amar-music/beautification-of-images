#### PLOTS ####

# Psychometric functions --------------------------------------------------

## Plot alpha data ----
ggplot(dfa, aes(x = alpha, y = acc)) +
  geom_line() +
  geom_point() + 
  geom_errorbar(aes(x = alpha, ymin = lower_ci, ymax = upper_ci), width=0.0025) + 
  geom_hline(yintercept=c(0.5, 1)) +
  ylab("Prop. of correct responses") +
  xlab("Alpha") +
  theme_apa()


## Psychometric functions ----
ggplot(dfa, aes(x=abs(alpha), y=acc)) +
  geom_point(aes(col=as.factor(positive)), key_glyph = 'point') +
  geom_line(data = qps_fit.n$curves, aes(x = x, y = y), size=1, col='#F8766D') +
  geom_line(data = qps_fit.p$curves, aes(x = x, y = y), size=1, col='#00BFC4') +
  geom_errorbar(aes(x = abs(alpha), ymin = lower_ci, ymax = upper_ci, color=as.factor(positive)), width=0, lwd=0.25) + 
  annotate('text', x=0.005, y=1, label='(b)', fontface=2) +
  scale_color_manual(labels=c(expression(paste('negative ', alpha)), expression(paste('positive ', alpha))), values=c('#F8766D', '#00BFC4')) +
  ylim(c(.5, 1)) +
  labs(
    x = expression(bold(paste('|', alpha, '|-value'))),
    y = "Proportion Agreement with GANalyze"
  ) +
  theme(
    strip.text.x = element_text(size=12, color='black', face='bold', margin=margin(b=8)),
    strip.background = element_rect(fill=NA),
    axis.title.x = element_text(size=12, color='black', face='bold', margin=margin(t=8)),
    axis.title.y = element_text(size=12, color='black', face='bold', margin=margin(r=8)),
    axis.ticks = element_line(color='black'),
    panel.border = element_rect(color='black', fill=NA),
    panel.background = element_rect(fill=NA),
    legend.title = element_blank(),
    legend.position = c(0.80, 0.15), 
    legend.spacing.x = unit(0.01, 'cm'),
    legend.text = element_text(size=8),
    legend.key.size = unit(0.4, 'cm'),
    legend.key = element_rect(fill='NA'),
    axis.text=element_text(size=9, color='black')
  )
ggsave(
  'alpha_correct.pdf',
  plot = last_plot(),
  device = 'pdf',
  path = '../Paper/images/results',
  scale = 1,
  width = 10,
  height = 9.5,
  units = "cm",
  dpi = 300,
  limitsize = TRUE,
  bg = NULL
)


## Asymmetrical model ----
ggplot(dfa.cont, aes(x=alpha, y=acc)) +
  geom_errorbar(data=dfa.cont, aes(x = alpha, ymin = lower_ci, ymax = upper_ci), width=0, lwd=0.25) + 
  geom_point(data = dfa.cont, aes(x=alpha, y=acc, col=as.factor(positive))) +
  geom_line(data = qps_fit.asym$curves, aes(x = x, y = y, col=positive), size=1, key_glyph = 'point') +
  annotate('text', x=-0.25, y=1, label='(a)', fontface=2) +
  scale_color_manual(labels=c(expression(paste('negative ', alpha)), expression(paste('positive ', alpha))), values=c('#F8766D', '#00BFC4')) +
  ylim(c(0, 1)) +
  labs(
    x = expression(bold(paste(alpha, '-value'))),
    y = "Proportion Choosing Non-Neutral Image"
  ) +
  geom_vline(xintercept=0, linetype=2) +
  theme(
    strip.text.x = element_text(size=12, color='black', face='bold', margin=margin(b=8)),
    strip.background = element_rect(fill=NA),
    axis.title.x = element_text(size=12, color='black', face='bold', margin=margin(t=8)),
    axis.title.y = element_text(size=12, color='black', face='bold', margin=margin(r=8)),
    axis.ticks = element_line(color='black'),
    panel.border = element_rect(color='black', fill=NA),
    panel.background = element_rect(fill=NA),
    legend.title = element_blank(),
    legend.position = c(0.80, 0.15), 
    legend.spacing.x = unit(0.01, 'cm'),
    legend.text = element_text(size=8),
    legend.key.size = unit(0.4, 'cm'),
    legend.key = element_rect(fill='NA'),
    axis.text=element_text(size=9, color='black')
  )
ggsave(
  'cont_asym.pdf',
  plot = last_plot(),
  device = 'pdf',
  path = '../Paper/images/results',
  scale = 1,
  width = 10,
  height = 9.5,
  units = "cm",
  dpi = 300,
  limitsize = TRUE,
  bg = NULL
)

## Symmetrical model ----
ggplot(df.cont, aes(x=alpha, y=cor)) +
  geom_point(data = dfa.cont, aes(x=alpha, y=acc)) +
  geom_line(data = qps_fit.sym$curves, aes(x = x, y = y)) +
  ylim(c(0, 1)) +
  labs(
    x = "Difference Between Stimuli",
    y = "Proportion Agreement with GANalyze"
  ) +
  geom_vline(xintercept=0, linetype=2) +
  theme_apa()




## Polynomial regression ----
ggplot(dfa, aes(abs(alpha), acc, group=positive, col=as.factor(positive))) +
  geom_point() +
  stat_smooth(method = lm, formula = y ~ poly(x, 2), key_glyph = 'point') +
  annotate('text', x=0.005, y=1, label='(b)', fontface=2) +
  scale_color_manual(labels=c(expression(paste('negative ', alpha)), expression(paste('positive ', alpha))), values=c('#F8766D', '#00BFC4')) +
  ylim(c(.5, 1)) +
  labs(
    x = expression(bold(paste('|', alpha, '|-value'))),
    y = "Proportion Agreement with GANalyze"
  ) +
  theme(
    strip.text.x = element_text(size=12, color='black', face='bold', margin=margin(b=8)),
    strip.background = element_rect(fill=NA),
    axis.title.x = element_text(size=12, color='black', face='bold', margin=margin(t=8)),
    axis.title.y = element_text(size=12, color='black', face='bold', margin=margin(r=8)),
    axis.ticks = element_line(color='black'),
    panel.border = element_rect(color='black', fill=NA),
    panel.background = element_rect(fill=NA),
    legend.title = element_blank(),
    legend.position = c(0.80, 0.15), 
    legend.spacing.x = unit(0.01, 'cm'),
    legend.text = element_text(size=8),
    legend.key.size = unit(0.4, 'cm'),
    legend.key = element_rect(fill='NA'),
    axis.text=element_text(size=9, color='black')
  )
ggsave(
  'poly_regression.pdf',
  plot = last_plot(),
  device = 'pdf',
  path = '../Paper/images/results',
  scale = 1,
  width = 10,
  height = 9.5,
  units = "cm",
  dpi = 300,
  limitsize = TRUE,
  bg = NULL
)







## Mean RT over trials ----
ggplot(df[df$rt < 15000,], aes(x = trial, y = rt)) +
  geom_point(aes(color = sub)) +
  xlab("Trial number") + ylab("Mean RT (s)") +
  theme_apa() +
  theme(legend.position = "none")

## Time elapsed ----
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

