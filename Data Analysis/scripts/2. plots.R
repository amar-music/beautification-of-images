#### PLOTS ####

# Behavioral functions --------------------------------------------------

## Plot alpha data ----
ggplot(dfa, aes(x = alpha, y = acc)) +
  geom_line() +
  geom_point() + 
  geom_errorbar(aes(x = alpha, ymin = lower_ci, ymax = upper_ci), width=0.0025) + 
  geom_hline(yintercept=c(0.5, 1)) +
  ylab("Prop. of correct responses") +
  xlab("Alpha") +
  theme_apa()


## Psychometric function ----
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
    strip.text.x = element_text(size=12, color='#414141', face='bold', margin=margin(b=8)),
    strip.background = element_rect(fill=NA),
    axis.title.x = element_text(size=12, color='#414141', face='bold', margin=margin(t=8)),
    axis.title.y = element_text(size=12, color='#414141', face='bold', margin=margin(r=8)),
    axis.ticks = element_line(color='#414141'),
    panel.border = element_rect(color='#414141', fill=NA),
    panel.background = element_rect(fill=NA),
    legend.title = element_blank(),
    legend.position = c(0.80, 0.15), 
    legend.spacing.x = unit(0.01, 'cm'),
    legend.text = element_text(size=8),
    legend.key.size = unit(0.4, 'cm'),
    legend.key = element_rect(fill='NA'),
    axis.text=element_text(size=9, color='#414141')
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
    strip.text.x = element_text(size=12, color='#414141', face='bold', margin=margin(b=8)),
    strip.background = element_rect(fill=NA),
    axis.title.x = element_text(size=12, color='#414141', face='bold', margin=margin(t=8)),
    axis.title.y = element_text(size=12, color='#414141', face='bold', margin=margin(r=8)),
    axis.ticks = element_line(color='#414141'),
    panel.border = element_rect(color='#414141', fill=NA),
    panel.background = element_rect(fill=NA),
    legend.title = element_blank(),
    legend.position = c(0.80, 0.15), 
    legend.spacing.x = unit(0.01, 'cm'),
    legend.text = element_text(size=8),
    legend.key.size = unit(0.4, 'cm'),
    legend.key = element_rect(fill='NA'),
    axis.text=element_text(size=9, color='#414141')
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





## Image features ----
feature_names <- c(
  'brightness' = "Brightness",
  'contrast' = "Contrast",
  'edges' = "Edges",
  'saturation' = "Saturation"
)

ggplot(df4a, aes(x=alpha, y = score, group = feature, color = feature)) + 
  geom_smooth(lwd=1, method='gam') +
  ylim(c(-2.5, 2.5)) +
  facet_grid(~feature, labeller = as_labeller(feature_names)) +
  labs(
    x = expression(bold(paste(alpha, '-value'))),
    y = "Standardized Score"
  ) +
  theme(
    strip.text.x = element_text(size=12, color='#414141', face='bold', margin=margin(b=8)),
    strip.background = element_rect(fill=NA),
    axis.title.x = element_text(size=12, color='#414141', face='bold', margin=margin(t=8)),
    axis.title.y = element_text(size=12, color='#414141', face='bold', margin=margin(r=8)),
    axis.ticks = element_line(color='#414141'),
    panel.border = element_rect(color='#414141', fill=NA),
    panel.background = element_rect(fill=NA),
    legend.position = "none",
    axis.text=element_text(size=9, color='#414141')
  )
ggsave(
  'img_features.pdf',
  plot = last_plot(),
  device = 'pdf',
  path = '../Paper/images/results',
  scale = 1,
  width = 16,
  height = 7,
  units = "cm",
  dpi = 300,
  limitsize = TRUE,
  bg = NULL
)



## Color distributions ----
ggplot(tbl, aes(x=value, col=key, group_by=alpha)) +
  #geom_histogram(position="identity", alpha = 0.8,  bins=512) +
  geom_freqpoly(stat="bin", position="identity", bins=1024, alpha = 0.6) +
  geom_freqpoly(aes(group=key),col="black", bins=257, lwd=0.2) +
  #scale_color_manual(values=c(red="#ff0000", green="#00ff00", blue="#0000ff")) +
  xlim(0, 256) +
  labs(
    x = "Pixel Intensity",
    y = "Count"
  ) +
  facet_grid(cat~alpha, labeller = labeller(cat = names(c("fish", "hen", "swan")))) +
  theme(
    strip.text.x = element_text(size=12, color='#414141', face='bold', margin=margin(b=8)),
    strip.text.y = element_blank(),
    strip.background = element_rect(fill=NA),
    axis.title.x = element_text(size=12, color='#414141', face='bold', margin=margin(t=8)),
    axis.title.y = element_blank(),
    axis.ticks = element_line(color='#414141'),
    axis.ticks.y = element_blank(),
    axis.text.y = element_blank(),
    panel.border = element_rect(color='black', fill=NA),
    panel.background = element_rect(fill=NA),
    panel.spacing.y = unit(1, "lines"),
    legend.title = element_blank(),
    legend.position = "none",
    axis.text=element_text(size=9, color='#414141')
  )
ggsave(
  'col_dists.pdf',
  plot = last_plot(),
  device = 'pdf',
  path = '../Paper/images/results',
  scale = 1,
  width = 18,
  height = 10,
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

