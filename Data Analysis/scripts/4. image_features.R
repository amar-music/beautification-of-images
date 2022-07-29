#### Image Features ####


ggplot(df4a, aes(x=alpha, y = score, group = feature, color = feature)) + 
  geom_line(lwd=1) +
  ylim(c(-2.5, 2.5)) +
  facet_grid(~feature) +
  labs(
    x = expression(bold(paste(alpha, '-value'))),
    y = "Standardized Score"
  ) +
  theme(
    strip.text.x = element_text(size=12, color='black', face='bold', margin=margin(b=8)),
    strip.background = element_rect(fill=NA),
    axis.title.x = element_text(size=12, color='black', face='bold', margin=margin(t=8)),
    axis.title.y = element_text(size=12, color='black', face='bold', margin=margin(r=8)),
    axis.ticks = element_line(color='black'),
    panel.border = element_rect(color='black', fill=NA),
    panel.background = element_rect(fill=NA),
    legend.position = "none",
    axis.text=element_text(size=9, color='black')
  )
