#### Testing plots ####
range01 <- function(x){(x-min(x))/(max(x)-min(x))}

# All used categories
all_c <- sort(unique(df$cat_no))

# Select category number
cn <- all_c

feature_names <- c(
  'brightness' = "Brightness",
  'contrast' = "Contrast",
  'edges' = "Sharpness",
  'saturation' = "Saturation"
)


# df grouped by cat
df.cat <- df.cont[df.cont$cat_no %in% cn,] %>% 
  group_by(alpha, cat_no) %>%  
  summarize(acc = mean(cor),
            sd = sd(cor),
            count = n())

df4.cat <- df4[(df4$cat %in% cn) & (df4$img != 100),] %>% 
  group_by(img, cat) %>%  
  summarize(edges = mean(edge_score),
            contrast = mean(contrast_score),
            brightness = mean(brightness_score),
            saturation = mean(saturation_score)) %>%
  mutate(alpha = 0.0025*img -0.25)

df4.cat_scaled <- df4.cat %>%
  group_by(cat) %>%
  mutate(edges = range01(edges),
         contrast = range01(contrast),
         brightness = range01(brightness),
         saturation = range01(saturation))

#df4.cat$edges <- range01(df4.cat$edges)
#df4.cat$contrast <- range01(df4.cat$contrast)
#df4.cat$brightness <- range01(df4.cat$brightness)
#df4.cat$saturation <- range01(df4.cat$saturation)

#df4.cat <- gather(df4.cat, key=feature, value = score, 
#               c("contrast", "edges", "brightness", "saturation"))

names(df4.cat_scaled)[2] <- 'cat_no'

df.cat_total <- merge(df.cat, df4.cat_scaled, by=c('alpha', 'cat_no'))

df.cat_total_gather <- gather(df.cat_total, key=feature, value = score, 
               c("contrast", "edges", "brightness", "saturation"))


scaleFactor <- max(df.cat_total_gather$acc) / max(df.cat_total_gather$score)



ggplot(df.cat_total_gather, aes(x=alpha, group=feature, col=feature)) +
  geom_line(aes(y=acc), size=1.5, key_glyph = 'point', col='black') +
  geom_line(aes(y=score * scaleFactor)) +
  scale_y_continuous("Features", sec.axis = sec_axis(~./scaleFactor, name='Features')) +
  geom_text(aes(label = cat_no), x = -0.25, y = Inf, hjust = 0, vjust = 1.5) +
  facet_wrap(~cat_no) +
  scale_color_manual(values=c('red', 'green', 'blue', 'purple'),
                     labels=c('Brightness', 'Contrast', 'Sharpness', 'Saturation'),
                     drop = FALSE) +
  #annotate('text', x=-0.25, y=1, label='(a)') +
  ylim(c(0, 1)) +
  labs(
    x = expression(paste(alpha, '-value')),
    y = "Proportion Choosing Non-Neutral Image"
  ) +
  #facet_wrap(~cat_no) + 
  geom_vline(xintercept=0, linetype=2) +
  theme(
    #strip.text.x = element_text(size=12, color='#414141', margin=margin(b=8)),
    strip.text.x = element_blank(),
    strip.background = element_rect(fill=NA),
    axis.title.x = element_text(size=12, color='#414141', margin=margin(t=8)),
    axis.title.y = element_text(size=12, color='#414141', margin=margin(r=8)),
    axis.ticks = element_line(color='#414141'),
    panel.border = element_rect(color='#414141', fill=NA),
    panel.background = element_rect(fill=NA),
    legend.title = element_blank(),
    #legend.position = 'none', 
    legend.spacing.x = unit(0.01, 'cm'),
    legend.text = element_text(size=8),
    legend.key.size = unit(0.4, 'cm'),
    legend.key = element_rect(fill='NA'),
    axis.text=element_text(size=9, color='#414141')
  )
ggsave(
  'helpme.pdf',
  plot = last_plot(),
  device = 'pdf',
  path = '../Paper/images/results',
  scale = 1,
  width = 50,
  height = 50,
  units = "in",
  dpi = 300,
  limitsize = FALSE,
  bg = NULL
)




## Linear model
reg <- lm(acc ~ alpha + score, data=df.cat_total_scaled)
summary(reg)




# Plot
ggplot(df.cat, aes(x=alpha, y=acc, group=cat_no)) +
  geom_line(size=1, key_glyph = 'point') +
  geom_line(data=df4.cat, aes(x=alpha, y = brightness)) +
  annotate('text', x=-0.25, y=1, label='(a)') +
  #scale_color_manual(labels=c(expression(paste('negative ', alpha)), expression(paste('positive ', alpha))), values=c('#F8766D', '#00BFC4')) +
  ylim(c(0, 1)) +
  labs(
    x = expression(paste(alpha, '-value')),
    y = "Proportion Choosing Non-Neutral Image"
  ) +
  #facet_wrap(~cat_no) + 
  geom_vline(xintercept=0, linetype=2) +
  theme(
    strip.text.x = element_text(size=12, color='#414141', margin=margin(b=8)),
    strip.background = element_rect(fill=NA),
    axis.title.x = element_text(size=12, color='#414141', margin=margin(t=8)),
    axis.title.y = element_text(size=12, color='#414141', margin=margin(r=8)),
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
