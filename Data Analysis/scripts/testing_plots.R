#### Testing plots ####
range01 <- function(x){(x-min(x))/(max(x)-min(x))}

# All used categories
all_c <- sort(unique(df$cat_no))

# Select category number
cn <- all_c
#cn <- diff_a25a10
cn <- diff_llf

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
  summarize(edges = edge_score,
            contrast = contrast_score,
            brightness = brightness_score,
            saturation = saturation_score) %>%
  mutate(alpha = 0.0025*img -0.25)

df4.cat_scaled <- df4.cat #%>%
#  group_by(cat) %>%
#  mutate(edges = range01(edges),
#         contrast = range01(contrast),
#         brightness = range01(brightness),
#         saturation = range01(saturation))


#df4.cat_scaled <- df4.cat %>%
#   group_by(cat) %>%
#   mutate(edges = scale(edges),
#          contrast = scale(contrast),
#          brightness = scale(brightness),
#          saturation = scale(saturation))
names(df4.cat_scaled)[2] <- 'cat_no'

df.cat_total <- merge(df.cat, df4.cat_scaled, by=c('alpha', 'cat_no'))
df.cat_total_gather <- gather(df.cat_total, key=feature, value = score, 
               c("contrast", "edges", "brightness", "saturation"))

scaleFactor <- max(df.cat_total_gather$acc) / max(df.cat_total_gather$score)



ggplot(df.cat_total_gather, aes(x=alpha, group=feature, col=feature)) +
  geom_line(aes(y=acc), size=1.5, col='black') +
  geom_line(aes(y=score * scaleFactor), key_glyph = 'rect') +
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
  #geom_vline(xintercept=0, linetype=2) +
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
    legend.position = 'top', 
    legend.spacing.x = unit(0.01, 'cm'),
    legend.text = element_text(size=9),
    legend.key.size = unit(0.1, 'cm'),
    legend.key = element_rect(fill='NA'),
    axis.text=element_text(size=9, color='#414141')
  )
ggsave(
  'aes_dip.pdf',
  plot = last_plot(),
  device = 'pdf',
  path = '../Paper/images/results',
  scale = 1,
  width = 18,
  height = 16,
  units = "cm",
  dpi = 300,
  limitsize = FALSE,
  bg = NULL
)



# Correlation matrix
cor(select(df.cat_total, edges:saturation))
cor(select(df.cat_total, alpha:acc, edges:saturation))

plot(df.cat_total$alpha, df.cat_total$saturation)



# Lowest aesthetic values for highest alpha
## Filter only a=0.25 and a=0.10
df.cat_decrease <- data.frame(df.cat_total[df.cat_total$alpha==0.25,]$cat_no, df.cat_total[df.cat_total$alpha==0.25,]$acc, df.cat_total[df.cat_total$alpha==0.10,]$acc)
names(df.cat_decrease) <- c("img", "a.25", "a.10")
df.cat_decrease <- df.cat_decrease[order(df.cat_decrease$img),]

## Calculate difference between a=0.25 and a=0.10
df.cat_decrease$comp <- df.cat_decrease$a.25 - df.cat_decrease$a.10

## Calculate the 95th percentile of the variable
upper_threshold <- quantile(df.cat_decrease$comp, 0.05)
up_rows <- which(df.cat_decrease$comp <= upper_threshold)

## Print category numbers of selected series
diff_a25a10 <- df.cat_decrease[up_rows,]$img



# Stable low-level features
df.cat_stable_pos <- select(df.cat_total[df.cat_total$alpha==0.25,], cat_no, edges:saturation)
df.cat_stable_neg <- select(df.cat_total[df.cat_total$alpha==-0.25,], cat_no, edges:saturation)
df.cat_stable_img <- select(df.cat_total[df.cat_total$alpha==0.25,], cat_no)

df.cat_stable <- merge(df.cat_stable_pos, df.cat_stable_neg, by = "cat_no")

df.cat_diff <- abs(df.cat_stable_neg-df.cat_stable_pos)
df.cat_diff[1] <- df.cat_stable_img

df.cat_diff_t <- t(df.cat_diff[2:ncol(df.cat_diff)])
colnames(df.cat_diff_t) <- df.cat_diff$cat_no

## Dataframe with sum of all differences for each image series
df.cat_stable_sums <- data.frame(colSums(df.cat_diff_t))
names(df.cat_stable_sums) <- c("diff")

stable_threshold <- quantile(df.cat_stable_sums$diff, 0.05)
under_rows <- which(df.cat_stable_sums <= stable_threshold)


diff_llf <- df.cat_diff[under_rows,]$cat_no



# FID ---------------------------------------------------------------------
ggplot(df.FID, aes(x=alpha, y = FID)) + 
  geom_line() +
  geom_point() +
  #ylim(c(-2.5, 2.5)) +
  #facet_grid(~feature, labeller = as_labeller(feature_names)) +
  labs(
    x = expression(paste(alpha, '-value')),
    y = "FID"
  ) +
  theme(
    strip.text.x = element_text(size=12, color='#414141', margin=margin(b=8)),
    strip.background = element_rect(fill=NA),
    axis.title.x = element_text(size=12, color='#414141', margin=margin(t=8)),
    axis.title.y = element_text(size=12, color='#414141', margin=margin(r=8)),
    axis.ticks = element_line(color='#414141'),
    panel.border = element_rect(color='#414141', fill=NA),
    panel.background = element_rect(fill=NA),
    legend.position = "none",
    axis.text=element_text(size=9, color='#414141')
  )
ggsave(
  'FID.pdf',
  plot = last_plot(),
  device = 'pdf',
  path = '../Paper/images/results',
  scale = 1,
  width = 10,
  height = 9.5,
  units = "cm",
  dpi = 300,
  limitsize = FALSE,
  bg = NULL
)






