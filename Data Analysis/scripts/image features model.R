#### Image Features ####

## Create df across ImageNet categories and summarize aes
ftrs <- df4 %>% 
  inner_join(subset(df, select=c(cat_no, img, cor, sub, positive)), 
             by=c("cat"="cat_no", "img"="img")) %>%
  mutate(cor = if_else(positive == 0, 1-cor, cor)) %>%
  mutate(brightness=c(scale(brightness_score)),
         contrast=c(scale(contrast_score)),
         edges=c(scale(edge_score)),
         saturation=c(scale(saturation_score))) %>%
  select(cat, img, brightness, contrast, edges, saturation, cor) %>%
  group_by(brightness, contrast, edges, saturation) %>%
  summarize(aes=mean(cor))

# Long df
ftrs.long <- ftrs %>%
  pivot_longer(cols=c(brightness, contrast, edges, saturation), names_to=c("feature"), values_to="score")


## Plot effect of image features on outcome aes
ggplot(ftrs.long, aes(x=score, y=aes, col=feature)) +
  ylim(0, 1) +
  geom_smooth(method="lm") +
  #geom_smooth(method="glm", method.args=list(family=quasibinomial)) +
  facet_grid(.~feature) +
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

## Linear regression for predicting aes based on image features
model1 <- lm(aes ~ brightness + contrast + edges + saturation, data=ftrs)
summary(model1)

## Quasibinomial logistic regression (check if this a correct approach)
model2 <- glm(aes ~ brightness + contrast + edges + saturation, data=ftrs, family=quasibinomial)
summary(model2)

