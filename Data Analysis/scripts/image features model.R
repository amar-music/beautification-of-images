#### Image Features ####
library(scales)
library(glmnet)

# All features ------------------------------------------------------------
## Create df across ImageNet categories and summarize aes
ftrs <- all_features %>% 
  inner_join(subset(df, select=c(cat_no, img, cor, sub, positive)), 
             by=c("cat"="cat_no", "img"="img")) %>%
  mutate(cor = if_else(positive == 0, 1-cor, cor)) %>%
  mutate(brightness=(rescale(brightness_score)),
         contrast=(rescale(contrast_score)),
         edges=(rescale(edge_score)),
         saturation=(rescale(saturation_score)),
         visual_complexity=(rescale(visual_complexity)),
         symmetry=(rescale(symmetry)),
         colorfulness=(rescale(colorfulness))) %>%
  select(cat, img, brightness, contrast, edges, saturation, visual_complexity, symmetry, colorfulness, cor) %>%
  group_by(brightness, contrast, edges, saturation, visual_complexity, symmetry, colorfulness) %>%
  summarize(aes=mean(cor))


## Long table
ftrs.long <- ftrs %>%
  pivot_longer(cols=c(brightness, contrast, edges, saturation, visual_complexity, symmetry, colorfulness), names_to=c("feature"), values_to="score")

# Label features
feature_names <- c(
  'brightness' = "Brightness",
  'contrast' = "Contrast",
  'edges' = "Sharpness",
  'saturation' = "Saturation",
  'colorfulness' = "Colorfulness",
  'visual_complexity' = "Complexity",
  'symmetry' = "Symmetry"
)



## Plot effect of image features on outcome aes
ggplot(ftrs.long, aes(x=score, y=aes, col=feature)) +
  ylim(0, 1) +
  #geom_smooth(method="lm") +
  geom_smooth(method="glm", method.args=list(family=quasibinomial)) +
  facet_grid(~feature, labeller = as_labeller(feature_names)) +
  labs(
    x = "Scaled Feature Score",
    y = "Aesthetic Appraisal"
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
    axis.text.x=element_text(size=8, angle=45, vjust=1, hjust=1, color='#414141')
  )

## Linear regression for predicting aes based on image features
lm.all <- lm(aes ~ brightness + contrast + edges + saturation + colorfulness + symmetry + visual_complexity, data=ftrs)
summary(lm.all)

lm.low <- lm(aes ~ brightness + contrast + edges + saturation, data=ftrs)
summary(lm.low)

lm.mid <- lm(aes ~ colorfulness + symmetry + visual_complexity, data=ftrs)
summary(lm.mid)

anova(lm.all, lm.low)

lm.colorfulness <- lm(aes ~ colorfulness, data=ftrs)
summary(lm.colorfulness)

corrplot(cor(ftrs))

cor(ftrs$colorfulness, ftrs$saturation)


## Quasibinomial logistic regression (check if this a correct approach)
model2 <- glm(aes ~ brightness + contrast + edges + saturation + colorfulness + symmetry + visual_complexity, data=ftrs, family=quasibinomial)
summary(model2)


hist(all_features$visual_complexity)
hist(all_features$symmetry)
hist(all_features$colorfulness)
hist(all_features$brightness_score)
hist(all_features$saturation_score)

hist(ftrs$visual_complexity)
hist(ftrs$symmetry)
hist(ftrs$colorfulness)
hist(ftrs$brightness)
hist(ftrs$saturation)


cor(all_features$visual_complexity, ftrs$visual_complexity)

length(all_features$visual_complexity)
length(ftrs$visual_complexity)




## Test for multicollinearity
vif(lm.all)   # VIF scores are around 4 for many predictors, indicating moderate correlation




## Ridge regression
y.ridge <- ftrs$aes
x.ridge <- data.matrix(ftrs[,c('brightness', 'contrast', 'edges', 'saturation', 'colorfulness', 'symmetry', 'visual_complexity')])

# Range of lambda values
lambda_seq <- seq(0, 10, by=.1)


# Fit model
fit.ridge <- glmnet(x.ridge, y.ridge, alpha=0, lambda=lambda_seq)
summary(fit.ridge)


# Cross-validation
ridge_cv <- cv.glmnet(x.ridge, y.ridge, alpha=0, lambda=lambda_seq)

# Best lambda
best_lambda <- ridge_cv$lambda.min
best_lambda    # Best lambda appears to be very low, therefore low penalty
plot(ridge_cv)



y_predicted <- predict(fit.ridge, s=0, newx=x.ridge)
sst <- sum(y.ridge^2)
sse <- sum((y_predicted - y.ridge)^2)

# R^2
rsq <- 1 - sse / sst
rsq
#

coef(fit.ridge)









# ridge.model <- glmnet(x.ridge, y.ridge, alpha=0)
# summary(ridge.model)
# 
# cv_model <- cv.glmnet(x.ridge, y.ridge, alpha=0)
# best_lambda <- cv_model$lambda.min
# best_lambda



fit.ridge2 <- ridge(y.ridge, x.ridge, data=ftrs, lambda=lambda_seq)
coef(fit.ridge2)
traceplot(fit.ridge2)
plot(fit.ridge2)
pairs(fit.ridge2)





# plot(cv_model)
# 
# 
# # Find coefficiencts of the best model
# best_ridge <- glmnet(x.ridge, y.ridge, alpha=0, lambda=best_lambda)
# coef(best_ridge)
# 
# 
# 
# #use fitted best model to make predictions
# y_predicted <- predict(best_ridge, s = best_lambda, newx = x.ridge)
# 
# #find SST and SSE
# sst <- sum((y.ridge - mean(y.ridge))^2)
# sse <- sum((y_predicted - y.ridge)^2)
# 
# #find R-Squared
# rsq <- 1 - sse/sst
# rsq






