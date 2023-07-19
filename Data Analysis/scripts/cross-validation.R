#### Cross Validation ####
all_features <- read_csv('extraction/all_features.csv')
df <- read_csv('extraction/clean_df.csv')

# Create dataframe
df_cv <- all_features %>%
  inner_join(subset(df, select=c(cat_no, img, cor, sub, positive)), 
             by=c("cat"="cat_no", "img"="img")) %>%
  mutate(cor = if_else(positive == 0, 1-cor, cor)) %>%
  mutate(brightness=brightness_score,
         contrast=contrast_score,
         edges=edge_score,
         saturation=saturation_score,
         visual_complexity=visual_complexity,
         symmetry=symmetry,
         colorfulness=colorfulness) %>%
  select(cat, img, brightness, contrast, edges, saturation, visual_complexity, symmetry, colorfulness, cor) %>%
  group_by(brightness, contrast, edges, saturation, visual_complexity, symmetry, colorfulness) %>%
  summarize(aes=mean(cor))

# Train-test split
set.seed(0) 
index = sample(1:nrow(df_cv), 0.8*nrow(df_cv)) 

train <- df_cv[index,]
test <- df_cv[-index,]

# train <- df_cv[index,] %>%
#   group_by(brightness, contrast, edges, saturation, visual_complexity, symmetry, colorfulness) %>%
#   summarize(aes=mean(cor))
# 
# test <- df_cv[-index,] %>%
#   group_by(brightness, contrast, edges, saturation, visual_complexity, symmetry, colorfulness) %>%
#   summarize(aes=mean(cor))

dim(train)
dim(test)

lr <- lm(aes ~ brightness + contrast + edges + saturation + colorfulness + symmetry + visual_complexity, data=train)
summary(lr)


# Evaluation metrics
eval_metrics = function(model, df, predictions, target){
  resids = df[,target] - predictions
  resids2 = resids**2
  N = length(predictions)
  r2 = as.character(round(summary(model)$r.squared, 4))
  adj_r2 = as.character(round(summary(model)$adj.r.squared, 4))
  print(paste(adj_r2, "(Adjusted R2)")) #Adjusted R-squared
  print(paste(as.character(round(sqrt(sum(resids2)/N), 4)), "(RMSE)")) #RMSE
}

# Evaluate Models

# Linear model
predictions <- predict(lr, newdata=train)
eval_metrics(lr, train, predictions, target='aes')

predictions <- predict(lr, newdata=test)
eval_metrics(lr, test, predictions, target='aes')


model_logistic <- glm(cor ~ brightness + contrast + edges + saturation + colorfulness + symmetry + visual_complexity, data=df_cv, family=binomial)
summary(model_logistic)
