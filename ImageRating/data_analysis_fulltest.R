library(readr)
library(tidyverse)
library(broom)

## import data
setwd("C:/Users/amarm/PycharmProjects/ImageRating/fake_data")
data_full <- read_csv("data_full.csv")

## set factors
data_full$delta_alpha_abs <- abs(data_full$delta_alpha)
data_full$delta_alpha_abs <- as.factor(data_full$delta_alpha_abs)
data_full$delta_alpha <- as.factor(data_full$delta_alpha)
df <- subset(data_full, select = c(subj, delta_alpha, eval))
df2 <- subset(data_full, select = c(subj, delta_alpha_abs, eval))
head(df)
head(df2)



#### GLM

## no abs
# average resp of 'eval'
msub <- df %>% 
  group_by(subj, delta_alpha) %>%  
  summarize(prop = mean(eval), correct = sum(eval), 
            n = n(), wrong = n-correct) 
head(msub)

## abs
# average resp of 'eval'
msub2 <- df2 %>% 
  group_by(subj, delta_alpha_abs) %>%  
  summarize(prop = mean(eval), correct = sum(eval), 
            n = n(), wrong = n-correct) 
head(msub2)


## plots
# no abs
msub %>% ggplot(aes(delta_alpha, prop, color=subj, group=subj)) +
  geom_point() + geom_line() + geom_hline(yintercept=0.5) +
  facet_wrap(~subj) + 
  ylab("Prop. of correct responses")

# abs
msub2 %>% ggplot(aes(delta_alpha_abs, prop, color=subj, group=subj)) +
  geom_point() + geom_line() + geom_hline(yintercept=0.5) +
  facet_wrap(~subj) + 
  ylab("Prop. of correct responses")




