library(readr)
library(tidyverse)
library(broom)

## import data
data <- read_csv("data/test2.csv")

## set factors
data$difference <- abs(data$alpha)
data$difference_factor <- as.factor(abs(data$alpha))
data$alpha_factor <- as.factor(data$alpha)
df <- subset(data, select = c(subject_id, alpha, difference, alpha_factor, difference_factor, eval))
head(df)

#### ANOVA
anova <- aov(eval~difference, data=df)
anova2 <- aov(eval~difference_factor, data=df)
summary(anova)
summary(anova2)


#### GLM

## no abs
# average resp of 'eval'
no_abs_f_msub <- df %>% 
  group_by(subject_id, alpha_factor) %>%  
  summarize(prop = mean(eval), correct = sum(eval), 
            n = n(), wrong = n-correct) 
head(no_abs_f_msub)

## abs
# average resp of 'eval'
abs_f_msub <- df %>% 
  group_by(subject_id, difference_factor) %>%  
  summarize(prop = mean(eval), correct = sum(eval), 
            n = n(), wrong = n-correct) 
head(abs_f_msub)

## abs no factor
abs_msub <- df %>% 
  group_by(subject_id, difference) %>%  
  summarize(prop = mean(eval), correct = sum(eval), 
            n = n(), wrong = n-correct) 
head(abs_msub)

## abs total
abs_total <- df %>% 
  group_by(difference) %>%  
  summarize(prop = mean(eval), correct = sum(eval), 
            n = n(), wrong = n-correct) 
head(abs_total)


## plots
# no abs (factor)
no_abs_f_msub %>% ggplot(aes(alpha_factor, prop, color=subject_id, group=subject_id)) +
  geom_point(size=2) + geom_line(size=1) + geom_hline(yintercept=c(0.5, 1)) +
  facet_wrap(~subject_id) + 
  ylab("Prop. of correct responses")

# abs (factor)
abs_f_msub %>% ggplot(aes(difference_factor, prop, color=subject_id, group=subject_id)) +
  geom_point(size=2) + geom_line(size=1) + geom_hline(yintercept=c(0.5, 1)) +
  facet_wrap(~subject_id) + 
  ylab("Prop. of correct responses")

# abs (number)
abs_msub %>% ggplot(aes(difference, prop, color=subject_id, group=subject_id)) +
  geom_point(size=2) + geom_line(size=1) + geom_hline(yintercept=c(0.5, 1)) +
  facet_wrap(~subject_id) + 
  ylab("Prop. of correct responses")

# total (number)
abs_total %>% ggplot(aes(difference, prop)) +
  geom_point(size=2) + geom_line(size=1) + geom_hline(yintercept=c(0.5, 1)) +
  ylab("Prop. of correct responses")



## GLM logistic regression
glm_results = abs_msub %>% 
  group_by(subject_id) %>%
  do(tidy (glm(cbind(correct,wrong) ~ difference, 
               family = binomial(logit), data=.)))
head(glm_results)

psy_results = glm_results %>%
  select(., one_of(c('subject_id', 'term','estimate'))) %>%
  spread(.,term, estimate) %>% rename(., b=difference, a = `(Intercept)`) %>%
  mutate(., pse = -a/b, jnd = log(3)/b)
head(psy_results)

# Psychometric curves. 
abs_msub %>%ggplot( aes(x=difference, y=prop, color=subject_id)) +  
  geom_point(aes(shape = subject_id), size=3) + geom_hline(yintercept=0.5) +
  geom_smooth(method=glm, method.args= list(family = binomial(logit)), 
              se = FALSE, aes(linetype=subject_id)) +
  xlab('Difference between images') + ylab('Prop. of Correct Responses') +
  facet_wrap(~subject_id)



## GLM logistic regression TOTAL
glm_results2 = abs_total %>% 
  do(tidy (glm(cbind(correct,wrong) ~ difference, 
               family = binomial(logit), data=.)))
head(glm_results2)

psy_results2 = glm_results2 %>%
  select(., one_of(c('term','estimate'))) %>%
  spread(.,term, estimate) %>% rename(., b=difference, a = `(Intercept)`) %>%
  mutate(., pse = -a/b, jnd = log(3)/b)
head(psy_results2)

# Psychometric curves. 
abs_total %>%ggplot( aes(x=difference, y=prop)) +  
  geom_point(size=3) + geom_hline(yintercept=0.5) +
  geom_smooth(method=glm, method.args= list(family = binomial(logit)), 
              se = FALSE) +
  xlab('Difference between images') + ylab('Prop. of correct responses')






## mean scores per label and random stuff
barplot(by(data=data$eval, INDICES = data$label_2, FUN = mean), ylim=c(0,1)) + 
  abline(h=0.5)
cor(data$rt, data$eval)
mean(data$eval[data$rt>2])
mean(data$eval[data$rt<2])
summary(aov(data$eval~data$label_2))
mean(df$eval[df$difference==0.0025])
table(data$alpha)

    