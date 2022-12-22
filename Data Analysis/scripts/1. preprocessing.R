#### PREPROCESSING ####

# Load libraries ----------------------------------------------------------
library("readr")
library("ggplot2")
library("ggpubr")
library("jtools")
library("dplyr")
library("tidySEM")
library("corrplot")
library("psych")
library("lavaan") 
library("lavaan.survey")
library("quickpsy")
library("tidyr")
library("purrr")


## Load functions ----
lower_ci <- function(mean, se, n, conf_level = 0.95){
  lower_ci <- mean - qt(1 - ((1 - conf_level) / 2), n - 1) * se
}

upper_ci <- function(mean, se, n, conf_level = 0.95){
  upper_ci <- mean + qt(1 - ((1 - conf_level) / 2), n - 1) * se
}

alphas = c(-0.25, -0.20, -0.15, -0.10, -0.05, -0.015, -0.0025, 
           0, 0.0025, 0.015, 0.05, 0.10, 0.15, 0.20, 0.25)


# Load and clean dataframe ------------------------------------------------
df <- read_csv('extraction/exp_data.csv')
#demographics <- read_csv('extraction/demographic.csv')
#demographics_subjects <- subset(demographics, select = c('participant_id', 'age', 'Nationality', 'Sex'))
#names(demographics_subjects) <- c('subject_id', 'age', 'nationality', 'sex')
#df <- merge(df, demographics_subjects, by='subject_id')
df.FID <- read.csv("extraction/FIDdf.csv")


# Select only trails from 1 seed version ----
#df <- df[25920:(nrow(df)),]
#names(df)[c(1, 2, 9, 10, 12, 13, 14)] <- c(
#  'sub', 'trial', 'base', 'img', 'c_key', 'u_key', 'cor')
names(df)[c(1, 8, 9, 11, 12, 13, 16)] <- c(
  'trial', 'base', 'img', 'c_key', 'u_key', 'cor', 'sub')
df$diff <- abs(df$alpha) # difference as number

df$aeq_total <- (df$emotional + df$cultural + df$perceptual + df$understanding
                 + df$`flow-proximal` + df$`flow-experience`)

## Remove trials longer than 10s ----
df.short <- df[df$rt < 5000,]

## Participants to exclude ----
bad_subs <- readLines("filter_participants.txt")
df <- df[!(df$sub %in% bad_subs),]
subs <- unique(df$sub);N <- length(subs)


## Response column to int ----
df <- df %>%
  mutate(u_key = replace(u_key, u_key == 'f', 0)) %>%
  mutate(u_key = replace(u_key, u_key == 'j', 1))
df$u_key <- as.integer(df$u_key)

df <- df %>%
  mutate(c_key = replace(c_key, c_key == 'f', 0)) %>%
  mutate(c_key = replace(c_key, c_key == 'j', 1))
df$c_key <- as.integer(df$c_key)

df <- df %>%
  mutate(positive = case_when(
    alpha < 0 ~ 0,
    alpha > 0 ~ 1
  ))
df$positive <- as.factor(df$positive)


df.cont <- df

df.cont <- df.cont %>%
  mutate(cor = if_else(positive == 0, 1-cor, cor))

## Make positive and negative dataframe
dfp <- df[df$alpha > 0,]
dfn <- df[df$alpha < 0,]











# Dataframes for plots ----------------------------------------------------
diff_correct.indiv <- df %>% 
  group_by(sub, diff) %>%  
  summarize(acc = mean(cor),
            sd = sd(cor),
            count = n()) %>%
  mutate (se = sd / sqrt(count),
          lower_ci = lower_ci(acc, se, count),
          upper_ci = upper_ci(acc, se, count))


## Accuracy for each difference level ----
dfd <- df %>% 
  group_by(diff) %>%  
  summarize(acc = mean(cor),
            sd = sd(cor),
            count = n()) %>%
  mutate (se = sd / sqrt(count),
          lower_ci = lower_ci(acc, se, count),
          upper_ci = upper_ci(acc, se, count))


## Accuracy for each alpha level withfor each participant ----
dfa <- df %>% 
  group_by(alpha) %>%  
  summarize(acc = mean(cor),
            sd = sd(cor),
            count = n(),
            nCor = sum(cor)) %>%
  mutate (se = sd / sqrt(count),
          positive = if_else(alpha<0, 0, 1),
          lower_ci = lower_ci(acc, se, count),
          upper_ci = upper_ci(acc, se, count))

dfa.cont <- df.cont %>% 
  group_by(alpha) %>%  
  summarize(acc = mean(cor),
            sd = sd(cor),
            count = n(),
            nCor = sum(cor)) %>%
  mutate (se = sd / sqrt(count),
          positive = if_else(alpha<0, 0, 1),
          lower_ci = lower_ci(acc, se, count),
          upper_ci = upper_ci(acc, se, count))



#df_participants <- df %>%
#  group_by(sub) %>%  
#  summarize(m_age = mean(age),
#            sd_age = sd(age),
#            sex = dplyr::first(sex))



### Negative only ----
dfan <- dfa[dfa$positive == 0,]
dfan$alpha <- abs(dfan$alpha)

### Positive only ----
dfap <- dfa[dfa$positive == 1,]

## Fit psychometric functions  ----
qps_fit.p <- quickpsy(dfap, alpha, nCor, count, guess=TRUE, lapses = TRUE)
qps_fit.n <- quickpsy(dfan, alpha, nCor, count, guess=TRUE, lapses = TRUE)

qps_fit.asym <- quickpsy(df.cont, alpha, cor, grouping=('positive'), guess=TRUE, lapses=TRUE, ci=.99)
qps_fit.sym <- quickpsy(df.cont, alpha, cor, guess=TRUE, lapses=TRUE, ci=.99)



# Dataframes for image features -------------------------------------------
df4 <- read_csv('extraction/image_features.csv')

df4a <- df4 %>% 
  group_by(img) %>%  
  summarize(m_edge = mean(edge_score),
            m_contrast = mean(contrast_score),
            m_brightness = mean(brightness_score),
            m_saturation = mean(saturation_score)) %>%
  mutate(alpha = alphas)

df4a$edges <- scale(df4a$m_edge)
df4a$contrast <- scale(df4a$m_contrast)
df4a$brightness <- scale(df4a$m_brightness)
df4a$saturation <- scale(df4a$m_saturation)

df4a <- gather(df4a, key=feature, value = score, 
               c("contrast", "edges", "brightness", "saturation"))


## Dataframe for plot image features per category
# PASTE HERE

# Color distributions -----------------------------------------------------
selected_alphas <- c(-0.25, -0.1, 0, 0.1, 0.25)

tbl <-
  list.files(pattern = "colors_cat*", recursive = TRUE) %>% 
  map_df(~read_csv(.)) %>%
  gather(key, value, -cat, -alpha) %>%
  filter(alpha %in% selected_alphas)
tbl$key <- factor(tbl$key, levels=c("red", "green", "blue"))






# Dataframes for SEM ------------------------------------------------------
# df2 <- df %>% 
#   group_by(sub, emotional, cultural, perceptual, understanding, `flow-proximal`,
#            `flow-experience`, aeq_total) %>%  
#   summarize(cor = mean(cor),
#             ntrials = n())
# df2 <- merge(df2, demographics_subjects, by.x='sub', by.y='subject_id')


## Remove all missing ----
# df2[df2 == 'CONSENT REVOKED'] <- NA
# df2[df2 == 'DATA EXPIRED'] <- NA
# df3 <- na.omit(df2)
# df3$sex <- ifelse(df3$sex=="Male",1,0) # male=1, female=0
# df3$sex <- as.factor(df3$sex)
# #df3$nationality <- as.factor(df3$nationality)
# 
# df3.2 <- subset(df3, select = c(
#   'emotional', 'cultural', 'perceptual', 'understanding', 'flow-proximal', 
#   'flow-experience', 'age', 'sex', 'nationality', 'cor'))



# Dataframes for world map ------------------------------------------------
#nationalities <- data.frame(table(demographics$Nationality))
#levels(nationalities$Var1)[levels(nationalities$Var1)=='United States'] <- 'USA'
#levels(nationalities$Var1)[levels(nationalities$Var1)=='United Kingdom'] <- 'UK'
#levels(nationalities$Var1)[levels(nationalities$Var1)=='Russian Federation'] <- 'Russia'
#levels(nationalities$Var1)[levels(nationalities$Var1)=='Congo the Democratic Republic of the'] <- 
#  'Democratic Republic of the Congo'
#world_map <- map_data("world")

