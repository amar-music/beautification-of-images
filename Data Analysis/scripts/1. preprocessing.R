#### PREPROCESSING ####

# Load libraries ----------------------------------------------------------
library("readr")
library("ggplot2")
library("ggpubr")
library("jtools")
library("dplyr")
library("ggplot2")
library("tidySEM")
library("corrplot")
library("psych")
library("lavaan") 
library("lavaan.survey")


## Load functions ----
lower_ci <- function(mean, se, n, conf_level = 0.95){
  lower_ci <- mean - qt(1 - ((1 - conf_level) / 2), n - 1) * se
}

upper_ci <- function(mean, se, n, conf_level = 0.95){
  upper_ci <- mean + qt(1 - ((1 - conf_level) / 2), n - 1) * se
}




# Load and clean dataframe ------------------------------------------------
df <- read_csv('extraction/test5.csv')
demographics <- read_csv('extraction/demographic.csv')
demographics_subjects <- subset(demographics, select = c('participant_id', 'age', 'Nationality', 'Sex'))
names(demographics_subjects) <- c('subject_id', 'age', 'nationality', 'sex')
df <- merge(df, demographics_subjects, by='subject_id')


# Select only trails from 1 seed version ----
#df <- df[25920:(nrow(df)),]
names(df)[c(1, 2, 9, 10, 12, 13, 14)] <- c(
  'sub', 'trial', 'base', 'img', 'c_key', 'u_key', 'cor')
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



# Dataframes for plots ----------------------------------------------------
diff_correct.indiv <- df %>% 
  group_by(sub, diff) %>%  
  summarize(acc = mean(cor),
            sd = sd(cor),
            count = n()) %>%
  mutate (se = sd / sqrt(count),
          lower_ci = lower_ci(acc, se, count),
          upper_ci = upper_ci(acc, se, count))


diff_correct.total <- df %>% 
  group_by(diff) %>%  
  summarize(acc = mean(cor),
            sd = sd(cor),
            count = n()) %>%
  mutate (se = sd / sqrt(count),
          lower_ci = lower_ci(acc, se, count),
          upper_ci = upper_ci(acc, se, count))


alpha_correct.total <- df %>% 
  group_by(alpha) %>%  
  summarize(acc = mean(cor),
            sd = sd(cor),
            count = n()) %>%
  mutate (se = sd / sqrt(count),
          lower_ci = lower_ci(acc, se, count),
          upper_ci = upper_ci(acc, se, count))


alpha_correct.total <- df %>% 
  group_by(alpha) %>%  
  summarize(acc = mean(cor), correct = sum(cor))



# Dataframes for SEM ------------------------------------------------------
df2 <- df %>% 
  group_by(sub, emotional, cultural, perceptual, understanding, `flow-proximal`,
           `flow-experience`, aeq_total) %>%  
  summarize(cor = mean(cor),
            ntrials = n())
df2 <- merge(df2, demographics_subjects, by.x='sub', by.y='subject_id')


## Remove all missing ----
df2[df2 == 'CONSENT REVOKED'] <- NA
df2[df2 == 'DATA EXPIRED'] <- NA
df3 <- na.omit(df2)
df3$sex <- ifelse(df3$sex=="Male",1,0) # male=1, female=0
df3$sex <- as.factor(df3$sex)
#df3$nationality <- as.factor(df3$nationality)

df3.2 <- subset(df3, select = c(
  'emotional', 'cultural', 'perceptual', 'understanding', 'flow-proximal', 
  'flow-experience', 'age', 'sex', 'nationality', 'cor'))

# df3.3 <- df3.2
# 
# 
# df3.3 <- df3.3 %>% mutate(dummy=1) %>%
#   spread(key=nationality, value=dummy, fill=0)





# Dataframes for world map ------------------------------------------------
nationalities <- data.frame(table(demographics$Nationality))
levels(nationalities$Var1)[levels(nationalities$Var1)=='United States'] <- 'USA'
levels(nationalities$Var1)[levels(nationalities$Var1)=='United Kingdom'] <- 'UK'
levels(nationalities$Var1)[levels(nationalities$Var1)=='Russian Federation'] <- 'Russia'
levels(nationalities$Var1)[levels(nationalities$Var1)=='Congo the Democratic Republic of the'] <- 
  'Democratic Republic of the Congo'
world_map <- map_data("world")



# Seed check
length(unique(df$seed))
