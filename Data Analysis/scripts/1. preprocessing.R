#### PREPROCESSING ####

# Load libraries ----------------------------------------------------------
library("readr")
library("ggplot2")
library("ggpubr")
library("jtools")
library("dplyr")

## Load functions ----
lower_ci <- function(mean, se, n, conf_level = 0.95){
  lower_ci <- mean - qt(1 - ((1 - conf_level) / 2), n - 1) * se
}

upper_ci <- function(mean, se, n, conf_level = 0.95){
  upper_ci <- mean + qt(1 - ((1 - conf_level) / 2), n - 1) * se
}




# Load and clean dataframe ------------------------------------------------
df <- read_csv('extraction/test4.csv')
df <- df[25920:(nrow(df)),] # Select only trails from 1 seed version
names(df)[c(1, 8, 9, 11, 12, 13, 16)] <- c(
  'trial', 'base', 'img', 'c_key', 'u_key', 'cor', 'sub')
df$diff <- abs(df$alpha) # difference as number

df$aeq_total <- (df$emotional + df$cultural + df$perceptual + df$understanding
                 + df$`flow-proximal` + df$`flow-experience`)

## Remove trials longer than 10s
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


length(unique(df$seed))




