#### PREPROCESSING ####

# Load libraries ----------------------------------------------------------
library("readr")
library("ggplot2")
library("ggpubr")
library("jtools")
library("dplyr")



# Load and clean dataframe ------------------------------------------------
df <- read_csv('extraction/test3.csv')
names(df)[c(1, 8, 9, 11, 12, 13, 16)] <- c(
  'trial', 'base', 'img', 'c_key', 'u_key', 'cor', 'sub')
df$diff <- abs(df$alpha) # difference as number
#df$diff <- as.factor(abs(data$alpha)) # difference as factor
#df$alpha <- as.factor(data$alpha) # alpha as factor

# SELECT ONLY TRIALS AFTER 23162 (last old trial)
df <- df[23163:(nrow(df)),]
df$aeq_total <- (df$emotional + df$cultural + df$perceptual + df$understanding
                 + df$`flow-proximal` + df$`flow-experience`)

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

## Single out one subject ----
#df <- df[df$sub == '60fe7180d082dab5c0e4c9c4',]


# Find bad participants ---------------------------------------------------

## Key press bias ----
key_presses_subs <- aggregate(df$u_key, by=list(df$sub), FUN=mean)
#hist(key_presses_subs$x)
#print(key_presses_subs[key_presses_subs$x < 0.25 | key_presses_subs$x > 0.75,])


## Performance at chance level ----
performance_subs <- aggregate(df$cor, by=list(df$sub), FUN=mean)
#hist(performance_subs$x)
#print(performance_subs[performance_subs$x < 0.55,])


## Too fast RT ----
rt_subs <- aggregate(df$rt, by=list(df$sub), FUN=median)
hist(rt_subs$x)
#print(rt_subs[rt_subs$x < 500,])


## Identify participants with < 100 trials ----
sub_trials <- data.frame(sort(table(df$sub)))
sub_trials[sub_trials$Freq < 100,]


## Check total time elapsed ----
total_time <- aggregate(df$time_elapsed, by=list(df$sub), FUN=last)
median(total_time$x)
#hist(total_time$x)




# Dataframes for plots ----------------------------------------------------
diff_correct.indiv <- df %>% 
  group_by(sub, diff) %>%  
  summarize(prop = mean(cor), correct = sum(cor), 
            n = n(), wrong = n-correct) 

diff_correct.total <- df %>% 
  group_by(diff) %>%  
  summarize(prop = mean(cor), correct = sum(cor), 
            n = n(), wrong = n-correct) 

alpha_correct.total <- df %>% 
  group_by(alpha) %>%  
  summarize(prop = mean(cor), correct = sum(cor), 
            n = n(), wrong = n-correct) 


length(unique(df$seed))




