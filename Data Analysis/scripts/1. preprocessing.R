#### PREPROCESSING ####

# Load libraries ----------------------------------------------------------
library("readr")
library("ggplot2")
library("dplyr")



# Load and clean dataframe ------------------------------------------------
df <- read_csv('extraction/test2.csv')


## Change column names
names(df)[c(1, 8, 9, 11, 12, 13, 16)] <- c('trial', 'base', 'img', 'c_key', 'u_key', 'cor', 'sub')


## Response column to int ----
df <- df %>%
  mutate(u_key = replace(u_key, u_key == 'f', 0)) %>%
  mutate(u_key = replace(u_key, u_key == 'j', 1))
df$u_key <- as.integer(df$u_key)

df <- df %>%
  mutate(c_key = replace(c_key, c_key == 'f', 0)) %>%
  mutate(c_key = replace(c_key, c_key == 'j', 1))
df$c_key <- as.integer(df$c_key)



# Find bad participants ---------------------------------------------------

## Key press bias
key_presses_subs <- aggregate(df$u_key, by=list(df$sub), FUN=mean)
hist(key_presses_subs$x)
print(key_presses_subs[key_presses_subs$x < 0.25 | key_presses_subs$x > 0.75,])


## Performance at chance level
performance_subs <- aggregate(df$cor, by=list(df$sub), FUN=mean)
hist(performance_subs$x)
print(performance_subs[performance_subs$x < 0.55,])


## Too fast RT
rt_subs <- aggregate(df$rt, by=list(df$sub), FUN=mean)
hist(rt_subs$x)
print(rt_subs[rt_subs$x < 500,])


