#### IDENTIFY BAD PARTICIPANTS ####

# Load and clean dataframe ------------------------------------------------
df <- read_csv('extraction/test4.csv')
names(df)[c(1, 8, 9, 11, 12, 13, 16)] <- c(
  'trial', 'base', 'img', 'c_key', 'u_key', 'cor', 'sub')
df$diff <- abs(df$alpha) # difference as number

# SELECT ONLY TRIALS AFTER 23162 (last old trial)
df <- df[25920:(nrow(df)),]
df$aeq_total <- (df$emotional + df$cultural + df$perceptual + df$understanding
                 + df$`flow-proximal` + df$`flow-experience`)

## Remove trials longer than 10s
df.short <- df[df$rt < 5000,]
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
df <- df[df$sub == '611d1134bcc4eda812acf219',]


# Find bad participants ---------------------------------------------------

## Key press bias ----
key_presses_subs <- aggregate(df$u_key, by=list(df$sub), FUN=mean)
#hist(key_presses_subs$x)
print(key_presses_subs[key_presses_subs$x < 0.25 | key_presses_subs$x > 0.75,])

all_key_presses <- subset(df, select = c('sub', 'u_key'))

## Detect L/R alternating
rep_alt <- c()
prev_i <- 0
for (i in 1:length(all_key_presses$u_key)){
  if (all_key_presses$u_key[i] == prev_i){
    rep_alt <- append(rep_alt, 1) # mark as repetition
  }
  else{
    rep_alt <- append(rep_alt, 0) # mark as alternate
  }
  prev_i <- all_key_presses$u_key[i]
}
alt_subs <- data.frame(all_key_presses, rep_alt)
alt_subs.agg <- aggregate(alt_subs$rep_alt, by=list(alt_subs$sub), FUN=mean)
print(alt_subs.agg[alt_subs.agg$x < 0.3,]$Group.1)


## Performance at chance level ----
performance_subs <- aggregate(df$cor, by=list(df$sub), FUN=mean)
#hist(performance_subs$x)
print(performance_subs[performance_subs$x < 0.60,])


## Too fast RT ----
rt_subs <- aggregate(df.short$rt, by=list(df.short$sub), FUN=median)
hist(rt_subs$x)
print(rt_subs[rt_subs$x < 500,])


## Identify participants with < 50 trials ----
sub_trials <- data.frame(sort(table(df$sub)))
sub_trials[sub_trials$Freq < 50,]


## Check total time elapsed ----
total_time <- aggregate(df$time_elapsed, by=list(df$sub), FUN=last)
median(total_time$x)
#hist(total_time$x)