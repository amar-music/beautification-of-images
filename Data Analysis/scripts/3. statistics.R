#### Statistics ####

# IDEA
# Split the dataframe in positive and negative alphas
# Take abs() of negative df
# Significant difference between negative and positive alphas?


# Quickpsy fit ------------------------------------------------------------
qps_fit.p <- quickpsy(dfap, alpha, nCor, count, guess=TRUE, lapses = TRUE)
qps_fit.n <- quickpsy(dfan, alpha, nCor, count, guess=TRUE, lapses = TRUE)

qps_fit <- quickpsy(df.cont, alpha, cor, grouping=('positive'), guess=TRUE, lapses=TRUE)

## Fit parameters ----
qps_fit.p$par    # p1= ..., p2= ..., p3=lapse
qps_fit.n$par

# A priori contrasts ------------------------------------------------------
plotthresholds(qps_fit.sym)
plotthresholds(qps_fit.asym)


plot(qps_fit.sym)
plot(qps_fit.asym)

qps_fit.sym$aic
qps_fit.asym$aic


qps_fit.asym$parcomparisons
