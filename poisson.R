library(tidyverse)

# Is it Poisson? ----
data <- c(
  rep(0, 7),
  rep(1, 18),
  rep(2, 24),
  rep(3, 24),
  rep(4, 20),
  rep(5, 8),
  rep(6, 7),
  rep(7, 1),
  rep(8, 2)
)

# Look at histogram
data_tibble <-
  tibble::tibble(
    data
  )

ggplot(data_tibble, aes(data)) +
  geom_histogram()

# Compare mean and variance
## Should be equal, approximately
mean(data)
var(data)

# Test of fit
## Table of observed frequencies
obs_freq <- table(data)
## Expected frequencies based on Poisson distribution
lambda <- mean(data)
exp_freq <- dpois(as.numeric(names(obs_freq)), lambda) * length(data)

## Chi-squared test
chisq_test <- chisq.test(obs_freq, p = exp_freq, rescale.p = TRUE)
print(chisq_test)
## It is Poisson of p-value larger than 0.05.

# Generating QQ plot
qqplot(qpois(ppoints(length(data)), lambda), data,
       main = "QQ Plot for Poisson Distribution",
       xlab = "Theoretical Quantiles",
       ylab = "Sample Quantiles")
abline(0, 1, col = "red")
## If the points lie approximately along the reference line, the data likely follows a Poisson distribution.

# Overdispersion Check
## Overdispersion occurs when the variance is greater than the mean,
## indicating that the data might not follow a Poisson distribution.
## A dispersion test can be performed using the AER package.
library(AER)
AER::dispersiontest(glm(data ~ 1, family = poisson))

# But then, is it also normal?
# Poisson can be approximated by normal when lambda is larger than 4 (about), and the continuity correction is applied.
