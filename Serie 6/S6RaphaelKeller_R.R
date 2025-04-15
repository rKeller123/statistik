

## Statistik Serie 6

# Raphael Keller
# 23-128-721

set.seed(123)

par(mfrow = c(2, 2))

# generates X_1, ..., X_n ramdom avariables (standard cauchy)
# calculates T_n by finding the X_(m) the m-th greatest random variable
empirical_median_from_cauchy <- function(n) {
  cauchy_variables <- rcauchy(n = n)
  m <- (n + 1) / 2
  sort(cauchy_variables, decreasing=TRUE)[m]
}

n <- c(7, 11, 51, 501)

x <- seq(-15, 15, 0.01)
# using the berry essen hint we find that the asymptotic density of sqrt(n)T_n
#    is normally distributed with mean 0 and sd = pi / 2
asymptotic_density <- dnorm(x, sd = pi / 2) 

for (i in n) {
  T <- sqrt(i) * replicate(1000, empirical_median_from_cauchy(i))
  hist(T,
       main = paste("Histogramm von T mit n =", i),
       xlab = expression(sqrt(n) * T[n]),
       probability = TRUE,
       col = "lightblue",
       ylim = c(0, max(asymptotic_density)))
  lines(x, asymptotic_density,
        col = "red",
        lwd = 2)
  
}


par(mfrow = c(1, 1))