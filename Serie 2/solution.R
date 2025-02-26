#### Serie 2 Statistik ####
# Raphael Keller
# 23-128-721


# 4
# a)

p_norm <- function(x) {
  p <- 0.2316419
  b <- c(
    0.319381530,
    -0.356563782,
    1.781477937,
    -1.821255978,
    1.330274429
  )
  phi_x <- dnorm(x)
  t <- 1 / (1 + p * x)
  1 - phi_x * sapply(t, function(x) sum(b * x^(1:5)))
}

x <- seq(-5, 5, 0.1)
plot(x, pnorm(x),
     main = "Approximation Verteilungsfunktion der Standardnormalverteilung",
     xlab = "x",
     ylab = "PHI(x)",
     type = "l",
     ylim = c(0, 1))

lines(x, p_norm(x), col = "blue")
legend(-4, 0.9,
       c("pnorm", "p_norm"),
       lty = 1,
       col = c("black", "blue"))

# b

?pnorm

p_norm <- function(x, mean = 0, var = 1) {
  sd <-  sqrt(abs(var))
  p <- 0.2316419
  b <- c(
    0.319381530,
    -0.356563782,
    1.781477937,
    -1.821255978,
    1.330274429
  )
  x <- (x - mean) / sd
  phi_x <- dnorm(x)
  t <- 1 / (1 + p * x )
  1 - phi_x * sapply(t, function(x) sum(b * x^(1:5)))
}

plot(x, pnorm(x, mean = 3, sqrt(2)),
     main = "Approximation Verteilungsfunktion der Standardnormalverteilung",
     xlab = "x",
     ylab = "PHI(x)",
     type = "l",
     ylim = c(0, 1))

lines(x, p_norm(x, mean = 3, 2), col = "blue")
legend(-4, 0.9,
       c("pnorm", "p_norm"),
       lty = 1,
       col = c("black", "blue"))

# c
x <- seq(-4, 10, 0.1)
var <- c(1, 2, 10)
mean <- 3
cols <- seq_along(var)

plot(NA,
     main = ("Abschätzung PHI(x) für Mittelwert 3"),
     ylim = c(0, 1),
     xlim = range(x),
     xlab = "x",
     ylab = "PHI(x)")
abline(v = 3, col = "grey")

for (i in seq_along(var)) {
  lines(x, p_norm(x, mean, var[i]), col = cols[i])
}
legend(min(x), 0.9,
       c(paste("var:", var), "Mittelwert"),
       col = c(cols, "grey"),
       lty = 1)

# d

select_even <- function(x) {
  x[x %% 2 == 0]
}

select_even(1:20)











