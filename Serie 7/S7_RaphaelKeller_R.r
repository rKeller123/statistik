##### Statistik Serie 7

### Raphael Keller
### 23-128-721

### Aufgabe 1

par(mfrow = c(2, 2), mar = c(2, 2, 2, 1), oma = c(1, 1, 1, 1))


f_1 <- function(x) { exp(-exp(-x)) * exp(-x) }
f_2 <- function(x) { 2 * x ^ (-3) }
f_3 <- function(x) { dcauchy(x) }

# these values have been calculated by hand
# expected_value <- c(0.577, 2, NA)
# median <- c(0.367, sqrt(2), 0)


curve(f_1, from = -3, to = 5, xlab = NA, ylab = NA, main = expression(f[1]))
abline(v = c(0.577, 0.367), col = c("red", "blue"))

curve(f_2, from = 1, to = 3, xlab = NA, ylab = NA, main = expression(f[2*";"~2]))
abline(v = c(2, sqrt(2)), col = c("red", "blue"))

curve(f_3, from = -3, to = 3, xlab = NA, ylab = NA, main = expression(f[3]))
abline(v = 0, col = "blue")

plot.new()
legend("center",
       c("Erwartungswert", "Median"),
       col = c("red", "blue"),
       lty = 1,
       bty = "n")

par(mfrow = c(1, 1))
