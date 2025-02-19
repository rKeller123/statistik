#### Serie 1 Statistik ####

# Raphael Keller
# 23-128-721

# 6 

# b
v <- 5:11
w <- setdiff(v, 8:10)

# c
x <- seq(-6, 6, .01)

# d
df <- c(2, 5, 10, 20)
plot(
  x, dt(x, df[1]),
  main = "Student- Verteilungen & standard Cauchy-Verteilung",
  ylab = "f(x)",
  type = "l",
  ylim = c(0, 0.4)
)
for (i in 2:4) {
  lines(x, dt(x, df[i]))
}
lines(x, dcauchy(x), col = "blue")

legend(-6, 0.4,
       c("Student-Dichten für df = 2, 5, 10, 20", "Standard Cauchy Dichte"),
       lty = 1,
       col = c("black", "blue"))

plot(
  x, pt(x, df[1]),
  main = "Student- Verteilungen & standard Cauchy-Verteilung",
  ylab = "F(x)",
  type = "l"
)
for (i in 2:4) {
  lines(x, pt(x, df[i]))
}
lines(x, pcauchy(x), col = "blue")

legend(-6, 1,
       c("Student-Verteilungen für df = 2, 5, 10, 20", "Standard Cauchy Verteilung"),
       lty = 1,
       col = c("black", "blue"))


# e

n <- 10
p <- 0.3

plot(pbinom(0:n, n, p), type = "s",
     main = "Binomialverteilung mit n = 10, p = 0.3",
     xlab = "x",
     ylab = NA)
points(dbinom(0:n, n, p), pch = 20, col = "darkgreen", cex = 1.5)

legend(1, 1,
       c("P[X <= x]", "P[X = x]"),
       col = c("black", "darkgreen"),
       lty = c(1, NA),
       pch = c(NA, 20))
