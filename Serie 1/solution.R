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


# e

plot(pbinom(x, 10, 0.3), type = "s")
