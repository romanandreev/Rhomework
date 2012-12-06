graphics.off()
set.seed(239)
a = 0.3



calcAns <- function(Finv, N, R) {
  Fx <- c(runif(N, 0, 1), 1)
  x <- Finv(Fx)
  p <- order(Fx)
  x <- x[p]
  Fx <- Fx[p]      
  ans <- 0 
  startid <- which(Fx >= a)[1]
  if (startid == N + 1) {
    return (ans)
  }
  ans <- max(ans, R((startid:N) / N / Fx[(startid:N)] - 1))
  ans <- max(ans, R(((startid:N) - 1) / N / Fx[(startid:N)] - 1))
  ans <- max(ans, R((startid - 1) / N / a - 1))
  return (ans * sqrt(N))
}
calcDist <- function(Finv, N, M, R) {
  return (replicate(M, calcAns(Finv, N, R)))
}
Rabs <- function(x) return (abs(x))
Rplus <- function(x) return (x)
Rminus <- function(x) return (-x)


f1 <- function(x) {return (qgamma(x, 2, 1))}
f2 <- function(x) {return (qexp(x, 2))}

dist <- function (f, g) {
  list <- c(knots(f), knots(g), -100, 100)
  return (max(f(list) - g(list)))
}

lim <- ecdf(calcDist(f1, 1000, 1000, Rabs))
print("L1 distances:")
for (i in seq(5, 55, 5)) {
  val <- ecdf(calcDist(f1, i, 1000, Rabs))
  print(paste("n =", i, ", distance =", dist(val, lim)))
}
windows()
print("Number of trials equals to 1000, then error will be approx 1/sqrt(1000) approx 0.03")

plot(lim, col = "red", do.points=FALSE, xlab="x", ylab="F(x)",
     main=expression(paste("Ecdf of R"[n], " for Gamma distribution (2, 1) and different n (M = 1000, a = 0.3)")));
val <- ecdf(calcDist(f1, 500, 1000, Rabs))
plot(val, col = "blue", add = TRUE, do.points=FALSE);
legend("right", c("n = 1000", "n = 500"), bty="n",
       fill = c("red", "blue"))

windows()
curve(f1, xlim = c(0, 1), col = "red", xlab = "x", ylab = "q(x)", 
      main = "Quantile functions for two different distributions")
curve(f2, xlim = c(0, 1), col = "blue", add = TRUE)
legend("topleft", c("Gamma distribution (2, 1)", "Exponential distribution (2)"), bty="n",
       fill = c("red", "blue"))

windows()
plot(ecdf(calcDist(f1, 800, 1000, Rabs)), col = "red", do.points=FALSE, lwd = 2, xlab="x", ylab="F(x)", 
     main=expression(paste("Ecdf of R"[n], " for different distributions (n = 800, M = 1000, a = 0.3)")))
plot(ecdf(calcDist(f2, 800, 1000, Rabs)), add = TRUE, col = "blue", do.points=FALSE, lwd = 2)
legend("topleft", c("Gamma distribution (2, 1)", "Exponential distribution (2)"), bty="n",
       fill = c("red", "blue"))

windows()
plot(ecdf(calcDist(f1, 500, 1000, Rplus)), col = "red", do.points=FALSE, lwd = 2, , xlab="x", ylab="F(x)", 
     main=expression(paste("Ecdf for R"[n]^'+', " and its limit (n = 500, M = 1000, a = 0.3)")))
curve(pmax(2 * pnorm(x * sqrt(a / (1 - a))) - 1, 0) , col = "blue", lwd = 2, add = TRUE)

legend("right", c("Gamma distribution (2, 1)",  
      expression(paste(textstyle(max), bgroup("(", 2 %.% paste(Phi, bgroup("(", x %.% paste(sqrt(frac(a , 1 - a)), " ") ,")") - 1 , ", 0") ,")")  ))),
       fill = c("red", "blue"), bty="n")



