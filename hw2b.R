set.seed(239)
a = 0.3

curve(pexp(x, 2), xlim = c(-3, 6), col = "blue")
curve(pgamma(x + 2, 2, 1), xlim = c(-3, 6), col = "red", add = TRUE)

calcAns <- function(Finv, N, R) {
  Fx <- runif(N, 0, 1)
  x <- Finv(Fx)
  p <- order(Fx)
  x <- x[p]
  Fx <- Fx[p]      
  ans <- 0 
  for (i in 1:N) {
    if (Fx[i] >= a) {
      ans <- max(ans, R(i / N / Fx[i] - 1))
    }
    if (i < N && Fx[i] < a && a < Fx[i + 1]) {
      ans <- max(ans, R(i / N / a - 1))
    }
  }
  return (ans * sqrt(N))
}
calcDist <- function(Finv, N, M, R) {
  return (replicate(M, calcAns(Finv, N, R)))
}
Ra <- function(x) {
  return (abs(x))
}

Rplus <- function(x) {
  return (x)
}

Rminus <- function(x) {
  return (-x)
}

f1 <- function(x) {return (qexp(x, 2))}
f2 <- function(x) {return (qgamma(x, 2, 1)) - 2}

#plot(ecdf(calc#Dist(f1, 400, 400, Ra)), col = "red", do.points=FALSE, lwd = 2, xlab="x", ylab="F(x)", main=expression(paste("Ecdf of R"[n], " for different F(x) (n = 400, a = 0.3)")))
#plot(ecdf(calc#Dist(f2, 400, 400, Ra)), add = TRUE, col = "blue", do.points=FALSE, lwd = 2)


plot(ecdf(calcDist(f1, 100, 3200, Rplus)), col = "red", do.points=FALSE, lwd = 2, , xlab="x", ylab="F(x)", main=expression(paste("Ecdf for R"[n]^'+', " and ")))
curve(pmax(2 * pnorm(x * sqrt(a / (1 - a))) - 1, 0) , col = "blue", lwd = 2, add = TRUE)



#plot(ecdf(calcDist(f1, 100, 100, Ra)), col = "red", do.points=FALSE, lwd = 2)
#plot(ecdf(calcDist(f1, 100, 200, Ra)), col = "green", do.points=FALSE, lwd = 2, add = TRUE)
#plot(ecdf(calcDist(f1, 100, 400, Ra)), col = "blue", do.points=FALSE, lwd = 2, add = TRUE)
#plot(ecdf(calcDist(f1, 100, 800, Ra)), col = "cyan", do.points=FALSE, lwd = 2, add = TRUE)
#plot(ecdf(calcDist(f1, 100, 1600, Ra)), col = "magenta", do.points=FALSE, lwd = 2, add = TRUE)
#plot(ecdf(calcDist(f1, 100, 3200, Ra)), col = "orange", do.points=FALSE, lwd = 2, add = TRUE)



