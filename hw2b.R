graphics.off()
set.seed(239)
a = 0.3

calcAns <- function(Finv, N, R, ...) { 
  Fx <- c(runif(N, 0, 1), 1)
  x <- Finv(Fx, ...)
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

replicate <- function(count, f, ...) {
  g <- function (x, ...) {
    return (f(...))
  }  
  return (sapply(integer(count), g, ...))
}

calcSample <- function(M, ...) {  
  return (replicate(M, calcAns, ...))
}

Rabs <- function(x) return (abs(x))
Rplus <- function(x) return (x)
Rminus <- function(x) return (-x)


dist <- function (f, g) {
  list <- c(knots(f), knots(g), -100, 100)
  return (max(f(list) - g(list)))
}

checkAll <- function(Finv, ...) {
  lim <- ecdf(calcSample(M = 1000, Finv = Finv, N = 1000, R = Rabs, ...))
  print("L_inf distances:")
  for (i in seq(5, 55, 5)) {
    val <- ecdf(calcSample(M = 1000, Finv = Finv, N = i, R = Rabs, ...))
    print(paste("n =", i, ", distance =", dist(val, lim)))
  }
  print("Number of trials equals to 1000, then error will be approx 1/sqrt(1000) approx 0.03")
  
  windows()
  plot(lim, col = "red", do.points=FALSE, xlab="x", ylab="F(x)",
       main=expression(paste("Ecdf of R"[n], " for Gamma distribution (2, 1) and different n (M = 1000, a = 0.3)")));
  val <- ecdf(calcSample(M = 1000, Finv = Finv, N = 500, R = Rabs, ...))
  plot(val, col = "blue", add = TRUE, do.points=FALSE);
  legend("right", c("n = 1000", "n = 500"), bty="n",
         fill = c("red", "blue"))
  
  windows()
  curve(Finv(x, shape = 2, rate = 1), xlim = c(0, 1), col = "red", xlab = "x", ylab = "q(x)", 
        main = "Quantile functions for two different distributions")
  curve(qexp(x, rate = 2), xlim = c(0, 1), col = "blue", add = TRUE)
  legend("topleft", c("Gamma distribution (2, 1)", "Exponential distribution (2)"), bty="n",
         fill = c("red", "blue"))
  
  windows()
  plot(ecdf(calcSample(Finv = Finv, N = 800, M = 1000, R = Rabs, ...)), 
       col = "red", do.points=FALSE, lwd = 2, xlab="x", ylab="F(x)", 
       main=expression(paste("Ecdf of R"[n], " for different distributions (n = 800, M = 1000, a = 0.3)")))
  plot(ecdf(calcSample(Finv = qexp, rate = 2, N = 800, M = 1000, R = Rabs)), 
       add = TRUE, col = "blue", do.points=FALSE, lwd = 2)
  legend("topleft", c("Gamma distribution (2, 1)", "Exponential distribution (2)"), bty="n",
         fill = c("red", "blue"))
  
  windows()
  plot(ecdf(calcSample(M = 1000, Finv = Finv, N = 500, R = Rplus, ...)), 
       col = "red", do.points=FALSE, lwd = 2, , xlab="x", ylab="F(x)", 
       main=expression(paste("Ecdf for R"[n]^'+', " and its limit (n = 500, M = 1000, a = 0.3)")))
  curve(pmax(2 * pnorm(x * sqrt(a / (1 - a))) - 1, 0) , col = "blue", lwd = 2, add = TRUE)
  
  legend("right", c("Gamma distribution (2, 1)",  
                    expression(paste(textstyle(max), bgroup("(", 2 %.% paste(Phi, bgroup("(", x %.% paste(sqrt(frac(a , 1 - a)), " ") ,")") - 1 , ", 0") ,")")  ))),
         fill = c("red", "blue"), bty="n")  
}
checkAll(Finv = qgamma, shape = 2, rate = 1)
