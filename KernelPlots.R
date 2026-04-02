#This is for the plots of the 6 example kernels
x <- seq(-3, 3, length.out = 3000)

#Defining Kernel Functions
K_rect <- function(x) 0.5 * (abs(x) <= 1)
K_tri  <- function(x) (1 - abs(x)) * (abs(x) <= 1)
K_epa  <- function(x) 0.75 * (1 - x^2) * (abs(x) <= 1)
K_bi   <- function(x) (15/16) * (1 - x^2)^2 * (abs(x) <= 1)
K_gauss <- function(x) dnorm(x)
K_silv <- function(x) {
  0.5 * exp(-abs(x)/sqrt(2)) *
    sin(abs(x)/sqrt(2) + pi/4)
}

#Plots
par(mfrow = c(2, 3), mar = c(4, 4, 2, 1))

plot(x, K_rect(x), type = "l", lwd = 1,
     main = "Rectangular", xlab = "t", ylab = "K(t)", xlim = c(-1.2,1.2))

plot(x, K_tri(x), type = "l", lwd = 1,
     main = "Triangular", xlab = "t", ylab = "K(t)", xlim = c(-1.2,1.2))

plot(x, K_epa(x), type = "l", lwd = 1,
     main = "Epanechnikov", xlab = "t", ylab = "K(t)", xlim = c(-1.2,1.2))

plot(x, K_bi(x), type = "l", lwd = 1,
     main = "Biweight", xlab = "t", ylab = "K(t)", xlim = c(-1.2,1.2))

plot(x, K_gauss(x), type = "l", lwd = 1,
     main = "Gaussian", xlab = "t", ylab = "K(t)", xlim = c(-2.5,2.5))

plot(x, K_silv(x), type = "l", lwd = 1,
     main = "Silverman", xlab = "t", ylab = "K(t)", xlim = c(-2.5,2.5))