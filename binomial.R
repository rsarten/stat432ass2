theta <- 0.2
x <- 4
n <- 10


bin_kernel <- function(theta, x, n) {
  x * log(theta) + (n - x) * log(1 - theta)
}

bin_multiple <- function(thetas, xvec, nvec) {
  sum(
    bin_kernel(thetas[1], xvec[1], nvec[1]),
    bin_kernel(thetas[2], xvec[2], nvec[2])
  )
}
bin_multiple(c(0.2, 0.2), xvec, nvec)

ntheta <- 21
thetavec <- seq(0.01, 0.99, length.out =  ntheta)
lmax <- bin_multiple(xvec/nvec, xvec, nvec)

library(dplyr)
lfuncmat <- array(apply(as.matrix(expand.grid(thetavec, thetavec)), 1, lfunc))
lfuncmat2 <- array(apply(as.matrix(expand.grid(thetavec, thetavec)), # work through expand.grid combinations
                         1, bin_multiple, xvec = xvec, nvec = nvec), 
                   dim = c(ntheta, ntheta)) # shape to ntheta*ntheta matrix

tidy_matrix <- lfuncmat2 %>% 
  as.data.frame() %>% 
  mutate(theta1 = 1:n()) %>% 
  tidyr::gather(key = "theta2", value = "value", V1:V21) %>% 
  mutate(theta1 = as.numeric(theta1),
         theta2 = stringr::str_remove(theta2, "[A-Z]"),
         theta2 = as.numeric(theta2))

ggplot(tidy_matrix, aes(theta1, theta2, z = value)) + geom_raster(aes(fill = value)) + geom_contour(bins = 20)

####################################################################################
## Numerical Optimation

x <- 3
n <- 10
llikeunfc <- function(theta, x, n) { 
  x * log(theta) + (n - x) * log(1 - theta) }

o1 <- optimise(llikeunfc, interval = c(0, 1), maximum = TRUE)
o1$maximum

o1 <- optim(par = 0.5, fn = llikeunfc, method = "L-BFGS-B",
            lower = 1e-05, upper = 1 - 1e-05, hessian = TRUE,
            control = list(fnscale = -1), x = 3, n = 10)

o1$par
o1$value

factorial(6)/6
factorial(5)
