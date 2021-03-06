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


########################################################
## Contour Plot

llfunc <- function(par, n1, n2, n3) {
  N <- par[1]
  theta <- par[2]
  lfactorial(N) - lfactorial(N - n1 - n2 - n3) +
    sum(n1, n2, n3)*log(theta) + 
    (3*N - 3*n1 - 2*n2 - n3)*log(1 - theta)
}

n1 <- 82L
n2 <- 54L
n3 <- 23L
n_vals <- seq(from = n1+n2+n3, to = 260, length = 100)
theta_vals <- seq(from = 0.2, to = 0.7, length = 100)

combos <- expand.grid(n_vals, theta_vals)
surface <- apply(expand.grid(n_vals, theta_vals), 1, llfunc, n1 = n1, n2 = n2, n3 = n3) %>% round(2)
outcome <- cbind(combos, surface)
names(outcome) <- c("N", "theta", "value")
summary(surface)

ggplot(data = outcome, mapping = aes(N, theta)) + 
  geom_contour(aes(z = value),
                   breaks = seq(from = min(outcome$value), to = max(outcome$value), by = 12)) +
  geom_contour(aes(z = value,
                   colour = factor(..level.. == max(surface) - 1.92,
                                   levels = c(F, T),
                                   labels = c("Single 95% CI "))),
               breaks = max(surface) - 1.92) +
  geom_contour(aes(z = value,
                   colour = factor(..level.. == max(surface) - 3,
                                   levels = c(F, T),
                                   labels = c("Joint 95% CI "))),
               breaks = max(surface) - 3) +
  geom_point(mapping = aes(x = MLE[1], y = MLE[2]), colour = "darkblue") +
  # scale_colour_manual(values = c("black", "red")) + 
  labs(colour = "Of interest:") +
  theme_minimal()
