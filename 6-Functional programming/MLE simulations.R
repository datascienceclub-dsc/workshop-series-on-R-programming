
# Initialization ====

#' log_likelihood
#' @description
#' 
log_likelihood <- function(lambda, x) {
  n <- length(x)
  (log(lambda) * sum(x)) - (n * lambda) - sum(lfactorial(x))
}

#' fn
#' @description Function factory
#' @examples
#' fn(runif, n = 50, min = 0, max = 1)
#' fn(rpois, n = 50, lambda = 0.3)
#' fn(log_likelihood, 0.3, x = c(2, 1, 3, 0, 2))
#' 
fn <- function(f, ...) f(...)

# Single iterations, fixed sample size ====
fn(optimise, 
   log_likelihood, 
   interval = c(0, 100), 
   x = fn(rpois, n = 50, lambda = 0.3), 
   maximum = TRUE)$maximum

# Single iterations, Varying sample size ====
#' << lambda is kept fixed to 0.3 >>
lambda_value <- 0.3
sample_size <- c(50, 100, 400) 
output <- list()

x_value <- list()
for (i in seq_along(sample_size)) {
  # i <- 1
  x_value <- fn(rpois, n = sample_size[i], lambda = lambda_value)
  output[[i]] <- fn(optimise, log_likelihood, 
                    interval = c(0, 100), 
                    x = x_value, 
                    maximum = TRUE)$maximum
  }
names(output) <- paste0("n=", sample_size)
output

# Multiple iterations, Varying sample size ====
#' << lambda is kept fixed to 0.3 >>
#' Taking mean of all iterations from a given sample size

## Base LOOP approach ====

lambda_value <- 0.3
sample_size <- c(50, 100, 400) 
n_iterations <- 100 # new parameter
output <- list()

x_value <- list()
output <- list()

for (i in seq_along(sample_size)) {
  
  lambda_optim <- list()
  for(j in 1:n_iterations) {
    x_value <- fn(rpois, n = sample_size[i], lambda = lambda_value)
    lambda_optim[[j]] <- fn(optimise, log_likelihood, 
                            interval = c(0, 100), 
                            x = x_value, 
                            maximum = TRUE)$maximum
  }
  lambda_optim <- unlist(lambda_optim)
  output[[i]] <- data.frame(sample = paste0("n=", sample_size[i]),
                            bias = mean(lambda_optim - lambda_value),
                            mabs = mean(abs(lambda_optim - lambda_value)),
                            mse = mean((lambda_optim - lambda_value) ^ 2))
  output[[i]]$rmse <- sqrt(output[[i]]$mse)
}

output
do.call(rbind, output)

## Base LAPPLY approach ====

### lapply for internal loop for j 
#' note that here we do not have parameters for the functional call,
#' it is just simulations
#' 
lapply(1:n_iterations, function(x) {
  fn(optimise, log_likelihood, 
     interval = c(0, 100), 
     x = fn(rpois, n = 50, lambda = lambda_value), 
     maximum = TRUE)$maximum
})

### lapply for final output 
output <- lapply(sample_size, function(nn) {
  
  lambda_optim <- unlist(lapply(1:n_iterations, function(x) {
    fn(optimise, log_likelihood, 
       interval = c(0, 100), 
       x = fn(rpois, n = nn, lambda = lambda_value), 
       maximum = TRUE)$maximum}))
  
  data.frame(sample = paste0("n=", nn),
             bias = mean(lambda_optim - lambda_value),
             mabs = mean(abs(lambda_optim - lambda_value)),
             mse = mean((lambda_optim - lambda_value) ^ 2),
             rmse = sqrt(mean((lambda_optim - lambda_value) ^ 2)))
  })

output
do.call(rbind, output)

## purrr MAP ====
library(purrr)

#' perf
#' #' @description performance measurement
#' @examples
#' perf(runif(100, 0, 1), 0.4)
#' perf(rpois(100, 0.4), 0.4)
#' 
perf <- function(x, target = mean(x)) {
  
  data.frame(bias = mean(x - target),
             mabs = mean(abs(x - target)),
             mse = mean((x - target) ^ 2),
             rmse = sqrt(mean((x - target) ^ 2)),
             target = target)
  }

#' mlepois
#' @description: 
#' 
mlepois <- function(n_size, n_iter = 10, lambda = 0.3) {
  ll <- map_dbl(1:n_iter, ~ fn(optimise, log_likelihood, 
                               interval = c(0, 100), 
                               x = fn(rpois, n = n_size, lambda = lambda), 
                               maximum = TRUE)$maximum)
  data.frame(sample = paste0("n=", n_size), perf(ll, target = lambda))
}

mlepois(n_size = 50, n_iter = 100, lambda = 0.3)
mlepois(n_size = 50, n_iter = 100, lambda = 0.4)

sample_size <- c(50, 100, 200)
do.call(rbind, lapply(sample_size, mlepois, n_iter = 100))

library(dplyr)
map(sample_size, mlepois, n_iter = 1000, lambda = 0.3) %>% bind_rows

## Varying lambda ====
params <- expand.grid(n_size = sample_size, lambda = c(0.2, 0.3, 0.4))
params
pmap(params, mlepois, n_iter = 100) %>% bind_rows()

# ------------------------------------------------------------------------------

# Generalizing MLE function ====
#' add possibility for making flexible the function to generate X 
#' and it's optimization function

#' mle
#' @description: finding...
#' 
#' @param n_size sample size
#' @param n_iter number of iterations
#' @param fn_rand function to generate random numbers
#' @param fn_optim function to optimize
#' @param target target value to compare
#' @param ... additional parameters to the functions
#' 
mle <- function(n_size, n_iter, fn_rand, fn_optim, target, ...) {
  
  ll <- map_dbl(1:n_iter, ~ fn(optimise, fn_optim, 
                               interval = c(0, 100), 
                               x = fn(fn_rand, n = n_size, ...), 
                               maximum = TRUE)$maximum)
  data.frame(sample = paste0("n=", n_size), perf(ll, target = target))
}

mle(n_size = 50, n_iter = 100, fn_rand = rpois, fn_optim = log_likelihood,
    target = 0.3, lambda = 0.3)

sample_size <- c(50, 100, 200)

library(dplyr)
map(sample_size, mle, n_iter = 100, 
    fn_rand = rpois, fn_optim = log_likelihood,
    target = 0.3, lambda = 0.3) %>%
  bind_rows

params <- expand.grid(n_size = sample_size, 
                      target = c(0.2, 0.3, 0.4),
                      lambda = 0.3,
                      n_iter = 100)
params
pmap(params, mle, fn_rand = rpois, fn_optim = log_likelihood) %>% bind_rows()
