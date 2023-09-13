
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
   x = rpois(n = 50, lambda = 0.3), 
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
#' 
perf <- function(x, target = mean(x)) {
  data.frame(bias = mean(x - target),
             mabs = mean(abs(x - target)),
             mse = mean((x - target) ^ 2),
             rmse = sqrt(mean((x - target) ^ 2)))
  }

#' mlepois
#' @description: 
#' 
mlepois <- function(n_size, n_iter = 10) {
  ll <- map_dbl(1:n_iter, ~ fn(optimise, log_likelihood, 
                               interval = c(0, 100), 
                               x = fn(rpois, n = n_size, lambda = 0.3), 
                               maximum = TRUE)$maximum)
  data.frame(sample = paste0("n=", n_size), perf(ll, target = 0.3))
}

mlepois(50, n_iter = 100)
do.call(rbind, lapply(c(50, 100, 200), mlepois, n_iter = 100))

library(dplyr)
map(seq(50, 500, 50), mlepois, n_iter = 1000) %>% bind_rows

