
# Problem statement ====
#' How can I incorporate  two optimisation methods in map_dbl for the same x
#' Two optimization techniques need the same x and is used as an input. 
#' How can I modify the function?

# Probable solution ====

#' You can execute optimization function twice in the existing function. 
#' But carefully return two sets of values obtained from the 2 optimizer. 
#' You can try something like this

#' mlepois
#' @description: 
#' 
mlepois <- function(n_size, n_iter = 10, lambda = 0.3) {
  
  xx <- map(1:n_iter, ~ fn(rpois, n = n_size, lambda = lambda))
  
  l1 <- map_dbl(xx, ~ fn(optimise, log_likelihood, interval = c(0, 100), 
                         x = .x, maximum = TRUE)$maximum)
  l2 <- map_dbl(xx, ~ fn(optimise, log_likelihood, interval = c(0, 100), 
                         x = .x, maximum = TRUE)$maximum)
  
  rbind.data.frame(
    data.frame(sample = paste0("n=", n_size), optim = 1, perf(l1, target = lambda)),
    data.frame(sample = paste0("n=", n_size), optim = 2, perf(l2, target = lambda))
  )
}

mlepois(n_size = 50, n_iter = 100, lambda = 0.3)
mlepois(n_size = 50, n_iter = 100, lambda = 0.4)

sample_size <- c(50, 100, 200)
do.call(rbind, lapply(sample_size, mlepois, n_iter = 100))
