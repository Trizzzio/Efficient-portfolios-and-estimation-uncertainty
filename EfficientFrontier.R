library(tidyverse)
library(tidyquant)
library(lubridate)
library(Matrix)

# Defining function to compute efficient frontier
compute_efficient_frontier <- function(sigma,mu) {
  
  # 1. Minimum variance portfolio using closed form solution
  N <- nrow(sigma)
  sigma_inv <- solve(sigma)
  iota <- matrix(rep(1,N),nrow=N)
  C <- (t(iota) %*% sigma_inv %*% iota)[1]
  
  w_mvp <- (sigma_inv %*% iota)/C
  
  # 2. Efficient portfolio with 2*w_mv'mu as desired return
  mu_bar <- 2*(t(w_mvp) %*% mu)[1]
  D <- (t(iota) %*% sigma_inv %*% mu)[1]
  E <- (t(mu) %*% sigma_inv %*% mu)[1]
  lambda_tilde <- 2*(mu_bar-D/C)/(E - D^2/C)
  
  w_eff <- w_mvp + lambda_tilde/2*((sigma_inv %*% mu) - D/C*(sigma_inv %*% iota))
  
  # 3. Two-mutual fund theorem to characterize the efficient frontier
  c <- seq(-0.1,1.2,by=0.01)
  init <- tibble(c=c,mu=0,sd=0,SR=0)
  
  # Calculating performance based on the true parameters
  for (i in 1:length(c)) {
    w <- c[i]*w_mvp + (1-c[i])*w_eff
    init$mu[i] <- (t(w) %*% mu_true)[1]
    init$sd[i] <- sqrt((t(w) %*% sigma_true %*% w)[1])
  }
  
  # Calculating Sharpe ratios
  init$SR <- init$mu/init$sd
  
  # Storing weights of mvp and eff for later calculations and tibble of efficient frontier
  return(list(w_mvp,w_eff,init))
}

# Using function to calculate efficient frontier
res <- compute_efficient_frontier(sigma_true,mu_true)

# Plotting the frontier
p1 <- ggplot(res[[3]], aes(x = sd, y = mu)) + 
  geom_point() + # Plot all sd/mu portfolio combinations 
  geom_point(data = res[[3]] %>% filter(c %in% c(0,1)), 
             color = "red",
             size = 4) + # locate the mvp and efficient portfolio
  geom_point(data = tibble(mu = mu_true, sd = sqrt(diag(sigma_true))), 
             aes(y = mu, x  = sd), color = "blue", size = 1) + # locate the individual assets 
  theme_minimal() # make the plot a bit nicer
if (plot_plots) p1

# Extracting weight (c) of mvp of the tangency portfolio, i.e. the one with the largest SR
tangent <- res[[3]][which(res[[3]]$SR==max(res[[3]]$SR)),c("c","SR")]
tangent <- cbind(tangent,matrix(round(tangent$c*res[[1]]+(1-tangent$c)*res[[2]],2),nrow=1))
print(paste0("SR of tangent portfolio: ",round(tangent$SR,2),""))
colnames(tangent) <- c(colnames(tangent)[1:2],rownames(res[[1]]))