library(MASS)


# Setting seed number
set.seed(1)

# Defining function to simulate hypothetical return sample of sample size N
simulate_returns <- function(N) {
  return(mvrnorm(n = N, mu=mu_true, Sigma=sigma_true))
}

# Wrapping everything in a function to repeat steps from PART 1
frontier <- function(sample) {
  mu <- apply(sample,2,function(x) mean(x))
  sigma <- cov(sample)
  
  # Only outputting the tibble
  res <- compute_efficient_frontier(sigma,mu)[[3]]
  return(res)
}

# Drawing sample of returns and calculating new frontier
N <- 100
sample <- simulate_returns(N)
new_frontier <- frontier(sample)[,c("mu","sd")]

# Plotting to compare with true frontier (black)
p2 <- ggplot() + 
  geom_point(aes(x=sd,y=mu),res[[3]],color = "black") +
  geom_point(aes(x=sd,y=mu),new_frontier,color = "blue") +
  theme_minimal()
if (plot_plots) p2

# Defining function to simulate and plot 250 efficient frontiers along with the true frontier
plot_frontiers <- function(N) {
  
  # Unique colors in R excluding white
  R_colors <- colors()[-1]
  
  # Initialize plot using true frontier
  init <- ggplot() + geom_point(aes(x=sd, y=mu),res[[3]][,c("mu","sd")],color = "black")+ ylim(0,2)+xlim(0,20)
  
  # Simulating return samples and calculating efficient frontiers 250 times
  for (i in 1:250) {
    x <- frontier(simulate_returns(N))[,c("mu","sd")]
    init <- init + geom_point(aes(x=sd,y=mu),x,color = R_colors[i])
  }
  
  # Applying minimal theme
  init <- init + theme_minimal()
  
  # Returning ggplot2 object such the figure is plotted when plot_frontiers is called
  return(init)
}

# Plotting with sample size N = 100
p3 <- plot_frontiers(N)
if (plot_plots) p3

# Plotting with sample size N = 1133 (initial sample size)
p4 <- plot_frontiers(nrow(data))
if (plot_plots) p4