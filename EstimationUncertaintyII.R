library(MASS)
library(tidyverse)
library(tidyquant)
library(lubridate)
library(Matrix)


# Defining function to calculate SRs of simulated tangent portfolios evaluated at true parameters mu and sigma
portfolio_performance <- function(N) {
  
  # Simulate returns and calculate efficient frontier
  sample <- simulate_returns(N)
  res <- frontier(sample)
  
  # Identiry efficient tangent portfolio and extract Sharpe Ratio
  SR <- res[which(res$SR==max(res$SR)),c("SR")]
  return(SR)
}

# Calculate Sharpe Ratios with N = 100
SRs <- data.frame(SR1=rep(0,250),SR2=rep(0,250))
for (i in 1:250) {
  SRs[i,"SR1"] <- portfolio_performance(N)
}

# Plot distribution of SRs along with SR based on true parameters (vertical line)
p5 <- ggplot() +
  geom_histogram(aes(x=SR1),SRs,color="rosybrown", fill="lightcoral",bins=20) +
  geom_vline(aes(xintercept=tangent$SR),color="black", linetype="dashed", size=1) + 
  theme_minimal() +
  labs(x = "Sharpe-ratio",y="")
if (plot_plots) p5

# Increase N to 1133 and compare to N = 100
for (i in 1:250) {
  SRs[i,"SR2"] <- portfolio_performance(nrow(FFIndustry))
}
p6 <- p5 + geom_histogram(aes(x=SR2),SRs,color="darkblue", fill="lightblue",bins=20)
if (plot_plots) p6

# Saving plots...
if (save_plots) {
  plots <- list(p1,p2,p3,p4,p5,p6)
  for (n in 1:length(plots)) {
    tiff(paste0("p",n,".tiff"), units="in", width=8, height=5, res=300)
    print(plots[[n]])
    dev.off()
  }
}