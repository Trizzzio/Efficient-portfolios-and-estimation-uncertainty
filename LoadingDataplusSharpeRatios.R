library(tidyverse)
library(tidyquant)
library(lubridate)

#Load data 

FFIndustry = read.csv("./10_Industry_Portfolios.csv", skip=11) %>% 
  rename(date=X) %>%
  mutate(date=ymd(parse_date_time(date,"%Y%m")))%>%
  transmute(date=ymd(date),
            NoDur=as.numeric(NoDur),
            Durbl=as.numeric(Durbl),
            Manuf=as.numeric(Manuf),
            Enrgy=as.numeric(Enrgy),
            HiTec=as.numeric(HiTec),
            Telcm=as.numeric(Telcm),
            Shops=as.numeric(Shops),
            Hlth=as.numeric(Hlth),
            Utils=as.numeric(Utils),
            Other=as.numeric(Other)) 

#Compute sample average mean and variance covariance matrix of return. which portfolio has highes sharpe-ratio

returns = FFIndustry %>% select(-date) 

sigma <- returns %>%
  cov(use = "pairwise.complete.obs") # Compute return sample covariance matrix

mu <- returns %>%
  colMeans() %>%
  as.matrix()

ann_mu=12*mu

ann_sigma=12*sigma



#Calculate Sharpe Ratio

sharpe = mu/sqrt(diag(sigma)) %>% as.tibble(rownames=NA)%>%
  rename(SharpeRatio = value) 

ann_sharpe=sqrt(12)*mu/sqrt(diag(sigma)) %>% as.tibble(rownames=NA)%>%
  rename(SharpeRatio = value) 

