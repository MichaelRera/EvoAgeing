library(dplyr)
library(tidyr)
library(tidyverse)
library(ggplot2)
library(gridExtra)

deriv_2 <- function(f, x, y, dx){
  return((f(x+dx, y)-f(x, y))/dx)
}
deriv_3 <- function(f, x, y, z, dx){
  return((f(x+dx, y, z)-f(x, y, z))/dx)
}
F <- function(a, b){
  u = 0
  if(a == 0){u = b - 1}
  else{u = ((1/a)*(1-exp(-a*b)))-1}
  return(u)
}
H <- function(a, b, d){
  u = 0
  if(a == 0){ u = d - exp(d-b)}
  else{u = ((1-exp(-a*d))/a) + (exp(d)/(1+a))*(exp(-d*(1+a))-exp(-b*(1+a)))-1}
  return(u)
}
G <- function(a, d){
  u = 0
  if(a == 0){u = d - 1}
  else{u = ((1/a)*(1-exp(-a*d)))-1}
  return(u)
}
malthus_nl <- function(b, d, x_0, epsilon, delta){
  if(b<=d){
    u = x_0
    while(abs(F(u, b))>epsilon){
      u = u - (F(u, b)/deriv_2(F, u, b, delta))
    }
    return(u)
  } else {
    v = x_0
    while((abs(H(v,b,d))>epsilon)){
      v = v - (H(v, b, d)/deriv_3(H, v, b, d, delta))
    }
    return(v)
  }
}
malthus_l <- function(b, d, x_0, epsilon, delta){
  if(b<=d){
    u = x_0
    while(abs(F(u, b))>epsilon){
      u = u - (F(u, b) / deriv_2(F, u, b, delta))
    }
    return(u)
  } else {
    v = x_0
    while(abs(G(v, d))>epsilon){
      v = v - (G(v, d) / deriv_2(G, v, d, delta))
    }
    return(v)
  }
}
x_0 = 0.5; epsilon = 0.00001; delta = 0.000001; u = 0

###Malthus / pop
# surv <- read_csv(file.choose(), col_types = cols(Time = col_integer(), n_pop = col_integer()))
# surv <- mutate(surv,value = ifelse(pop_type=="Lansing",malthus_l(mean_b, mean_d, x_0, epsilon, delta),malthus_nl(mean_b, mean_d, x_0, epsilon, delta)))
# surv <- surv %>% mutate(pop_type = replace(pop_type, pop_type == "Lansing", "1"))
# surv <- surv %>% mutate(pop_type = replace(pop_type, pop_type == "noLansing", "0"))
# Malthus_pop <- ggplot(data = subset(surv, mean_d > 0), aes(Time, value, colour = as.logical(as.numeric(pop_type)))) +
#   #ggtitle("Evolution of fitness ", paste0("c",c,"p",p))+
#   #geom_jitter(alpha = 0.1, size = 0.85) +
#   xlab("Time of birth") +
#   ylab("Malthusian parameter") +
#   geom_jitter(alpha = 0.01, size = 0.05) +
#   #geom_point(alpha = 0.01, size = 0.05) +
#   geom_quantile(quantiles = c(0.1, 0.5, 0.9),method="rqss")+
#   theme(legend.position="none") +
#   ylim(0, 1) +
#   xlim(0, 125)

#Malthus / individual
Malthus_param <- function(data = data_all){
  data_all <- mutate(data, Malthus = ifelse(Lans==TRUE,malthus_l(b, d, x_0, epsilon, delta),malthus_nl(b, d, x_0, epsilon, delta)))
  return(data_all)
}


# set.seed(11031983)
Malthus_plot <- function(data = data_all){
  minidata_all <- sample_n(data %>% filter(lifespan>0),100000)
  malthus_plot <- ggplot(data = subset(minidata_all, d > 0), aes(t_birth, Malthus, colour = as.logical(Lans))) +
  #ggtitle("Evolution of fitness ", paste0("c",c,"p",p))+
  #geom_jitter(alpha = 0.1, size = 0.85) +
    xlab("Time of birth") +
    ylab("Malthusian parameter") +
  #   geom_smooth() +
  #   geom_jitter() +
  # #theme(legend.position="none") +
    ylim(0,1)
  #xlim(0,160)
 return(malthus_plot)
}
  


