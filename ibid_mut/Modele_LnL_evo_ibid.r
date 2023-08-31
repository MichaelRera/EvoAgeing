#update.packages(ask = FALSE)

library("IBMPopSim")
library("ggplot2")
library("Rcpp")
library(dplyr)
library(gridExtra)
library(ggExtra)

### new attempt for xb - xd uniform generator without effet de bord
# Number of random numbers to generate
n <- 1000
# Generate uniform random numbers between 0 and 1
random_nums <- runif(n)
# Scale and shift to interval [-10, 10]
random_diffs <- (random_nums * 20) - 10
hist(random_diffs, main = "Uniformly Distributed Differences", xlab = "Value", ylab = "Frequency", breaks = 40)
hist(random_nums, main = "Uniformly Distributed Differences", xlab = "Value", ylab = "Frequency", breaks = 40)

# Generate initial population
N <- 10000  # Number of individuals in the initial population
# xb - xd is distributed uniformly on -10 ,10
#xd <- runif(N,0,10)
pop_init <- data.frame(
  "birth" = rep(0,N), 
  "death" = as.double(NA),
  "Lansing" = rep(c(FALSE,FALSE),N/2),
    "ib"=1,
    "i_d"=1,
#    "xb"= 10-xd, 
#    "xd" =  xd, 
    "xb" = 3,
    "xd" = 2,
    "lignee"=1:N
)
get_characteristics(pop_init)


head(pop_init)
#plot.ecdf(pop_init$xb-pop_init$xd)

# Events and model creation
 # There are 2 possible events :

# - Birth (with or without mutation)
# - Death

# Each event is characterized by its intensity and  kernel code, described below.

## Birth event with individual intensity

### Parameters



params_birth <- list("p"=0.1 , "var_mut"=0.05)

birth_event <- mk_event_individual( type = "birth",
  intensity_code = ' if (age(I,t) < I.xb)
                        result = I.ib; 
                    else 
                        result =0;',  # each individual  I can give birth at rate ib if its age is less than xb
  kernel_code = 'if (CUnif() < p){
                     newI.xb = max(0., CNorm(I.xb, var_mut));
                     newI.ib = max(0., CNorm(I.ib, var_mut));
                     newI.i_d = max(0., CNorm(I.i_d, var_mut));}
                 else{
                     newI.xb = I.xb;
                     newI.ib = I.ib;
                     newI.i_d = I.i_d;}
                if (I.Lansing & (age(I,t)> I.xd) & (age(I,t)<I.xb))
                     newI.xd =0;
                 else{
                    if (CUnif()<p)
                        newI.xd =max(0., CNorm(I.xd, var_mut));
                     else 
                        newI.xd =I.xd;}
                 newI.Lansing =I.Lansing;
                 newI.lignee =I.lignee;') 
# An individual I can give birth to an individual newI. The kernel code defines characteristics of individual newI
# Attention la manière dont est calculée le trait après mutation est un peu différente du code du Tristan

## Death event 
### parameters
params_death <- list("compet"= 0.0009)

## Deaths due to interactions
death_event1 <- mk_event_interaction(name='death1',
  type = "death",
  interaction_code = "result = compet;" 
)

## Deaths due to aging 
death_event2 <- mk_event_individual(name='death2', type="death",
                  intensity_code = ' if (age(I,t)>I.xd) result= I.i_d; 
                                     else result =0;')

# Model creation 
model <- mk_model(
  characteristics = get_characteristics(pop_init),
  events = list(birth_event, death_event1, death_event2),
  parameters = c(params_birth, params_death)

)
summary(model)

## Bounds for birth and death rates 
birth_intensity_max <- 10
interaction_fun_max <- params_death$compet
death2_max <- 10

T = 300 # Simulation end time 

sim_out <- popsim(model = model,
  population = pop_init,
  events_bounds = c('birth'=birth_intensity_max, 'death1'=interaction_fun_max,'death2'= death2_max),
  parameters = c(params_birth, params_death),
  time = T)

# Simulation with different parameters

#The model can be simulated with different parameters without being recompiled.


#sim_out$logs["duration_main_algorithm"]
#sim_out$logs

# Outputs

str(sim_out$population)

pop_out <- sim_out$population
head(pop_out)
tail(pop_out)


# Create a sampled dataset 
sample_data <- pop_out %>% sample_n(10000)
# Generate the ggplot
p0 <- ggplot(sample_data, aes(x = birth, alpha = 0.1)) + 
  geom_point(aes(y = ib, color = 'ib'), na.rm = TRUE) +
  geom_smooth(aes(y = ib,colour="green"))+
  geom_point(aes(y = i_d, color = 'id'), na.rm = TRUE) +
  geom_smooth(aes(y = i_d,colour="red"))+
  scale_color_manual(
    name = "",
    values = c("ib" = "#98FB98", "id" = "#FF8585")
  ) +
  scale_alpha(guide = 'none') +  # Hide alpha legend
  labs(
    x = "Time",
    y = "Value",
    title = "ib and id as a function of time",
    #subtitle = "Your Plot Subtitle",
    #caption = "Your Caption"
  ) +
  theme_minimal(base_size = 14) +
  theme(
    legend.position = "bottom",
    panel.grid.major = element_line(color = "white")
  )

p0_ <- ggplot(sample_data, aes(x = birth, alpha = 0.1)) + 
  geom_point(aes(y = xb, color = 'xb'), na.rm = TRUE) +
  geom_smooth(aes(y = xb,colour="green"))+
  geom_point(aes(y = xd, color = 'xd'), na.rm = TRUE) +
  geom_smooth(aes(y = xd,colour="red"))+
  scale_color_manual(
    name = "",
    values = c("xb" = "#98FB98", "xd" = "#FF8585")
  ) +
  scale_alpha(guide = 'none') +  # Hide alpha legend
  labs(
    x = "Time",
    y = "Value",
    title = "xb and xd as a function of time",
    #subtitle = "Your Plot Subtitle",
    #caption = "Your Caption"
  ) +
  theme_minimal(base_size = 14) +
  theme(
    legend.position = "bottom"
  )

#options(repr.plot.width=15, repr.plot.height=15)
p1<-ggplot(sample_data) + 
  geom_point(aes(x=xb,y=ib, alpha = 0.1, colour=birth))+ theme(legend.position = "none") 
p1<-ggMarginal(p1,type = "histogram", xparams = list(binwidth = 0.1)) 
p2<-ggplot(sample_data) + 
  geom_point(aes(x=xd,y=i_d, alpha = 0.1, colour=birth))+ theme(legend.position = "none") 
p2<-ggMarginal(p2,type = "histogram", xparams = list(binwidth = 0.1))


t0density <-  ggplot(pop_out %>% filter(birth==0), aes(x=xb-xd, color=Lansing, fill=Lansing, height=..scaled.., alpha = 0.1)) + 
                geom_density() + 
                geom_vline(xintercept = 0, color = "blue", linetype="dashed")  +
                geom_vline(xintercept = log(3)/2, color = "red", linetype="dashed") +
                xlim(min(pop_out$xb-pop_out$xd),max(pop_out$xb-pop_out$xd)) + 
                coord_flip() +
                theme(legend.position="none")

soldensity <- ggplot(pop_out %>% filter(birth>290), aes(x=xb-xd, color=Lansing, fill=Lansing, height=..scaled.., alpha = 0.1)) + 
                geom_density() + 
                geom_vline(xintercept = 0, color = "blue", linetype="dashed")  +
                geom_vline(xintercept = log(3)/2, color = "red", linetype="dashed") +
                xlim(min(pop_out$xb-pop_out$xd),max(pop_out$xb-pop_out$xd)) + 
                coord_flip() +
                theme(legend.position="none")

xbxd_evol <- ggplot() +
                geom_segment(data = pop_out %>% sample_n(length(sample_data$death)*10), aes(x = birth, xend = death, y = xb - xd, yend = xb - xd, alpha = 0.1, color = as.logical(Lansing)), na.rm = TRUE) +
                geom_segment(data = sample_data %>% filter(lignee == pop_out %>% filter(is.na(death)) %>% select(lignee) %>% unique() %>% sample(1)), aes(x = birth, xend = death, y = xb - xd, yend = xb - xd), color = "black", alpha=0.7) +
                xlab("Time") +
                ylab("xb - xd") +
                geom_hline(yintercept = log(3) / 2, color = "red", linetype = "dashed") +
                geom_hline(yintercept = 0, color = "blue", linetype = "dashed") +
                ylim(min(pop_out$xb - pop_out$xd), max(pop_out$xb - pop_out$xd)) +
                theme(legend.position = "none")

p = grid.arrange(t0density, xbxd_evol, soldensity, ncol=3, nrow = 1, widths=c(2,6,2))

#options(repr.plot.width = 20, repr.plot.height = 20, repr.plot.res = 100)
layout_matrix <- rbind(
  c(1, 2, 3, 4),
  c(5, 5, 5, 5))
grid.arrange(p0_, p0, p1, p2, p, layout_matrix=layout_matrix)




##################"#calculation of Malthusian parameters (Newton's method)
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
t=seq(0, round(max(pop_out$birth)))
pop_out_mod <- pop_out %>% rowwise() %>% mutate(Malthus = ifelse(Lansing==TRUE,malthus_l(xb, xd, x_0, epsilon, delta),malthus_nl(xb, xd, x_0, epsilon, delta)))




################################### TO REWRITE ##########################################"

###Calculating survival
fun = function(t){
    pop_t <- population_alive(pop_out_mod, t) 
    pop_t_L <- pop_t %>% filter(Lansing)
    pop_t_nL <- pop_t %>% filter(!Lansing)
    rbind(t, ifelse(pop_t_L$birth, pop_t_L %>% count(), 0), ifelse(pop_t_L$birth,pop_t_L[,7] %>% unlist() %>% as.numeric() %>% median(), 0), ifelse(pop_t_nL$birth,pop_t_nL%>% count(),0), ifelse(pop_t_nL$birth,pop_t_nL[,7]  %>% unlist() %>% as.numeric() %>% median(),0))
} 

surv_table <- as.data.frame(t(matrix(unlist(cbind(lapply(t, fun))),5)))
colnames(surv_table) <- c("time", "Lansing", "mean_Malthus_L","nonLansing", "mean_Malthus_nL")
max_pop <- surv_table %>% filter(time > 5) %>% select(Lansing, nonLansing) %>% max()
max_pop
head(surv_table)

 ggplot(surv_table) + 
  geom_line(aes(x=time, y=Lansing), color = "#00BDD0" )+
  geom_line(aes(x=time, y=nonLansing), color = "#F8766D" ) +
  ylim(0, max_pop)+
  theme(axis.text=element_text(size=12),
        axis.title=element_text(size=14,face="bold"))

 ggplot(surv_table) + 
  geom_line(aes(x=time, y=mean_Malthus_L), color = "#00BDD0" )+
  geom_line(aes(x=time, y=mean_Malthus_nL), color = "#F8766D" ) +
  #ylim(0, 1)+
  theme(axis.text=element_text(size=12),
        axis.title=element_text(size=14,face="bold"))


