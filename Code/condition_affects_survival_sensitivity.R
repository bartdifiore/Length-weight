# -----------------------------------------------------------------------------
# Sensitivity Analysis 2: Condition (with an addition effect on survival)
# If we assume condition impacts survival, how do the spawning stock abundance
# and total population change?
# -----------------------------------------------------------------------------
library(tidyverse)

# A matrix - transition matrix
A<-matrix(c(0,0,4,10,
            0.2,0,0,0,
            0,0.7,0,0,
            0,0,0.8,0.9),nrow=4,byrow=T)

# density function
density <- tibble(N=c(0,50000,100000,150000,200000,250000,300000,350000,400000,450000,500000),
                  s1=c(0.99,0.98,0.88,0.75,0.6,0.5,0.35,0.20,0.1,0.05,0.01))
totdensity<-nls(s1~1/(1+exp((c*N)+b)),data=density,start=list(c=0.00001,b=-3.05))
tot_dens<-summary(totdensity)
c<-tot_dens$coefficients[1,1]
b<-tot_dens$coefficients[2,1]
N<-seq(1,500000,100)
s<-1/(1+exp((c*N)+b))

# recruitment function
dita_r <- tibble(totaleggs=c(1000,8000,16000,25000,40000,70000,100000),age1=c(1,2.5,4,7,10,10.2,10.3))
recruitment <- nls(age1~alpha*(totaleggs^beta),data=dita_r,start=list(alpha=1,beta=1))
alpha_r <- summary(recruitment)$coefficients[1,1]
beta_r<-summary(recruitment)$coefficients[2,1]

# condition & total eggs function
dita_c <- tibble(condition=c(0,0.3,0.6,1,1.3,1.6,2),
                 totaleggs=c(1000,8000,16000,25000,40000,70000,100000))
condition <- nls(totaleggs~alpha*condition^beta,data=dita_c,start=list(alpha=10000,beta=2))
alpha_c <- summary(condition)$coefficients[1,1]
beta_c <- summary(condition)$coefficients[2,1]
cond <- seq(0,2,0.1)
eggz <- alpha_c*(cond^beta_c)

# condition and survival function (polynomial regression)
dita_cs <- tibble(condition = c(0, 0.2, 0.3, 0.6, 0.7, 1, 1.3, 1.6, 2), 
                  survival = c(0, 0, 0.05, 0.35, 0.5, 0.8, 0.9, 0.93, 0.95))
ggplot(data = dita_cs, aes(x=condition, y=survival)) + 
  geom_point() + 
  labs(title = "Condition and Survival") +
  theme_bw()
poly_model <- lm(survival ~ poly(condition, 4, raw = TRUE), data = dita_cs)
alpha_cs <- summary(poly_model)$coefficients[1, 1]
beta_cs1 <- summary(poly_model)$coefficients[2, 1]
beta_cs2 <- summary(poly_model)$coefficients[3, 1]
beta_cs3 <- summary(poly_model)$coefficients[4, 1]
beta_cs4 <- summary(poly_model)$coefficients[5, 1]
condition_seq <- seq(0, 2, 0.05)
pred_surv <- alpha_cs + beta_cs1 * condition_seq + beta_cs2 * condition_seq^2 + 
  beta_cs3 * condition_seq^3 + beta_cs4 * condition_seq^4
pred_surv <- pmax(pmin(pred_surv, 0.9), 0)
pred_data <- tibble(condition = condition_seq, survival = pred_surv)
ggplot(data = pred_data, aes(x = condition, y = survival)) +
  geom_line(color = "blue") +
  labs(title = "Condition and Survival Function") +
  theme_minimal()

# store results
sens_res <- list()

# adjust condition & survival for each simulation
cond_sens <- seq(0.3,1,length.out=8)
surv_prob <- vector(length=length(cond_sens))
for (i in seq_along(cond_sens)) {
  x <- alpha_cs + beta_cs1 * cond_sens[i] + beta_cs2 * cond_sens[i]^2 + 
    beta_cs3 * cond_sens[i]^3 + beta_cs4 * cond_sens[i]^4
  surv_prob[i] <- x
}
cond_sens
surv_prob

# sensitivity loop
for(j in 1:length(cond_sens)){
  
  # transition matrix
  A<-matrix(c(0,0,4,10,
              0.2,0,0,0,
              0,0.7,0,0,
              0,0,0.8,0.9),nrow=4,byrow=T)
  
  # matrix to hold simulation
  population<-matrix(0,nrow=4,ncol=101)
  
  # seed population with 10 age-4 individuals
  population[,1]<-c(0,0,0,10)
  
  # simulate the population for 100 years
  for(i in 2:101){
    
    # total population at time step i-1
    N<-sum(population[,i-1])
    
    # density-dependent survival function for total population
    dens_eff<-1/(1+exp((c*N)+b))
    
    # multiply survival parameters by density effect
    # density effect is greater on older individuals
    # add in survival probability change due to condition
    A[2,1]<-mean(c(0.2,0.2,0.2,0.2*dens_eff*cond_sens[j]*surv_prob[j]))
    A[3,2]<-mean(c(0.7,0.7,0.7*dens_eff*cond_sens[j]*surv_prob[j]))
    A[4,3]<-mean(c(0.8,0.8*dens_eff*cond_sens[j]*surv_prob[j]))
    A[4,4]<-0.9*dens_eff*cond_sens[j]*surv_prob[j]
    
    # condition and total eggs function
    eggz_age3<-alpha_c*((cond_sens[j]*1.75)^beta_c)
    eggz_age4<-alpha_c*((cond_sens[j]*2)^beta_c)
    
    # recruitment function
    A[1,3]<-alpha_r*(eggz_age3^beta_r)
    A[1,4]<-alpha_r*(eggz_age4^beta_r)
    
    population[,i]<-A%*%population[,i-1]
  }
  
  # prep data for plotting
  p<-as_tibble(population)
  p$age<-c("age1","age2","age3","age4+")
  p%>%pivot_longer(-age,names_prefix="V",names_to="timestep",values_to="num")->p
  p$timestep<-as.numeric(p$timestep)
  
  # save as a list
  sens_res[[j]]<-p
  
}

# simulation 1: condition x 0.5
sens_res[[1]]%>%
  filter(timestep<101)%>%
  group_by(timestep)%>%
  mutate(numint=as.integer(num))%>%
  ggplot(aes(x=timestep,y=numint,fill=age))+
  geom_col(width=1,color="gray50")+
  facet_grid(rows=vars(age),scales="free_y")+
  scale_fill_manual(values=c("#1b9e77","#d95f02","#7570b3","#e7298a"))+
  labs(x="timestep",y="number of individuals")+
  theme_bw()+
  theme(legend.position="none",strip.background=element_blank(),strip.text.y=element_text(angle=0))

# simulation 10: condition x 2
sens_res[[10]]%>%
  filter(timestep<101)%>%
  group_by(timestep)%>%
  mutate(numint=as.integer(num))%>%
  ggplot(aes(x=timestep,y=numint,fill=age))+
  geom_col(width=1,color="gray50")+
  facet_grid(rows=vars(age),scales="free_y")+
  scale_fill_manual(values=c("#1b9e77","#d95f02","#7570b3","#e7298a"))+
  labs(x="timestep",y="number of individuals")+
  theme_bw()+
  theme(legend.position="none",strip.background=element_blank(),strip.text.y=element_text(angle=0))

# calculate spawning stock abundance at timestep 100
ssa_res<-c()
for(i in 1:8){
  sens_res[[i]]%>%
    filter(age%in%c("age3","age4+")&timestep==100)%>%
    select(num)%>%
    pull()%>%
    sum()->x
  ssa_res<-c(ssa_res,x)
}

ssa_res

tibble(cond_s=cond_sens,ssa=ssa_res)%>%
  ggplot(aes(x=factor(cond_s),y=ssa))+
  geom_col()+
  labs("Sensitivity of ")
