# -----------------------------------------------------------------------------
# Sensitivity Analysis 2: Condition (with an addition effect on survival)
# If we assume condition impacts survival, how do the spawning stock abundance
# and total population change?
# -----------------------------------------------------------------------------
library(tidyverse)

# A matrix
# transition matrix
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

# condition function
dita_c <- tibble(condition=c(0,0.3,0.6,1,1.3,1.6,2),totaleggs=c(1000,8000,16000,25000,40000,70000,100000))
condition <- nls(totaleggs~alpha*condition^beta,data=dita_c,start=list(alpha=10000,beta=2))
alpha_c <- summary(condition)$coefficients[1,1]
beta_c <- summary(condition)$coefficients[2,1]
cond <- seq(0,2,0.1)
eggz <- alpha_c*(cond^beta_c)

# store results
sens_res <- list()

# adjust condition for each simulation
cond_sens<-seq(0.5,1.5,length.out=11)
cond_sens

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
    
    # total population at timestep i-1
    N<-sum(population[,i-1])
    
    # density-dependent survival function for total population
    dens_eff<-1/(1+exp((c*N)+b))
    
    # multiply survival parameters by density effect
    # density effect is greater on older individuals
    A[2,1]<-mean(c(0.2,0.2,0.2,0.2*dens_eff*cond_sens[j]))
    A[3,2]<-mean(c(0.7,0.7,0.7*dens_eff*cond_sens[j]))
    A[4,3]<-mean(c(0.8,0.8*dens_eff*cond_sens[j]))
    A[4,4]<-0.9*dens_eff*cond_sens[j]
    
    # conditon and total eggs function
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

# plot some simulations

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
for(i in 1:11){
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
  geom_col()
