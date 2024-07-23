# -----------------------------------------------------------------------------
# Sensitivity Analysis 2: Fecundity
# How do changes in fecundity affect SSA and total population?
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

# condition function
dita_c <- tibble(condition=c(0,0.3,0.6,1,1.3,1.6,2),totaleggs=c(1000,8000,16000,25000,40000,70000,100000))
condition <- nls(totaleggs~alpha*condition^beta,data=dita_c,start=list(alpha=10000,beta=2))
alpha_c <- summary(condition)$coefficients[1,1]
beta_c <- summary(condition)$coefficients[2,1]
cond <- seq(0,2,0.1)
eggz <- alpha_c*(cond^beta_c)

# store results
sens_res <- list()

# adjust recruitment for each simulation: from 0.05 to 1 in 0.05 increments
fecundity_sens <- seq(0.1,1,length.out=10)

# sensitivity loop - fecundity
for(j in 1:length(fecundity_sens)){
  
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
    
    # multiply survival parameters by density effect - greater on older fish
    A[2,1]<-mean(c(0.2,0.2,0.2,0.2*dens_eff))
    A[3,2]<-mean(c(0.7,0.7,0.7*dens_eff))
    A[4,3]<-mean(c(0.8,0.8*dens_eff))
    A[4,4]<-0.9*dens_eff
    
    # condition and total eggs function
    eggz_age3<-alpha_c*(1.75^beta_c)
    eggz_age4<-alpha_c*(2^beta_c)
    
    # recruitment function
    A[1,3]<-alpha_r*((eggz_age3*fecundity_sens[j])^beta_r)
    A[1,4]<-alpha_r*((eggz_age4*fecundity_sens[j])^beta_r)
    
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

# simulation 1: fecundity x 0.1
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

# simulation 10: fecundity x 1
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

# calculate spawning stock abundance at time step 100
ssa_res<-c()
for(i in 1:10){
  sens_res[[i]]%>%
    filter(age%in%c("age3","age4+")&timestep==100)%>%
    dplyr::select(num)%>%
    pull()%>%
    sum()->x
  ssa_res<-c(ssa_res,x)
}
ssa_res
min(ssa_res)
max(ssa_res)

tibble(fecundity_s=fecundity_sens,ssa=ssa_res)%>%
  ggplot(aes(x=factor(fecundity_s),y=ssa))+
  geom_col()+
  labs(title="Fecundity Sensitivity Analysis: SSA", 
       x="Fecundity Sensitivity Factor", 
       y="Spawning Stock Abundance (SSA)")+
  theme_bw()

# calculating total pop at time step 100
pop_res<-c()
for(i in 1:10){
  sens_res[[i]]%>%
    filter(timestep==100)%>%
    dplyr::select(num)%>%
    pull()%>%
    sum()->x
  pop_res<-c(pop_res,x)
}
pop_res
min(pop_res)
max(pop_res)

tibble(fecundity_s=fecundity_sens,pop=pop_res)%>%
  ggplot(aes(x=factor(fecundity_s),y=pop))+
  geom_col()+
  labs(title="Fecundity Sensitivity Analysis: Total Population", 
       x="Fecundity Sensitivity Factor", 
       y="Total Population")+
  theme_bw()

