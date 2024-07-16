# -----------------------------------------------------------------------------
# Sensitivity Analysis 1: Recruitment
# How does total recruitment affect spawning stock abundance & total population?
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

# adjust recruitment for each simulation: from 0.05 to 1 in 0.05 increments
recruit_sens <- seq(0.05,1,length.out=20)

# sensitivity loop
for(j in 1:length(recruit_sens)){
  
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
    A[2,1]<-mean(c(0.2,0.2,0.2,0.2*dens_eff))
    A[3,2]<-mean(c(0.7,0.7,0.7*dens_eff))
    A[4,3]<-mean(c(0.8,0.8*dens_eff))
    A[4,4]<-0.9*dens_eff
    
    # condition and total eggs function
    eggz_age3<-alpha_c*(1.75^beta_c)
    eggz_age4<-alpha_c*(2^beta_c)
    
    # recruitment function
    A[1,3]<-alpha_r*((eggz_age3*recruit_sens[j])^beta_r)
    A[1,4]<-alpha_r*((eggz_age4*recruit_sens[j])^beta_r)
    
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

# simulation 1: recruitment x 0.05
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

# simulation 20: recruitment x 1
sens_res[[20]]%>%
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
for(i in 1:20){
  sens_res[[i]]%>%
    filter(age%in%c("age3","age4+")&timestep==100)%>%
    select(num)%>%
    pull()%>%
    sum()->x
  ssa_res<-c(ssa_res,x)
}

ssa_res

tibble(recruitment_s=recruit_sens,ssa=ssa_res)%>%
  ggplot(aes(x=factor(recruitment_s),y=ssa))+
  geom_col()

# it looks like even a small change in recruitment (a 5% decrease) lowers the
# spawning stock abundance by over 36%! spawning stock abundance does not change 
# much between a 95% and 20% decrease in recruitment, however.

# calculating total pop at time step 100
pop_res<-c()
for(i in 1:20){
  sens_res[[i]]%>%
    filter(timestep==100)%>%
    select(num)%>%
    pull()%>%
    sum()->x
  pop_res<-c(pop_res,x)
}

tibble(recruitment_s=recruit_sens,pop=pop_res)%>%
  ggplot(aes(x=factor(recruitment_s),y=pop))+
  geom_col()

# similar results can be seen looking at the entire population, although the 
# total population always decreases as recruitment decreases by 5%.
# again, the largest difference occurs when recruitment drops by the initial 5%,
# resulting in a 37.1% decrease in the total population.

### ---------------------------------------------------------------------------
# when we add in a 50% drop in eggs after year 50, things get a little weird
### ---------------------------------------------------------------------------

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

# adjusted recruitment function/parameter (time step >= 50)
dita<-tibble(totaleggs=c(1000,8000,16000,25000,40000,70000,100000),age1=c(1,2.5,4,7,10,10.2,10.3)*0.5)
algo<-nls(age1~alpha*(totaleggs^beta),data=dita,start=list(alpha=1,beta=1))
algo_param<-summary(algo)
alphaz2<-algo_param$coefficients[1,1]
betaz2<-algo_param$coefficients[2,1]

# store results
sens_res <- list()

# adjust recruitment for each simulation: from 0.05 to 1 in 0.05 increments
recruit_sens <- seq(0.05,1,length.out=20)

# sensitivity loop
for(j in 1:length(recruit_sens)){
  
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
    A[2,1]<-mean(c(0.2,0.2,0.2,0.2*dens_eff))
    A[3,2]<-mean(c(0.7,0.7,0.7*dens_eff))
    A[4,3]<-mean(c(0.8,0.8*dens_eff))
    A[4,4]<-0.9*dens_eff
    
    # condition and total eggs function
    eggz_age3<-alpha_c*(1.75^beta_c)
    eggz_age4<-alpha_c*(2^beta_c)
    
    # recruitment function
    A[1,3]<-alpha_r*((eggz_age3*recruit_sens[j])^beta_r)
    A[1,4]<-alpha_r*((eggz_age4*recruit_sens[j])^beta_r)
    
    # from time steps 50 to 100, decrease total eggs by 50% through condition function
    # also adjusted parameters for recruitment relationship
    if(i>=50){
      
      # condition and total eggs function
      eggz_age3<-alpha_c*(1.2^beta_c)
      eggz_age4<-alpha_c*(1.4^beta_c)
      
      # recruitment function
      A[1,3]<-alphaz2*((eggz_age3*recruit_sens[j])^betaz2) 
      A[1,4]<-alphaz2*((eggz_age4*recruit_sens[j])^betaz2) 
    }
    
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

# simulation 1: recruitment x 0.05
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

# simulation 20: recruitment x 1
sens_res[[20]]%>%
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
for(i in 1:20){
  sens_res[[i]]%>%
    filter(age%in%c("age3","age4+")&timestep==100)%>%
    select(num)%>%
    pull()%>%
    sum()->x
  ssa_res<-c(ssa_res,x)
}

ssa_res

tibble(recruitment_s=recruit_sens,ssa=ssa_res)%>%
  ggplot(aes(x=factor(recruitment_s),y=ssa))+
  geom_col()

# spawning stock abundance peaks when recruitment is decreased by 50%. odd.
# although it stays relatively similar between a 70% decrease and 0% decrease.
# the greatest effect, obviously, is observed at a 95% decrease in recruitment,
# but the effect even isn't that drastic at a 90% decrease.
# density dependent effects?

# calculating total pop at time step 100
pop_res<-c()
for(i in 1:20){
  sens_res[[i]]%>%
    filter(timestep==100)%>%
    select(num)%>%
    pull()%>%
    sum()->x
  pop_res<-c(pop_res,x)
}

tibble(recruitment_s=recruit_sens,pop=pop_res)%>%
  ggplot(aes(x=factor(recruitment_s),y=pop))+
  geom_col()

# looking at the total population, a constant decrease can be seen, but the drop
# off from one 5% decrease to the next is relatively small until you reach about
# a 30% decrease.