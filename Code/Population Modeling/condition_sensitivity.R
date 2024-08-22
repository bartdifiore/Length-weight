# -----------------------------------------------------------------------------
# Sensitivity Analysis 1: Condition
# How do changes in condition affect SSA and total population?
# -----------------------------------------------------------------------------
library(tidyverse)
# A matrix - transition matrix
A<-matrix(c(0,0,4,10,
            0.2,0,0,0,
            0,0.7,0,0,
            0,0,0.8,0.9),nrow=4,byrow=T)

# Density Function
density <- tibble(N=c(0,50000,100000,150000,200000,250000,300000,350000,400000,450000,500000),
                  s1=c(0.99,0.98,0.88,0.75,0.6,0.5,0.35,0.20,0.1,0.05,0.01))
totdensity<-nls(s1~1/(1+exp((c*N)+b)),data=density,start=list(c=0.00001,b=-3.05))
tot_dens<-summary(totdensity)
c<-tot_dens$coefficients[1,1]
b<-tot_dens$coefficients[2,1]
N<-seq(1,500000,100)
s<-1/(1+exp((c*N)+b))

# Recruitment Function
dita_r <- tibble(totaleggs=c(1000,8000,16000,25000,40000,70000,100000),age1=c(1,2.5,4,7,10,10.2,10.3))
recruitment <- nls(age1~alpha*(totaleggs^beta),data=dita_r,start=list(alpha=1,beta=1))
alpha_r <- summary(recruitment)$coefficients[1,1]
beta_r<-summary(recruitment)$coefficients[2,1]

# Condition Function
dita_c <- tibble(condition=c(0,0.3,0.6,1,1.3,1.6,2),totaleggs=c(1000,8000,16000,25000,40000,70000,100000))
condition <- nls(totaleggs~alpha*condition^beta,data=dita_c,start=list(alpha=10000,beta=2))
alpha_c <- summary(condition)$coefficients[1,1]
beta_c <- summary(condition)$coefficients[2,1]
cond <- seq(0,2,0.1)
eggz <- alpha_c*(cond^beta_c)

# list to store results
sens_res<-list()

# Adjust condition between 0.1 and 1.0
cond_sens<-seq(0.1,1,length.out=10)

# Sensitivity loop
for(j in 1:length(cond_sens)){
  
  # A matrix - Transition matrix
  A<-matrix(c(0,0,4,10,
              0.2,0,0,0,
              0,0.7,0,0,
              0,0,0.8,0.9),nrow=4,byrow=T)
  
  # Matrix to hold simulation
  population<-matrix(0,nrow=4,ncol=101)
  
  # Seed population with 10 age-4 individuals
  population[,1]<-c(0,0,0,10)
  
  # Simulate the population for 100 years
  for(i in 2:101){
    
    # Total population at time step i-1
    N<-sum(population[,i-1])
    
    # Density-dependent survival function for total population
    dens_eff<-1/(1+exp((c*N)+b))
    
    # Multiply survival parameters by density effect- greater on older individuals
    A[2,1]<-mean(c(0.2,0.2,0.2,0.2*dens_eff))
    A[3,2]<-mean(c(0.7,0.7,0.7*dens_eff))
    A[4,3]<-mean(c(0.8,0.8*dens_eff))
    A[4,4]<-0.9*dens_eff
    
    # Condition and total eggs function (affected by sensitivity)
    eggz_age3<-alpha_c*((cond_sens[j]*1.75)^beta_c)
    eggz_age4<-alpha_c*((cond_sens[j]*2)^beta_c)
    
    # Recruitment function
    A[1,3]<-alpha_r*(eggz_age3^beta_r)
    A[1,4]<-alpha_r*(eggz_age4^beta_r)
    
    population[,i]<-A%*%population[,i-1]
  }
  # Prep data for plotting
  p<-as_tibble(population)
  p$age<-c("age1","age2","age3","age4+")
  p%>%pivot_longer(-age,names_prefix="V",names_to="timestep",values_to="num")->p
  p$timestep<-as.numeric(p$timestep)
  
  # Save as a list
  sens_res[[j]]<-p
}

# Plotting some simulations:

# simulation 1: condition x 0.1
cond.1 <- sens_res[[1]]%>%
  filter(timestep<101)%>%
  group_by(timestep)%>%
  mutate(numint=as.integer(num))%>%
  ggplot(aes(x=timestep,y=numint,fill=age))+
  geom_col(width=1,color="gray50")+
  facet_grid(rows=vars(age),scales="free_y")+
  scale_fill_manual(values=c("#1b9e77","#d95f02","#7570b3","#e7298a"))+
  labs(x="Timestep",y="Number of Individuals",
       title = "Population Over Time with Condition x 0.1")+
  theme_bw()+
  theme(legend.position="none",
        strip.background=element_blank(),
        strip.text.y=element_text(angle=0),
        text=element_text(size=15))
cond.1
ggsave(filename="Cond01.png", plot=cond.1, height=6, width=10)

# simulation 10: condition x 1
cond1 <- sens_res[[10]]%>%
  filter(timestep<101)%>%
  group_by(timestep)%>%
  mutate(numint=as.integer(num))%>%
  ggplot(aes(x=timestep,y=numint,fill=age))+
  geom_col(width=1,color="gray50")+
  facet_grid(rows=vars(age),scales="free_y")+
  scale_fill_manual(values=c("#1b9e77","#d95f02","#7570b3","#e7298a"))+
  labs(x="Timestep",y="Number of Individuals",
       title = "Population Over Time with Condition x 1")+
  theme_bw()+
  theme(legend.position="none",
        strip.background=element_blank(),
        strip.text.y=element_text(angle=0),
        text=element_text(size=15))
ggsave(filename="Cond1.png", plot=cond1, height=6, width=10)


# Calculate spawning stock abundance at time step 100 for all sensitivities
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
ssa_res[10] / ssa_res[9]

ssa_dat <- tibble(cond_s=cond_sens,ssa=ssa_res)
ggplot(ssa_dat, aes(x=factor(cond_s),y=ssa))+
  geom_col()+
  labs(title="Condition Sensitivity Analysis: SSA",
       x="Condition Sensitivity Factor",
       y="Spawning Stock Abundance (SSA)")+
  theme_bw() +
  theme(text=element_text(size=15)) -> ssa_plot
ggsave(filename="CondSensSSA.png", plot=ssa_plot, height=6, width=10)

# Calculate total population at time step 100 for all sensitivities
pop_res<-c()
for(i in 1:10){
  sens_res[[i]]%>%
    filter(timestep==100)%>%
    dplyr::select(num)%>%
    pull()%>%
    sum()->x
  pop_res<-c(pop_res,x)
}
pop_res[10]/pop_res[9]

pop_dat <- tibble(cond_s=cond_sens,pop=pop_res)
ggplot(pop_dat, aes(x=factor(cond_s),y=pop))+
  geom_col()+
  labs(title="Condition Sensitivity Analysis: Total Population",
       x="Condition Sensitivity Factor",
       y="Total Population")+
  theme_bw()  +
  theme(text=element_text(size=15)) -> pop_plot
pop_plot
ggsave(filename="CondSensPop.png", plot=pop_plot, height=6, width=10)
