library(tidyverse)

# age structured matrix population model
# four age-groups
# age-1 and age-2 are juveniles
# age-3 and age-4+ are adults

# transition matrix
A<-matrix(c(0,0,4,10,
            0.2,0,0,0,
            0,0.7,0,0,
            0,0,0.8,0.9),nrow=4,byrow=T)
A

# first row represents numbers of offspring by age-3 and age-4 groups
# subdiagonal cells are survival probabilities
# bottom right corner is survival probability for age-4+ grops

# matrix to hold simulation
population<-matrix(0,nrow=4,ncol=100)

# seed population with 10 age-4 individuals
population[,1]<-c(0,0,0,10)

# simulate the population for 100 years
for(i in 2:100){
  population[,i]<-A%*%population[,i-1] # matrix multiplication
}

# prep data for plotting
p<-as_tibble(population)
p$age<-c("age1","age2","age3","age4+")
p%>%pivot_longer(-age,names_prefix="V",names_to="timestep",values_to="num")->p
p$timestep<-as.numeric(p$timestep)

p%>%
  group_by(timestep)%>%
  mutate(numint=as.integer(num))%>%
  ggplot(aes(x=timestep,y=numint,fill=age))+
  geom_col(width=1,color="gray50")+
  facet_grid(rows=vars(age),scales="free_y")+
  scale_fill_manual(values=c("#1b9e77","#d95f02","#7570b3","#e7298a"))+
  labs(x="timestep",y="number of individuals")+
  theme_bw()+
  theme(legend.position="none",strip.background=element_blank(),strip.text.y=element_text(angle=0))

# population growth rate
# if > 1, pop is growing
# if < 1, pop is shrinking
# if = 1, pop is stable
popgrow<-as.numeric(eigen(A)$values[1])

# stable age distribution
# what proportions of each age class make up the total population
w<-as.numeric(eigen(A)$vectors[,1])
w<-w/sum(w)
w

########################################################################################################

# density-dependent survival function for age-1 
c<-0.0001
N<-1:20000
b<-0.05
s1<-1/(1+exp((c*N)+b))

tibble(N,s1)%>%
  ggplot(aes(x=N,y=s1))+
  geom_line()+
  labs(x="Number of age-1 individuals",y="age-1 survival",subtitle="Density-dependent survival function")

# matrix to hold simulation
population<-matrix(0,nrow=4,ncol=100)

# seed population with 10 age-4 individuals
population[,1]<-c(0,0,0,10)

# simulate the population for 100 years
for(i in 2:100){
  N<-population[1,i-1]
  s1<-1/(1+exp((c*N)+b))
  A[2,1]<-s1
  population[,i]<-A%*%population[,i-1]
}

# prep data for plotting
p<-as_tibble(population)
p$age<-c("age1","age2","age3","age4+")
p%>%pivot_longer(-age,names_prefix="V",names_to="timestep",values_to="num")->p
p$timestep<-as.numeric(p$timestep)

p%>%
  group_by(timestep)%>%
  mutate(numint=as.integer(num))%>%
  ggplot(aes(x=timestep,y=numint,fill=age))+
  geom_col(width=1,color="gray50")+
  facet_grid(rows=vars(age),scales="free_y")+
  scale_fill_manual(values=c("#1b9e77","#d95f02","#7570b3","#e7298a"))+
  labs(x="timestep",y="number of individuals")+
  theme_bw()+
  theme(legend.position="none",strip.background=element_blank(),strip.text.y=element_text(angle=0))

# population growth rate
popgrow<-as.numeric(eigen(A)$values[1])
popgrow

# stable age distribution
w<-as.numeric(eigen(A)$vectors[,1])
w<-w/sum(w)
w

########################################################################################################

library(tidyverse)

# age structured matrix population model
# four age-groups
# age-1 and age-2 are juveniles
# age-3 and age-4+ are adults

# transition matrix
A<-matrix(c(0,0,4,10,
            0.2,0,0,0,
            0,0.7,0,0,
            0,0,0.8,0.9),nrow=4,byrow=T)

# Recruitment Function

# up unitl now we assume that adult stages 3 and 4 contribute 4 and 10 age-1 individuals each timestep
A

# lets explicity code a recruitment function that models egg survival into age-1

# create data set that suggests a logarithmic relationship
dita<-tibble(totaleggs=c(1000,8000,16000,25000,40000,70000,100000),age1=c(1,2.5,4,7,10,10.2,10.3))

dita

# plot the points
dita%>%
  ggplot(aes(x=totaleggs,y=age1))+
  geom_point()

# solve using the nls() function nonlinear least squares algorithm
algo<-nls(age1~alpha*(totaleggs^beta),data=dita,start=list(alpha=1,beta=1))

# here are our alpha and beta parameters
algo_param<-summary(algo)

# using our new parameters, lets build the recruitment function
alphaz<-algo_param$coefficients[1,1]
betaz<-algo_param$coefficients[2,1]

# sequence of total eggs from 1 to 100,000 by 10
totegg<-seq(1,100000,10)

# recruitment function
age1s<-alphaz*(totegg^betaz)

# plot recruitment function
tibble(totegg,age1s)%>%
  ggplot(aes(x=totegg,y=age1s))+
  geom_line()+
  labs(x="Total Eggs",y="Age-1 Recruitment",subtitle="Recruitment Function")

# now lets plug recruitment function into the population model
alphaz*(10000^betaz) # 10,000 eggs ~ 4 age-1 individuals
alphaz*(75000^betaz) # 75,000 eggs ~ 10 age-1 individuals

# matrix to hold simulation
population<-matrix(0,nrow=4,ncol=100)

# seed population with 10 age-4 individuals
population[,1]<-c(0,0,0,10)

# simulate the population for 100 years
for(i in 2:100){
  N<-population[1,i-1]
  s1<-1/(1+exp((c*N)+b))
  A[2,1]<-s1
  # recruitment function
  A[1,3]<-alphaz*(10000^betaz) # 10,000 eggs ~ 4 age-1 individuals
  A[1,4]<-alphaz*(75000^betaz) # 75,000 eggs ~ 10 age-1 individuals
  population[,i]<-A%*%population[,i-1]
}

# prep data for plotting
p<-as_tibble(population)
p$age<-c("age1","age2","age3","age4+")
p%>%pivot_longer(-age,names_prefix="V",names_to="timestep",values_to="num")->p
p$timestep<-as.numeric(p$timestep)

p%>%
  group_by(timestep)%>%
  mutate(numint=as.integer(num))%>%
  ggplot(aes(x=timestep,y=numint,fill=age))+
  geom_col(width=1,color="gray50")+
  facet_grid(rows=vars(age),scales="free_y")+
  scale_fill_manual(values=c("#1b9e77","#d95f02","#7570b3","#e7298a"))+
  labs(x="timestep",y="number of individuals")+
  theme_bw()+
  theme(legend.position="none",strip.background=element_blank(),strip.text.y=element_text(angle=0))

# next step is to relate size to eggs (fecundity)
# rather than having fixed egg numbers like 10,000 and 75,000
# these numbers would increase or decrease depending on changes in size

# matrix to hold simulation
population<-matrix(0,nrow=4,ncol=100)

# seed population with 10 age-4 individuals
population[,1]<-c(0,0,0,10)

# simulate the population for 100 years
for(i in 2:100){
  N<-population[1,i-1]
  s1<-1/(1+exp((c*N)+b))
  A[2,1]<-s1
  # recruitment function
  A[1,3]<-alphaz*(10000^betaz) # 10,000 eggs ~ 4 age-1 individuals
  A[1,4]<-alphaz*(75000^betaz) # 75,000 eggs ~ 10 age-1 individuals
  
  # from timesteps 60 to 100, decrease total eggs by 50%
  if(i>=60){
    A[1,3]<-alphaz*((10000*0.5)^betaz)
    A[1,4]<-alphaz*((75000*0.5)^betaz)
  }
  
  population[,i]<-A%*%population[,i-1]
}

# prep data for plotting
p<-as_tibble(population)
p$age<-c("age1","age2","age3","age4+")
p%>%pivot_longer(-age,names_prefix="V",names_to="timestep",values_to="num")->p
p$timestep<-as.numeric(p$timestep)

p%>%
  group_by(timestep)%>%
  mutate(numint=as.integer(num))%>%
  ggplot(aes(x=timestep,y=numint,fill=age))+
  geom_col(width=1,color="gray50")+
  facet_grid(rows=vars(age),scales="free_y")+
  scale_fill_manual(values=c("#1b9e77","#d95f02","#7570b3","#e7298a"))+
  labs(x="timestep",y="number of individuals")+
  theme_bw()+
  theme(legend.position="none",strip.background=element_blank(),strip.text.y=element_text(angle=0))

# population increases?! things got weird!
# less eggs -> less age-1 -> less density-dependence -> more survive to age-2?
# should perhaps have density-dependence on each age?

# create data that suggests impact of high density
tibble(N=c(0,50000,100000,150000,200000,250000,300000,350000,400000,450000,500000),
       s1=c(0.99,0.98,0.88,0.75,0.6,0.5,0.35,0.20,0.1,0.05,0.01))->d

d%>%
  ggplot(aes(x=N,y=s1))+
  geom_point()
# as N increases, density effect approaches zero

# fit parameters to data
totdensity<-nls(s1~1/(1+exp((c*N)+b)),data=d,start=list(c=0.00001,b=-3.05))
tot_dens<-summary(totdensity)
c<-tot_dens$coefficients[1,1]
b<-tot_dens$coefficients[2,1]
N<-seq(1,500000,100)
s<-1/(1+exp((c*N)+b))

# density-dependent survival function for total population
tibble(N,s)%>%
  ggplot(aes(x=N,y=s))+
  geom_line()+
  labs(x="Total Population",y="Density Effect")+
  theme_bw()

##########################################################################################

# transition matrix
A<-matrix(c(0,0,4,10,
            0.2,0,0,0,
            0,0.7,0,0,
            0,0,0.8,0.9),nrow=4,byrow=T)

as.numeric(eigen(A)$values[1])

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
  A[2,1]<-mean(c(0.2,0.2,0.2,0.2*dens_eff))
  A[3,2]<-mean(c(0.7,0.7,0.7*dens_eff))
  A[4,3]<-mean(c(0.8,0.8*dens_eff))
  A[4,4]<-0.9*dens_eff
  
  # recruitment function
  A[1,3]<-alphaz*(10000^betaz) # 10,000 eggs ~ 4 age-1 individuals
  A[1,4]<-alphaz*(75000^betaz) # 75,000 eggs ~ 10 age-1 individuals
  
  # from timesteps 50 to 100, decrease total eggs by 50%
  if(i>=50){
    A[1,3]<-alphaz*((10000*0.5)^betaz) #A[1,3]<-0.0318*((10000*0.5)^0.452) 
    A[1,4]<-alphaz*((75000*0.5)^betaz) #A[1,4]<-0.0318*((75000*0.5)^0.452) 
  }
  
  population[,i]<-A%*%population[,i-1]
}

# prep data for plotting
p<-as_tibble(population)
p$age<-c("age1","age2","age3","age4+")
p%>%pivot_longer(-age,names_prefix="V",names_to="timestep",values_to="num")->p
p$timestep<-as.numeric(p$timestep)

p%>%
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

# ok. huh. age1 decreases, age4+ increases.
# total population decreases and size structure finds new equilibrium distribution...
# next question...would we expect the recruitment relationship to also change?
# does less eggs also mean less good eggs?

#######################################################################################

# reduce recruitment by half
# create data set that suggests a logarithmic relationship
dita<-tibble(totaleggs=c(1000,8000,16000,25000,40000,70000,100000),age1=c(1,2.5,4,7,10,10.2,10.3)*0.5)

# plot the points
dita%>%
  ggplot(aes(x=totaleggs,y=age1))+
  geom_point()

# solve using the nls() function nonlinear least squares algorithm
algo<-nls(age1~alpha*(totaleggs^beta),data=dita,start=list(alpha=1,beta=1))

# here are our alpha and beta parameters
algo_param<-summary(algo)

# using our new parameters, lets build the recruitment function
alphaz2<-algo_param$coefficients[1,1]
betaz2<-algo_param$coefficients[2,1]

# transition matrix
A<-matrix(c(0,0,4,10,
            0.2,0,0,0,
            0,0.7,0,0,
            0,0,0.8,0.9),nrow=4,byrow=T)

as.numeric(eigen(A)$values[1])

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
  
  # recruitment function
  A[1,3]<-alphaz*(10000^betaz) # 10,000 eggs ~ 4 age-1 individuals
  A[1,4]<-alphaz*(75000^betaz) # 75,000 eggs ~ 10 age-1 individuals
  
  # from timesteps 50 to 100, decrease total eggs by 50%
  # also adjusted parameters for recruitment relationship
  if(i>=50){
    A[1,3]<-alphaz2*((10000*0.5)^betaz2) 
    A[1,4]<-alphaz2*((75000*0.5)^betaz2) 
  }
  
  population[,i]<-A%*%population[,i-1]
}

# prep data for plotting
p<-as_tibble(population)
p$age<-c("age1","age2","age3","age4+")
p%>%pivot_longer(-age,names_prefix="V",names_to="timestep",values_to="num")->p
p$timestep<-as.numeric(p$timestep)

p%>%
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

# dramatic change!

#######################################################################################

# next step is to link condition to eggs
# Fulton condition factor, K
# K = 100 * weight/length^3
# K < 1, skinny fish
# K > 1, fat fish

dita<-tibble(condition=c(0,0.3,0.6,1,1.3,1.6,2),totaleggs=c(1000,8000,16000,25000,40000,70000,100000))

# plot the points
dita%>%
  ggplot(aes(x=condition,y=totaleggs))+
  geom_point()

# solve using the nls() function nonlinear least squares algorithm
algo<-nls(totaleggs~alpha*condition^beta,data=dita,start=list(alpha=10000,beta=2))

# here are our alpha and beta parameters
algo_param<-summary(algo)

# using our new parameters, lets build the recruitment function
alphaz3<-algo_param$coefficients[1,1]
betaz3<-algo_param$coefficients[2,1]

# sequence of condition from 0 to 2 by 0.1
cond<-seq(0,2,0.1)

# conditon and total eggs function
eggz<-alphaz3*(cond^betaz3)

# plot condition and total eggs function
tibble(cond,eggz)%>%
  ggplot(aes(x=cond,y=eggz))+
  geom_line()+
  labs(x="Condition",y="Total Eggs",subtitle="Condition and Total Eggs Function")+
  theme_bw()

#######################################################################################

# transition matrix
A<-matrix(c(0,0,4,10,
            0.2,0,0,0,
            0,0.7,0,0,
            0,0,0.8,0.9),nrow=4,byrow=T)

as.numeric(eigen(A)$values[1])

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
  A[2,1]<-mean(c(0.2,0.2,0.2,0.2*dens_eff))
  A[3,2]<-mean(c(0.7,0.7,0.7*dens_eff))
  A[4,3]<-mean(c(0.8,0.8*dens_eff))
  A[4,4]<-0.9*dens_eff
  
  # condition and total eggs function
  eggz_age3<-alphaz3*(1.75^betaz3)
  eggz_age4<-alphaz3*(2^betaz3)
  
  # recruitment function
  A[1,3]<-alphaz*(eggz_age3^betaz) # 10,000 eggs ~ 4 age-1 individuals
  A[1,4]<-alphaz*(eggz_age4^betaz) # 75,000 eggs ~ 10 age-1 individuals
  
  # from timesteps 50 to 100, decrease total eggs by 50% through condition function
  # also adjusted parameters for recruitment relationship
  if(i>=50){
    
    # conditon and total eggs function
    eggz_age3<-alphaz3*(1.2^betaz3)
    eggz_age4<-alphaz3*(1.4^betaz3)
    
    # recruitment function
    A[1,3]<-alphaz2*(eggz_age3^betaz2) 
    A[1,4]<-alphaz2*(eggz_age4^betaz2) 
  }
  
  population[,i]<-A%*%population[,i-1]
}

A

# prep data for plotting
p<-as_tibble(population)
p$age<-c("age1","age2","age3","age4+")
p%>%pivot_longer(-age,names_prefix="V",names_to="timestep",values_to="num")->p
p$timestep<-as.numeric(p$timestep)

p%>%
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

#######################################################################################

# sensitivity analysis

# choose a metric to measure sensitivity...total population, just adults, size structure?
# metric should be based on a question...?
# how do changes in condtion affect total spawning stock abundance?

# adjust condition parameter by a certain percentage
# run simulation
# store simulation outputs from each run
# compare and plot changes

# store results
sens_res<-list()

# adjust condition for each simulation
cond_sens<-seq(0.1,1,length.out=10)

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
    A[2,1]<-mean(c(0.2,0.2,0.2,0.2*dens_eff))
    A[3,2]<-mean(c(0.7,0.7,0.7*dens_eff))
    A[4,3]<-mean(c(0.8,0.8*dens_eff))
    A[4,4]<-0.9*dens_eff
    
    # conditon and total eggs function
    eggz_age3<-alphaz3*((cond_sens[j]*1.75)^betaz3)
    eggz_age4<-alphaz3*((cond_sens[j]*2)^betaz3)
    
    # recruitment function
    A[1,3]<-alphaz*(eggz_age3^betaz)
    A[1,4]<-alphaz*(eggz_age4^betaz)
    
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

# simulation 1: condition x 0.1
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

# simulation 10: condition x 1
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

sens_res[[1]]%>%
  filter(age%in%c("age3","age4+")&timestep==100)%>%
  select(num)%>%
  pull()%>%
  sum()->ssa_sim1

sens_res[[10]]%>%
  filter(age%in%c("age3","age4+")&timestep==100)%>%
  select(num)%>%
  pull()%>%
  sum()->ssa_sim10

ssa_res<-c()
for(i in 1:10){
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

