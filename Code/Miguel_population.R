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
# sub-diagonal cells are survival probabilities
# bottom right corner is survival probability for age-4+ groups

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
p
p$age<-c("age1","age2","age3","age4+")
p%>%pivot_longer(-age,names_prefix="V",names_to="timestep",values_to="num")->p
p
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
