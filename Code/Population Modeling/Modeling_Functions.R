# -----------------------------------------------------------------------------
# Population Modeling Function Plots
# -----------------------------------------------------------------------------

# Density Function
density <- tibble(N=c(0,50000,100000,150000,200000,250000,300000,350000,400000,450000,500000),
                  s1=c(0.99,0.98,0.88,0.75,0.6,0.5,0.35,0.20,0.1,0.05,0.01))
totdensity<-nls(s1~1/(1+exp((c*N)+b)),data=density,start=list(c=0.00001,b=-3.05))
tot_dens<-summary(totdensity)
c<-tot_dens$coefficients[1,1]
b<-tot_dens$coefficients[2,1]
N_seq<-seq(1,500000,100)
s<-1/(1+exp((c*N_seq)+b))
predicted_data <- tibble(N = N_seq, s = s)
ggplot() +
  geom_line(data = predicted_data, aes(x = N, y = s), color = "blue", size = 1) +
  labs(title = "Density-Dependent Survival Function",
       x = "N (total population)",
       y = "Survival Multiplier") +
  theme_minimal()

# Condition & Total Eggs
dita_c <- tibble(condition=c(0,0.3,0.6,1,1.3,1.6,2),totaleggs=c(1000,8000,16000,25000,40000,70000,100000))
condition <- nls(totaleggs~alpha*condition^beta,data=dita_c,start=list(alpha=10000,beta=2))
alpha_c <- summary(condition)$coefficients[1,1]
beta_c <- summary(condition)$coefficients[2,1]
cond <- seq(0,2,0.1)
eggz <- alpha_c*(cond^beta_c)
predicted_condition <- tibble(condition = cond, totaleggs = eggz)
ggplot() +
  geom_line(data = predicted_condition, aes(x = condition, y = totaleggs), color = "blue", size = 1) +
  labs(title = "Condition and Total Eggs Function",
       x = "Condition",
       y = "Total Eggs") +
  theme_minimal()


# Recruitment Function
dita_r <- tibble(totaleggs=c(1000,8000,16000,25000,40000,70000,100000),age1=c(1,2.5,4,7,10,10.2,10.3))
recruitment <- nls(age1~alpha*(totaleggs^beta),data=dita_r,start=list(alpha=1,beta=1))
alpha_r <- summary(recruitment)$coefficients[1,1]
beta_r<-summary(recruitment)$coefficients[2,1]
totaleggs_seq <- seq(min(dita_r$totaleggs), max(dita_r$totaleggs), length.out = 100)
age1_pred <- alpha_r * (totaleggs_seq^beta_r)
predicted_recruitment <- tibble(totaleggs = totaleggs_seq, age1 = age1_pred)
ggplot() +
  geom_line(data = predicted_recruitment, aes(x = totaleggs, y = age1), color = "blue", size = 1) +
  labs(title = "Recruitment Function",
       x = "Total Eggs",
       y = "Number of Age 1 Individuals") +
  theme_minimal()

# Condition Affects Survival
condsurv <- function(x, x0, r, k, off){
  k/(1 + exp(-r*(x - x0))) + off
}
cond_sens <- seq(0.1,1,length.out=10)
cond_surv_effect <- vector(length=length(cond_sens))
x = seq(0,1, length.out = 100)
y <- condsurv(x = x, r = 10, k = 0.9, x0 = 0.5, off = 0.1)
for (i in seq_along(cond_sens)) {
  index <- which.min(abs(x - cond_sens[i]))
  cond_surv_effect[i] <- y[index]
}
cond_surv_data <- tibble(condition = x, survival_probability = y)
ggplot(cond_surv_data, aes(x = condition, y = survival_probability)) +
  geom_line(color = "blue", size = 1) +
  labs(title = "Condition Affects Survival Function",
       x = "Condition",
       y = "Survival Probability Multiplier") +
  theme_minimal()
