##### PART II: SV model

# Set dataset
data <- sv
data1 <- as.data.frame(data)

#data transformation
y <- as.numeric(data1$GBPUSD)
y <- y/100

###question a
#plot the data
#yt is the first difference of the lagged exchange rate between pound sterling and US dollar.
plot(y, type = "l", ylab="", xlab = "", main = "First difference log exchange rate ?/$")
abline(h=0)

#descriptive stats
var(y)
summary(y)
#descr(y)

###question b
#x_t = log(y_t - mu)^2
mu <- mean(y)
x <- log((y - mu)^2)

plot(x, type = "l", ylab="", xlab = "", main="Transformed data")

###question c
#########loglikelihood##############
###for the SV model###
#phi, omega and sigma2_eta need to be estimated
#sigma_e is constant at pi^2/2

#initialisation
x <- as.data.frame(x)
sigma2_e <- (pi)^2 / 2
phi <- 0.9950 #taken from DK book like the assignment notes suggest
sigma2_eta <- (1 - phi^2)*(var(x$x) - pi^2/2)
omega <- (1 - phi)*(mean(x$x) + 1.27)

######likelihood
loglikelihood<-function(x, par){
  z <- 1
  d <- -1.27
  H <- (pi)^2 / 2
  t <- exp(par[1])/(1 + exp(par[1]))#phi
  c <- par[2]#omega
  Q <- exp(par[3])#sigma2_eta
  R <- 1
  
  n<-nrow(data)
  output_matrix<-data.frame(matrix(ncol = 5, nrow = n))
  colnames(output_matrix)<-c("v_t","k_t","f_t","a_t","p_t")
  output_matrix$a_t[1]<-omega / (1 - t)
  output_matrix$p_t[1]<- sigma2_eta / (1 - t^2)
  for (i in 1:(n-1)){
    output_matrix$v_t[i]<-x$x[i]-z*output_matrix$a_t[i] - d
    output_matrix$f_t[i]<-z^2*output_matrix$p_t[i]+ H
    output_matrix$k_t[i]<-(t*output_matrix$p_t[i]*z)/output_matrix$f_t[i]
    output_matrix$a_t[i+1]<-t*output_matrix$a_t[i]+output_matrix$k_t[i]*output_matrix$v_t[i]+ c
    output_matrix$p_t[i+1]<-t^2*output_matrix$p_t[i] + R^2*Q - output_matrix$k_t[i]^2*output_matrix$f_t[i]
  }
  output_matrix$v_t[n]<-x$x[n]-z*output_matrix$a_t[n] - d
  output_matrix$f_t[n]<-z^2*output_matrix$p_t[n]+ H
  output_matrix$k_t[n]<-(t*output_matrix$p_t[n]*z)/output_matrix$f_t[n]
  
  f_t <- output_matrix$f_t
  v_t <- output_matrix$v_t
  
  #likelihood
  sum<-0
  for (i in 1:length(f_t)){
    sum<-sum+(log(abs(f_t[i]))+v_t[i]^2/(f_t[i]))
  }
  likelihood<-(-0.5*(length(f_t)+1)*log(2*pi))-0.5*sum
  
  return(likelihood)
}

#optimization
par_ini <- c(log(phi/(1-phi)),omega,log(sigma2_eta))
est <- optim(par = par_ini, fn=function(par)-loglikelihood(x, par), method = "BFGS")

#output optimization
phi_hat <- exp(est$par[1])/(1+exp(est$par[1]))
omega_hat <- est$par[2]
sigma2_eta_hat <- exp(est$par[3])

psi_hat <- c(phi_hat, omega_hat, sigma2_eta_hat)
cat("The parameter estimates are:")
round(psi_hat,4)

cat("The log-likelihood value is:")
-est$value

###question d
####Kalman filter#######
kalman_filter<-function(x, phi, omega, sigma2_eta){
  z <- 1
  d <- -1.27
  H <- (pi)^2 / 2
  t <- phi
  c <- omega
  Q <- sigma2_eta
  R <- 1
  
  n<-nrow(data)
  output_matrix<-data.frame(matrix(ncol = 5, nrow = n))
  colnames(output_matrix)<-c("v_t","k_t","f_t","a_t","p_t")
  output_matrix$a_t[1]<-omega / (1 - t)
  output_matrix$p_t[1]<- sigma2_eta / (1 - t^2)
  for (i in 1:(n-1)){
    output_matrix$v_t[i]<-x$x[i]-z*output_matrix$a_t[i] - d
    output_matrix$f_t[i]<-z^2*output_matrix$p_t[i]+ H
    output_matrix$k_t[i]<-(t*output_matrix$p_t[i]*z)/output_matrix$f_t[i]
    output_matrix$a_t[i+1]<-t*output_matrix$a_t[i]+output_matrix$k_t[i]*output_matrix$v_t[i]+ c
    output_matrix$p_t[i+1]<-t^2*output_matrix$p_t[i] + R^2*Q - output_matrix$k_t[i]^2*output_matrix$f_t[i]
  }
  output_matrix$v_t[n]<-x$x[n]-z*output_matrix$a_t[n] - d
  output_matrix$f_t[n]<-z^2*output_matrix$p_t[n]+ H
  output_matrix$k_t[n]<-(t*output_matrix$p_t[n]*z)/output_matrix$f_t[n]
  return(output_matrix)
}

#output of kalman filter
output<-kalman_filter(x, phi_hat, omega_hat, sigma2_eta_hat)

f_t<-as.numeric(output$f_t)
v_t<-as.numeric(output$v_t)
k_t<-output$k_t
a_t<-output$a_t
p_t<-output$p_t

###Kalman smoother######
smoother<-function(phi,f_t,k_t,a_t,v_t,p_t){
n<-length(f_t)
n_2<-length(f_t)+1
output_2<-data.frame(matrix(0,ncol =4, nrow = n_2))
colnames(output_2)<-c("r_t","V_t","alpha_t","N_t")
for (i in n_2:2){
  output_2$r_t[i-1]<-f_t[i-1]^(-1)*v_t[i-1]+(phi-k_t[i-1])*output_2$r_t[i]
  output_2$N_t[i-1]<-f_t[i-1]^(-1)+(phi-k_t[i-1])^2*output_2$N_t[i]
}
for (i in 2:n_2){
  output_2$alpha_t[i]<-a_t[i-1]+p_t[i-1]*output_2$r_t[i-1]
  output_2$V_t[i]<-p_t[i-1]-p_t[i-1]^2*output_2$N_t[i-1]
}
return(output_2)
}

#output of kalman smoother
output_2<-smoother(phi_hat,f_t,k_t,a_t,v_t,p_t)

r_t <- output_2$r_t[-101]
N_t <- output_2$N_t[-101]
V_t <- output_2$V_t[-1]
alpha_t <- output_2$alpha_t[-1]

##graph of smoothed h_t (alpha_t) and x_t
plot(x$x, type = "l", ylab="", xlab = "", main="Smoothed data")
lines(alpha_t, col="red")
legend("bottomleft", legend=c("Transformed data", "Smoothed data"),
       col=c("black", "red"), lty=1:2, cex=0.7)

##calculate ksi and H_t
ksi_hat <- omega_hat / (1 - phi_hat)

#filtered expectation H_t | Y_t
H_t <- a_t - ksi_hat
# smoothed expectation H_t | Y_n
H_n <- alpha_t - ksi_hat
  
#plot filtered H_t nd smoothed H_t
plot(H_t, type="l", ylab="", xlab = "", main="Filtered and smoothed H")
lines(H_n, col="red")
legend("bottomleft", legend=c("Filtered H", "Smoothed H"),
        col=c("black", "red"), lty=1:2, cex=0.6)


############### 
########### question f: Bootstrap particle filter

# PART I: applied on the original dataset

theta <- c(phi_hat, omega_hat, sigma2_eta_hat)

particlefilter<- function(data,theta){
   
  # Extract parameters: 
  omega<-theta[1]
  phi<-theta[2]
  ksi<-omega/(1-phi)
  sigma_eta<-theta[3]
  
  # Initialise variables: 
  n<-length(data)
  N<-10000
  H<- rep(0,n)
  state_particles <- rep(0, N)
  H_hat<- rep(0,(n+1))
  weights_norm <- rep(1, N)
  y<-data-mean(data)
  
  # Run particle filter:
  for(i in 1:n){
    
    # Update particles: 
    state_particles <-phi*state_particles + sigma_eta*rnorm(N, sigma_eta)
    
    # Compute weights:
    weights_particles<-weights_norm *(exp(-(1/2)*log(2*pi)-(1/2)*(ksi+state_particles)-(1/2)*((y[1]^2)/(exp(ksi+state_particles)))))
    
    # Normalise weights:
    weights_norm <-weights_particles/sum(weights_particles)
    
    # Compute ESS & resample if neccessary:
    # Effective Sample Size: efficiency measure
    ESS <- 1/sum(weights_norm ^2)
    
    state_particles [1:N] <- ifelse(ESS >= 0.75*N, state_particles[1:N], sample(state_particles, N, replace=TRUE, prob=weights_norm))
    
    H_hat[i+1]<-t(weights_norm)%*%state_particles 
    
    H <-sample(state_particles,N, replace=TRUE, prob=weights_norm)
  }
  return(H_hat)
}

dff <- data$GBPUSD

H_hat <- particlefilter(dff, theta)


# PLOT I 
plot(H_hat, type="l", ylab="", xlab = "", main="Bootstrap particle filter estimates: initial data", ylim=c(-0.020, 0.020))
lines(H_t, col="red")
legend("bottomleft", legend=c("Filtered h", "Filtered QML h"),
       col=c("black", "red"), lty=1:2, cex=0.6)


### PART II: applied on stock index data from e)

############### 
########### question e(c)

####initial parameters########
sp500 <- HistoricalData_1678716045367
closing_prices<-rev(sp500$Close.Last)
y<-(diff(log(closing_prices)))
mu<-mean(y)
demeaned_y<-y-mu
x<-log(demeaned_y^2)

sigma2_e <- (pi)^2 / 2
x1<-x[1:1256]
x2<-x[2:1257]
phi <- cov(x1,x2)/(var(x)-pi^2/2)  
sigma2_eta <- (1 - phi^2)*(var(x) - pi^2/2)
omega <- (1 - phi)*(mean(x) + 1.27)

loglikelihood<-function(x, par){
  z <- 1
  d <- -1.27
  H <- (pi)^2 / 2
  t <- exp(par[1])/(1 + exp(par[1]))#phi
  c <- par[2]#omega
  Q <- exp(par[3])#sigma2_eta
  R <- 1
  
  x<-as.data.frame(x)
  n<-nrow(x)
  output_matrix<-data.frame(matrix(ncol = 5, nrow = n))
  colnames(output_matrix)<-c("v_t","k_t","f_t","a_t","p_t")
  output_matrix$a_t[1]<-omega / (1 - t)
  output_matrix$p_t[1]<- sigma2_eta / (1 - t^2)
  for (i in 1:(n-1)){
    output_matrix$v_t[i]<-x$x[i]-z*output_matrix$a_t[i] - d
    output_matrix$f_t[i]<-z^2*output_matrix$p_t[i]+ H
    output_matrix$k_t[i]<-(t*output_matrix$p_t[i]*z)/output_matrix$f_t[i]
    output_matrix$a_t[i+1]<-t*output_matrix$a_t[i]+output_matrix$k_t[i]*output_matrix$v_t[i]+ c
    output_matrix$p_t[i+1]<-t^2*output_matrix$p_t[i] + R^2*Q - output_matrix$k_t[i]^2*output_matrix$f_t[i]
  }
  output_matrix$v_t[n]<-x$x[n]-z*output_matrix$a_t[n] - d
  output_matrix$f_t[n]<-z^2*output_matrix$p_t[n]+ H
  output_matrix$k_t[n]<-(t*output_matrix$p_t[n]*z)/output_matrix$f_t[n]
  
  f_t <- output_matrix$f_t
  v_t <- output_matrix$v_t
  
  #likelihood
  sum<-0
  for (i in 1:length(f_t)){
    sum<-sum+(log(abs(f_t[i]))+v_t[i]^2/(f_t[i]))
  }
  likelihood<-(-0.5*(length(f_t)+1)*log(2*pi))-0.5*sum
  
  return(-likelihood)
}


par_ini <- c(log(phi/(1-phi)),omega,log(sigma2_eta))
est <- optim(par = par_ini, fn=loglikelihood, x = x, method = "BFGS")
phi_hat2 <- exp(est$par[1])/(1+exp(est$par[1]))
omega_hat2 <- est$par[2]
sigma2_eta_hat_2 <- exp(est$par[3])
log_likelihood<--est$value

#####

## Apply the particle filter on stock index data 

theta2 <- c(phi_hat2, omega_hat2, sigma2_eta_hat_2)

particlefilter_E<- function(data,theta){
  
  # Extract parameters: 
  omega<-theta2[1]
  phi<-theta2[2]
  ksi<-omega/(1-phi)
  sigma_eta<-theta2[3]
  
  # Initialise variables: 
  n<-length(data)
  N<-10000
  H<- rep(0,n)
  state_particles <- rep(0, N)
  H_hat<- rep(0,(n+1))
  weights_norm <- rep(1, N)
  y<-data-mean(data)
  
  # Run particle filter:
  for(i in 1:n){
    
    # Update particles: 
    state_particles <-phi*state_particles + sigma_eta*rnorm(N, sigma_eta)
    
    # Compute weights:
    weights_particles<-weights_norm *(exp(-(1/2)*log(2*pi)-(1/2)*(ksi+state_particles)-(1/2)*((y[1]^2)/(exp(ksi+state_particles)))))
    
    # Normalise weights:
    weights_norm <-weights_particles/sum(weights_particles)
    
    # Compute ESS 
    # Effective Sample Size: efficiency measure
    ESS <- 1/sum(weights_norm ^2)
    
    state_particles [1:N] <- ifelse(ESS >= 0.75*N, state_particles[1:N], sample(state_particles, N, replace=TRUE, prob=weights_norm))
    
    H_hat[i+1]<-t(weights_norm)%*%state_particles 
    
    H <-sample(state_particles,N, replace=TRUE, prob=weights_norm)
  }
  return(H_hat)
}

data_E <-(diff(log(closing_prices)))

H_hat2 <- particlefilter_E(data_E, theta2)
plot(H_hat2, type="l")

# PLOT II
plot(H_hat2, type="l", ylab="", xlab = "", main="Bootstrap particle filter estimates: stock index data", ylim=c(-0.70, 0.70))
lines(H_t, col="red")
legend("bottomleft", legend=c("Filtered h", "Filtered QML h"),
       col=c("black", "red"), lty=1:2, cex=0.6)

