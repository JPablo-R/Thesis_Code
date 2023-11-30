#Modified X chart
getRL = function(n,mu0,mu1,phi,sigma0,k, sigma1){
  IC = TRUE
  RL = 0
  se = sqrt(1-phi^2)*sigma1
  x0 = rnorm(n = 1, mean = mu1, sd = sigma1)
  while (IC){
    RL = RL + 1
    e = (rnorm(n=1, mean= 0, sd= se))
    X =   mu1 + phi*(x0-mu1)+e
    x0 = X
    Z = (X-mu0)/sigma0
    UCL = k
    if (abs(Z)>UCL){
      IC = FALSE
    }
  }
  return(RL)
}

getRL(n=1,mu0=100,mu1=100,phi=0,sigma0=10,k=3, sigma1=sigma0)



getARLsim = function(n,mu0,mu1, phi, sigma0,sigma1,k,iterations){
  RL = rep(NA,iterations)
  for(i in 1:iterations){
    RL[i]=getRL(n=n,mu0=mu0,mu1=mu1,phi=phi,sigma0=sigma0, sigma1=sigma1,k=k)
  }
  ARLsim=mean(RL)
  return(ARLsim)
}

getARLsim(n=1,mu0=0,mu1=0.5,phi=0.8,sigma0=1,sigma1=1,k=3,iterations=10000)




#####################################################
#Residuals Chart
getRLres = function(n,mu0,mu1,phi,sigma0,k, sigma1){
  IC = TRUE
  RL = 0
  se = sqrt(1-phi^2)*sigma0
  x0 = rnorm(n = 1, mean = mu1, sd = sigma0)
  while (IC){
    RL = RL + 1
    e = (rnorm(n=1, mean= 0, sd= se))
    X =   mu1 + phi*(x0-mu1)+sigma0*e
    Xhat= mu0+phi*(x0-mu0)
    residuo = X-Xhat
    x0 = X
    UCL = k*se*sigma0
    if (abs(residuo)>UCL){
      IC = FALSE
    }
  }
  return(RL)
}

getRLres(n=1,mu0=100,mu1=100,phi=0,sigma0=10,k=3, sigma1=sigma0)

#Getting the ARL residual function

getARLsimres = function(n,mu0,mu1, phi, sigma0,sigma1,k,iterations){
  RL = rep(NA,iterations)
  for(i in 1:iterations){
    RL[i]=getRLres(n=n,mu0=mu0,mu1=mu1,phi=phi,sigma0=sigma0, sigma1=sigma1,k=k)
  }
  ARLsim=mean(RL)
  return(ARLsim)
}


getARLsimres(n=1,mu0=0,mu1=0.5*sqrt(1-0.25^2),phi=0.25,sigma0=1,sigma1=1,k=3,iterations=10000)


##############






