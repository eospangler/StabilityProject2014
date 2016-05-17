#Question 2b----

#Clear the console, clear the workspace, and detach all dataframes----

cat("\014")
rm(list = ls())

#Set working directory

#Note! Gamma1 is the minimum for the DV
#Gamma2 is the minimum for the other good
#Beta is the preference for the DV. 
#Minimize NLS

#Freedonia
Freedonia<- read.csv ("~/StabilityProject/Parameter Estimation/Freedonia.csv",
                      header=T,
                      na.strings=c("#VALUE!", " ", "NA","na"))
attach(Freedonia)

f = GEf
fnObs = length(f)
#Nonlinear least squares (indVar1 is price, indVar2 is income)
NLSfxn = function(theta,indVar1,indVar2,depVar,n){
  gamma1 = theta[1]
  gamma2 = theta[2]
  beta1 = theta[3]
  g = gamma1-beta1*(indVar1-gamma1-gamma2*indVar2)
  betaNLS = (1/n)*(t(depVar-g)%*%(depVar-g))
  return(betaNLS)
}
#Minimize NLS
thetaStart = c(0.25,0.25,0.25)
GEfNLeastSq = optim(thetaStart,NLSfxn,gr = NULL, method = "L-BFGS-B",
                    lower = c(1,1,1,rep.int(-Inf,3)),
                    upper = c(Inf,Inf,Inf,rep.int(Inf,3)),
                    control = list(maxit = 1000),
                    hessian = TRUE,
                    indVar1 = GDPf, indVar2 = MEf, depVar = f, n = fnObs)
GEfNLeastSq

mean(GDPf)

#Merikia!
Merika<- read.csv ("~/StabilityProject/Parameter Estimation/Merika.csv",
                   header=T,
                   na.strings=c("#VALUE!", " ", "NA","na"))
attach(Merika)

m = GEm
mnObs = length(m)
#Nonlinear least squares (indVar1 is price, indVar2 is income)
NLSfxn = function(theta,indVar1,indVar2,depVar,n){
  gamma1 = theta[1]
  gamma2 = theta[2]
  beta1 = theta[3]
  g = gamma1-beta1*(indVar1-gamma1-gamma2*indVar2)
  betaNLS = (1/n)*(t(depVar-g)%*%(depVar-g))
  return(betaNLS)
}
#Minimize NLS
thetaStart = c(0.25,0.25,0.25)
GEmNLeastSq = optim(thetaStart,NLSfxn,gr = NULL, method = "L-BFGS-B",
                    lower = c(1,1,1,rep.int(-Inf,3)),
                    upper = c(Inf,Inf,Inf,rep.int(Inf,3)),
                    control = list(maxit = 1000),
                    hessian = TRUE,
                    indVar1 = GDPm, indVar2 = MEm, depVar = m, n = mnObs)
GEmNLeastSq

mean(GDPm)


#Kleptopia
Kleptopia<- read.csv ("~/StabilityProject/Parameter Estimation/Kleptopia.csv",
                   header=T,
                   na.strings=c("#VALUE!", " ", "NA","na"))
attach(Kleptopia)

k = GEk
knObs = length(k)
#Nonlinear least squares (indVar1 is price, indVar2 is income)
NLSfxn = function(theta,indVar1,indVar2,depVar,n){
  gamma1 = theta[1]
  gamma2 = theta[2]
  beta1 = theta[3]
  g = gamma1-beta1*(indVar1-gamma1-gamma2*indVar2)
  betaNLS = (1/n)*(t(depVar-g)%*%(depVar-g))
  return(betaNLS)
}
#Minimize NLS
thetaStart = c(0.25,0.25,0.25)
GEkNLeastSq = optim(thetaStart,NLSfxn,gr = NULL, method = "L-BFGS-B",
                    lower = c(1,1,1,rep.int(-Inf,3)),
                    upper = c(Inf,Inf,Inf,rep.int(Inf,3)),
                    control = list(maxit = 1000),
                    hessian = TRUE,
                    indVar1 = GDPk, indVar2 = MEk, depVar = k, n = knObs)
GEkNLeastSq
mean(GDPk)

#Cathay
Cathay<- read.csv ("~/StabilityProject/Parameter Estimation/Cathay.csv",
                   header=T,
                   na.strings=c("#VALUE!", " ", "NA","na"))
attach(Cathay)

c = GEc
cnObs = length(c)
#Nonlinear least squares (indVar1 is price, indVar2 is income)
NLSfxn = function(theta,indVar1,indVar2,depVar,n){
  gamma1 = theta[1]
  gamma2 = theta[2]
  beta1 = theta[3]
  g = gamma1-beta1*(indVar1-gamma1-gamma2*indVar2)
  betaNLS = (1/n)*(t(depVar-g)%*%(depVar-g))
  return(betaNLS)
}
#Minimize NLS
thetaStart = c(0.25,0.25,0.25)
GEcNLeastSq = optim(thetaStart,NLSfxn,gr = NULL, method = "L-BFGS-B",
                    lower = c(1,1,1,rep.int(-Inf,3)),
                    upper = c(Inf,Inf,Inf,rep.int(Inf,3)),
                    control = list(maxit = 1000),
                    hessian = TRUE,
                    indVar1 = GDPc, indVar2 = MEc, depVar = c, n = cnObs)
GEcNLeastSq

mean(GDPc)

#Rentistan

Rentistan<- read.csv ("~/StabilityProject/Parameter Estimation/Rentistan.csv",
                   header=T,
                   na.strings=c("#VALUE!", " ", "NA","na"))
attach(Rentistan)

r = GEr
rObs = length(r)
#Nonlinear least squares (indVar1 is price, indVar2 is income)
NLSfxn = function(theta,indVar1,indVar2,depVar,n){
  gamma1 = theta[1]
  gamma2 = theta[2]
  beta1 = theta[3]
  g = gamma1-beta1*(indVar1-gamma1-gamma2*indVar2)
  betaNLS = (1/n)*(t(depVar-g)%*%(depVar-g))
  return(betaNLS)
}
#Minimize NLS
thetaStart = c(0.25,0.25,0.25)
GErNLeastSq = optim(thetaStart,NLSfxn,gr = NULL, method = "L-BFGS-B",
                    lower = c(1,1,1,rep.int(-Inf,3)),
                    upper = c(Inf,Inf,Inf,rep.int(Inf,3)),
                    control = list(maxit = 1000),
                    hessian = TRUE,
                    indVar1 = GDPr, indVar2 = MEr, depVar = GEr, n = rObs)
GErNLeastSq

mean(GDPr)

#Bellicostia
Bellicostia<- read.csv ("~/StabilityProject/Parameter Estimation/Bellicostia.csv",
                   header=T,
                   na.strings=c("#VALUE!", " ", "NA","na"))
attach(Bellicostia)

b = GEb
bnObs = length(b)
#Nonlinear least squares (indVar1 is price, indVar2 is income)
NLSfxn = function(theta,indVar1,indVar2,depVar,n){
  gamma1 = theta[1]
  gamma2 = theta[2]
  beta1 = theta[3]
  g = gamma1-beta1*(indVar1-gamma1-gamma2*indVar2)
  betaNLS = (1/n)*(t(depVar-g)%*%(depVar-g))
  return(betaNLS)
}
#Minimize NLS
thetaStart = c(0.25,0.25,0.25)
GEbNLeastSq = optim(thetaStart,NLSfxn,gr = NULL, method = "L-BFGS-B",
                    lower = c(1,1,1,rep.int(-Inf,3)),
                    upper = c(Inf,Inf,Inf,rep.int(Inf,3)),
                    control = list(maxit = 1000),
                    hessian = TRUE,
                    indVar1 = GDPb, indVar2 = MEb, depVar = b, n = bnObs)
GEbNLeastSq

mean(GDPb)


#Hippieberg
Hippieberg<- read.csv ("~/StabilityProject/Parameter Estimation/Hippieberg.csv",
                   header=T,
                   na.strings=c("#VALUE!", " ", "NA","na"))
attach(Hippieberg)

h = GEb
hnObs = length(h)
#Nonlinear least squares (indVar1 is price, indVar2 is income)
NLSfxn = function(theta,indVar1,indVar2,depVar,n){
  gamma1 = theta[1]
  gamma2 = theta[2]
  beta1 = theta[3]
  g = gamma1-beta1*(indVar1-gamma1-gamma2*indVar2)
  betaNLS = (1/n)*(t(depVar-g)%*%(depVar-g))
  return(betaNLS)
}
#Minimize NLS
thetaStart = c(0.25,0.25,0.25)
GEhNLeastSq = optim(thetaStart,NLSfxn,gr = NULL, method = "L-BFGS-B",
                    lower = c(1,1,1,rep.int(-Inf,3)),
                    upper = c(Inf,Inf,Inf,rep.int(Inf,3)),
                    control = list(maxit = 1000),
                    hessian = TRUE,
                    indVar1 = GDPh, indVar2 = MEh, depVar = h, n = hnObs)
GEhNLeastSq

mean(GDPh)

#Develpolus
#Merikia!
Develpolus<- read.csv ("~/StabilityProject/Parameter Estimation/Develpolus.csv",
                   header=T,
                   na.strings=c("#VALUE!", " ", "NA","na"))
attach(Develpolus)

d = GEm
dnObs = length(d)
#Nonlinear least squares (indVar1 is price, indVar2 is income)
NLSfxn = function(theta,indVar1,indVar2,depVar,n){
  gamma1 = theta[1]
  gamma2 = theta[2]
  beta1 = theta[3]
  g = gamma1-beta1*(indVar1-gamma1-gamma2*indVar2)
  betaNLS = (1/n)*(t(depVar-g)%*%(depVar-g))
  return(betaNLS)
}
#Minimize NLS
thetaStart = c(0.25,0.25,0.25)
GEdNLeastSq = optim(thetaStart,NLSfxn,gr = NULL, method = "L-BFGS-B",
                    lower = c(1,1,1,rep.int(-Inf,3)),
                    upper = c(Inf,Inf,Inf,rep.int(Inf,3)),
                    control = list(maxit = 1000),
                    hessian = TRUE,
                    indVar1 = GDPd, indVar2 = MEd, depVar = d, n = dnObs)
GEdNLeastSq

mean(GDPd)
