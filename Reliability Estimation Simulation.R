#code written by Ashley Edwards
#This code simulates data based on a given factor structure and sample size 1000 times 
#and calculates reliability using six estimators (Cronbach’s alpha, Omega, Omega Hierarchical, 
#Revelle’s Omega, Greatest Lower Bound, and Coefficient H) for each of the 1000 simulated datasets
#and averages across these datasets to determine the mean estimate for each estimator

alpha = c(rep(NA, 1000)) #create a vector with 1000 NAs
coefH = c(rep(NA, 1000))
glb = c(rep(NA, 1000))
omega = c(rep(NA, 1000))
omegapsy = c(rep(NA, 1000))
omegah = c(rep(NA, 1000))
d = data.frame(alpha, coefH, glb, omega, omegapsy, omegah) #create a dataframe with 1000 rows of NAs

library(lavaan)
library(userfriendlyscience)
breaks = 0 #set the number of breaks (data simulation failed to converge) equal to zero
imposs = 0 #set the number of impossible omega values (omega > 1) equal to zero 
nextstart = 1 #nextstart tells you what row you are on, start at row 1
n = 100 #sample size
mod1 <- paste0("f1=~ ", .2, "*x1 + ", .2, "*x2 + ", .2, "*x3 + ", .2, "*x4 + ", .2, "*x5 + ", .2, "*x6") #factor loadings for a 6 item scale

#When the data simulation fails to converge it will stop whatever loop it is in, thus multiple loops are run in sequence in order to reach the intended 1000 simulations
for(i in nextstart:1000){
  
  #simulate data:
  datum <- simulateData(mod1, sample.nobs=n, standardized=TRUE)
  
  
  # calculate statistics:
  a1 = scaleStructure(dat=datum, ci=FALSE)
  
  alpha1 = a1$output$cronbach.alpha
  coefH1 = a1$output$coefficientH
  glb1 = a1$output$glb
  omega1 = a1$output$omega
  omegapsy1 = a1$output$omega.psych
  omegah1 = a1$intermediate$omega.psych$omega_h
  
  #add statistics from this run to the data frame (d) of all runs:
  d$alpha[i] = alpha1 #in dataset d add the value of alpha1 to the ith row of column alpha
  d$coefH[i] = coefH1
  d$glb[i] = glb1
  d$omega[i] = omega1
  d$omegapsy[i] = omegapsy1
  d$omegah[i] = omegah1
  
}

#if 1000 simulations have not been reached, continue with simulation
if((nrow(na.omit(d))+breaks)<1000){
  breaks = breaks+1 #add another break to the count (since we are starting a new loop prior to 1000 rows that means we were broken out of the previous loop)
  nextstart = (nrow(na.omit(d))+breaks)+1 #next row to start at is the number of filled rows plus the number of breaks (breaks are to be left with NA in cells)
  print(nextstart) 
  
  for(i in nextstart:1000){
    
    #simulate data:
    datum <- simulateData(mod1, sample.nobs=n, standardized=TRUE)
    
    
    # calculate statistics:
    a1 = scaleStructure(dat=datum, ci=FALSE)
    
    alpha1 = a1$output$cronbach.alpha
    coefH1 = a1$output$coefficientH
    glb1 = a1$output$glb
    omega1 = a1$output$omega
    omegapsy1 = a1$output$omega.psych
    omegah1 = a1$intermediate$omega.psych$omega_h
    
    #add statistics from this run to the data frame (d) of all runs:
    d$alpha[i] = alpha1
    d$coefH[i] = coefH1
    d$glb[i] = glb1
    d$omega[i] = omega1
    d$omegapsy[i] = omegapsy1
    d$omegah[i] = omegah1
    
  }
}

if((nrow(na.omit(d))+breaks)<1000){
  breaks = breaks+1
  nextstart = (nrow(na.omit(d))+breaks)+1
  print(nextstart)
  
  for(i in nextstart:1000){
    
    datum <- simulateData(mod1, sample.nobs=n, standardized=TRUE)
    
    
    # calculate statistics:
    a1 = scaleStructure(dat=datum, ci=FALSE)
    
    alpha1 = a1$output$cronbach.alpha
    coefH1 = a1$output$coefficientH
    glb1 = a1$output$glb
    omega1 = a1$output$omega
    omegapsy1 = a1$output$omega.psych
    omegah1 = a1$intermediate$omega.psych$omega_h
    
    d$alpha[i] = alpha1
    d$coefH[i] = coefH1
    d$glb[i] = glb1
    d$omega[i] = omega1
    d$omegapsy[i] = omegapsy1
    d$omegah[i] = omegah1
    
  }
}
if((nrow(na.omit(d))+breaks)<1000){
  breaks = breaks+1
  nextstart = (nrow(na.omit(d))+breaks)+1
  print(nextstart)
  
  for(i in nextstart:1000){
    
    datum <- simulateData(mod1, sample.nobs=n, standardized=TRUE)
    
    
    # calculate statistics:
    a1 = scaleStructure(dat=datum, ci=FALSE)
    
    alpha1 = a1$output$cronbach.alpha
    coefH1 = a1$output$coefficientH
    glb1 = a1$output$glb
    omega1 = a1$output$omega
    omegapsy1 = a1$output$omega.psych
    omegah1 = a1$intermediate$omega.psych$omega_h
    
    d$alpha[i] = alpha1
    d$coefH[i] = coefH1
    d$glb[i] = glb1
    d$omega[i] = omega1
    d$omegapsy[i] = omegapsy1
    d$omegah[i] = omegah1
    
  }
}

if((nrow(na.omit(d))+breaks)<1000){
  breaks = breaks+1
  nextstart = (nrow(na.omit(d))+breaks)+1
  print(nextstart)
  
  for(i in nextstart:1000){
    
    datum <- simulateData(mod1, sample.nobs=n, standardized=TRUE)
    
    # calculate statistics:
    a1 = scaleStructure(dat=datum, ci=FALSE)
    
    alpha1 = a1$output$cronbach.alpha
    coefH1 = a1$output$coefficientH
    glb1 = a1$output$glb
    omega1 = a1$output$omega
    omegapsy1 = a1$output$omega.psych
    omegah1 = a1$intermediate$omega.psych$omega_h
    
    d$alpha[i] = alpha1
    d$coefH[i] = coefH1
    d$glb[i] = glb1
    d$omega[i] = omega1
    d$omegapsy[i] = omegapsy1
    d$omegah[i] = omegah1
    
  }
}

if((nrow(na.omit(d))+breaks)<1000){
  breaks = breaks+1
  nextstart = (nrow(na.omit(d))+breaks)+1
  print(nextstart)
  
  for(i in nextstart:1000){
    
    datum <- simulateData(mod1, sample.nobs=n, standardized=TRUE)
    
    
    # calculate statistics:
    a1 = scaleStructure(dat=datum, ci=FALSE)
    
    alpha1 = a1$output$cronbach.alpha
    coefH1 = a1$output$coefficientH
    glb1 = a1$output$glb
    omega1 = a1$output$omega
    omegapsy1 = a1$output$omega.psych
    omegah1 = a1$intermediate$omega.psych$omega_h
    
    d$alpha[i] = alpha1
    d$coefH[i] = coefH1
    d$glb[i] = glb1
    d$omega[i] = omega1
    d$omegapsy[i] = omegapsy1
    d$omegah[i] = omegah1
    
  }
}

if((nrow(na.omit(d))+breaks)<1000){
  breaks = breaks+1
  nextstart = (nrow(na.omit(d))+breaks)+1
  print(nextstart)
  
  for(i in nextstart:1000){
    
    datum <- simulateData(mod1, sample.nobs=n, standardized=TRUE)
    
    
    # calculate statistics:
    a1 = scaleStructure(dat=datum, ci=FALSE)
    
    alpha1 = a1$output$cronbach.alpha
    coefH1 = a1$output$coefficientH
    glb1 = a1$output$glb
    omega1 = a1$output$omega
    omegapsy1 = a1$output$omega.psych
    omegah1 = a1$intermediate$omega.psych$omega_h
    
    d$alpha[i] = alpha1
    d$coefH[i] = coefH1
    d$glb[i] = glb1
    d$omega[i] = omega1
    d$omegapsy[i] = omegapsy1
    d$omegah[i] = omegah1
    
  }
}
if((nrow(na.omit(d))+breaks)<1000){
  breaks = breaks+1
  nextstart = (nrow(na.omit(d))+breaks)+1
  print(nextstart)
  
  for(i in nextstart:1000){
    
    datum <- simulateData(mod1, sample.nobs=n, standardized=TRUE)
    
    
    # calculate statistics:
    a1 = scaleStructure(dat=datum, ci=FALSE)
    
    alpha1 = a1$output$cronbach.alpha
    coefH1 = a1$output$coefficientH
    glb1 = a1$output$glb
    omega1 = a1$output$omega
    omegapsy1 = a1$output$omega.psych
    omegah1 = a1$intermediate$omega.psych$omega_h
    
    d$alpha[i] = alpha1
    d$coefH[i] = coefH1
    d$glb[i] = glb1
    d$omega[i] = omega1
    d$omegapsy[i] = omegapsy1
    d$omegah[i] = omegah1
    
  }
}

if((nrow(na.omit(d))+breaks)<1000){
  breaks = breaks+1
  nextstart = (nrow(na.omit(d))+breaks)+1
  print(nextstart)
  
  for(i in nextstart:1000){
    
    datum <- simulateData(mod1, sample.nobs=n, standardized=TRUE)
    
    # calculate statistics:
    a1 = scaleStructure(dat=datum, ci=FALSE)
    
    alpha1 = a1$output$cronbach.alpha
    coefH1 = a1$output$coefficientH
    glb1 = a1$output$glb
    omega1 = a1$output$omega
    omegapsy1 = a1$output$omega.psych
    omegah1 = a1$intermediate$omega.psych$omega_h
    
    d$alpha[i] = alpha1
    d$coefH[i] = coefH1
    d$glb[i] = glb1
    d$omega[i] = omega1
    d$omegapsy[i] = omegapsy1
    d$omegah[i] = omegah1
    
  }
}
if((nrow(na.omit(d))+breaks)<1000){
  breaks = breaks+1
  nextstart = (nrow(na.omit(d))+breaks)+1
  print(nextstart)
  
  for(i in nextstart:1000){
    
    datum <- simulateData(mod1, sample.nobs=n, standardized=TRUE)
    
    
    # calculate statistics:
    a1 = scaleStructure(dat=datum, ci=FALSE)
    
    alpha1 = a1$output$cronbach.alpha
    coefH1 = a1$output$coefficientH
    glb1 = a1$output$glb
    omega1 = a1$output$omega
    omegapsy1 = a1$output$omega.psych
    omegah1 = a1$intermediate$omega.psych$omega_h
    
    d$alpha[i] = alpha1
    d$coefH[i] = coefH1
    d$glb[i] = glb1
    d$omega[i] = omega1
    d$omegapsy[i] = omegapsy1
    d$omegah[i] = omegah1
    
  }
}

if((nrow(na.omit(d))+breaks)<1000){
  breaks = breaks+1
  nextstart = (nrow(na.omit(d))+breaks)+1
  print(nextstart)
  
  for(i in nextstart:1000){
    
    datum <- simulateData(mod1, sample.nobs=n, standardized=TRUE)
    
    
    # calculate statistics:
    a1 = scaleStructure(dat=datum, ci=FALSE)
    
    alpha1 = a1$output$cronbach.alpha
    coefH1 = a1$output$coefficientH
    glb1 = a1$output$glb
    omega1 = a1$output$omega
    omegapsy1 = a1$output$omega.psych
    omegah1 = a1$intermediate$omega.psych$omega_h
    
    d$alpha[i] = alpha1
    d$coefH[i] = coefH1
    d$glb[i] = glb1
    d$omega[i] = omega1
    d$omegapsy[i] = omegapsy1
    d$omegah[i] = omegah1
    
  }
}
if((nrow(na.omit(d))+breaks)<1000){
  breaks = breaks+1
  nextstart = (nrow(na.omit(d))+breaks)+1
  print(nextstart)
  
  for(i in nextstart:1000){
    
    datum <- simulateData(mod1, sample.nobs=n, standardized=TRUE)
    
    
    # calculate statistics:
    a1 = scaleStructure(dat=datum, ci=FALSE)
    
    alpha1 = a1$output$cronbach.alpha
    coefH1 = a1$output$coefficientH
    glb1 = a1$output$glb
    omega1 = a1$output$omega
    omegapsy1 = a1$output$omega.psych
    omegah1 = a1$intermediate$omega.psych$omega_h
    
    d$alpha[i] = alpha1
    d$coefH[i] = coefH1
    d$glb[i] = glb1
    d$omega[i] = omega1
    d$omegapsy[i] = omegapsy1
    d$omegah[i] = omegah1
    
  }
}

if((nrow(na.omit(d))+breaks)<1000){
  breaks = breaks+1
  nextstart = (nrow(na.omit(d))+breaks)+1
  print(nextstart)
  
  for(i in nextstart:1000){
    
    datum <- simulateData(mod1, sample.nobs=n, standardized=TRUE)
    
    # calculate statistics:
    a1 = scaleStructure(dat=datum, ci=FALSE)
    
    alpha1 = a1$output$cronbach.alpha
    coefH1 = a1$output$coefficientH
    glb1 = a1$output$glb
    omega1 = a1$output$omega
    omegapsy1 = a1$output$omega.psych
    omegah1 = a1$intermediate$omega.psych$omega_h
    
    d$alpha[i] = alpha1
    d$coefH[i] = coefH1
    d$glb[i] = glb1
    d$omega[i] = omega1
    d$omegapsy[i] = omegapsy1
    d$omegah[i] = omegah1
    
  }
}

if((nrow(na.omit(d))+breaks)<1000){
  breaks = breaks+1
  nextstart = (nrow(na.omit(d))+breaks)+1
  print(nextstart)
  
  for(i in nextstart:1000){
    
    datum <- simulateData(mod1, sample.nobs=n, standardized=TRUE)
    
    
    # calculate statistics:
    a1 = scaleStructure(dat=datum, ci=FALSE)
    
    alpha1 = a1$output$cronbach.alpha
    coefH1 = a1$output$coefficientH
    glb1 = a1$output$glb
    omega1 = a1$output$omega
    omegapsy1 = a1$output$omega.psych
    omegah1 = a1$intermediate$omega.psych$omega_h
    
    d$alpha[i] = alpha1
    d$coefH[i] = coefH1
    d$glb[i] = glb1
    d$omega[i] = omega1
    d$omegapsy[i] = omegapsy1
    d$omegah[i] = omegah1
    
  }
}

if((nrow(na.omit(d))+breaks)<1000){
  breaks = breaks+1
  nextstart = (nrow(na.omit(d))+breaks)+1
  print(nextstart)
  
  for(i in nextstart:1000){
    
    datum <- simulateData(mod1, sample.nobs=n, standardized=TRUE)
    
    
    # calculate statistics:
    a1 = scaleStructure(dat=datum, ci=FALSE)
    
    alpha1 = a1$output$cronbach.alpha
    coefH1 = a1$output$coefficientH
    glb1 = a1$output$glb
    omega1 = a1$output$omega
    omegapsy1 = a1$output$omega.psych
    omegah1 = a1$intermediate$omega.psych$omega_h
    
    d$alpha[i] = alpha1
    d$coefH[i] = coefH1
    d$glb[i] = glb1
    d$omega[i] = omega1
    d$omegapsy[i] = omegapsy1
    d$omegah[i] = omegah1
    
  }
}
if((nrow(na.omit(d))+breaks)<1000){
  breaks = breaks+1
  nextstart = (nrow(na.omit(d))+breaks)+1
  print(nextstart)
  
  for(i in nextstart:1000){
    
    datum <- simulateData(mod1, sample.nobs=n, standardized=TRUE)
    
    
    # calculate statistics:
    a1 = scaleStructure(dat=datum, ci=FALSE)
    
    alpha1 = a1$output$cronbach.alpha
    coefH1 = a1$output$coefficientH
    glb1 = a1$output$glb
    omega1 = a1$output$omega
    omegapsy1 = a1$output$omega.psych
    omegah1 = a1$intermediate$omega.psych$omega_h
    
    d$alpha[i] = alpha1
    d$coefH[i] = coefH1
    d$glb[i] = glb1
    d$omega[i] = omega1
    d$omegapsy[i] = omegapsy1
    d$omegah[i] = omegah1
    
  }
}

if((nrow(na.omit(d))+breaks)<1000){
  breaks = breaks+1
  nextstart = (nrow(na.omit(d))+breaks)+1
  print(nextstart)
  
  for(i in nextstart:1000){
    
    datum <- simulateData(mod1, sample.nobs=n, standardized=TRUE)
    
    # calculate statistics:
    a1 = scaleStructure(dat=datum, ci=FALSE)
    
    alpha1 = a1$output$cronbach.alpha
    coefH1 = a1$output$coefficientH
    glb1 = a1$output$glb
    omega1 = a1$output$omega
    omegapsy1 = a1$output$omega.psych
    omegah1 = a1$intermediate$omega.psych$omega_h
    
    d$alpha[i] = alpha1
    d$coefH[i] = coefH1
    d$glb[i] = glb1
    d$omega[i] = omega1
    d$omegapsy[i] = omegapsy1
    d$omegah[i] = omegah1
    
  }
}

if((nrow(na.omit(d))+breaks)<1000){
  breaks = breaks+1
  nextstart = (nrow(na.omit(d))+breaks)+1
  print(nextstart)
  
  for(i in nextstart:1000){
    
    datum <- simulateData(mod1, sample.nobs=n, standardized=TRUE)
    
    
    # calculate statistics:
    a1 = scaleStructure(dat=datum, ci=FALSE)
    
    alpha1 = a1$output$cronbach.alpha
    coefH1 = a1$output$coefficientH
    glb1 = a1$output$glb
    omega1 = a1$output$omega
    omegapsy1 = a1$output$omega.psych
    omegah1 = a1$intermediate$omega.psych$omega_h
    
    d$alpha[i] = alpha1
    d$coefH[i] = coefH1
    d$glb[i] = glb1
    d$omega[i] = omega1
    d$omegapsy[i] = omegapsy1
    d$omegah[i] = omegah1
    
  }
}

if((nrow(na.omit(d))+breaks)<1000){
  breaks = breaks+1
  nextstart = (nrow(na.omit(d))+breaks)+1
  print(nextstart)
  
  for(i in nextstart:1000){
    
    datum <- simulateData(mod1, sample.nobs=n, standardized=TRUE)
    
    
    # calculate statistics:
    a1 = scaleStructure(dat=datum, ci=FALSE)
    
    alpha1 = a1$output$cronbach.alpha
    coefH1 = a1$output$coefficientH
    glb1 = a1$output$glb
    omega1 = a1$output$omega
    omegapsy1 = a1$output$omega.psych
    omegah1 = a1$intermediate$omega.psych$omega_h
    
    d$alpha[i] = alpha1
    d$coefH[i] = coefH1
    d$glb[i] = glb1
    d$omega[i] = omega1
    d$omegapsy[i] = omegapsy1
    d$omegah[i] = omegah1
    
  }
}
if((nrow(na.omit(d))+breaks)<1000){
  breaks = breaks+1
  nextstart = (nrow(na.omit(d))+breaks)+1
  print(nextstart)
  
  for(i in nextstart:1000){
    
    datum <- simulateData(mod1, sample.nobs=n, standardized=TRUE)
    
    
    # calculate statistics:
    a1 = scaleStructure(dat=datum, ci=FALSE)
    
    alpha1 = a1$output$cronbach.alpha
    coefH1 = a1$output$coefficientH
    glb1 = a1$output$glb
    omega1 = a1$output$omega
    omegapsy1 = a1$output$omega.psych
    omegah1 = a1$intermediate$omega.psych$omega_h
    
    d$alpha[i] = alpha1
    d$coefH[i] = coefH1
    d$glb[i] = glb1
    d$omega[i] = omega1
    d$omegapsy[i] = omegapsy1
    d$omegah[i] = omegah1
    
  }
}

if((nrow(na.omit(d))+breaks)<1000){
  breaks = breaks+1
  nextstart = (nrow(na.omit(d))+breaks)+1
  print(nextstart)
  
  for(i in nextstart:1000){
    
    datum <- simulateData(mod1, sample.nobs=n, standardized=TRUE)
    
    # calculate statistics:
    a1 = scaleStructure(dat=datum, ci=FALSE)
    
    alpha1 = a1$output$cronbach.alpha
    coefH1 = a1$output$coefficientH
    glb1 = a1$output$glb
    omega1 = a1$output$omega
    omegapsy1 = a1$output$omega.psych
    omegah1 = a1$intermediate$omega.psych$omega_h
    
    d$alpha[i] = alpha1
    d$coefH[i] = coefH1
    d$glb[i] = glb1
    d$omega[i] = omega1
    d$omegapsy[i] = omegapsy1
    d$omegah[i] = omegah1
    
  }
}

if((nrow(na.omit(d))+breaks)<1000){
  breaks = breaks+1
  nextstart = (nrow(na.omit(d))+breaks)+1
  print(nextstart)
  
  for(i in nextstart:1000){
    
    datum <- simulateData(mod1, sample.nobs=n, standardized=TRUE)
    
    
    # calculate statistics:
    a1 = scaleStructure(dat=datum, ci=FALSE)
    
    alpha1 = a1$output$cronbach.alpha
    coefH1 = a1$output$coefficientH
    glb1 = a1$output$glb
    omega1 = a1$output$omega
    omegapsy1 = a1$output$omega.psych
    omegah1 = a1$intermediate$omega.psych$omega_h
    
    d$alpha[i] = alpha1
    d$coefH[i] = coefH1
    d$glb[i] = glb1
    d$omega[i] = omega1
    d$omegapsy[i] = omegapsy1
    d$omegah[i] = omegah1
    
  }
}

if((nrow(na.omit(d))+breaks)<1000){
  breaks = breaks+1
  nextstart = (nrow(na.omit(d))+breaks)+1
  print(nextstart)
  
  for(i in nextstart:1000){
    
    datum <- simulateData(mod1, sample.nobs=n, standardized=TRUE)
    
    
    # calculate statistics:
    a1 = scaleStructure(dat=datum, ci=FALSE)
    
    alpha1 = a1$output$cronbach.alpha
    coefH1 = a1$output$coefficientH
    glb1 = a1$output$glb
    omega1 = a1$output$omega
    omegapsy1 = a1$output$omega.psych
    omegah1 = a1$intermediate$omega.psych$omega_h
    
    d$alpha[i] = alpha1
    d$coefH[i] = coefH1
    d$glb[i] = glb1
    d$omega[i] = omega1
    d$omegapsy[i] = omegapsy1
    d$omegah[i] = omegah1
    
  }
}
if((nrow(na.omit(d))+breaks)<1000){
  breaks = breaks+1
  nextstart = (nrow(na.omit(d))+breaks)+1
  print(nextstart)
  
  for(i in nextstart:1000){
    
    datum <- simulateData(mod1, sample.nobs=n, standardized=TRUE)
    
    
    # calculate statistics:
    a1 = scaleStructure(dat=datum, ci=FALSE)
    
    alpha1 = a1$output$cronbach.alpha
    coefH1 = a1$output$coefficientH
    glb1 = a1$output$glb
    omega1 = a1$output$omega
    omegapsy1 = a1$output$omega.psych
    omegah1 = a1$intermediate$omega.psych$omega_h
    
    d$alpha[i] = alpha1
    d$coefH[i] = coefH1
    d$glb[i] = glb1
    d$omega[i] = omega1
    d$omegapsy[i] = omegapsy1
    d$omegah[i] = omegah1
    
  }
}

if((nrow(na.omit(d))+breaks)<1000){
  breaks = breaks+1
  nextstart = (nrow(na.omit(d))+breaks)+1
  print(nextstart)
  
  for(i in nextstart:1000){
    
    datum <- simulateData(mod1, sample.nobs=n, standardized=TRUE)
    
    # calculate statistics:
    a1 = scaleStructure(dat=datum, ci=FALSE)
    
    alpha1 = a1$output$cronbach.alpha
    coefH1 = a1$output$coefficientH
    glb1 = a1$output$glb
    omega1 = a1$output$omega
    omegapsy1 = a1$output$omega.psych
    omegah1 = a1$intermediate$omega.psych$omega_h
    
    d$alpha[i] = alpha1
    d$coefH[i] = coefH1
    d$glb[i] = glb1
    d$omega[i] = omega1
    d$omegapsy[i] = omegapsy1
    d$omegah[i] = omegah1
    
  }
}
if((nrow(na.omit(d))+breaks)<1000){
  breaks = breaks+1
  nextstart = (nrow(na.omit(d))+breaks)+1
  print(nextstart)
  
  for(i in nextstart:1000){
    
    datum <- simulateData(mod1, sample.nobs=n, standardized=TRUE)
    
    
    # calculate statistics:
    a1 = scaleStructure(dat=datum, ci=FALSE)
    
    alpha1 = a1$output$cronbach.alpha
    coefH1 = a1$output$coefficientH
    glb1 = a1$output$glb
    omega1 = a1$output$omega
    omegapsy1 = a1$output$omega.psych
    omegah1 = a1$intermediate$omega.psych$omega_h
    
    d$alpha[i] = alpha1
    d$coefH[i] = coefH1
    d$glb[i] = glb1
    d$omega[i] = omega1
    d$omegapsy[i] = omegapsy1
    d$omegah[i] = omegah1
    
  }
}

if((nrow(na.omit(d))+breaks)<1000){
  breaks = breaks+1
  nextstart = (nrow(na.omit(d))+breaks)+1
  print(nextstart)
  
  for(i in nextstart:1000){
    
    datum <- simulateData(mod1, sample.nobs=n, standardized=TRUE)
    
    
    # calculate statistics:
    a1 = scaleStructure(dat=datum, ci=FALSE)
    
    alpha1 = a1$output$cronbach.alpha
    coefH1 = a1$output$coefficientH
    glb1 = a1$output$glb
    omega1 = a1$output$omega
    omegapsy1 = a1$output$omega.psych
    omegah1 = a1$intermediate$omega.psych$omega_h
    
    d$alpha[i] = alpha1
    d$coefH[i] = coefH1
    d$glb[i] = glb1
    d$omega[i] = omega1
    d$omegapsy[i] = omegapsy1
    d$omegah[i] = omegah1
    
  }
}
if((nrow(na.omit(d))+breaks)<1000){
  breaks = breaks+1
  nextstart = (nrow(na.omit(d))+breaks)+1
  print(nextstart)
  
  for(i in nextstart:1000){
    
    datum <- simulateData(mod1, sample.nobs=n, standardized=TRUE)
    
    
    # calculate statistics:
    a1 = scaleStructure(dat=datum, ci=FALSE)
    
    alpha1 = a1$output$cronbach.alpha
    coefH1 = a1$output$coefficientH
    glb1 = a1$output$glb
    omega1 = a1$output$omega
    omegapsy1 = a1$output$omega.psych
    omegah1 = a1$intermediate$omega.psych$omega_h
    
    d$alpha[i] = alpha1
    d$coefH[i] = coefH1
    d$glb[i] = glb1
    d$omega[i] = omega1
    d$omegapsy[i] = omegapsy1
    d$omegah[i] = omegah1
    
  }
}

if((nrow(na.omit(d))+breaks)<1000){
  breaks = breaks+1
  nextstart = (nrow(na.omit(d))+breaks)+1
  print(nextstart)
  
  for(i in nextstart:1000){
    
    datum <- simulateData(mod1, sample.nobs=n, standardized=TRUE)
    
    # calculate statistics:
    a1 = scaleStructure(dat=datum, ci=FALSE)
    
    alpha1 = a1$output$cronbach.alpha
    coefH1 = a1$output$coefficientH
    glb1 = a1$output$glb
    omega1 = a1$output$omega
    omegapsy1 = a1$output$omega.psych
    omegah1 = a1$intermediate$omega.psych$omega_h
    
    d$alpha[i] = alpha1
    d$coefH[i] = coefH1
    d$glb[i] = glb1
    d$omega[i] = omega1
    d$omegapsy[i] = omegapsy1
    d$omegah[i] = omegah1
    
  }
}

if((nrow(na.omit(d))+breaks)<1000){
  breaks = breaks+1
  nextstart = (nrow(na.omit(d))+breaks)+1
  print(nextstart)
  
  for(i in nextstart:1000){
    
    datum <- simulateData(mod1, sample.nobs=n, standardized=TRUE)
    
    
    # calculate statistics:
    a1 = scaleStructure(dat=datum, ci=FALSE)
    
    alpha1 = a1$output$cronbach.alpha
    coefH1 = a1$output$coefficientH
    glb1 = a1$output$glb
    omega1 = a1$output$omega
    omegapsy1 = a1$output$omega.psych
    omegah1 = a1$intermediate$omega.psych$omega_h
    
    d$alpha[i] = alpha1
    d$coefH[i] = coefH1
    d$glb[i] = glb1
    d$omega[i] = omega1
    d$omegapsy[i] = omegapsy1
    d$omegah[i] = omegah1
    
  }
}

if((nrow(na.omit(d))+breaks)<1000){
  breaks = breaks+1
  nextstart = (nrow(na.omit(d))+breaks)+1
  print(nextstart)
  
  for(i in nextstart:1000){
    
    datum <- simulateData(mod1, sample.nobs=n, standardized=TRUE)
    
    
    # calculate statistics:
    a1 = scaleStructure(dat=datum, ci=FALSE)
    
    alpha1 = a1$output$cronbach.alpha
    coefH1 = a1$output$coefficientH
    glb1 = a1$output$glb
    omega1 = a1$output$omega
    omegapsy1 = a1$output$omega.psych
    omegah1 = a1$intermediate$omega.psych$omega_h
    
    d$alpha[i] = alpha1
    d$coefH[i] = coefH1
    d$glb[i] = glb1
    d$omega[i] = omega1
    d$omegapsy[i] = omegapsy1
    d$omegah[i] = omegah1
    
  }
}
if((nrow(na.omit(d))+breaks)<1000){
  breaks = breaks+1
  nextstart = (nrow(na.omit(d))+breaks)+1
  print(nextstart)
  
  for(i in nextstart:1000){
    
    datum <- simulateData(mod1, sample.nobs=n, standardized=TRUE)
    
    
    # calculate statistics:
    a1 = scaleStructure(dat=datum, ci=FALSE)
    
    alpha1 = a1$output$cronbach.alpha
    coefH1 = a1$output$coefficientH
    glb1 = a1$output$glb
    omega1 = a1$output$omega
    omegapsy1 = a1$output$omega.psych
    omegah1 = a1$intermediate$omega.psych$omega_h
    
    d$alpha[i] = alpha1
    d$coefH[i] = coefH1
    d$glb[i] = glb1
    d$omega[i] = omega1
    d$omegapsy[i] = omegapsy1
    d$omegah[i] = omegah1
    
  }
}

if((nrow(na.omit(d))+breaks)<1000){
  breaks = breaks+1
  nextstart = (nrow(na.omit(d))+breaks)+1
  print(nextstart)
  
  for(i in nextstart:1000){
    
    datum <- simulateData(mod1, sample.nobs=n, standardized=TRUE)
    
    # calculate statistics:
    a1 = scaleStructure(dat=datum, ci=FALSE)
    
    alpha1 = a1$output$cronbach.alpha
    coefH1 = a1$output$coefficientH
    glb1 = a1$output$glb
    omega1 = a1$output$omega
    omegapsy1 = a1$output$omega.psych
    omegah1 = a1$intermediate$omega.psych$omega_h
    
    d$alpha[i] = alpha1
    d$coefH[i] = coefH1
    d$glb[i] = glb1
    d$omega[i] = omega1
    d$omegapsy[i] = omegapsy1
    d$omegah[i] = omegah1
    
  }
}

# run if statements until 1000 is reached
#############################################################

breaks
nrow(na.omit(d))
nrow(na.omit(d))+breaks
countbeforerm = nrow(na.omit(d)) #total number of successful runs
n

#save a file with all data prior to removing impossible values:
write.csv(d, file = "preclean_2_100.csv")

#remove data where omega is greater than 1:
d$omega[d$omega>1]=NA
d$alpha[d$omega>1]=NA
d$coefH[d$omega>1]=NA
d$glb[d$omega>1]=NA
d$omegah[d$omega>1]=NA
d$omegapsy[d$omega>1]=NA

imposs = countbeforerm - nrow(na.omit(d))  #number of impossible omega values is the number of rows before removal minus the number of rows after removal
imposs

colMeans(d, na.rm = T) #mean of each column, this gives the the average estimate for each estimator

#save a file with data after removing impossible values:
write.csv(d, file = "results_2_100.csv")
