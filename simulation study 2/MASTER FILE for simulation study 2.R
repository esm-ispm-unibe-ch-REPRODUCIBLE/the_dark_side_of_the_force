###################################################
#       Master file for simulation Study 2       #
###################################################


#empty memory  
rm(list=ls())

# install packages if needed
#install.packages("R2jags")
# load packages
library(R2jags)

set.seed(42)

N.sim=1000  #### set number of simulations

# set working directory
setwd("C:/Users/efthimiou/Google Drive/PROJECT/multiple testing NMA/Simulations/simulation study 2")
# choose scenario (N.scen=E1)
N.scen="H1"
# generate the data for scenario N.scen
source(paste("Data generation_Scenario ",N.scen,".R",sep=""))



##################################################
############### Fit the models ###################
##################################################


##################################################

# model I
source("Model I - Fixed Effect - flat prior.R")

# model II
source("Model II - Fixed Effect.R")

# model III
source("Model III - Fixed Effect.R")

# model IV
source("Model IV- Fixed Effect.R")

# model V
source("Model V- Fixed Effect.R")



##################################################
### summarize results
##################################################

##############################
### scenarios E.1 and E.2   ##
##############################
mean(MAE) # mean absolute error (basic parameters)
mean(bias) # mean bias (basic parameters)
mean(coverage) # coverage (basic parameters)
mean(difference.best.worst) # mean estimated difference between best and worst treatment
mean(sd.best.worst) # mean standard deviation for the estimated difference between best and worst treatment
mean(difference.best.reference) # mean estimated difference between best and reference treatment
mean(sd.best.ref) # mean standard deviation for the estimated difference between best and reference treatment


#### False positives: % of NMAs showing with confidence that there are treatment effects among equally effective treatments
e1=substr(rownames(D[[1]]),nchar(rownames(D[[1]]))-1,nchar(rownames(D[[1]]))-1)
fun=function(x){return(sum(
  x[e1!="1",][,1]>0 |  x[e1!="1",][,2]<0 
  ))}
e2= lapply(D,fun)
sum(unlist(e2)>0)/(N.sim)


#### Power: % of treatment effects that were shown with confidence to be non zero 
e4= lapply(D, 
           function(x){
             return(sum(x[e1=="1",][,1]>0))})
sum(unlist(e4))/(N.sim*(N.treat-1))


##############################
### scenarios F.1 and F.2   ##
##############################
mean(MAE) # mean absolute error (basic parameters)
mean(bias) # mean bias (basic parameters)
mean(difference.best.worst) # mean estimated difference between best and worst treatment
mean(sd.best.worst) # mean standard deviation for the estimated difference between best and worst treatment
mean(difference.best.reference) # mean estimated difference between best and reference treatment
mean(sd.best.ref) # mean standard deviation for the estimated difference between best and reference treatment
mean(coverage) # coverage (basic parameters)


#### False positives: % of NMAs showing with confidence that there are treatment effects among equally effective treatments
fun_1=function(x){return(sum(x[10:19,1]>0 |  x[10:19,2]<0))} ### among group 1
f1= lapply(D,fun_1)
fun_2=function(x){return(sum(x[c(6:9,30,36:37,43:45),1]>0 |  x[c(6:9,30,36:37,43:45),2] <0 ))} ### among group 2
f2= lapply(D,fun_2)
f3=unlist(f1)+unlist(f2)
sum(f3>0)/(N.sim) #False positives: % of NMAs showing with confidence that there are treatment effects  (among equally effective treatments)


#### Power: % of treatment effects that were shown with confidence to be non zero (when in truth they were not)
fun_3=function(x){return(sum(x[c(1:5,30,20:29,31:35,38:42),1]>0))} ### among group 2
f4= lapply(D,fun_3)
f5=unlist(f4)
sum(f5)/(25*N.sim) #Power % of treatment effects that were shown with confidence to be non zero (when in truth they were not)


##############################
### scenarios G.1 and G.2   ##
##############################
mean(MAE) # mean absolute error (basic parameters)
mean(bias) # mean bias (basic parameters)
mean(difference.best.worst) # mean estimated difference between best and worst treatment
mean(sd.best.worst) # mean standard deviation for the estimated difference between best and worst treatment
mean(difference.best.reference) # mean estimated difference between best and reference treatment
mean(sd.best.ref) # mean standard deviation for the estimated difference between best and reference treatment
mean(coverage) # coverage (basic parameters)



#### Power: % of treatment effects that were shown with confidence to be non zero (when in truth they were not)
fun_3=function(x){return(sum(x[,1]>0))} 
f4= lapply(D,fun_3)
f5=unlist(f4)
sum(f5)/(45*N.sim) #Power % of treatment effects that were shown with confidence to be non zero (when in truth they were not)


#### reversal of effects
fun_4=function(x){return(sum(x[,2]<0))} ### among group 2
f6= lapply(D,fun_4)
f7=unlist(f6)
sum(f7)/(45*N.sim) # % of reversed effects



##############################
### scenarios H.1 and H.2   ##
##############################
mean(MAE) # mean absolute error (basic parameters)
mean(bias) # mean bias (basic parameters)
mean(difference.best.worst) # mean estimated difference between best and worst treatment
mean(sd.best.worst) # mean standard deviation for the estimated difference between best and worst treatment
mean(difference.best.reference) # mean estimated difference between best and reference treatment
mean(sd.best.ref) # mean standard deviation for the estimated difference between best and reference treatment
mean(coverage) # coverage (basic parameters)


#### False positives: % of NMAs showing with confidence that there are treatment effects among equally effective treatments
fun1=function(x){return(sum(x[c(12,14,15,17:19),1]>0 |  x[c(12,14,15,17:19),2]<0 ))  } ###  group 1
fun2=function(x){return(sum(x[c(6:9,30,36:37,43:45),1]>0 |   x[c(6:9,30,36:37,43:45),2]<0))  } ###  group 1
h1= lapply(D,fun1)
h2= lapply(D,fun2)
sum(unlist(h1)+unlist(h2)>0)/(N.sim)

#### Power: % of treatment effects that were shown with confidence to be non zero (when in truth they were not)
fun3=function(x){return(sum(x[-c(12,14,15,17:19,6:9,30,36:37,43:45),1]>0))}
h4= lapply(D,fun3)
h5=unlist(h4)
sum(h5)/(29*N.sim) #Power % of treatment effects that were shown with confidence to be non zero (when in truth they were not)


