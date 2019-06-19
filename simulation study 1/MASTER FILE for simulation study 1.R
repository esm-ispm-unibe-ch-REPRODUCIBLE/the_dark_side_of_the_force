###################################################
#       Master file for simulation Study 1        #
###################################################


#empty memory  
rm(list=ls())

# install if needed
#install.packages("R2jags")
# load package
library(R2jags)
set.seed(42) # THE ANSWER TO LIFE, THE UNIVERSE AND EVERYTHING
options(digits=3)
N.sim=1000

# set working directory
setwd("C:/Users/efthimiou/Google Drive/PROJECT/multiple testing NMA/Simulations/simulation study 1")
# choose scenario (N.scen=A1, A2, B1, B2, C or D)
N.scen="A1"
# generate the data for scenario k
source(paste("Data generation_Scenario ",N.scen,".R",sep=""))


##################################################
############### Fit the models ###################
##################################################


##################################################
# For scenarios A, B or D run one of the following models
# model I
source("Model I - Fixed Effect - flat prior.R")
source("Model I - Fixed Effect - Informative prior.R")
source("Model I - Random Effect -flat prior.R")
source("Model I - Random Effect - Informative prior.R")
# model II
source("Model II - Fixed Effect.R")
source("Model II - Random Effect.R")
# model III
source("Model III - Fixed Effect.R")
source("Model III - Random Effect.R")


##################################################
# For scenario C run one of the following
# model I
source("Model I - Fixed Effect - flat prior (scenario C).R")
source("Model I - Fixed Effect - Informative prior (Scenario C).R")
# model II
source("Model II - Fixed Effect (Scenario C).R")
# model III
source("Model III - Fixed Effect (Scenario C).R")

##################################################
### summarize results
mean(MAE) # mean absolute error (basic parameters)
mean(coverage) # mean coverage of 95% CrI (basic parameters)
mean(bias) # mean bias (basic parameters)
sum(SSresults>0)/N.sim # percent of NMAs where at least one treatment effect was estimated to be non-zero with confidence
mean(difference.best.worst) # mean estimated difference between best and worst treatment
mean(sd.best.worst) # mean standard deviation for the estimated difference between best and worst treatment

mean(difference.best.reference) # mean estimated difference between best and worst treatment
mean(sd.best.ref) # mean standard deviation for the estimated difference between best and worst treatment



