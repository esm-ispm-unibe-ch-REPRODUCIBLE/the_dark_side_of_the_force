set.seed(42)
library(rjags)

########### load data ##########
#       DRUGS          codes
#1     bupropion         1
#2    citalopram         2
#3    duloxetine         3
#4  escitalopram         4
#5    fluoxetine         5
#6   fluvoxamine         6
#7   milnacipran         7
#8   mirtazapine         8
#9    paroxetine         9
#10   reboxetine        10
#11   sertraline        11
#12  venlafaxine        12


setwd("C:/Users/efthimiou/Google Drive/PROJECT/multiple testing NMA/Simulations/antidepressants analysis")
dataJAGS=readRDS( c("antidepressants"))

######## usual NMA model I ####
model1.string <-  "
model {
for(i in 1:ns) { 
w[i,1]<- 0
theta[i,t[i,1]]<- 0                                             

##binomial likelihood of number of events for each arm k of study i		     
for (k in 1:na[i]) {r[i,t[i,k]] ~ dbin(p[i,t[i,k]],n[i,t[i,k]])}    

##parameterization of the 'true' effect of each comparison 
##of arm k vs. baseline arm (1) of study i                 
logit(p[i,t[i,1]])<- u[i]		                    
for (k in 2:na[i]) {
logit(p[i,t[i,k]])<- u[i] + theta[i,t[i,k]]

##distribution of random effects
theta[i,t[i,k]] ~ dnorm(md[i,t[i,k]],precd[i,t[i,k]])


## accounting for correlation between effect sizes estimated in multi-arm trials				             
md[i,t[i,k]]<- mean[i,k] + sw[i,k]                                   
w[i,k]<- (theta[i,t[i,k]]  - mean[i,k])          
sw[i,k]<- sum(w[i,1:(k-1)])/(k-1)
precd[i,t[i,k]]<- prec *2*(k-1)/k  

##consistency equations
mean[i,k] <-d[t[i,k]] - d[t[i,1]] 

}}

##prior distribution for log-odds in baseline arm of study i
for (i in 1:ns) {u[i] ~ dnorm(0,.01)}

##prior distribution for heterogeneity	
tau ~ dunif(0,3)                                   
prec<- 1/pow(tau,2)
tau.sq<- pow(tau,2)

##prior distribution for basic parameters		
d[ref] <- 0	
for(k in 1:(ref-1)) {d[k] ~ dnorm(0,.0001)}
for(k in (ref+1):nt) {d[k] ~ dnorm(0,.0001)}

##OR for each comparison 
for(i in 1:(nt-1)) {
for (j in (i+1):nt) {
OR[j,i]<- exp(d[j] - d[i])
LOR[j,i]<- d[j] - d[i]}}

for(j in 1:(ref-1)){ORref[j]<- exp(d[j] - d[ref])}
for(j in (ref+1):nt) {ORref[j]<- exp(d[j] - d[ref])}


for(j in 2:nt){dd[j]<-d[j]-d[1]}

#Ranking of treatments
#TreatmeNT hierarchy
order[1:nt]<- nt+1- rank(d[1:nt])
for(k in 1:nt) {
# this is when the outcome is positive - omit  'nt+1-' when the outcome is negative
most.effective[k]<-equals(order[k],1)
for(j in 1:nt) {
effectiveness[k,j]<- equals(order[k],j)}}
for(k in 1:nt) {
for(j in 1:nt) {
cumeffectiveness[k,j]<- sum(effectiveness[k,1:j])}}

#SUCRAS
for(k in 1:nt) {
SUCRA[k]<- sum(cumeffectiveness[k,1:(nt-1)]) /(nt-1)}}

#for(k in 1:nt) {
#order[k]<- rank(d[],k) # this is when the outcome is negative - change to 'order[k]<- t+1-rank(d[],k) when the outcome is positive
#most.effective[k]<-equals(order[k],1)
#for(j in 1:nt) {effectiveness[k,j]<- equals(order[k],j)
#cumeffectiveness[k,j]<- sum(effectiveness[k,1:j])}}		

##SUCRAS
#for(k in 1:nt) {
#SUCRA[k]<- sum(cumeffectiveness[k,1:(nt-1)]) /(nt-1)
#}}

"
####



#### fit model I #####
model1.spec<-textConnection(model1.string) 
data <- list(ns=dataJAGS$ns,ref=1, nt=dataJAGS$nt, n=dataJAGS$n, r=dataJAGS$r, t=dataJAGS$t, na=dataJAGS$na)
jags.m <- jags.model(model1.spec, data = data, n.chains = 2, n.adapt = 5000)

params <- c("dd[2]","dd[3]","dd[4]","dd[5]","dd[6]","dd[7]","dd[8]","dd[9]","dd[10]","dd[11]","dd[12]", "tau")

for (i in 1:11){
  for (j in (i+1):12){
    params=c(params, paste("OR[",j,",",i,"]",sep=""))
    closeAllConnections()    
  }
}

for (i in 1:12){
  params=c(params, paste("SUCRA[",i,"]" ,sep=""))
}


samps<- coda.samples(jags.m, params, n.iter = 10000)
burn.in <- 5000 
A1=summary(samps,start = burn.in)

A2=data.frame(A1$quantiles[,"50%"],A1$quantiles[,"2.5%"],A1$quantiles[,"97.5%"] )
A2=A2[1:66,]
A2=round(A2, digits=2)
colnames(A2)=c("median","low","up")

A2$x=0
A2$y=0

nam=rownames(A2)
A2[substr(nam, start=5, stop=5)==",",]$x=substr(nam[substr(nam, start=5, stop=5)==","], start=4, stop=4)
A2[substr(nam, start=5, stop=5)==",",]$y=substr(nam[substr(nam, start=5, stop=5)==","], start=6, stop=6)
A2[substr(nam, start=6, stop=6)==",",]$x=substr(nam[substr(nam, start=6, stop=6)==","], start=4, stop=5)
A2[substr(nam, start=6, stop=6)==",",]$y=substr(nam[substr(nam, start=6, stop=6)==","], start=7, stop=7)
A2[substr(nam, start=6, stop=6)==","&nchar(nam)==9,]$y=substr(nam[substr(nam, start=6, stop=6)==","&nchar(nam)==9], start=7, stop=8)
A2$x=as.numeric(A2$x)
A2$y=as.numeric(A2$y)
A3=A2[order(A2$y,A2$x),]

league.table=matrix(rep(0,144),nrow=12)
league.table[lower.tri(league.table, diag=FALSE)]=round(1/A3$median,digits=2)
llow=matrix(rep(0,144),nrow=12)
llow[lower.tri(league.table, diag=FALSE)]=round(1/A3$up,digits=2)
lup=matrix(rep(0,144),nrow=12)
lup[lower.tri(league.table, diag=FALSE)]=round(1/A3$low,digits=2)

league.table1=matrix(paste(league.table,"[",llow,";",lup,"]", sep=""),ncol=ncol(league.table))
names=c("bupropion","citalopram","duloxetine",
        "escitalopram","fluoxetine","fluvoxamine","milnacipran","mirtazapine",
        "paroxetine","reboxetine","sertraline","venlafaxine")
diag(league.table1)=names
league.table1[upper.tri(league.table1)]=""
league.table=data.frame(league.table1)
league.tableST=league.table


#######

######## model II ####
model1.string <-  "
model {
for(i in 1:ns) { 
w[i,1]<- 0
theta[i,t[i,1]]<- 0                                             

##binomial likelihood of number of events for each arm k of study i		     
for (k in 1:na[i]) {r[i,t[i,k]] ~ dbin(p[i,t[i,k]],n[i,t[i,k]])}    

##parameterization of the 'true' effect of each comparison 
##of arm k vs. baseline arm (1) of study i                 
logit(p[i,t[i,1]])<- u[i]		                    
for (k in 2:na[i]) {
logit(p[i,t[i,k]])<- u[i] + theta[i,t[i,k]]

##distribution of random effects
theta[i,t[i,k]] ~ dnorm(md[i,t[i,k]],precd[i,t[i,k]])


## accounting for correlation between effect sizes estimated in multi-arm trials				             
md[i,t[i,k]]<- mean[i,k] + sw[i,k]                                   
w[i,k]<- (theta[i,t[i,k]]  - mean[i,k])          
sw[i,k]<- sum(w[i,1:(k-1)])/(k-1)
precd[i,t[i,k]]<- prec *2*(k-1)/k  

##consistency equations
mean[i,k] <-d[t[i,k]] - d[t[i,1]] 

}}

##prior distribution for log-odds in baseline arm of study i
for (i in 1:ns) {u[i] ~ dnorm(0,.01)}

##prior distribution for heterogeneity	
tau ~ dunif(0,3)                                      
prec<- 1/pow(tau,2)
tau.sq<- pow(tau,2)

##prior distribution for basic parameters		
d[ref] <- 0	
for(k in 1:(ref-1)) {d[k] ~ dnorm(md1,prd1)}
for(k in (ref+1):nt) {d[k] ~ dnorm(md1,prd1)}

md1~dnorm(0,1)
taud ~ dnorm(0,1)I(0,)                               
prd1<- 1/pow(taud,2)
tau.sqd<- pow(taud,2)


##OR for each comparison 
for(i in 1:(nt-1)) {
for (j in (i+1):nt) {
OR[j,i]<- exp(d[j] - d[i])
LOR[j,i]<- d[j] - d[i]}}

for(j in 1:(ref-1)){ORref[j]<- exp(d[j] - d[ref])}
for(j in (ref+1):nt) {ORref[j]<- exp(d[j] - d[ref])}


for(j in 2:nt){dd[j]<-d[j]-d[1]}

#Ranking of treatments
#TreatmeNT hierarchy
order[1:nt]<- nt+1- rank(d[1:nt])
for(k in 1:nt) {
# this is when the outcome is positive - omit  'nt+1-' when the outcome is negative
most.effective[k]<-equals(order[k],1)
for(j in 1:nt) {
effectiveness[k,j]<- equals(order[k],j)}}
for(k in 1:nt) {
for(j in 1:nt) {
cumeffectiveness[k,j]<- sum(effectiveness[k,1:j])}}

#SUCRAS
for(k in 1:nt) {
SUCRA[k]<- sum(cumeffectiveness[k,1:(nt-1)]) /(nt-1)}}

#for(k in 1:nt) {
#order[k]<- rank(d[],k) # this is when the outcome is negative - change to 'order[k]<- t+1-rank(d[],k) when the outcome is positive
#most.effective[k]<-equals(order[k],1)
#for(j in 1:nt) {effectiveness[k,j]<- equals(order[k],j)
#cumeffectiveness[k,j]<- sum(effectiveness[k,1:j])}}		

##SUCRAS
#for(k in 1:nt) {
#SUCRA[k]<- sum(cumeffectiveness[k,1:(nt-1)]) /(nt-1)
#}}

"
########

##### model III ######
model3.string <-  "
model {
for(i in 1:ns) { 
w[i,1]<- 0
theta[i,t[i,1]]<- 0                                             

##binomial likelihood of number of events for each arm k of study i		     
for (k in 1:na[i]) {r[i,t[i,k]] ~ dbin(p[i,t[i,k]],n[i,t[i,k]])}    

##parameterization of the 'true' effect of each comparison 
##of arm k vs. baseline arm (1) of study i                 
logit(p[i,t[i,1]])<- u[i]		                    
for (k in 2:na[i]) {
logit(p[i,t[i,k]])<- u[i] + theta[i,t[i,k]]

##distribution of random effects
theta[i,t[i,k]] ~ dnorm(md[i,t[i,k]],precd[i,t[i,k]])


## accounting for correlation between effect sizes estimated in multi-arm trials				             
md[i,t[i,k]]<- mean[i,k] + sw[i,k]                                   
w[i,k]<- (theta[i,t[i,k]]  - mean[i,k])          
sw[i,k]<- sum(w[i,1:(k-1)])/(k-1)
precd[i,t[i,k]]<- prec *2*(k-1)/k  

##consistency equations
mean[i,k] <-d[t[i,k]] - d[t[i,1]] 

}}

##prior distribution for log-odds in baseline arm of study i
for (i in 1:ns) {u[i] ~ dnorm(0,.01)}

##prior distribution for heterogeneity	
tau ~ dunif(0,3)                                     
prec<- 1/pow(tau,2)
tau.sq<- pow(tau,2)

##prior distribution for basic parameters		
d[1:nt] ~ dmnorm(md1[1:(nt)],prec1[1:(nt),1:(nt)])

for (i in 1:(nt)) {md1[i]<-md0 }		

for(k in 1:(nt)){taud1[k,k]<-tau.sqd}

for (i in 1:(nt-1)){
for (k in (i+1):(nt)){
taud1[i,k]<-0.5*tau.sqd
taud1[k,i]<-taud1[i,k]
}}

prec1[1:(nt),1:(nt)]<-inverse(taud1[1:(nt),1:(nt)])


md0~dnorm(0,1)
taud ~ dnorm(0,1)I(0,)                                 
tau.sqd<- pow(taud,2)



##OR for each comparison 
for(i in 1:(nt-1)) {
for (j in (i+1):nt) {
OR[j,i]<- exp(d[j] - d[i])
LOR[j,i]<- d[j] - d[i]}}

for(j in 1:(ref-1)){ORref[j]<- exp(d[j] - d[ref])}
for(j in (ref+1):nt) {ORref[j]<- exp(d[j] - d[ref])}


#Ranking of treatments
#TreatmeNT hierarchy
order[1:nt]<- nt+1- rank(d[1:nt])
for(k in 1:nt) {
# this is when the outcome is positive - omit  'nt+1-' when the outcome is negative
most.effective[k]<-equals(order[k],1)
for(j in 1:nt) {
effectiveness[k,j]<- equals(order[k],j)}}
for(k in 1:nt) {
for(j in 1:nt) {
cumeffectiveness[k,j]<- sum(effectiveness[k,1:j])}}

#SUCRAS
for(k in 1:nt) {
SUCRA[k]<- sum(cumeffectiveness[k,1:(nt-1)]) /(nt-1)}}

#for(k in 1:nt) {
#order[k]<- rank(d[],k) # this is when the outcome is negative - change to 'order[k]<- t+1-rank(d[],k) when the outcome is positive
#most.effective[k]<-equals(order[k],1)
#for(j in 1:nt) {effectiveness[k,j]<- equals(order[k],j)
#cumeffectiveness[k,j]<- sum(effectiveness[k,1:j])}}		

##SUCRAS
#for(k in 1:nt) {
#SUCRA[k]<- sum(cumeffectiveness[k,1:(nt-1)]) /(nt-1)
#}}

"
#########
#### fit model III #####
model3.spec<-textConnection(model3.string) 
data <- list(ns=dataJAGS$ns,ref=1, nt=dataJAGS$nt, n=dataJAGS$n, r=dataJAGS$r, t=dataJAGS$t, na=dataJAGS$na)
jags.m <- jags.model(model3.spec, data = data, n.chains = 2, n.adapt = 5000)

params <- c("dd[2]","dd[3]","dd[4]","dd[5]","dd[6]","dd[7]","dd[8]","dd[9]","dd[10]","dd[11]","dd[12]", "tau")

for (i in 1:11){
  for (j in (i+1):12){
    params=c(params, paste("OR[",j,",",i,"]",sep=""))
    closeAllConnections()    
  }
}

for (i in 1:12){
  params=c(params, paste("SUCRA[",i,"]" ,sep=""))
}


samps<- coda.samples(jags.m, params, n.iter = 10000)
burn.in <- 5000 
A5=summary(samps,start = burn.in)

A6=data.frame(A5$quantiles[,"50%"],A5$quantiles[,"2.5%"],A5$quantiles[,"97.5%"] )
A6=A6[1:66,]
A6=round(A6, digits=2)
colnames(A6)=c("median","low","up")

A6$x=0
A6$y=0

nam=rownames(A6)
A6[substr(nam, start=5, stop=5)==",",]$x=substr(nam[substr(nam, start=5, stop=5)==","], start=4, stop=4)
A6[substr(nam, start=5, stop=5)==",",]$y=substr(nam[substr(nam, start=5, stop=5)==","], start=6, stop=6)
A6[substr(nam, start=6, stop=6)==",",]$x=substr(nam[substr(nam, start=6, stop=6)==","], start=4, stop=5)
A6[substr(nam, start=6, stop=6)==",",]$y=substr(nam[substr(nam, start=6, stop=6)==","], start=7, stop=7)
A6[substr(nam, start=6, stop=6)==","&nchar(nam)==9,]$y=substr(nam[substr(nam, start=6, stop=6)==","&nchar(nam)==9], start=7, stop=8)
A6$x=as.numeric(A6$x)
A6$y=as.numeric(A6$y)
A7=A6[order(A6$y,A6$x),]

league.table=matrix(rep(0,144),nrow=12)
league.table[lower.tri(league.table, diag=FALSE)]=round(1/A7$median,digits=2)
llow=matrix(rep(0,144),nrow=12)
llow[lower.tri(league.table, diag=FALSE)]=round(1/A7$up,digits=2)
lup=matrix(rep(0,144),nrow=12)
lup[lower.tri(league.table, diag=FALSE)]=round(1/A7$low,digits=2)

league.table1=matrix(paste(league.table,"[",llow,";",lup,"]", sep=""),ncol=ncol(league.table))
names=c("bupropion","citalopram","duloxetine",
        "escitalopram","fluoxetine","fluvoxamine","milnacipran","mirtazapine",
        "paroxetine","reboxetine","sertraline","venlafaxine")
diag(league.table1)=names
league.table1[upper.tri(league.table1)]=""
league.table=data.frame(t(league.table1))
league.tableST3=league.table
##########

##### create a single table combining results with of  models I and III #######
league.table4=as.matrix(league.tableST3)
league.table5=as.matrix(league.tableST)
league.table6=matrix(paste(league.table4,league.table5, sep=""),ncol=ncol(league.table))
diag(league.table6)=names
league.tableALL=data.frame(league.table6)
league.tableALL
#######


