#################
#### model II ###
#################

##### some definitions
MAE=c()
bias=c()
SSresults=c()
difference.best.worst=c()
difference.best.reference=c()
sd.best.worst=c()
sd.best.ref=c()
coverage=c()
jags.m3=list()
#####################


#####################
model3=function() {

for(i in 1:NS){
dm[i]<-d[t2[i]]-d[t1[i]]
prec[i]<-1/(SE[i]*SE[i])
y[i]~dnorm(dm[i],prec[i])}
d[1:NT] ~ dmnorm(md1[1:(NT)],prec1[1:(NT),1:(NT)])

for (i in 1:(NT)) {md1[i]<-md0 }		

for(k in 1:(NT)){taud1[k,k]<-tau.sqd}

for (i in 1:(NT-1)){
for (k in (i+1):(NT)){
taud1[i,k]<-0.5*tau.sqd
taud1[k,i]<-taud1[i,k]}}

prec1[1:(NT),1:(NT)]<-inverse(taud1[1:(NT),1:(NT)])

md0~dnorm(0,0.1)
taud ~ dunif(0,2)                                  
tau.sqd<- pow(taud,2)

for (i in 1:NT){
for (j in i:NT){
D[j,i]<-d[j]-d[i]}}

for (i in 2:NT){
dref[i]<-d[i]-d[1]}

#TreatmeNT hierarchy
order[1:NT]<- NT+1- rank(d[1:NT])
for(k in 1:NT) {
  # this is when the outcome is positive - omit  'NT+1-' when the outcome is negative
  most.effective[k]<-equals(order[k],1)
  for(j in 1:NT) {
    effectiveness[k,j]<- equals(order[k],j)}}
for(k in 1:NT) {
  for(j in 1:NT) {
    cumeffectiveness[k,j]<- sum(effectiveness[k,1:j])}}

#SUCRAS#
for(k in 1:NT) {
  SUCRA[k]<- sum(cumeffectiveness[k,1:(NT-1)]) /(NT-1)}}


params=c() 
for (i in 1:(N.treat-1)){
  for (j in (i+1):N.treat){
    params=c(params, paste("D[",j,",",i,"]",sep=""))
  }}
for (i in 2:(N.treat)){
  params=c(params, paste("dref[",i,"]",sep=""))
}
for (i in 1:(N.treat)){
  params=c(params, paste("SUCRA[",i,"]",sep=""))
}

#number of D parameters
no.D=N.treat*(N.treat-1)/2



for (i in 1:N.sim){
  initialval = NULL
  data2 <- list(y = data1[[i]]$TE,SE=data1[[i]]$seTE, NS=length(data1[[i]]$studlab), t1=data1[[i]]$t1,t2=data1[[i]]$t2, NT=N.treat)
  
  jags.m3[[i]] <- jags.parallel(data=data2,initialval,parameters.to.save = params, n.chains = 2, n.iter = 15000, n.thin=1, n.burnin = 5000, DIC=F, model.file = model3)
  print(i)
  coverage[i]=(mean(jags.m3[[i]]$BUGSoutput$summary[(no.D+N.treat+1):(no.D+2*N.treat-1),3]<0&jags.m3[[i]]$BUGSoutput$summary[(no.D+N.treat+1):(no.D+2*N.treat-1),7]>0))
  
  bias[i]=(mean(jags.m3[[i]]$BUGSoutput$summary[(no.D+N.treat+1):(no.D+2*N.treat-1),1]))
  MAE[i]=mean(abs(jags.m3[[i]]$BUGSoutput$summary[(no.D+N.treat+1):(no.D+2*N.treat-1),1]))
 
  
  difference.best.worst[i]=abs(jags.m3[[i]]$BUGSoutput$summary[1,1])
  sd.best.worst[i]=abs(jags.m3[[i]]$BUGSoutput$summary[1,2])
  
  jags.m3[[i]]=NULL
} 







