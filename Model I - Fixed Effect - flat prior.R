library(rjags)

##################################################
##### fit the model #############################
##################################################
model1.string <-  "
model{
for(i in 1:NS){
dm[i]<-d[t2[i]]-d[t1[i]]
prec[i]<-1/(SE[i]*SE[i])
y[i]~dnorm(dm[i],prec[i])}

d[1]<-0
for(i in 2:NT){
d[i]~dnorm(0,0.0001)}

for (i in 1:NT){
for (j in i:NT){
D[j,i]<-d[j]-d[i]}}

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
"
jags.m=list()
samps=list()
A1=list()
####### definitions 
count1=N.treat*(N.treat-1)/2+N.treat
lower=list()
upper=list()
lower_D=list()
upper_D=list()
SUCRA=list()
count2=N.treat*(N.treat-1)/2
best.worst=c()
best.2worst=c()
count3=N.treat*(N.treat-1)/2
lower3=list()
sortedSUCRA=list()




for (i in 1:N.sim){
  model1.spec<-textConnection(model1.string) 
  data <- list(y = data1[[i]]$TE,SE=data1[[i]]$seTE, NS=length(data1[[i]]$studlab), t1=data1[[i]]$t1,t2=data1[[i]]$t2, NT=N.treat)
  jags.m[[i]] <- jags.model(model1.spec, data = data, n.chains = 2, n.adapt = 5000)
  print(i)

    params <- c("d[2]","d[3]","d[4]","d[5]","d[6]","d[7]","d[8]","d[9]","d[10]")
  
  for (i in 1:9){
    for (j in (i+1):10){
      params=c(params, paste("D[",j,",",i,"]",sep=""))
      closeAllConnections()        }  }
  
  for (i in 1:10){
    params=c(params, paste("SUCRA[",i,"]" ,sep=""))
  }}

for (i in 1:N.sim){
  samps[[i]] <- coda.samples(jags.m[[i]], params, n.iter = 40000)
  
  ######### fit the model 
  #plot(samps)
  burn.in <- 20000
  A1[[i]]=summary(window(samps[[i]],start = burn.in))
  lower[[i]]= A1[[i]]$quantiles[(count1+1):(length(A1[[i]]$quantiles[,1])),1]
  upper[[i]]= A1[[i]]$quantiles[(count1+1):(length(A1[[i]]$quantiles[,1])),5]
  
  SUCRA[[i]]=A1[[i]]$statistics[(count2+1):(count2+N.treat),1] 
  lower_D[[i]]=A1[[i]]$quantiles[1:count2,1]
  upper_D[[i]]=A1[[i]]$quantiles[1:count2,5]

  samps[[i]]=c(0)
  A1[[i]]=c(0)
  jags.m[[i]]=c(0)
  print(i)
  closeAllConnections()
}


#######################################################
############### measuring model performance ##########
######################################################## 

#### when all treatment effects are zero (scenarios A-D)####

sum1=c(rep(100,N.sim))
for (i in 1:N.sim)
{  sum1[i]=sum(lower[[i]]>0)+sum(upper[[i]]<0)}
sum(sum1>0)/N.sim ## percent of NMAs with at least one st. sign. basic parameter. 

sum2=c(rep(100,N.sim))
for (i in 1:N.sim)
{  sum2[i]=sum(lower_D[[i]]>0)+sum(upper_D[[i]]<0)}
sum(sum2>0)/N.sim  # percent of NMAs with at least one stat. sign. finding


#### SUCRA values
SUCRA_max=c()
SUCRA_min=c()
for (i in 1:N.sim){
  SUCRA_max[i]=max(SUCRA[[i]])
  SUCRA_min[i]=min(SUCRA[[i]])}

mean(SUCRA_max)
min(SUCRA_max)
max(SUCRA_max)

mean(SUCRA_min)
min(SUCRA_min)
max(SUCRA_min)

########################################################################
####  scenario E
### false positive rates in all comparisons
sum11=c(rep(100,N.sim))
sum12=c(rep(100,N.sim))
low=list()
up=list()
low1=list()
up1=list()
sum11=c(rep(100,N.sim))
sum12=c(rep(100,N.sim))
for (i in 1:N.sim)
{ low[[i]]=as.matrix(lower_D[[i]])
up[[i]]=as.matrix(upper_D[[i]])
low1[[i]]=low[[i]][substr(rownames(low[[i]]),nchar(rownames(low[[i]]))-1,nchar(rownames(low[[i]]))-1)!="1"]
up1[[i]]=up[[i]][substr(rownames(up[[i]]),nchar(rownames(up[[i]]))-1,nchar(rownames(up[[i]]))-1)!="1"]
}

for (i in 1:N.sim)
{  sum11[i]=sum(low1[[i]]>0)
sum12[i]=sum(up1[[i]]<0)
}

sum(sum11+sum12)/(N.sim*(N.treat-1)*(N.treat-2)/2)


### power to detect effects vs placebo
sum9=c(rep(100,N.sim))
for (i in 1:N.sim)
{  sum9[i]=sum(lower[[i]]>0)}
sum(sum9)/(N.sim*(N.treat-1))  # percent of treat vs placebo comparisons that were SS in all datasets 
sum(sum9!=0)/N.sim# percent of networks with at least 1 SS in comparisons vs placebo 


###########################################################################################################
#### scenario F ######
##### type 1 error for differences in group 1
sum1=c(rep(100,N.sim))
lower_g1=list()
upper_g1=list()
for (i in 1:N.sim)
{   lower_g1[[i]]=c(lower_D[[i]][10:19])
upper_g1[[i]]=c(upper_D[[i]][10:19])
sum1[i]=sum(lower_g1[[i]]>0)+sum(upper_g1[[i]]<0)}
sum(sum1)/(10*N.sim) ## percent of SS findings between treatments in group 1
sum(sum1>0)/N.sim ## percent of datasets with at least 1 SS findings between treatments in group 1

##### type 1 error for differences in group 2 
sum2=c(rep(100,N.sim))
lower_g2=list()
upper_g2=list()
for (i in 1:N.sim)
{   lower_g2[[i]]=c(lower_D[[i]][6:9],lower_D[[i]][30],lower_D[[i]][36:37],lower_D[[i]][43:45])
upper_g2[[i]]=c(upper_D[[i]][6:9],upper_D[[i]][30],upper_D[[i]][36:37],upper_D[[i]][43:45])
sum2[i]=sum(lower_g2[[i]]>0)+sum(upper_g2[[i]]<0)}
sum(sum2)/(10*N.sim) ## percent of SS findings between treatments in group 2 
sum(sum2>0)/N.sim ## percent of datasets with at least 1 SS findings between group 2 treatments

##### type 1 error for differences overall
sum(sum1>0|sum2>0)/N.sim


#### power to detect differences between drugs
sum3=c(rep(100,N.sim))
lower_g3=list()
# upper_g3=list()
for (i in 1:N.sim)
{   lower_g3[[i]]=c(lower_D[[i]][1:5],lower_D[[i]][20:29],lower_D[[i]][31:35],lower_D[[i]][38:42])
sum3[i]=sum(lower_g3[[i]]>0)}
sum(sum3)/(25*N.sim) ## power: percent of SS findings between treatments of group 2 vs 1
sum(sum3>0)/N.sim ## percent of datasets with at least 1 SS findings between treatments group 2 vs 1



###########################################################################################################
#### scenarios G and H ######
#### power to detect differences between drugs and placebo
sum3=c(rep(100,N.sim))
lower_g3=list()
for (i in 1:N.sim)
{   sum3[i]=sum(lower_D[[i]]>0)}
sum(sum3)/(45*N.sim) ## power: percent of SS findings between all treatments

##### false positives
sum4=c(rep(100,N.sim))
for (i in 1:N.sim)
{   sum4[i]=sum(upper_D[[i]]<0)}
sum(sum4)/(45*N.sim) ## percent of false findings (wrong direction of effects) 


