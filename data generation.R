set.seed(42)
rm(list = ls())


####################################
###### SIMULATION STUDY 1 ##########
####################################

##### SCENARIO A.1 ##############
N.sim=1000
N.treat=10
N.studies.comp=1 # studies per comparison
data1=list()
#define the treatments in the studies
t1=c(rep(1,N.studies.comp*(N.treat-1)))
t2=c()
for (j in 2:N.treat)  {t2=c(t2,rep(j,N.studies.comp))}
###generate data
for (i in 1:N.sim)
{ seTE=c(rep(0,N.studies.comp*(N.treat-1))) 
 TE=c(rep(0,N.studies.comp*(N.treat-1))) 
 data1[[i]]=data.frame(TE,seTE)
  data1[[i]]$seTE=0.2*rchisq(N.studies.comp*(N.treat-1),df=1)+0.5
  data1[[i]]$TE=rnorm(N.studies.comp*(N.treat-1),0,data1[[i]]$seTE)
  data1[[i]]$studlab=c(1:(N.studies.comp*(N.treat-1)))
  data1[[i]]$t1=t1
  data1[[i]]$t2=t2}
##############

##### SCENARIO A.2 ##############
N.sim=1000
N.treat=10
N.studies.comp=1 # studies per comparison
data1=list()

#define the treatments in the studies
t1=c()
t2=c()
for (i in 1:(N.treat-1)){
  for (k in (i+1):N.treat){
    for(j in 1:N.studies.comp){
      t1=c(t1,i)
      t2=c(t2,k)      }}}
N.stud=length(t1)
for (i in 1:N.sim)
{  seTE=c(rep(0,N.stud)) 
  TE=c(rep(0,N.stud)) 
  data1[[i]]=data.frame(TE,seTE)
  data1[[i]]$seTE=0.2*rchisq(N.stud,df=1)+0.5
  data1[[i]]$TE=rnorm(N.stud,0,data1[[i]]$seTE)
  data1[[i]]$studlab=c(1:(N.stud))
  data1[[i]]$t1=t1
  data1[[i]]$t2=t2}
##############

##### SCENARIO B.1 #######
N.sim=1000
N.treat=10
N.studies.comp=3 # studies per comparison
data1=list()
#define the treatments in the studies
t1=c(rep(1,N.studies.comp*(N.treat-1)))
t2=c()
for (j in 2:N.treat)  {t2=c(t2,rep(j,N.studies.comp))}
###generate data
tau_sq=rlnorm(N.sim,-3,1.5)
tau=sqrt(tau_sq)
for (i in 1:N.sim)
{  seTE=c(rep(0,N.studies.comp*(N.treat-1))) 
  TE_true=c(rep(0,N.studies.comp*(N.treat-1))) 
  TE=c(rep(0,N.studies.comp*(N.treat-1))) 
  data1[[i]]=data.frame(TE,seTE)
  data1[[i]]$seTE=0.2*rchisq(N.studies.comp*(N.treat-1),df=1)+0.5
  data1[[i]]$TE_true=rnorm(N.studies.comp*(N.treat-1),0,tau[i])
  data1[[i]]$TE=rnorm(N.studies.comp*(N.treat-1),data1[[i]]$TE_true,data1[[i]]$seTE)
  data1[[i]]$studlab=c(1:(N.studies.comp*(N.treat-1)))
  data1[[i]]$t1=t1
  data1[[i]]$t2=t2}
############

##### SCENARIO B.2 #######
N.sim=1000
N.treat=10
N.studies.comp=3 # studies per comparison
data1=list()
#define the treatments in the studies
t1=c()
t2=c()
for (i in 1:(N.treat-1)){
  for (k in (i+1):N.treat){
    for(j in 1:N.studies.comp){
      t1=c(t1,i)
      t2=c(t2,k)    }}}
###generate data
tau_sq=rlnorm(N.sim,-3,1.5)
tau=sqrt(tau_sq)
N.stud=length(t1)
for (i in 1:N.sim)
{  seTE=c(rep(0,N.stud)) 
  TE_true=c(rep(0,N.stud)) 
  TE=c(rep(0,N.stud))
  data1[[i]]=data.frame(TE,seTE)
  data1[[i]]$seTE=0.2*rchisq(N.stud,df=1)+0.5
  data1[[i]]$TE_true=rnorm(N.stud,0,tau[i])
  data1[[i]]$TE=rnorm(N.stud,data1[[i]]$TE_true,data1[[i]]$seTE)
  data1[[i]]$studlab=c(1:(N.stud))
  data1[[i]]$t1=t1
  data1[[i]]$t2=t2}
#########

##### SCENARIO C #######
N.sim=1000
N.treat=2
N.studies.comp=5 # studies per comparison
data1=list()
#define the treatments in the studies
t1=c(rep(1,N.studies.comp*(N.treat-1)))
t2=c()
for (j in 2:N.treat)  {t2=c(t2,rep(j,N.studies.comp))}

###generate data
for (i in 1:N.sim)
{  seTE=c(rep(0,N.studies.comp*(N.treat-1))) 
  TE=c(rep(0,N.studies.comp*(N.treat-1))) 
  data1[[i]]=data.frame(TE,seTE)
  data1[[i]]$seTE=0.2*rchisq(N.studies.comp*(N.treat-1),df=1)+0.5
  data1[[i]]$TE=rnorm(N.studies.comp*(N.treat-1),0,data1[[i]]$seTE)
  data1[[i]]$studlab=c(1:(N.studies.comp*(N.treat-1)))
  data1[[i]]$t1=t1
  data1[[i]]$t2=t2}
############

##### SCENARIO D #######
N.sim=1000
N.treat=3
N.studies.comp=1 # studies per comparison
data1=list()
#define the treatments in the studies
t1=c()
t2=c()
for (i in 1:(N.treat-1)){
  for (k in (i+1):N.treat){
    for(j in 1:N.studies.comp){
      t1=c(t1,i)
      t2=c(t2,k)    }}}
N.stud=length(t1)
for (i in 1:N.sim)
{  seTE=c(rep(0,N.stud)) 
  TE=c(rep(0,N.stud)) 
  data1[[i]]=data.frame(TE,seTE)
  data1[[i]]$seTE=0.2*rchisq(N.stud,df=1)+0.5
  data1[[i]]$TE=rnorm(N.stud,0,data1[[i]]$seTE)
  data1[[i]]$studlab=c(1:(N.stud))
  data1[[i]]$t1=t1
  data1[[i]]$t2=t2}
##########


####################################
###### SIMULATION STUDY 2 ##########
####################################

##### SCENARIO E.1 #######
N.sim=1000
N.treat=10
N.studies.comp=1 # studies per comparison
data1=list()
#define the treatments in the studies
t1=c(rep(1,N.studies.comp*(N.treat-1)))
t2=c()
for (j in 2:N.treat)  {t2=c(t2,rep(j,N.studies.comp))}
N.stud=length(t1)
# all treatment effects vs. placebo are equal to 1. No relative effects between treatments
for (i in 1:N.sim)
{  seTE=c(rep(0,N.studies.comp*(N.treat-1))) 
  TE=c(rep(1,N.studies.comp*(N.treat-1))) 
  data1[[i]]=data.frame(TE,seTE)
  data1[[i]]$seTE=0.2*rchisq(N.stud,df=1)+0.5
  data1[[i]]$TE=rnorm(N.studies.comp*(N.treat-1),1,data1[[i]]$seTE)
  data1[[i]]$studlab=c(1:(N.studies.comp*(N.treat-1)))
  data1[[i]]$t1=t1
  data1[[i]]$t2=t2}
##########

##### SCENARIO E.2 #######
N.sim=1000
N.treat=10
N.studies.comp=1 # studies per comparison
data1=list()
#define the treatments in the studies
t1=c()
t2=c()
for (i in 1:(N.treat-1)){
  for (k in (i+1):N.treat){
    for(j in 1:N.studies.comp){
      t1=c(t1,i)
      t2=c(t2,k)  }}}
N.stud=length(t1)
for (i in 1:N.sim)
{ seTE=c(rep(0,N.stud)) 
  TE=c(rep(1,N.treat-1),rep(0,N.stud-N.treat+1)) 
  data1[[i]]=data.frame(TE,seTE)
  data1[[i]]$seTE=0.2*rchisq(N.stud,df=1)+0.5
  data1[[i]]$TE=rnorm(N.stud,TE,data1[[i]]$seTE)
  data1[[i]]$studlab=c(1:(N.stud))
  data1[[i]]$t1=t1
  data1[[i]]$t2=t2}
#######

##### SCENARIO F.1 #######
N.sim=1000
N.treat=10
N.studies.comp=1 # studies per comparison
data1=list()
#define the treatments in the studies
t1=c()
t2=c()
t1=c(rep(1,N.studies.comp*(N.treat-1)))
for (j in 2:N.treat)  {t2=c(t2,rep(j,N.studies.comp))}
N.stud=length(t1)
comp=c(rep(1,N.stud))
for (i in 1:N.stud){if ((t1[i]<6&t2[i]<6)|(t1[i]>5&t2[i]>5)){comp[i]=0}}
for (i in 1:N.sim)
{  seTE=c(rep(0,N.stud)) 
TE=c(rep(0,N.stud)) 
data1[[i]]=data.frame(TE,seTE)
data1[[i]]$seTE=0.2*rchisq(N.stud,df=1)+0.5
data1[[i]]$TE=rnorm(N.stud,comp,data1[[i]]$seTE)
data1[[i]]$studlab=c(1:(N.stud))
data1[[i]]$t1=t1
data1[[i]]$t2=t2}
#####

##### SCENARIO F.2 #######
N.sim=1000
N.treat=10
N.studies.comp=1 # studies per comparison
data1=list()
#define the treatments in the studies
t1=c()
t2=c()
for (i in 1:(N.treat-1)){
  for (k in (i+1):N.treat){
    for(j in 1:N.studies.comp){
      t1=c(t1,i)
      t2=c(t2,k)  }}}
N.stud=length(t1)
comp=c(rep(1,N.stud))
for (i in 1:N.stud){if ((t1[i]<6&t2[i]<6)|(t1[i]>5&t2[i]>5)){comp[i]=0}}
for (i in 1:N.sim)
{  seTE=c(rep(0,N.stud)) 
  TE=c(rep(0,N.stud)) 
  data1[[i]]=data.frame(TE,seTE)
  data1[[i]]$seTE=0.2*rchisq(N.stud,df=1)+0.5
  data1[[i]]$TE=rnorm(N.stud,comp,data1[[i]]$seTE)
  data1[[i]]$studlab=c(1:(N.stud))
  data1[[i]]$t1=t1
  data1[[i]]$t2=t2}

######

##### SCENARIO G.1 #######
N.sim=1000
N.treat=10
N.studies.comp=1 # studies per comparison
data1=list()
net1=list()
#define the treatments in the studies
t1=c(rep(1,N.studies.comp*(N.treat-1)))
t2=c()
for (j in 2:N.treat)  {t2=c(t2,rep(j,N.studies.comp))}
N.stud=length(t1)
TE=c(0.7+0.3*(1:(N.treat-1)))
for (i in 1:N.sim)
{ seTE=c(rep(0,N.studies.comp*(N.treat-1))) 
  data1[[i]]=data.frame(TE,seTE)
  data1[[i]]$seTE=0.2*rchisq(N.stud,df=1)+0.5
  data1[[i]]$TE=rnorm(N.studies.comp*(N.treat-1),TE,data1[[i]]$seTE)
  data1[[i]]$studlab=c(1:(N.studies.comp*(N.treat-1)))
  data1[[i]]$t1=t1
  data1[[i]]$t2=t2}
######

##### SCENARIO G.2 ########
N.sim=1000
N.treat=10
N.studies.comp=1 # studies per comparison
data1=list()
#define the treatments in the studies
t1=c()
t2=c()
for (i in 1:(N.treat-1)){
  for (k in (i+1):N.treat){
    for(j in 1:N.studies.comp){
      t1=c(t1,i)
      t2=c(t2,k)  } }}
TE1=c(0.7+0.3*(1:(N.treat-1)))
TE1=c(0,TE1)
TE2=(TE1[t2]-TE1[t1])
N.stud=length(t1)
seTE=c(rep(0,N.stud)) 
TE=c(rep(0,N.stud)) 
for (i in 1:N.sim)
{  data1[[i]]=data.frame(TE,seTE)
  data1[[i]]$seTE=0.2*rchisq(N.stud,df=1)+0.5
  data1[[i]]$TE=rnorm(N.stud,TE2,data1[[i]]$seTE)
  data1[[i]]$studlab=c(1:(N.stud))
  data1[[i]]$t1=t1
  data1[[i]]$t2=t2}
############

##### SCENARIO H.1 ########
N.sim=1000
N.treat=10
N.studies.comp=1 # studies per comparison
data1=list()
TE1=list()
TE2=list()
#define the treatments in the studies
t1=c()
t2=c()
t1=c(rep(1,N.studies.comp*(N.treat-1)))
t2=c()
for (j in 2:N.treat)  {t2=c(t2,rep(j,N.studies.comp))}
N.stud=length(t1)
seTE=c(rep(0,N.stud)) 
TE=c(rep(0,N.stud)) 
for (i in 1:N.sim)
{  TE1[[i]]=sort(rnorm(10,1,0.5))
TE2[[i]]=(TE1[[i]][t2]-TE1[[i]][t1])
data1[[i]]=data.frame(TE,seTE)
data1[[i]]$seTE=0.2*rchisq(N.stud,df=1)+0.5
data1[[i]]$TE=rnorm(N.stud,TE2[[i]],data1[[i]]$seTE)
data1[[i]]$studlab=c(1:(N.stud))
data1[[i]]$t1=t1
data1[[i]]$t2=t2}
###########

##### SCENARIO H.2 ########
N.sim=1000
N.treat=10
N.studies.comp=1 # studies per comparison
data1=list()
TE1=list()
TE2=list()
#define the treatments in the studies
t1=c()
t2=c()
for (i in 1:(N.treat-1)){
  for (k in (i+1):N.treat){
    for(j in 1:N.studies.comp){
      t1=c(t1,i)
      t2=c(t2,k)    }}}
N.stud=length(t1)
seTE=c(rep(0,N.stud)) 
TE=c(rep(0,N.stud)) 
for (i in 1:N.sim)
{  TE1[[i]]=sort(rnorm(10,1,0.5))
  TE2[[i]]=(TE1[[i]][t2]-TE1[[i]][t1])
  data1[[i]]=data.frame(TE,seTE)
  data1[[i]]$seTE=0.2*rchisq(N.stud,df=1)+0.5
  data1[[i]]$TE=rnorm(N.stud,TE2[[i]],data1[[i]]$seTE)
  data1[[i]]$studlab=c(1:(N.stud))
  data1[[i]]$t1=t1
  data1[[i]]$t2=t2}
##########






