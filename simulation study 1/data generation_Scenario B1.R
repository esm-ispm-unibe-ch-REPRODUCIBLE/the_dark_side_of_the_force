set.seed(42)

##### SCENARIO B.1 #######
#### 10 treatments, no treatment effects in the network, star network, heterogeneity
N.treat=10
N.studies.comp=3 # studies per comparison
data1=list()
#define the treatments in the studies
t1=c(rep(1,N.studies.comp*(N.treat-1)))
t2=c()
for (j in 2:N.treat)  {t2=c(t2,rep(j,N.studies.comp))}
###generate data
tau_sq=rlnorm(N.sim,-2.56,1.74)
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

