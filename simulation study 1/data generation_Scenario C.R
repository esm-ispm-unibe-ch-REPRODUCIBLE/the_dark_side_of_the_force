set.seed(42)


##### SCENARIO C #######
### 2 treatments, zero treatment effects
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


