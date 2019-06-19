set.seed(42)

##### SCENARIO G.1 #######
N.treat=10
N.studies.comp=1 # studies per comparison
data1=list()
net1=list()
#define the treatments in the studies
t1=c(rep(1,N.studies.comp*(N.treat-1)))
t2=c()
for (j in 2:N.treat)  {t2=c(t2,rep(j,N.studies.comp))}
N.stud=length(t1)
TE=c(0.9+0.1*(1:(N.treat-1)))
for (i in 1:N.sim)
{ seTE=c(rep(0,N.studies.comp*(N.treat-1))) 
data1[[i]]=data.frame(TE,seTE)
data1[[i]]$seTE=0.2*rchisq(N.stud,df=1)+0.5
data1[[i]]$TE=rnorm(N.studies.comp*(N.treat-1),TE,data1[[i]]$seTE)
data1[[i]]$studlab=c(1:(N.studies.comp*(N.treat-1)))
data1[[i]]$t1=t1
data1[[i]]$t2=t2}
######