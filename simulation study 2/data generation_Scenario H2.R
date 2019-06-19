set.seed(42)

##### SCENARIO H.2 #######
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
comp=c(rep(0,N.stud))
for (i in 1:N.stud){if  ((t1[i]==1&t2[i] %in% c(2:5))){comp[i]=1}
  if  ((t1[i]==1&t2[i] %in% c(6:10))){comp[i]=2}
  if  ((t1[i]%in% c(2:5)&t2[i] %in% c(6:10))){comp[i]=1}
  }
for (i in 1:N.sim)
{  seTE=c(rep(0,N.stud)) 
TE=c(rep(0,N.stud)) 
data1[[i]]=data.frame(TE,seTE)
data1[[i]]$seTE=0.2*rchisq(N.stud,df=1)+0.5
data1[[i]]$TE=rnorm(N.stud,comp,data1[[i]]$seTE)
data1[[i]]$studlab=c(1:(N.stud))
data1[[i]]$t1=t1
data1[[i]]$t2=t2}
TE=c(1,1,1,1,2,2,2,2,2)
######

