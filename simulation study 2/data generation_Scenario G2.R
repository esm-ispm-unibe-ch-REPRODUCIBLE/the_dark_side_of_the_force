
##### SCENARIO G.2 ########
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
TE1=c(0.9+0.1*(1:(N.treat-1)))
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
TE=TE1[2:N.treat]
############