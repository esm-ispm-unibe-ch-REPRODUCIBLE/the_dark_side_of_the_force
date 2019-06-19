set.seed(42)

##### SCENARIO A.2 ##############
#### 10 treatments, no treatment effects in the network, fullz connected network, homogeneity
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
TE=0
  data1[[i]]=data.frame(TE,seTE)
  data1[[i]]$seTE=0.2*rchisq(N.stud,df=1)+0.5
  data1[[i]]$TE=rnorm(N.stud,0,data1[[i]]$seTE)
  data1[[i]]$studlab=c(1:(N.stud))
  data1[[i]]$t1=t1
  data1[[i]]$t2=t2}
##############
