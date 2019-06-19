##### SCENARIO E.2 #######
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
TE=rep(1,9) ### true treatment effects for basic parameters
for (i in 1:N.sim)
{ seTE=c(rep(0,N.stud)) 
TE1=c(rep(1,N.studies.comp*(N.treat-1)),rep(0,N.stud-N.studies.comp*(N.treat-1))) 
data1[[i]]=data.frame(TE1,seTE)
data1[[i]]$seTE=0.2*rchisq(N.stud,df=1)+0.5
data1[[i]]$TE=rnorm(N.stud,TE1,data1[[i]]$seTE)
data1[[i]]$studlab=c(1:(N.stud))
data1[[i]]$t1=t1
data1[[i]]$t2=t2}
#######
