#library(devtools)
#install_github("esm-ispm-unibe-ch/NMAJags")
set.seed(42)
library(NMAJags)
library(rjags)

########### load data ##########
d <- read.table(textConnection("
                               study treatment wks rating  responders sampleSize responseimputedR eventD totalR_D
                               Annseaau1993 fluvoxamine 6 HAMD21 24 64 0 23 64
                               Annseaau1993 paroxetine 6 HAMD21 24 56 0 16 56
                               Clerc2001 fluvoxamine 6 HAMD24 32 56 0 17 56
                               Clerc2001 milnacipran 6 HAMD24 40 57 0 15 57
                               Dalery1998 fluvoxamine 6 HAMD17 53 90 0 24 90
                               Dalery1998 fluoxetine 6 HAMD17 54 94 0 20 94
                               Hackett1998 fluvoxamine 6 MADRS 14 34 1 13 34
                               Hackett1998 venlafaxine 6 MADRS 48 77 1 18 77
                               Kato2006 fluvoxamine 6 HAMD21 31 49 0 8 49
                               Kato2006 paroxetine 6 HAMD21 37 52 0 13 52
                               Kiev1997 fluvoxamine 7 HAMD21 17 30 1 11 30
                               Kiev1997 paroxetine 7 HAMD21 16 30 1 9 30
                               Nemeroff1995 fluvoxamine 7 HAMD21 19 49 1 21 49
                               Nemeroff1995 sertraline 7 HAMD21 21 48 1 9 48
                               Rapaport1995 fluvoxamine 7 HAMD21 30 51 1 7 51
                               Rapaport1995 fluoxetine 7 HAMD21 29 49 1 8 49
                               Rossini2002 fluvoxamine 7 HAMD21 29 40 0 1 40
                               Rossini2002 sertraline 7 HAMD21 28 48 0 3 48
                               Schoemaker2002 fluvoxamine 6 HAMD17 127 207 0 41 207
                               Schoemaker2002 mirtazapine 6 HAMD17 132 205 0 47 205
                               Timmerman1993 fluvoxamine 6 HAMD17 31 109 0 29 109
                               Timmerman1993 citalopram 6 HAMD17 33 108 0 22 108
                               Annseau1994 milnacipran 6 HAMD24 32 97 1 23 97
                               Annseau1994 fluoxetine 6 HAMD24 43 93 1 18 93
                               Guelfi1998 milnacipran 12 HAMD17 109 200 0 99 200
                               Guelfi1998 fluoxetine 12 HAMD17 51 100 0 50 100
                               Lee2002 milnacipran 6 HAMD17 15 39 1 16 39
                               Lee2002 fluoxetine 6 HAMD17 12 31 1 15 31
                               Sechter2000 milnacipran 6 HAMD17 74 149 1 29 149
                               Sechter2000 paroxetine 6 HAMD17 78 153 1 33 153
                               Yang2003 milnacipran 8 HAMD17 4 27 1 15 27
                               Yang2003 sertraline 8 HAMD17 2 26 1 11 26
                               Agren1999 mirtazapine 8 MADRS 116 137 0 18 137
                               Agren1999 citalopram 8 MADRS 117 133 0 8 133
                               Amini2005 mirtazapine 6 HAMD17 12 18 0 2 18
                               Amini2005 fluoxetine 6 HAMD17 8 18 0 3 18
                               Benkert1999 mirtazapine 6 HAMD17 74 139 0 30 139
                               Benkert1999 paroxetine 6 HAMD17 66 136 0 33 136
                               Guelfi2000 mirtazapine 8 HAMD17 48 78 0 18 78
                               Guelfi2000 venlafaxine 8 HAMD17 39 79 0 28 79
                               Hong2003 mirtazapine 6 HAMD17 35 66 0 30 66
                               Hong2003 fluoxetine 6 HAMD17 30 66 0 22 66
                               Kjernisted2002 mirtazapine 8 HAMD17 117 176 0 41 176
                               Kjernisted2002 sertraline 8 HAMD17 114 170 0 32 170
                               Schazberg2000 mirtazapine 8 HAMD17 72 128 0 29 128
                               Schazberg2000 paroxetine 8 HAMD17 60 126 0 39 126
                               Szegedi2005 mirtazapine 6 HAMD17 65 130 1 39 130
                               Szegedi2005 venlafaxine 6 HAMD17 52 128 1 47 128
                               Versiani2005 mirtazapine 8 HAMD17 106 147 0 NA 147
                               Versiani2005 fluoxetine 8 HAMD17 104 152 0 NA 152
                               Wade2003 mirtazapine 24 HAMD17 38 99 0 40 99
                               Wade2003 paroxetine 24 HAMD17 34 98 0 38 98
                               Wheatley1997 mirtazapine 6 HAMD17 39 66 0 17 66
                               Wheatley1997 fluoxetine 6 HAMD17 28 67 0 21 67
                               Winokur2000 mirtazapine 8 HAMD21 8 9 1 1 9
                               Winokur2000 fluoxetine 8 HAMD21 6 13 1 2 13
                               Akkaya2003 reboxetine 10 HAMD17 32 57 0 7 57
                               Akkaya2003 venlafaxine 10 HAMD17 37 50 0 7 50
                               Bosc1997a reboxetine 8 NA 49 126 0 38 126
                               Bosc1997a fluoxetine 8 NA 55 127 0 30 127
                               Bosc1997b reboxetine 8 NA 46 79 0 20 79
                               Bosc1997b fluoxetine 8 NA 51 89 0 20 89
                               Berlanga2004 reboxetine 8 HAMD21 27 47 0 8 47
                               Berlanga2004 citalopram 8 HAMD21 33 54 0 15 54
                               Clayton2002 reboxetine 8 HAMD17 59 150 0 63 150
                               Clayton2002 fluoxetine 8 HAMD17 79 150 0 47 150
                               Eker2005 reboxetine 10 HAMD17 16 25 0 5 25
                               Eker2005 sertraline 10 HAMD17 17 24 0 3 24
                               Langworth2006 reboxetine 12 HAMD21 83 177 0 65 177
                               Langworth2006 citalopram 12 HAMD21 112 173 0 36 173
                               Taner2006 reboxetine 8 HAMD17 14 22 1 5 22
                               Taner2006 fluoxetine 8 HAMD17 19 21 1 1 21
                               Coleman1999 sertraline 8 HAMD31 67 118 0 43 118
                               Coleman1999 bupropion 8 HAMD31 78 122 0 27 122
                               Chen2001 sertraline 6 CGI 26 45 0 0 45
                               Chen2001 venlafaxine 6 CGI 31 44 0 1 44
                               Bennie1995 sertraline 6 HAMD17 73 142 0 24 142
                               Bennie1995 fluoxetine 6 HAMD17 63 144 0 23 144
                               Fava2000 sertraline 12 HAMD17 37 43 0 10 43
                               Fava2000 fluoxetine 12 HAMD17 26 35 0 16 35
                               Fava2000 paroxetine 12 HAMD17 23 30 0 13 30
                               Fava2002 sertraline 10 HAMD17 70 96 0 26 96
                               Fava2002 fluoxetine 10 HAMD17 57 92 0 24 92
                               Fava2002 paroxetine 10 HAMD17 64 96 0 27 96
                               Newhouse2000 sertraline 12 HAMD24 85 117 0 37 117
                               Newhouse2000 fluoxetine 12 HAMD24 84 119 0 39 119
                               VanMoffaert1995 sertraline 8 HAMD17 49 83 0 14 83
                               VanMoffaert1995 fluoxetine 8 HAMD17 48 82 0 16 82
                               Oslin2003 sertraline 10 HAMD17 9 25 1 5 25
                               Oslin2003 venlafaxine 10 HAMD17 5 27 1 17 27
                               Mehtonen2000 sertraline 8 HAMD21 41 72 0 12 72
                               Mehtonen2000 venlafaxine 8 HAMD21 49 75 0 16 75
                               Kavoussi1997 sertraline 16 HAMD31 85 126 0 NA 126
                               Kavoussi1997 bupropion 16 HAMD31 82 122 0 NA 122
                               Croft1999 sertraline 8 HAMD29 79 119 0 39 119
                               Croft1999 bupropion 8 HAMD29 77 120 0 36 120
                               Zanardi1996 sertraline 6 HAMD21 18 24 0 0 24
                               Zanardi1996 paroxetine 6 HAMD21 6 22 0 9 22
                               Aguglia1993 sertraline 8 HAMD17 35 52 1 17 52
                               Aguglia1993 fluoxetine 8 HAMD17 26 56 1 31 56
                               Suri2000 sertraline 6 HAMD21 9 35 0 7 35
                               Suri2000 fluoxetine 6 HAMD21 5 18 0 2 18
                               Sir2005 sertraline 8 HAMD17 56 79 0 13 79
                               Sir2005 venlafaxine 8 HAMD17 56 84 0 25 79
                               Shelton2006 sertraline 8 HAMD17 45 82 0 19 82
                               Shelton2006 venlafaxine 8 HAMD17 49 78 0 11 78
                               DeWilde1993 paroxetine 6 HAMD21 25 37 0 6 37
                               DeWilde1993 fluoxetine 6 HAMD21 26 41 0 9 41
                               MY1045BRL0290601 paroxetine 12 HAMD21 217 357 0 137 357
                               MY1045BRL0290601 fluoxetine 12 HAMD21 244 351 0 114 351
                               MY1043BRL029060115 paroxetine 12 HAMD21 152 284 0 109 284
                               MY1043BRL029060115 fluoxetine 12 HAMD21 167 289 0 108 289
                               Tignol1993 paroxetine 6 MADRS 53 89 0 25 89
                               Tignol1993 fluoxetine 6 MADRS 54 87 0 33 87
                               Gagiano1993 paroxetine 6 HAMD21 30 45 0 8 45
                               Gagiano1993 fluoxetine 6 HAMD21 27 45 0 10 45
                               Wehis2000 paroxetine 6 HAMD31 40 52 0 8 52
                               Wehis2000 bupropion 6 HAMD31 34 48 0 8 48
                               z2906421 paroxetine 6 CGI-I NA 123 0 50 123
                               z2906421 fluoxetine 6 CGI-I NA 119 0 45 119
                               AbergWisted2000 paroxetine 8 MADRS 111 177 0 26 177
                               AbergWisted2000 sertraline 8 MADRS 116 176 0 33 176
                               Goldstein2004 paroxetine 8 HAMD17 34 87 0 38 87
                               Goldstein2004 duloxetine 8 HAMD17 81 177 0 69 177
                               Detke2004 paroxetine 8 HAMD17 63 86 0 10 86
                               Detke2004 duloxetine 8 HAMD17 126 188 0 21 188
                               z29060785 paroxetine 6 MADRS 77 199 0 41 199
                               z29060785 citalopram 6 MADRS 102 207 0 43 207
                               ID4091 paroxetine 8 HAMD17 38 89 0 31 89
                               ID4091 duloxetine 8 HAMD17 52 175 0 58 175
                               Chouinard1999 paroxetine 12 HAMD21 51 102 0 40 102
                               Chouinard1999 fluoxetine 12 HAMD21 53 101 0 33 101
                               Fava1998 paroxetine 12 HAMD21 32 55 0 16 55
                               Fava1998 fluoxetine 12 HAMD21 31 54 0 16 54
                               Ontiveros1994 paroxetine 6 HAMD 41 60 0 7 60
                               Ontiveros1994 fluoxetine 6 HAMD 35 62 0 9 62
                               z29060365 paroxetine 8 HAMD17 33 68 1 21 68
                               z29060365 fluoxetine 8 HAMD17 32 70 1 27 70
                               Baldwin2005 paroxetine 12 MADRS 111 159 0 14 159
                               Baldwin2005 escitalopram 12 MADRS 112 166 0 15 166
                               GeretseggerMY1021/BRC paroxetine 6 HAMD21 19 54 0 9 54
                               GeretseggerMY1021/BRC fluoxetine 6 HAMD21 8 52 0 9 52
                               Perahia2006 paroxetine 8 HAMD17 65 97 0 11 97
                               Perahia2006 duloxetine 8 HAMD17 139 196 0 23 196
                               Goldstein2002 duloxetine 8 HAMD17 32 70 0 24 70
                               Goldstein2002 fluoxetine 8 HAMD17 15 33 0 12 33
                               Allard2004 citalopram 12 MADRS 50 75 0 NA 75
                               Allard2004 venlafaxine 12 MADRS 49 76 0 NA 76
                               Ekselius1997 citalopram 8 MADRS 136 200 0 36 200
                               Ekselius1997 sertraline 8 MADRS 139 200 0 52 200
                               Khanzode2003 citalopram 12 HAMD21 3 33 1 3 33
                               Khanzode2003 fluoxetine 12 HAMD21 4 34 1 2 34
                               Stahl2000 citalopram 8 HAMD21 NA 107 0 24 107
                               Stahl2000 sertraline 8 HAMD21 NA 108 0 30 108
                               Bielski2004 escitalopram 8 MADRS 59 101 0 29 101
                               Bielski2004 venlafaxine 8 MADRS 47 101 0 35 101
                               Burke2000 escitalopram 8 MADRS 122 252 0 63 252
                               Burke2000 citalopram 8 MADRS 57 127 0 34 127
                               Lepola2001 escitalopram 8 MADRS 95 156 0 10 156
                               Lepola2001 citalopram 8 MADRS 79 161 0 9 161
                               Montgomery2004 escitalopram 8 HAMD17 113 148 0 23 148
                               Montgomery2004 venlafaxine 8 HAMD17 113 145 0 21 145
                               Colonna2005 escitalopram 12 MADRS 104 175 0 25 175
                               Colonna2005 citalopram 12 MADRS 96 182 0 30 182
                               Moore2005 escitalopram 8 MADRS 105 142 0 10 142
                               Moore2005 citalopram 8 MADRS 87 152 0 25 152
                               Nieremberg2007 escitalopram 8 HAMD17 109 274 0 66 274
                               Nieremberg2007 duloxetine 8 HAMD17 113 273 0 85 273
                               Wade2007 escitalopram 12 HAMD17 94 144 0 NA 144
                               Wade2007 duloxetine 12 HAMD17 81 151 0 NA 151
                               Khan2007 escitalopram 8 HAMD24 83 140 0 21 140
                               Khan2007 duloxetine 8 HAMD24 66 138 0 46 138
                               SCT-MD16 escitalopram 8 MADRS 65 102 1 36 102
                               SCT-MD16 fluoxetine 8 MADRS 65 103 1 26 103
                               Ventura2007 escitalopram 8 HAMD24 75 107 0 19 107
                               Ventura2007 sertraline 8 HAMD24 74 108 0 15 108
                               SCT-MD35 escitalopram 8 MADRS NA 138 0 32 138
                               SCT-MD35 bupropion 8 MADRS NA 138 0 34 138
                               SCT-MD02 escitalopram 8 MADRS NA 129 0 33 129
                               SCT-MD02 citalopram 8 MADRS NA 128 0 29 128
                               SCT-MD27 escitalopram 8 MADRS 69 136 1 28 136
                               SCT-MD27 sertraline 8 MADRS 78 138 1 25 138
                               Kasper2005 escitalopram 8 CGI-I 78 174 0 30 174
                               Kasper2005 fluoxetine 8 CGI-I 61 164 0 42 164
                               Alves1999 fluoxetine 12 HAMD17 35 47 0 9 47
                               Alves1999 venlafaxine 12 HAMD17 36 40 0 10 40
                               Bougerol1997a fluoxetine 8 HAMD21 113 184 1 21 184
                               Bougerol1997a citalopram 8 HAMD21 115 173 1 24 173
                               Bougerol1997b fluoxetine 8 MADRS 102 158 1 45 158
                               Bougerol1997b citalopram 8 MADRS 98 158 1 48 158
                               Clerc1994 fluoxetine 6 HAMD21 17 34 0 12 34
                               Clerc1994 venlafaxine 6 HAMD21 24 34 0 6 34
                               Coleman2001 fluoxetine 8 HAMD21 83 154 0 57 154
                               Coleman2001 bupropion 8 HAMD21 76 150 0 56 150
                               Costaesilva1998 fluoxetine 8 HAMD17 153 186 0 18 186
                               Costaesilva1998 venlafaxine 8 HAMD17 170 196 0 29 196
                               DeNayer2002 fluoxetine 12 HAMD17 29 73 0 24 73
                               DeNayer2002 venlafaxine 12 HAMD17 37 73 0 29 73
                               DiazMaritinez1998 fluoxetine 8 HAMD17 46 75 1 20 75
                               DiazMaritinez1998 venlafaxine 8 HAMD17 45 70 1 15 70
                               Dierick1996 fluoxetine 8 HAMD17 67 161 0 40 161
                               Dierick1996 venlafaxine 8 HAMD17 80 153 0 38 153
                               Feighner1991 fluoxetine 6 HAMD17 35 62 0 18 62
                               Feighner1991 bupropion 6 HAMD17 37 61 0 16 61
                               Rudolph1999 fluoxetine 8 HAMD21 51 103 0 29 103
                               Rudolph1999 venlafaxine 8 HAMD21 54 100 0 19 100
                               Sechter1999 sertraline 8 HAMD17 48 118 0 NA 118
                               Sechter1999 fluoxetine 8 HAMD17 35 120 0 NA 120
                               Silverstone1999 fluoxetine 12 HAMD21 75 119 0 32 119
                               Silverstone1999 venlafaxine 12 HAMD21 79 122 0 37 122
                               Tylee1997 fluoxetine 12 HAMD17 58 170 0 46 170
                               Tylee1997 venlafaxine 12 HAMD17 67 171 0 47 171
                               Tzanakaki2000 fluoxetine 6 HAMD21 31 54 0 12 54
                               Tzanakaki2000 venlafaxine 6 HAMD21  36 55 0 12 55
                               Schatzberg2006 fluoxetine 8 HAMD21 NA 100 0 30 100
                               Schatzberg2006 venlafaxine 8 HAMD21 NA 104 0 36 104
                               Nemeroff2007 fluoxetine 6 HAMD21 45 104 0 18 104
                               Nemeroff2007 venlafaxine 6 HAMD21 51 102 0 24 102
                               Thase2006 venlafaxine 12 HAMD17 82 177 0 81 177
                               Thase2006 bupropion 12 HAMD17 86 171 0 72 171
                               WXL101497 bupropion 8 MADRS 106 188 0 33 188
                               WXL101497 venlafaxine 8 MADRS 120 189 0 25 189
                               Clayton2006aw bupropion 8 HAMD17 82 141 0 32 141
                               Clayton2006aw escitalopram 8 HAMD17 90 138 0 33 138
                               Clayton2006bw bupropion 8 HAMD17 81 138 0 39 138
                               Clayton2006bw escitalopram 8 HAMD17 82 149 0 44 149
                               AK130939 bupropion 8 MADRS 115 204 0 45 204
                               AK130939 venlafaxine 8 MADRS 127 198 0 46 198
                               WELL_AK1A4006 bupropion 8 HAMD21 74 158 0 62 158
                               WELL_AK1A4006 fluoxetine 8 HAMD21 88 155 0 59 155
                               WELL_AK140016 bupropion 8 SIGH-D NA NA 0 14 69
                               WELL_AK140016 paroxetine 8 SIGH-D NA NA 0 18 71
                               McPartlin1998 venlafaxine 12 MADRS 113 183 0 47 183
                               McPartlin1998 paroxetine 12 MADRS 105 178 0 52 178
                               Boulenger2006 escitalopram 24 MADRS 162 232 1 25 232
                               Boulenger2006 paroxetine 24 MADRS 144 227 1 36 227"), header = TRUE)


data1=data.frame(as.character(d$study),as.character(d$treatment),d$responders,d$sampleSize, stringsAsFactors = F)

data1=data1[!is.na(data1$d.responders),]
colnames(data1)=c("study","treat","responders","total")

stun=unique(data1$study)
for (i in 1:length(data1$study)){
  for( j in 1:length(stun)){
    if (data1$study[i]==stun[j]){data1$study1[i]=j}
  }  }


##############

dataJAGS=make.jagsNMA.data(studyid=study1, r=responders,n=total, t=treat, data=data1, type="binary")
dataJAGS$ns

#       DRUGS          codes
#1     bupropion         1
#2    citalopram         2
#3    duloxetine         3
#4  escitalopram         4
#5    fluoxetine         5
#6   fluvoxamine         6
#7   milnacipran         7
#8   mirtazapine         8
#9    paroxetine         9
#10   reboxetine        10
#11   sertraline        11
#12  venlafaxine        12

######## usual NMA model I ####
model1.string <-  "
model {
for(i in 1:ns) { 
w[i,1]<- 0
theta[i,t[i,1]]<- 0                                             

##binomial likelihood of number of events for each arm k of study i		     
for (k in 1:na[i]) {r[i,t[i,k]] ~ dbin(p[i,t[i,k]],n[i,t[i,k]])}    

##parameterization of the 'true' effect of each comparison 
##of arm k vs. baseline arm (1) of study i                 
logit(p[i,t[i,1]])<- u[i]		                    
for (k in 2:na[i]) {
logit(p[i,t[i,k]])<- u[i] + theta[i,t[i,k]]

##distribution of random effects
theta[i,t[i,k]] ~ dnorm(md[i,t[i,k]],precd[i,t[i,k]])


## accounting for correlation between effect sizes estimated in multi-arm trials				             
md[i,t[i,k]]<- mean[i,k] + sw[i,k]                                   
w[i,k]<- (theta[i,t[i,k]]  - mean[i,k])          
sw[i,k]<- sum(w[i,1:(k-1)])/(k-1)
precd[i,t[i,k]]<- prec *2*(k-1)/k  

##consistency equations
mean[i,k] <-d[t[i,k]] - d[t[i,1]] 

}}

##prior distribution for log-odds in baseline arm of study i
for (i in 1:ns) {u[i] ~ dnorm(0,.01)}

##prior distribution for heterogeneity	
tau ~ dunif(0,3)                                   
prec<- 1/pow(tau,2)
tau.sq<- pow(tau,2)

##prior distribution for basic parameters		
d[ref] <- 0	
for(k in 1:(ref-1)) {d[k] ~ dnorm(0,.0001)}
for(k in (ref+1):nt) {d[k] ~ dnorm(0,.0001)}

##OR for each comparison 
for(i in 1:(nt-1)) {
for (j in (i+1):nt) {
OR[j,i]<- exp(d[j] - d[i])
LOR[j,i]<- d[j] - d[i]}}

for(j in 1:(ref-1)){ORref[j]<- exp(d[j] - d[ref])}
for(j in (ref+1):nt) {ORref[j]<- exp(d[j] - d[ref])}


for(j in 2:nt){dd[j]<-d[j]-d[1]}

#Ranking of treatments
#TreatmeNT hierarchy
order[1:nt]<- nt+1- rank(d[1:nt])
for(k in 1:nt) {
# this is when the outcome is positive - omit  'nt+1-' when the outcome is negative
most.effective[k]<-equals(order[k],1)
for(j in 1:nt) {
effectiveness[k,j]<- equals(order[k],j)}}
for(k in 1:nt) {
for(j in 1:nt) {
cumeffectiveness[k,j]<- sum(effectiveness[k,1:j])}}

#SUCRAS
for(k in 1:nt) {
SUCRA[k]<- sum(cumeffectiveness[k,1:(nt-1)]) /(nt-1)}}

#for(k in 1:nt) {
#order[k]<- rank(d[],k) # this is when the outcome is negative - change to 'order[k]<- t+1-rank(d[],k) when the outcome is positive
#most.effective[k]<-equals(order[k],1)
#for(j in 1:nt) {effectiveness[k,j]<- equals(order[k],j)
#cumeffectiveness[k,j]<- sum(effectiveness[k,1:j])}}		

##SUCRAS
#for(k in 1:nt) {
#SUCRA[k]<- sum(cumeffectiveness[k,1:(nt-1)]) /(nt-1)
#}}

"
####



#### fit model I #####
model1.spec<-textConnection(model1.string) 
data <- list(ns=dataJAGS$ns,ref=1, nt=dataJAGS$nt, n=dataJAGS$n, r=dataJAGS$r, t=dataJAGS$t, na=dataJAGS$na)
jags.m <- jags.model(model1.spec, data = data, n.chains = 2, n.adapt = 5000)

params <- c("dd[2]","dd[3]","dd[4]","dd[5]","dd[6]","dd[7]","dd[8]","dd[9]","dd[10]","dd[11]","dd[12]", "tau")

for (i in 1:11){
  for (j in (i+1):12){
    params=c(params, paste("OR[",j,",",i,"]",sep=""))
    closeAllConnections()    
  }
}

for (i in 1:12){
  params=c(params, paste("SUCRA[",i,"]" ,sep=""))
}


samps<- coda.samples(jags.m, params, n.iter = 10000)
burn.in <- 5000 
A1=summary(samps,start = burn.in)

A2=data.frame(A1$quantiles[,"50%"],A1$quantiles[,"2.5%"],A1$quantiles[,"97.5%"] )
A2=A2[1:66,]
A2=round(A2, digits=2)
colnames(A2)=c("median","low","up")

A2$x=0
A2$y=0

nam=rownames(A2)
A2[substr(nam, start=5, stop=5)==",",]$x=substr(nam[substr(nam, start=5, stop=5)==","], start=4, stop=4)
A2[substr(nam, start=5, stop=5)==",",]$y=substr(nam[substr(nam, start=5, stop=5)==","], start=6, stop=6)
A2[substr(nam, start=6, stop=6)==",",]$x=substr(nam[substr(nam, start=6, stop=6)==","], start=4, stop=5)
A2[substr(nam, start=6, stop=6)==",",]$y=substr(nam[substr(nam, start=6, stop=6)==","], start=7, stop=7)
A2[substr(nam, start=6, stop=6)==","&nchar(nam)==9,]$y=substr(nam[substr(nam, start=6, stop=6)==","&nchar(nam)==9], start=7, stop=8)
A2$x=as.numeric(A2$x)
A2$y=as.numeric(A2$y)
A3=A2[order(A2$y,A2$x),]

league.table=matrix(rep(0,144),nrow=12)
league.table[lower.tri(league.table, diag=FALSE)]=round(1/A3$median,digits=2)
llow=matrix(rep(0,144),nrow=12)
llow[lower.tri(league.table, diag=FALSE)]=round(1/A3$up,digits=2)
lup=matrix(rep(0,144),nrow=12)
lup[lower.tri(league.table, diag=FALSE)]=round(1/A3$low,digits=2)

league.table1=matrix(paste(league.table,"[",llow,";",lup,"]", sep=""),ncol=ncol(league.table))
names=c("bupropion","citalopram","duloxetine",
        "escitalopram","fluoxetine","fluvoxamine","milnacipran","mirtazapine",
        "paroxetine","reboxetine","sertraline","venlafaxine")
diag(league.table1)=names
league.table1[upper.tri(league.table1)]=""
league.table=data.frame(league.table1)
league.tableST=league.table


#######

######## model II####
model1.string <-  "
model {
for(i in 1:ns) { 
w[i,1]<- 0
theta[i,t[i,1]]<- 0                                             

##binomial likelihood of number of events for each arm k of study i		     
for (k in 1:na[i]) {r[i,t[i,k]] ~ dbin(p[i,t[i,k]],n[i,t[i,k]])}    

##parameterization of the 'true' effect of each comparison 
##of arm k vs. baseline arm (1) of study i                 
logit(p[i,t[i,1]])<- u[i]		                    
for (k in 2:na[i]) {
logit(p[i,t[i,k]])<- u[i] + theta[i,t[i,k]]

##distribution of random effects
theta[i,t[i,k]] ~ dnorm(md[i,t[i,k]],precd[i,t[i,k]])


## accounting for correlation between effect sizes estimated in multi-arm trials				             
md[i,t[i,k]]<- mean[i,k] + sw[i,k]                                   
w[i,k]<- (theta[i,t[i,k]]  - mean[i,k])          
sw[i,k]<- sum(w[i,1:(k-1)])/(k-1)
precd[i,t[i,k]]<- prec *2*(k-1)/k  

##consistency equations
mean[i,k] <-d[t[i,k]] - d[t[i,1]] 

}}

##prior distribution for log-odds in baseline arm of study i
for (i in 1:ns) {u[i] ~ dnorm(0,.01)}

##prior distribution for heterogeneity	
tau ~ dunif(0,3)                                      
prec<- 1/pow(tau,2)
tau.sq<- pow(tau,2)

##prior distribution for basic parameters		
d[ref] <- 0	
for(k in 1:(ref-1)) {d[k] ~ dnorm(md1,prd1)}
for(k in (ref+1):nt) {d[k] ~ dnorm(md1,prd1)}

md1~dnorm(0,1)
taud ~ dnorm(0,1)I(0,)                               
prd1<- 1/pow(taud,2)
tau.sqd<- pow(taud,2)


##OR for each comparison 
for(i in 1:(nt-1)) {
for (j in (i+1):nt) {
OR[j,i]<- exp(d[j] - d[i])
LOR[j,i]<- d[j] - d[i]}}

for(j in 1:(ref-1)){ORref[j]<- exp(d[j] - d[ref])}
for(j in (ref+1):nt) {ORref[j]<- exp(d[j] - d[ref])}


for(j in 2:nt){dd[j]<-d[j]-d[1]}

#Ranking of treatments
#TreatmeNT hierarchy
order[1:nt]<- nt+1- rank(d[1:nt])
for(k in 1:nt) {
# this is when the outcome is positive - omit  'nt+1-' when the outcome is negative
most.effective[k]<-equals(order[k],1)
for(j in 1:nt) {
effectiveness[k,j]<- equals(order[k],j)}}
for(k in 1:nt) {
for(j in 1:nt) {
cumeffectiveness[k,j]<- sum(effectiveness[k,1:j])}}

#SUCRAS
for(k in 1:nt) {
SUCRA[k]<- sum(cumeffectiveness[k,1:(nt-1)]) /(nt-1)}}

#for(k in 1:nt) {
#order[k]<- rank(d[],k) # this is when the outcome is negative - change to 'order[k]<- t+1-rank(d[],k) when the outcome is positive
#most.effective[k]<-equals(order[k],1)
#for(j in 1:nt) {effectiveness[k,j]<- equals(order[k],j)
#cumeffectiveness[k,j]<- sum(effectiveness[k,1:j])}}		

##SUCRAS
#for(k in 1:nt) {
#SUCRA[k]<- sum(cumeffectiveness[k,1:(nt-1)]) /(nt-1)
#}}

"
########

##### model III ######
model3.string <-  "
model {
for(i in 1:ns) { 
w[i,1]<- 0
theta[i,t[i,1]]<- 0                                             

##binomial likelihood of number of events for each arm k of study i		     
for (k in 1:na[i]) {r[i,t[i,k]] ~ dbin(p[i,t[i,k]],n[i,t[i,k]])}    

##parameterization of the 'true' effect of each comparison 
##of arm k vs. baseline arm (1) of study i                 
logit(p[i,t[i,1]])<- u[i]		                    
for (k in 2:na[i]) {
logit(p[i,t[i,k]])<- u[i] + theta[i,t[i,k]]

##distribution of random effects
theta[i,t[i,k]] ~ dnorm(md[i,t[i,k]],precd[i,t[i,k]])


## accounting for correlation between effect sizes estimated in multi-arm trials				             
md[i,t[i,k]]<- mean[i,k] + sw[i,k]                                   
w[i,k]<- (theta[i,t[i,k]]  - mean[i,k])          
sw[i,k]<- sum(w[i,1:(k-1)])/(k-1)
precd[i,t[i,k]]<- prec *2*(k-1)/k  

##consistency equations
mean[i,k] <-d[t[i,k]] - d[t[i,1]] 

}}

##prior distribution for log-odds in baseline arm of study i
for (i in 1:ns) {u[i] ~ dnorm(0,.01)}

##prior distribution for heterogeneity	
tau ~ dunif(0,3)                                     
prec<- 1/pow(tau,2)
tau.sq<- pow(tau,2)

##prior distribution for basic parameters		
d[1:nt] ~ dmnorm(md1[1:(nt)],prec1[1:(nt),1:(nt)])

for (i in 1:(nt)) {md1[i]<-md0 }		

for(k in 1:(nt)){taud1[k,k]<-tau.sqd}

for (i in 1:(nt-1)){
for (k in (i+1):(nt)){
taud1[i,k]<-0.5*tau.sqd
taud1[k,i]<-taud1[i,k]
}}

prec1[1:(nt),1:(nt)]<-inverse(taud1[1:(nt),1:(nt)])


md0~dnorm(0,1)
taud ~ dnorm(0,1)I(0,)                                 
tau.sqd<- pow(taud,2)



##OR for each comparison 
for(i in 1:(nt-1)) {
for (j in (i+1):nt) {
OR[j,i]<- exp(d[j] - d[i])
LOR[j,i]<- d[j] - d[i]}}

for(j in 1:(ref-1)){ORref[j]<- exp(d[j] - d[ref])}
for(j in (ref+1):nt) {ORref[j]<- exp(d[j] - d[ref])}


#Ranking of treatments
#TreatmeNT hierarchy
order[1:nt]<- nt+1- rank(d[1:nt])
for(k in 1:nt) {
# this is when the outcome is positive - omit  'nt+1-' when the outcome is negative
most.effective[k]<-equals(order[k],1)
for(j in 1:nt) {
effectiveness[k,j]<- equals(order[k],j)}}
for(k in 1:nt) {
for(j in 1:nt) {
cumeffectiveness[k,j]<- sum(effectiveness[k,1:j])}}

#SUCRAS
for(k in 1:nt) {
SUCRA[k]<- sum(cumeffectiveness[k,1:(nt-1)]) /(nt-1)}}

#for(k in 1:nt) {
#order[k]<- rank(d[],k) # this is when the outcome is negative - change to 'order[k]<- t+1-rank(d[],k) when the outcome is positive
#most.effective[k]<-equals(order[k],1)
#for(j in 1:nt) {effectiveness[k,j]<- equals(order[k],j)
#cumeffectiveness[k,j]<- sum(effectiveness[k,1:j])}}		

##SUCRAS
#for(k in 1:nt) {
#SUCRA[k]<- sum(cumeffectiveness[k,1:(nt-1)]) /(nt-1)
#}}

"
#########
#### fit model III #####
model3.spec<-textConnection(model3.string) 
data <- list(ns=dataJAGS$ns,ref=1, nt=dataJAGS$nt, n=dataJAGS$n, r=dataJAGS$r, t=dataJAGS$t, na=dataJAGS$na)
jags.m <- jags.model(model3.spec, data = data, n.chains = 2, n.adapt = 5000)

params <- c("dd[2]","dd[3]","dd[4]","dd[5]","dd[6]","dd[7]","dd[8]","dd[9]","dd[10]","dd[11]","dd[12]", "tau")

for (i in 1:11){
  for (j in (i+1):12){
    params=c(params, paste("OR[",j,",",i,"]",sep=""))
    closeAllConnections()    
  }
}

for (i in 1:12){
  params=c(params, paste("SUCRA[",i,"]" ,sep=""))
}


samps<- coda.samples(jags.m, params, n.iter = 10000)
burn.in <- 5000 
A5=summary(samps,start = burn.in)

A6=data.frame(A5$quantiles[,"50%"],A5$quantiles[,"2.5%"],A5$quantiles[,"97.5%"] )
A6=A6[1:66,]
A6=round(A6, digits=2)
colnames(A6)=c("median","low","up")

A6$x=0
A6$y=0

nam=rownames(A6)
A6[substr(nam, start=5, stop=5)==",",]$x=substr(nam[substr(nam, start=5, stop=5)==","], start=4, stop=4)
A6[substr(nam, start=5, stop=5)==",",]$y=substr(nam[substr(nam, start=5, stop=5)==","], start=6, stop=6)
A6[substr(nam, start=6, stop=6)==",",]$x=substr(nam[substr(nam, start=6, stop=6)==","], start=4, stop=5)
A6[substr(nam, start=6, stop=6)==",",]$y=substr(nam[substr(nam, start=6, stop=6)==","], start=7, stop=7)
A6[substr(nam, start=6, stop=6)==","&nchar(nam)==9,]$y=substr(nam[substr(nam, start=6, stop=6)==","&nchar(nam)==9], start=7, stop=8)
A6$x=as.numeric(A6$x)
A6$y=as.numeric(A6$y)
A7=A6[order(A6$y,A6$x),]

league.table=matrix(rep(0,144),nrow=12)
league.table[lower.tri(league.table, diag=FALSE)]=round(1/A7$median,digits=2)
llow=matrix(rep(0,144),nrow=12)
llow[lower.tri(league.table, diag=FALSE)]=round(1/A7$up,digits=2)
lup=matrix(rep(0,144),nrow=12)
lup[lower.tri(league.table, diag=FALSE)]=round(1/A7$low,digits=2)

league.table1=matrix(paste(league.table,"[",llow,";",lup,"]", sep=""),ncol=ncol(league.table))
names=c("bupropion","citalopram","duloxetine",
        "escitalopram","fluoxetine","fluvoxamine","milnacipran","mirtazapine",
        "paroxetine","reboxetine","sertraline","venlafaxine")
diag(league.table1)=names
league.table1[upper.tri(league.table1)]=""
league.table=data.frame(t(league.table1))
league.tableST3=league.table
##########

##### create a single table combining results with the usual model #######
league.table4=as.matrix(league.tableST3)
league.table5=as.matrix(league.tableST)
league.table6=matrix(paste(league.table4,league.table5, sep=""),ncol=ncol(league.table))
diag(league.table6)=names
league.tableALL=data.frame(league.table6)
league.tableALL
#######


