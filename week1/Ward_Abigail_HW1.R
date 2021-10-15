#Abby Ward
#problem set 1

library(knitr)
library(ggplot2)
library(HistData)
dat<-PolioTrials

#question 1
para_placebo<-dat$Paralytic[2]/dat$Population[2]
notInoc_RandomControl<-dat$Paralytic[3]/dat$Population[3] 
para_placebo
notInoc_RandomControl

#question 2
prob_sucess<-(dat$Paralytic[2]+dat$Paralytic[3])/(dat$Population[2]+dat$Population[3]) 
trials<-dat$Population[2]
1-pbinom(dat$Paralytic[2]-1, trials, prob_sucess)

#PART 2
#question 3
dat.2<-t(dat[1:2,4:6]) 
dat.2<-data.frame(dat.2)
names(dat.2)<-c("Vaccinated","Placebo")

vac_paralytic<-dat.2$Vaccinated[1]/(sum(dat.2$Vaccinated))
vac_NotParalytic<-dat.2$Vaccinated[2]/(sum(dat.2$Vaccinated))
vac_False<-dat.2$Vaccinated[3]/(sum(dat.2$Vaccinated))

placebo_Paralytic<-dat.2$Placebo[1]/(sum(dat.2$Placebo))
placebo_NotParalytic<-dat.2$Placebo[2]/(sum(dat.2$Placebo))
placebo_False<-dat.2$Placebo[3]/(sum(dat.2$Placebo))


#question 4
#Please describe a probability model for a simulation-based hypothesis test that addresses whether the two groups 
    #can reasonably be considered to come from populations with the same proportions of Paralytic,NonParalytic, 
    #and FalseReport
"Null hypothesis is that there is no difference in the proportion of paralytic 
vs non-paralytic vs false reports between the vaccinated group and the placebo
group. Probability model will run a simulation and determine the probability of
the placebo and the vaccinated sample results occurred"

#How is the test statistic computed?
"Test statistic is a random variable calculated from the sample
data and used to show the agreement between the sample data and null hypothesis 
(x-u)/s"
distance is the test statistic 
goal is to have a distance of zero
make a histogram of those distances
trying to see if observed value is reproducbile due to random chance
#What is the probability model that captures the null hypothesis?
#How can the probability model be simulated?
#What comparison of the observed statistic and the values of the test statistics from the simulations addresses the question?
pop<-rep(row.names(dat.2), times=dat.2$Vaccinated+dat.2$Placebo)
table(pop)

k<-10
samp.perm<-sample(pop,k)
samp.perm

outcome.prop<-(dat.2$Vaccinated+dat.2$Placebo)/sum(dat.2$Vaccinated+dat.2$Placebo)
outcome.prop

set.seed(345)
samp<-sample(c("Paralytic", "NonParalytic", "FalseReports"), k, replace=TRUE, prob=outcome.prop)

counts<-table(samp)
counts
props<-counts/sum(counts)
props


k1<-10
k2<-20
set.seed(345)
samp1<-sample(c("Paralytic", "NonParalytic", "FalseReports"), k1, replace=TRUE, prob=outcome.prop)
samp2<-sample(c("Paralytic", "NonParalytic", "FalseReports"), k2, replace=TRUE, prob=outcome.prop)

counts1<-table(samp1)
counts1
props1<-counts1/sum(counts1)
props1

counts2<-table(samp2)
counts2
props2<-counts2/sum(counts2)
props2

dist.eu<-sqrt(sum((props1-props2)^2))
dist.eu

dist.mann<-sum(abs(props1-props2))
dist.mann

prop.vacc<-dat.2$Vaccinated/sum(dat.2$Vaccinated)
prop.vacc

prop.plac<-dat.2$Placebo/sum(dat.2$Placebo)
prop.plac

test.stat<-sqrt(sum((prop.vacc-prop.plac)^2))
test.stat

  
#simulation and graph  
n<-10000

k1<-sum(dat.2$Vaccinated)
k1
k2<-sum(dat.2$Placebo)
k2

sim= rep(NA, n)
for(i in 1:n){
  samp1<-sample(c("Paralytic", "NonParalytic","FalseReports"), k1, replace=TRUE, prob = outcome.prop)
  samp2<-sample(c("Paralytic", "NonParalytic","FalseReports"), k2, replace=TRUE, prob = outcome.prop)
  counts1<-table(samp1)
  counts2<-table(samp2)
  props1<-counts1/sum(counts1)
  props2<-counts2/sum(counts2)
  dist.eu<-sqrt(sum((props1-props2)^2))
  
  sim[i]<-dist.eu
}
dat.temp<-data.frame(sim=sim)
ggplot(data =dat.temp, aes(x=sim))+geom_histogram()+geom_vline(xintercept=test.stat, color="blue")
mean(sim>=test.stat)




##set simulation up
samp1<-sample(c("Paralytic", "NonParalytic","FalseReports"), k1, replace=TRUE, prob = outcome.prop)
table(samp1)

samp2<-sample(c("Paralytic", "NonParalytic","FalseReports"), k2, replace=TRUE, prob = outcome.prop)
table(samp2)

