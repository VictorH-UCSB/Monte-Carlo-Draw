nSims=10000
theta=2
sigma2=1
N=10000
n=1000
type='normal'

source('set_para.R')
source('pop_gen.R')
source('pop_draw.R')
source('simulation.R')

parameter<- set_para(theta,sigma2)
y<-pop_gen(parameter,N,type)
samp_mean<-rep(NA,nSims)
mean_samp_mean<-rep(NA,as.integer(nSims/100))
var_samp_mean<-rep(NA,as.integer(nSims/100))
for (i in 1:nSims){
  samp_mean[i]<-pop_draw(y,n)[[2]]
  if (i%%100==0){
  mean_samp_mean[as.integer(i/100)]<-mean(samp_mean,na.rm = T)
  var_samp_mean[as.integer(i/100)]<-var(samp_mean,na.rm =T) 
  }
}


data<-as.data.frame(cbind(Number_of_tests=c(1:as.integer(nSims/100)),sample_mean=mean_samp_mean,variance_sample=var_samp_mean))

ggplot(data,aes(x=Number_of_tests,y=sample_mean))+geom_point()+geom_line(linetype = "dashed")+geom_hline(yintercept = theta)
ggplot(data,aes(x=Number_of_tests,y=variance_sample))+geom_point()+geom_hline(yintercept = (N-n)/(N*n)*sigma2)
