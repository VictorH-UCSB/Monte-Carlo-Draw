simulation<- function(nSims,theta,sigma2,N,n){
  parameter<- set_para(theta,sigma2)
  y<-pop_gen(parameter,N)
  samp_mean<-rep(NA,nSims)
  for (i in 1:nSims){
    samp_mean[i]<-pop_draw(y,n)[[2]]
  }
  theo_var<-(N-n)/(N*n)*sigma2
  out<-data.frame(sampleMean=c(mean(samp_mean),theta),
                  sampleVar=c(var(samp_mean),theo_var),
                  row.names = c('Observed','Theorethical'))
  return (out)
}