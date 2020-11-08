simulation<- function(nSims,mu,sigma2,N,n){
  parameter<- set_para(mu,sigma2)
  pop<-pop_gen(parameter,N)
  samp_mean<-rep(NA,nSims)
  for (i in 1:nSims){
    samp_mean[i]<-pop_draw(pop,n)[[2]]
  }
  theo_var<-(N-n)/(N*n)*sigma2
  out<-data.frame(sampleMean=c(mean(samp_mean),mu),
                  sampleVar=c(var(samp_mean),theo_var),
                  row.names = c('Observed','Theorethical'))
  return (out)
}