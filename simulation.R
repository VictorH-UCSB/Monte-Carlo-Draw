simulation<- function(nSims,theta,sigma2,N,n,type){
  source(paste0(here::here(),"/set_para.R", ''))
  source(paste0(here::here(),"/pop_gen.R", ''))
  source(paste0(here::here(),"/pop_draw.R", ''))

  parameter<- set_para(theta,sigma2)
  y<-pop_gen(parameter,N,type)
  samp_mean<-rep(NA,nSims)
  var_samp_mean<-rep(NA,nSims)
  for (i in 1:nSims){
    samp_mean[i]<-pop_draw(y,n)[[2]]
  }
  theo_var<-(N-n)/(N*n)*sigma2
  out<-list(data.frame(sampleMean=c(mean(samp_mean),theta),
                  sampleVar=c(var(samp_mean),theo_var),
                  row.names = c('Observed','Theorethical')),
            c('nSims'=nSims,'theta'=theta,'sigma2'=sigma2,'N'=N,'n'=n),
            c('Expected_Mean'='theta','Expected_Variance'='(N-n)/(N*n)*sigma2'))
  return (out)
}