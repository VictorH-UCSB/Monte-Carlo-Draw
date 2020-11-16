simulation2<- function(y,x,p,sigma_epi2,nSims){
  source(paste0(here::here(),"/set_para.R", ''))
  source(paste0(here::here(),"/pop_gen.R", ''))
  source(paste0(here::here(),"/pop_draw.R", ''))
  
  
  out<-rep(NA,nSims)
  
  for (i in c(1:nSims)){
    parameter<- set_para(0,sigma_epi2)
    epi<-pop_gen(parameter,length(y),'normal')
    
  
    cap_y<-y+epi
    
    sample<-runif(length(y),0,1)<p
    
    x_select<-x*sample
    y_select<-y*sample
    capy_select<-cap_y*sample
    
    beta_hat<-length(x_select)*cov(capy_select,x_select)/
      (var(x_select)*(length(x_select)-1))
    out[i]<-beta_hat
  }
  return(out)
}