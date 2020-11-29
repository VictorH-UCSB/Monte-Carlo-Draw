simulation2<- function(cap_y,x,p,nSims){
  source(paste0(here::here(),"/functions/set_para.R", ''))
  source(paste0(here::here(),"/functions/pop_gen.R", ''))
  source(paste0(here::here(),"/functions/pop_draw.R", ''))
  
  
  out<-rep(NA,nSims)
  for ( i in c(1:nSims)){
    sample<-runif(N,0,1)<p
    
    x_select<-x[sample]
    capy_select<-cap_y[sample]
    

    #beta_hat<-lm(formula=capy_select~x_select+0)[[1]] %>% as.numeric()
    beta_hat<-sum(capy_select*x_select)/sum(x_select**2)
    out[i]<-beta_hat
  }
  return(out)
}