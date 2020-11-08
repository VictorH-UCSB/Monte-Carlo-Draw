pop_draw <- function  (pop,n){
  samp<-sample(pop,n,replace=FALSE)
  samp_mean<-mean(samp)
  out<- list(samp,samp_mean)
  return(out)
}