#  It should take two inputs, one is the parameter just generated,
#  The other should be the number of population.

pop_gen <- function (para,N,type){
  if (!typeof(N) %in% c("double","integer")){
    stop('Error: N must be a number')
  }
  if (!type%in% c('normal','uniform')){
    stop('Error: type incorrect.')
  } else if (type=='normal'){
    out<-rnorm(N,para[1],sqrt(para[2]))
  } else if (type=='uniform'){
    out<-runif(N,para[1],para[2])
  }
  return(out)
}