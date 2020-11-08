#  It should take two inputs, one is the parameter just generated,
#  The other should be the number of population.

pop_gen <- function (para,N){
  if (typeof(N)!="double"){
    stop('Error: N must be a number')
  }
  
  out<-rnorm(N,para[1],sqrt(para[2]))
  
  return(out)
}