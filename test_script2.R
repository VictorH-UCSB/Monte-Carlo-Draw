rm(list=ls())

source('set_para.R')
source('pop_gen.R')
source('pop_draw.R')
source('simulation2.R')


start.time<-Sys.time()

sigma_e2<-1
N<-10000
beta<-2


x_para<-set_para(0,1)
e_para<-set_para(0,sigma_e2)

xi<-pop_gen(x_para,N,'uniform')
ei<-pop_gen(e_para,N,'normal')

yi<-xi*beta+ei

p<-0.1
sigma_epi2<-2

var_beta<-var(simulation2(yi,xi,p,sigma_epi2,10000))
theo_var<-sigma_epi2**2/(sum(xi**2))+(1-p)/p*(sum(xi**2*ei**2))/((sum(xi**2))**2)

out<-cbind(obs=var_beta,theo=theo_var)
barplot(out)

end.time<-Sys.time()
print(end.time-start.time)
