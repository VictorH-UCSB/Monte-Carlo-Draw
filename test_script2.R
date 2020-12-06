rm(list=ls())

source(paste0(here::here(),"/functions/set_para.R", ''))
source(paste0(here::here(),"/functions/pop_gen.R", ''))
source(paste0(here::here(),"/functions/pop_draw.R", ''))
source(paste0(here::here(),"/functions/simulation3.R", ''))
library(tidyverse)

# setting parameters
sigma_e2<-1
N<-10000
beta<-2
nSims<-1000

p<-c(0.01,0.1,0.5,0.9,0.99)
sigma_epi2<-c(0,1,2)

x_para<-set_para(0,1)
e_para<-set_para(0,sigma_e2)

# generating population
xi<-pop_gen(x_para,N,'uniform')
ei<-pop_gen(e_para,N,'normal')

yi<-xi*beta+ei



result<-NULL

# for each group of simulations, generate one population with 
# independent error epislon.

for (j in c(1:length(sigma_epi2))){
    # sample from the population with probability p and return the 
  # variance of the beta.
  for (i in c(1:length(p))){
    start.time.small<-Sys.time()
    var_beta<-var(simulation3(yi,xi,p[i],sigma_epi2[j],nSims))
    theo_var<-sigma_epi2[j]/(sum(xi**2))+((1-p[i])/p[i])*
      ((sum(xi**2*ei**2))/((sum(xi**2))**2)+sigma_epi2[j]/(sum(xi**2)))
    
    end.time.small<-Sys.time()
    
    # rearranging the data for presentation and making graph
    out<-cbind(obs=var_beta,theo=theo_var,prob=sprintf('p = %f ',p[i]),
               sigma=sprintf('sigma_epi2 = %f',sigma_epi2[j]),
               time=end.time.small-start.time.small)
    result<-rbind(result,out)
  }
}


result1<-as.data.frame(result) %>% gather(type,num,obs:theo) %>% type_convert()

for (i in (c(1:length(p)))){
  a<-ggplot(result1 %>% filter(prob==sprintf('p = %f',p[i])))+
    geom_col(aes(sigma,num,fill=type),position='dodge',width = 0.9)+
    geom_text(aes(sigma,num,label=round(as.numeric(num),5),group=type),
              position = position_dodge(width = 0.9))+
    theme(axis.title.x=element_blank(),
          axis.ticks.x=element_blank(),
          plot.margin = unit(c(1,1,1,1), "cm"))+
    ggtitle(sprintf('Observation with p= %f',p[i]))
  plot(a)
}
