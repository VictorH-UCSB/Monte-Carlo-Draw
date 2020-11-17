rm(list=ls())

source(paste0(here::here(),"/functions/set_para.R", ''))
source(paste0(here::here(),"/functions/pop_gen.R", ''))
source(paste0(here::here(),"/functions/pop_draw.R", ''))
source(paste0(here::here(),"/functions/simulation2.R", ''))
library(tidyverse)

start.time<-Sys.time()

sigma_e2<-1
N<-10000
beta<-2


x_para<-set_para(0,1)
e_para<-set_para(0,sigma_e2)

xi<-pop_gen(x_para,N,'uniform')
ei<-pop_gen(e_para,N,'normal')

yi<-xi*beta+ei

nSims<-10000

p<-c(0.01,0.1,0.5,0.9,0.99)
sigma_epi2<-c(0,1,2)

result<-NULL

for (i in c(1:length(p))){
  for (j in c(1:length(sigma_epi2))){
    start.time.small<-Sys.time()
    var_beta<-var(simulation2(yi,xi,p[i],sigma_epi2[j],nSims))
    theo_var<-sigma_epi2[j]**2/(sum(xi**2))+(1-p[i])/p[i]*(sum(xi**2*ei**2))/((sum(xi**2))**2)
    
    end.time.small<-Sys.time()
    out<-cbind(obs=var_beta,theo=theo_var,group=sprintf('observation %i and %i',i,j),time=end.time.small-start.time.small)
    result<-rbind(result,out)
  }
}

result1<-as.data.frame(result) %>% gather(type,num,obs:theo)
ggplot(result1)+geom_col(aes(group,num,fill=type),position='dodge',width = 0.5)+
  geom_text(aes(group,num,label=round(as.numeric(num),5)),position = position_dodge(width = 0.5),hjust=-0.5)+
  theme(axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank())+coord_flip()
end.time<-Sys.time()
print(rbind(as.data.frame(result) %>% select(group,time),c('time in total',end.time-start.time)))
