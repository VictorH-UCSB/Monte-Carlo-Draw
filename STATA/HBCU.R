library(haven)
library(tidyverse)
library(ggplot2)
setwd(dirname(rstudioapi::getSourceEditorContext()$path))

c2019_a<-read_dta("./C2019_A/dct_C2019_A.dta")
f1718_f1a<-read_dta("./F1718_F1A/dct_F1718_F1A.dta")
f1718_f2<-read_dta("./F1718_F2/dct_F1718_F2.dta")
gr2019<-read_dta("./GR2019/dct_efia2019.dta")
gr2019_p<-read_dta("./GR2019_PELL_SSL/dct_efia2019.dta")
hd2019<- read_dta("./HD2019/dct_hd2019.dta")
ic2019<-read_dta("./IC2019/dct_ic2019.dta")


joined_1<-inner_join(hd2019,ic2019)
joined_1<-joined_1 %>% filter(iclevel==1) %>% filter(stusrv1 %in% c(0,1))
joined_1$hbcu<--(joined_1$hbcu-2)


model<-lm(stusrv1~(hbcu-1)+1,data=joined_1)
summary(model)




       