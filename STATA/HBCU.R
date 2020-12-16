library(haven)
library(tidyverse)
library(ggplot2)
setwd(dirname(rstudioapi::getSourceEditorContext()$path))

c2019_a<-read_dta("./C2019_A/dct_C2019_A.dta")
f1718_f1a<-read_dta("./F1718_F1A/dct_F1718_F1A.dta")
f1718_f2<-read_dta("./F1718_F2/dct_F1718_F2.dta")
f1718_f3<-read_dta("./F1718_F3/dct_F1718_F3.dta")
gr2019<-read_dta("./GR2019/dct_efia2019.dta")
gr2019_p<-read_dta("./GR2019_PELL_SSL/dct_efia2019.dta")
hd2019<- read_dta("./HD2019/dct_hd2019.dta")
ic2019<-read_dta("./IC2019/dct_ic2019.dta")


hd2019_1 <- hd2019 %>% select(unitid,iclevel,hbcu)
ic2019_1 <- ic2019 %>% select(unitid,stusrv1)
joined_1<-left_join(hd2019_1,ic2019_1)
joined_1<-joined_1 %>% filter(iclevel==1) %>% filter(stusrv1 %in% c(0,1))
joined_1$hbcu<--(joined_1$hbcu-2)


model<-lm(stusrv1~(hbcu)+1,data=joined_1)
summary(model)

c2019_a_1 <- c2019_a %>% group_by(unitid) %>% filter(cipcode==99) %>% summarise_if(is.numeric,sum,na.rm=T) %>%
  mutate('Afri_ratio'=cbkaat/ctotalt) %>% select(unitid,cbkaat,ctotalt,Afri_ratio)
f1718_f1a_1 <- f1718_f1a%>% select(unitid,'ph_gr'=f1e01)
f1718_f2_1 <- f1718_f2 %>% select(unitid,'ph_gr'=f2c01)
f1718_f3_1 <- f1718_f3 %>% select(unitid,'ph_gr'=f3c01)
f1718_pg<- rbind(f1718_f1a_1,f1718_f2_1,f1718_f3_1)
f1718_pg[is.na(f1718_pg)]<-0
joined_2<-right_join(f1718_pg,c2019_a_1) %>% right_join(joined_1)
joined_2$ph_gr<- joined_2$ph_gr/1000000
joined_2<-joined_2 %>% mutate(high_afri=ifelse(Afri_ratio>=0.12518,1,0))
joined_2<-joined_2 %>% mutate(high_gr=ifelse(ph_gr>=10,1,0))
joined_2<- joined_2 %>% mutate(hbcu_bar=coef[2]*high_afri+coef[3]*high_gr+coef[1])

model_2<-lm(stusrv1~hbcu+ph_gr+Afri_ratio,data=joined_2)
summary(model_2)

hd2019 %>% filter(unitid==484613) %>% select(instnm)



model_3<-lm(stusrv1~hbcu+high_gr+Afri_ratio,data=joined_2)
summary(model_3)


#Consider morphing ph_gr to some categorical data just to make it more comparable,

# Note to self, remedial services is but a categorical value with have or not have.
# Try to find ways to account for quasi-experiment problems.
# Using data ins c2019a to find percentage of black student vs non-black student and phell grants to study them.
# Find the expenses for remedial services for each school (Hopefully.)

# Outliers with Phell Grant, see if I can find more.