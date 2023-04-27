library(foreign)
library(dplyr)
library(ggplot2)
welfare <- read.spss("Koweps_hpc10_2015_beta1.sav", to.data.frame = TRUE)
welfare <- as_tibble(raw_welfare) #바로 df대신 tibble 사용 조금 더 정리가 쉬움 
welfare <- welfare %>%  select(
  gender = h10_g3,
  birth = h10_g4,
  marriage = h10_g10,
  religion = h10_g11,
  income = p1002_8aq1,
  job = h10_eco9,
  region = h10_reg7)
glimpse(welfare)
#p.244 종교 유무에 따른 이혼율 
# 변수 전처리 (종교, 혼인상태) -> 변수 간 관계 분석 (종교 유무에따른 이혼율 표, 그래프)
#종교
class(welfare$religion)
table(welfare$religion) #1= 종교있음 , 2= 없음
welfare$religion<- ifelse(welfare$religion==1, "yes", "no")
qplot(welfare$religion)
#혼인상태
table(welfare$marriage) #1: 유배우 , 3: 이혼 
welfare$gmarriage <-ifelse(welfare$marriage ==1, "marriage",
                           ifelse(welfare$marriage==3, "divorce", NA))
table(is.na(welfare$gmarriage))
qplot(welfare$gmarriage)
#종교유무에 따른 이혼율 표
religion_marriage <- welfare %>%
  filter(!is.na(gmarriage))     %>% 
  group_by(religion,gmarriage) %>%
  summarise(n=n()) %>% 
  mutate(tot_group= sum(n) ,
         pct= round (n/tot_group*100, 1))
