library(foreign)
library(dplyr)
library(ggplot2)
raw_welfare <- read.spss("Koweps_hpc10_2015_beta1.sav")
#파일명 그대로 복붙하면 읽힘. 

rm(welfare)
colnames(welfare)
getwd()
welfare <- as.data.frame(raw_welfare)

str(raw_welfare)
#코드북(엑셀파일) 코드보고 변수명 바꾸기 
#파일이 유출되어도 알아보지 못하게 바꿔놓기 때문. 
welfare <- rename( welfare, gender =h10_g3, birth = h10_g4,
          marriage=h10_g10, religion=h10_g11,
           income= p1002_8aq1, job = h10_eco9,
          region=h10_reg7)

welfare = welfare %>%
  select(gender = h10_g3,
         birth = h10_g4,
         marriage = h10_g10,
         religion = h10_g11,
         income = p1002_8aq1,
         job = h10_eco9,
         region = h10_reg7) 
head(welfare)
class(welfare)

welfare <-  as_tibble(welfare)
#tibble : df보다 정제된 형태
length(names(welfare))
str(welfare) #데이터타입을 살펴보기위해 

summary(welfare) #NA값을 모아준다.
plot(welfare) #전체적인 표
boxplot(welfare)
sum(is.na(welfare))
colSums(is.na(welfare)) #컬럼별로 NA갯수
mean(welfare$income) #NA값이 있을땐 옵션 
mean(welfare$income, na.rm=T)
range(welfare$income, na.rm=T)
welfare$income <- ifelse(welfare$income==0,NA,welfare$income)
#0을 NA로 바꾼것 
summary(welfare$income)

boxplot(welfare$income)
ggplot(welfare,aes(x=income))+geom_density()
ggplot(welfare,aes(x=income, color=factor(gender)))+geom_density()
#gender가 numeric이라 그림그릴때만 factor로 잠시 전환 

#성별로 이름 붙이기 
welfare$gender <-  ifelse(welfare$gender==1,"male","female")
summary(welfare)
table(welfare$gender)


ggplot(welfare,aes(x=gender, fill= gender))+geom_bar ()


#1. 여자 평균 소득 , 남자 평균 소득
mean_income <- welfare %>% group_by(gender) %>% summarise(avg_income = mean(income, na.rm = T))
ggplot(mean_income, aes(x = gender, y = avg_income, fill = gender)) + geom_col() #막대그래프 x,y

welfare %>%ggplot(aes(income,color=gender))+geom_density()


#2. 나이에 따른 소득차이  -몇살에 수입이 가장 많은가?
names(welfare)

class(welfare$birth)
min(welfare$birth)
max(welfare$birth) #2014년에 조사 시작됨, 2014년생 1살 

welfare$age <- (2015- welfare$birth)
welfare$age <- as.data.frame()
welfare %>% select(birth,age)
range(welfare$age)
welfare2<- welfare %>% group_by(age) %>% summarise(평균 = mean(income, na.rm=T)) 
ggplot(welfare2,aes(x=age, y=평균))+geom_line()+xlim(20,80)

welfare %>% group_by(age) %>% summarise(평균 = mean(income, na.rm=T)) %>% ggplot(aes(x=age, y=평균))+geom_line()+xlim(20,80) #아동착취할수 없으니 구간 20- 80으로 나눔 

rm(welfare)

library(foreign)
library(dplyr)
library(ggplot2)
raw_welfare <- read.spss("Koweps_hpc10_2015_beta1.sav")
class(raw_welfare) #리스트로 불러와짐
raw_welfare <- as.data.frame()
welfare <- as.data.frame(raw_welfare)

head(welfare)
summary(welfare)
dim(welfare)
View(welfare)

welfare <- rename(welfare, 
                  gender = h10_g3,
                  birth = h10_g4,
                  marriage = h10_g10,
                  religion = h10_g11,
                  income = p1002_8aq1,
                  job = h10_eco9,
                  region = h10_reg7)
#성별에 따른 월급차이
#성별 이상치 결측치 확인 후 처리
class(welfare$gender)
table(welfare$gender)
is.na(welfare$gender)
welfare$gender <- ifelse(welfare$gender ==9, NA, welfare$gender)
table(is.na(welfare$gender))

welfare$gender <- ifelse(welfare$gender==1, "male","female")
table(welfare$gender)
qplot(welfare$gender)
#월급 확인
class(welfare$income)
table(welfare$income) # income 연속 변수라 너무 많음, summary로
summary(welfare$income)
qplot(welfare$income)
#월급 1-9998인데 최솟값 0, NA가 12030 r개

welfare$income <- ifelse(welfare$income %in% c(0,9999),NA, welfare$income)
table(is.na(welfare$income))

#성별 월급 분석
gender_income <-  welfare %>% filter(!is.na(income)) %>% group_by(gender) %>% 
  summarise(mean_income=mean(income))
gender_income

#그래프
ggplot(gender_income, aes(x=gender, y=mean_income))+geom_col()



#2 나이와 월급의 관계
class(welfare$birth)
summary(welfare$birth) #min 1907, max 2014,  코드북 무응답 9999

#결측치 확인
table(is.na(welfare$birth))
#이상치
welfare$birth <- ifelse(welfare$birth==9999, NA, welfare$birth)
table(is.na(welfare$birth))

#파생변수 나이 
welfare$age <- 2015-welfare$birth
summary(welfare$age)

age_income <- welfare %>% filter(!is.na(income)) %>% group_by(age) %>% 
  summarise(mean_income= mean(income))
head(age_income)

#그래프
ggplot(age_income,aes(x=age, y=mean_income))+geom_line()
