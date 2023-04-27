
#복습  
mpg
ggplot(mpg, aes(x=displ, y=hwy))+geom_point()+geom_smooth()+labs(title="배기량에 따른 도시연비 비교")
ggplot(economics,aes(x= date, y= unemploy))+geom_line()+theme_economist()
#테마 ggthemes 다운 

#제조사가 audi인 행만 뽑기, basic R
mpg[mpg$manufacturer=="audi",  ]
#displ이 2보다 작은 행
mpg[mpg$displ <2,]

#남자 평균 소득 , 여자 평균 소득 
library(foreign)
library(dplyr)
library(ggplot2)
welfare <- read.spss("Koweps_hpc10_2015_beta1.sav", to.data.frame = TRUE)
dim(welfare)
str(welfare)
#바로 df로 읽어줌 
rm(welfare)
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

table(welfare$gender)
welfare$gender <- ifelse(welfare$gender==9,NA ,welfare$gender)
table(is.na(welfare$gender))
welfare$gender <- ifelse(welfare$gender==1,"Male","Female")
table(welfare$gender)

qplot(welfare$gender)

summary(welfare$income)
welfare$income <- ifelse(welfare$income%in% c(0,9999),NA,welfare$income)

gender_income <- welfare %>% filter(!is.na(income)) %>% group_by(gender) %>% 
  summarise(mean_income=mean(income))
gender_income
#그래프 표현 
ggplot(gender_income, aes(x=factor(gender), y= mean_income, fill=factor(gender))) +geom_col()

welfare$gender

#R에서의 factor 의미는? 범주형 변수 ,카테고리 
#dataframe을 tibble로 변경할때 쓰는 명령어? as_tibble()
#readExcel ,   read_excel 두가지 방법 , readxl 패키지 안에

welfare$marriage <- ifelse (welfare$marriage==1,"기혼","미혼")
welfare$marriage

welfare$age <-  2015-welfare$birth
welfare %>% filter(20 <=age & age <30 )%>% 
ggplot(aes(marriage=="기혼"))+geom_bar(position = "dodge")

welfare %>% filter(age>=20 & age < 30) %>% ggplot(aes(age, fill=marriage)) + geom_bar(position = "dodge") #dodge 종류를 따로 나눠서 보여줌 


#수업 
#p.225 연령대별 평균소득
welfare <- welfare %>% filter(age >20)%>%mutate (age_g=ifelse(age <30,"young",ifelse(age <=50, "middle","old")))

welfare$age
head(welfare$age)
dim(welfare)
table(age_g_income)

age_g_income <- welfare %>%  filter(!is.na(income)) %>% group_by(age_g) %>% 
  summarise(mean_income=mean(income))
#앞에 age_g_income 그래프 전 실행시켜서 테이블 보기 
age_g_income <- welfare %>% group_by(age_g) %>% 
  summarise(mean_income=mean(income,na.rm=T)) 
ggplot(age_g_income, aes(x=age_g,y= mean_income, fill=age_g))+geom_col() +
  scale_x_discrete(limits=c("young", "middle", "old"))
  #scale_x_discrete(limits=c("young", "middle", "old"))이 순서대로 그래프 세우기 
#p.228 나이와 성별에 따른 소득 차이 
gender_income <- welfare %>% group_by(age_g, gender) %>% summarise(mean_income=mean(income, na.rm=T))
ggplot(gender_income, aes(age_g, mean_income, fill=gender))+geom_col(position = "dodge")+scale_x_discrete(limits=c("young", "middle", "old")) 
gender_age <- welfare %>% filter(!is.na(income)) %>% group_by(age,gender) %>% summarise(mean_income=mean(income,na.rm=T))
ggplot(gender_age, aes(age,mean_income,color=gender))+geom_line()
#geom_density, geom_freqpoly() 1차원, 변수가 한개일 때 
rm(welfare)
#p.235
#left_join(welfare,list_job,id="code_job")
library(welfare)
library(readxl)
list_job <-  read_excel("Koweps_Codebook.xlsx", col_names=T , sheet= 2)
list_job$code_job
welfare<- welfare %>% rename(code_job=job) # job을 code_job으로 공통이름변경후 left_join
welfare <- left_join(welfare, list_job, by= "code_job")
dim(list_job)
colnames(welfare)
#w직업별 평균소득 
welfare %>% filter(!is.na(code_job)) %>% select(code_job,job) %>% head(10)
welfare %>% group_by(job) %>% summarise(mean_income=mean(income, na.rm=T))
colnames(welfare)
job_income <- welfare %>%  filter(!is.na(job) & !is.na(income)) %>% 
  group_by(job) %>%  summarise(mean_income= mean(income))
head(job_income)

top10 <-  job_income %>% arrange(desc(mean_income)) %>%  head(10)
top10
ggplot(top10,aes(x=reorder(job, mean_income),y= mean_income, fill=job))+ geom_col()+coord_flip()

#지역별 인구 분포 분석
welfare
table(welfare$region) #1지역:2486명, 2지역 3711명 ... 
region_map <-data.frame(region=c(1,2,3,4,5,6,7), 
                        지역 = c("서울", "수도권(인천/경기)", "부산/경남/울산", "대구/경북",
                           "대전/충남", "강원/충북", "광주/전남/전북/제주도") )
welfare<- left_join(welfare, region_map, by = "region") 
head(welfare)
welfare %>%  mutate(인구수= total(gender))

welfare %>% group_by(지역) %>% 
  summarise(n=n()) %>% 
  ggplot(aes(x=지역, y=n, fill=지역))+
  geom_col()+
  labs(x="지역명",y="인구수")  #인구수 설정까지 함 

# round() :정수로 반올림 round(   , 소수 몇째 자리 수 )


rm(
  welfare
)
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
  summarise(n= n()) %>% 
  mutate(tot_group= sum(n) ,
         pct= round (n/tot_group*100, 1))
#round( , 소수점 자리 ) 반올림
#count() 집단별 빈도 이용해서 비율구하기 
religion_marriage <- welfare %>% 
  filter(!is.na(gmarriage)) %>% 
  count(religion, gmarriage) %>%  
  group_by(religion) %>% 
  mutate(pct= round(n/ sum(n)*100, 1))

#이혼사람들의 religion, pct 추출
divorce <-  religion_marriage %>%  
  filter(gmarriage =="divorce") %>% 
  select(religion, pct)
divorce

ggplot(divorce, aes(x=religion, y=pct))+geom_col()

#p. 252 연령대 및 종교 유무에 따른 이혼율표 
ageg_religion_marriage <- welfare %>% 
  filter(!is.na(gmarriage)& age_g != "young") %>% 
  group_by(age_g, religion, gmarriage) %>% 
  summarise(n= n()) %>% 
  mutate(tot_group= sum(n),
         pct=round(n/tot_group*100, 1))
rm(ageg_religion_marriage)
or
ageg_religion_marriage <-  welfare %>% 
  filter(!is.na(gmarriage)& age_g != "young")  %>% 
  count(age_g,religion, gmarriage) %>% 
  group_by(age_g,religion) %>% 
  mutate(pct= round(n/sum(n)*100, 1))

#연령대별 및 종교 유무별 이혼율 표 만들기
df_divorce <-  ageg_religion_marriage %>% 
  filter(gmarriage == "divorce") %>% 
  select(age_g,religion, pct)
df_divorce


#p.254 지역별 연령대 비율
#변수 처리 (지역, 연령대)> 관계 (지역별 연령대 비율표, 그래프)
class(welfare$code_region)
ggplot(df_divorce, aes(x=age_g, y= pct, fill= religion))+
  geom_col(position="dodge")


#Text Mining
#java 설치
install.packages("multilinguer")
multilinguer::install_jdk()
#의존성 패키지 설치
install.packages(c('stringr', 'hash', 'tau', 'Sejong', 'RSQLite', 'devtools'), type = "binary")
#깃허브를 통해 koNLP 다운로드 
install.packages("remotes")
remotes::install_github('haven-jeon/KoNLP', upgrade = "never", force = TRUE, INSTALL_opts=c("--no-multiarch"))

remotes::install_github('haven-jeon/KoNLP', upgrade = "never", INSTALL_opts=c("--no-multiarch"))
library(KoNLP)

#형태소 사전 설치
useNIADic()
useSejongDic()

text <- "안녕하세요 , 좋은 아침입니다."
library(dplyr)
library(ggplot2)
extractNoun("안녕하세요 좋은 아침입니다")
text <- readLines("ahn.txt")
nouns<- extractNoun(text) #명사추출
words<- unlist(nouns)
words <- words [nchar(words)>=2] #한단어는 의미가없어서 제외 
table(words) #단어수 빈도표 (1차원)
df <- as.data.frame(table(words)) #데이터 프레임 만들기 
head(df)
df<- df %>% arrange(desc(Freq)) %>% head(20) #빈도수 상위 20개 

#wordcloud 사용하기 ,wordcloud 패키지 다운 
install.packages("wordcloud2")
library(wordcloud2)
wordcloud2(df)
df
#막대그래프 coord_flip() 세로그래프 가로로 바꾸기 
g1 <- ggplot(df, aes(x=reorder(words,-Freq),y=Freq, fill=words) )+geom_col()+coord_flip() 

#p.289 Interactive Graph (마우스 움직임에 반응하는 그래프 )
mpg
g<- ggplot(mpg,aes(x=displ, y=hwy, color=drv))+geom_point()
g
#potly 설치
install.packages("plotly")
library(plotly)s
ggplotly(g) #a마우스에 반응함 , 마우스로 범위 잡으면 거기만 보여줌 리밋 안잡아도 개쩐당 ㅡㅡ !!  
ggplotly(g1)

install.packages("dygraphs")
library(dygraphs)
economics
library(xts) #시계열 만들기 위해 데이터가 시간 속성을 지니는 xts타입
eco <-xts(economics$unemploy,order.by = economics$date)
dygraph(eco)

#dyRangeSelector() 그래프 아래 날짜 범위 선택기능
dygraph(eco) %>% dyRangeSelector()
