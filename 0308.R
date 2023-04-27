library(foreign)
library(dplyr)
library(ggplot2)
library(readxl)
welfare <- read.spss("Koweps_hpc10_2015_beta1.sav", to.data.frame = TRUE)
welfare <- welfare %>%  select(
  gender = h10_g3,
  birth = h10_g4,
  marriage = h10_g10,
  religion = h10_g11,
  income = p1002_8aq1,
  job = h10_eco9,
  region = h10_reg7)
glimpse(welfare)

welfare$age <-  2015-welfare$birth
welfare %>% filter(20 <=age & age <30 )%>% 
  ggplot(aes(marriage=="기혼"))+geom_bar(position = "dodge")

welfare %>% filter(age>=20 & age < 30) %>% ggplot(aes(age, fill=marriage)) + geom_bar(position = "dodge") #dodge 종류를 따로 나눠서 보여줌 
welfare <- welfare %>% filter(age >20)%>%mutate (age_g=ifelse(age <30,"young",ifelse(age <=50, "middle","old")))
ge_g_income <- welfare %>%  filter(!is.na(income)) %>% group_by(age_g) %>% 
  summarise(mean_income=mean(income))
age_g_income <- welfare %>% group_by(age_g) %>% 
  summarise(mean_income=mean(income,na.rm=T)) 
ggplot(age_g_income, aes(x=age_g,y= mean_income, fill=age_g))+geom_col() +
  scale_x_discrete(limits=c("young", "middle", "old"))

welfare$group_marriage <-ifelse(welfare$marriage ==1, "기혼",
                           ifelse(welfare$marriage==3, "미혼", NA))
#미혼을 이혼으로 바꾸기
welfare$group_marriage=="미혼"
welfare$group_marriage[welfare$group_marriage == "미혼"]<- "이혼"
welfare$group_marriage <- rename(welfare$group_ marriage, "이혼" = "미혼")
#job과 group_marriage가 결측치가 아닌 행만 추출
welfare %>%
  filter(!is.na(group_marriage)& (!is.na(job)))%>% 
  select(job,group_marriage)
#직업에 따른 이혼율 빈도 상위 10개
list_job <- read_excel("Koweps_Codebook.xlsx", col_names = T, sheet = 2)
welfare <- welfare %>% rename(code_job = job)
welfare <- left_join(welfare, list_job, by="code_job") 

df<- welfare %>% 
  filter(!is.na(group_marriage)) %>% 
  count(job, group_marriage) %>%  
  group_by(job) %>% 
  mutate(pct= round(n/ sum(n)*100, 1)) %>% 
  filter(group_marriage=="이혼") %>% 
  select(job,pct) %>%  arrange(-pct) %>% head(10)
df

ggplot(df,aes(x=reorder(job,pct),y=pct, fill=job))+geom_col()
#데이터 프레임 합치기 left_join 
a <- data.frame( class = c(1:5), math= c(10,20,30,40,50))
b <- data.frame( class = c(1:5), english= c(15,25,35,45,55))
ab <- left_join(a, b, by= "class")
#
library(KoNLP)
text <- readLines("ahn.txt")
words <- words [nchar(words)>=2]
#str_length  = 문자열의 길이, stringr::str_length 사용 , nchar 안되면 
stringr::str_length()

#수업 
#크롤링 Crawling
스크레이핑 Scrapping , 웹페이지를 그대로 가져와서 데이터 추출하는 행위
#https://selectorgadget.com/
  
  
install.packages("rvest")
install.packages("stringr")
library(rvest)
library(stringr)
library(xml2)
library(tidy)
title=c()
body= c()

read_html("http://www.naver.com")
temp <-read_html
temp[2]


hd <- read_html("https://search.daum.net/search?w=news&DA=PGD&enc=utf8&cluster=y&cluster_page=1&q=%ED%8A%B8%EC%99%80%EC%9D%B4%EC%8A%A4&p=1")

#페이지, 우클릭, 검사 기사 제목 해당하는 부분 찾기 


hdoc<- html_nodes(hd,".desc") #기사 두줄
temp<- html_nodes(hd,".tit_main.fn_tit_u")  #제목 

temp <- html_nodes(hd, ".desc")
paste(temp[1])
html_text(temp[1])

hdoc %>% html_nodes(".tit_mainfn_tit_u") %>%html_text()

title <- c(title, "a")
title <- c(title, "b")
title <- c(title, "c")

for (i in 1:3){
  print(i*10)
}
#뉴스 페이지 1
hd <- read_html("https://search.daum.net/search?w=news&DA=PGD&enc=utf8&cluster=y&cluster_page=1&q=%ED%8A%B8%EC%99%80%EC%9D%B4%EC%8A%A4&p=1")
title <- hd %>% html_nodes(".tit_main.fn_tit_u") %>% html_text()
body <- hd %>% html_nodes(".desc") %>% html_text()

#뉴스 페이지 2
hd <- read_html("https://search.daum.net/search?w=news&DA=PGD&enc=utf8&cluster=y&cluster_page=1&q=%ED%8A%B8%EC%99%80%EC%9D%B4%EC%8A%A4&p=2")
title_temp <- hd %>% html_nodes(".tit_main.fn_tit_u") %>% html_text()
title <-  c(title, title_temp)
body_temp<- hd %>% html_nodes(".desc") %>% html_text()
body <- c(body, body_temp)
#title, body 열면 20개씩 나옴 

url <- "https://search.daum.net/search?w=news&DA=PGD&enc=utf8&cluster=y&cluster_page=1&q=%ED%8A%B8%EC%99%80%EC%9D%B4%EC%8A%A4&p="

#뉴스 1 - 10페이지까지 크롤링 
title= c()
body= c()
for (i in 1:10){
  url_1 <-"https://search.daum.net/search?w=news&DA=PGD&enc=utf8&cluster=y&cluster_page=1&q=%ED%8A%B8%EC%99%80%EC%9D%B4%EC%8A%A4&p="
  url <-paste(url_1, i, sep="") #띄어쓰기 없이 
  print(url)
  
  hdoc <- read_html(url)
  
  title_part <- hd %>% html_nodes(".tit_main.fn_tit_u") %>% html_text()
  #클래스 복사해서 
  title <- c( title, title_part) #글자만 뽑아내기 
  
  body_part <- hd %>% html_nodes(".desc") %>% html_text()
  body <-  c(body, body_part)
}
  print(title)
  print(body)
  
  #news = data.frame(제목 = title, 본문 = body) 
  
news = cbind(title, body)
  
write.csv(news, "twice.csv")  #wd에 저장 
getwd()
setwd("C:/Rdata")
# 너무 많은 크롤링하면 막힘. 

write.csv(news,"뉴진스.csv",fileEncoding = 'utf-8')
  
  
  temp<- html_nodes(hd,".tit_main.fn_tit_u")  
  paste(temp[1])
  html_text(temp[1])
  
  temp <- html_nodes(hd, ".desc")
  paste(temp[1])
  html_text(temp[1])
  
  print(i*10)


#paste () 문자와 숫자 합쳐줌 
library(dplyr)
library(wordcloud2)
library(ggplot2)
library(KoNLP)
#트와이스 기사에서 빈도수 상위 20개 단어 wordcloud

nouns<- extractNoun(body)
words<- unlist(nouns)
words <- words [nchar(words)>=2] #한단어는 의미가없어서 제외 
table(words) #단어수 빈도표 (1차원)
df <- as.data.frame(table(words)) #데이터 프레임 만들기 
head(df)
df<- df %>% arrange(desc(Freq)) %>% head(20) #빈도수 상위 20개 
df
wordcloud2(df)


rm(exam)
#sample(x,size,replace=false,prob=NULL)
x <- 1:12
x <- 1:5
sample(x, 3)   
sample(x, 3, replace=TRUE) #replace=T 중복숫자나오기 가능  
#bootstrap resampling -- only ig length(x) >
set.seed(1234) #위에 임의로 뽑았던 수 또 나오게 
sample(1:10,5)
exam <- read.csv("exam.csv")
exam [1:10 , ]
exam [seq(1,20,2), ]
exam[sample(1,20,2),  ]

#id지우기
exam <-exam[-10] 
exam <- exam[ ,2:ncol(exam)]
length(names(exam))
dim(exam)[2]
exam[ ,-1]

set.seed(1234)
exam[sample(1:20,10), ]

#5394개뽑기)
sample(1:5394, )

#5:3:2 비율로 서로다른세트
n <- nrow(diamonds)
num <- sample(nrow(diamonds),nrow(diamonds)*0.5)
num1 <- sample(nrow(temp),nrow(diamonds)*0.3)

train <- diamonds[num, ]
temp <- diamonds[-num, ]
val <- temp[num1, ]
test<- temp[-num1, ]

nrow(train)
nrow(temp)
nrow(val)
nrow(test)

num = sample(nrow(diamonds), nrow(diamonds)*.5)
num_2 = sample(nrow(diamonds[-num,]), nrow(diamonds[-num,])*.6)
dia = diamonds[num,]
dia_2 = diamonds[num_2,]
dia_3 = diamonds[-num,]
dia_4 = dia_3[-num_2,]
nrow(dia)
nrow(dia_2)
nrow(dia_4) 


sample()
dia<- diamonds[num, ]
ggplot(dia, aes(x=carat, y=price, color=color))+geom_point()
nrow(test)
#sample의 반만 뽑아줌 
sample_n(diamonds,nrow(diamonds)*0.5)
sample_frac(diamonds, 0.5)
#fraction 분수
sample(c("A", "B"), 5, replace = T)
sample(c("A", "B"), 10, replace = T, prob =c(0.7,0.3) )
#A,B를 무작위로 5개 , 10개 비율 정해서 (소수점 아니어도 댐 5,2 ㄱㄴ)


#그래프 색상 임의로 바꾸기 
ggplot(mpg,aes(x=drv, fill=drv))+geom_bar()+scale_fill_hue(c=30,20)
ggplot(mpg,aes(x=drv, fill=drv))+geom_bar()+scale_fill_brewer(palette="Set2")
#1. scale_fill_hue() 그래프 색상 임의로 바꾸기 (0-360)
#2. scale_fill_brewer Set 색상 외에도 엄청 많음 구글링 하면 나옴
#3. scale_fill_manual() 직접 지정 
scale_fill


#p.280 미국 주별 강력 범죄율 단계 구분도 만들기 
install.packages("mapproj")
install.packages("ggiraphExtra")
library(ggiraphExtra)
USArrests
#현재 나라이름이 칼럼이 아닌 인덱스임 . 그러므로 인덱스 > 칼럼 바꾸기
library(tibble)
crime <- rownames_to_column(USArrests, var= "state") # rownames_to_column index > column 보내주는 함수 
crime$state <- tolower(crime$state) #소문자 변경 
str(crime)

#지도 데이터
install.packages("maps")
library(ggplot2)
states_map <-  map_data("state")
str(states_map)

#단계 구분도  ggiraphExtra의 ggChoropleth() 이용 
ggChoropleth(data=crime,  #지도에 표현할 데이터
             aes(fill=Murder, # 표현할 변수
                 map_id= state), #지역 기준 변수
             map=states_map,      #지도 데이터 
             interactive = T)   #인터렉티브 마우스 반응

#p.284 대한민국 시도별 인구, 결핵 환자 수 단계 구분도 
install.packages("stringi")
install.packages("devtools")
devtools::install_github("cardiomoon/kormaps2014")
library(kormaps2014)
library(ggiraphExtra)
library(dplyr)
rm(korpop1)
str(changeCode(korpop1, to="utf-8")) %>% head
#changecode가 원래 깨진문자 살려주는데 한글을 못읽어와서 지정해줌 to이용
colnames(korpop1)
korpop1 <- rename(korpop1, pop= 총인구_명, 
                  name= 행정구역별_읍면동)
korpop1$name <- iconv(korpop1$name, "UTF-8", "CP949")

#대한민국 지도 데이터 kormaps2014 ::kormap1
str(changeCode(kormap1))
library(ggplot2)
options(encoding="UTF-8")
#단계 구분도 만들기
ggChoropleth(data=korpop1,     #지도에 표현할 데이터
             aes(fill=pop,     #색깔로 표현할 변수
                 map_id= code,#지역 기준 변수
                 tooltip=name), #지도 위에 표시할 지역명
             map=kormap1,                #지도 데이터
             interactive= T)

#대한민국시도별 결핵환자 수 단계 구분도 
str(changeCode(tbc, to="utf-8"))

tbc$name <-  iconv(tbc$name, "UTF-8", "CP949")
ggChoropleth(data=tbc,     #지도에 표현할 데이터
             aes(fill=NewPts,     #색깔로 표현할 변수
                 map_id= code,#지역 기준 변수
                 tooltip=name, to="utf-8"), #지도 위에 표시할 지역명
             map=kormap1,                 #지도 데이터
             interactive= T)
