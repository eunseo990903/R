library(ggplot2)
library(dplyr)
library(KoNLP)
library(rvest)
library(stringr)
library(foreign)
library(readxl)
exam <-read_excel("excel_exam.xlsx")
exam
#무작위로 다섯명을 추출 
set.seed(1111) #비밀번호 마냥 정해진 숫자 , 재현하고자 할 때 
math1<- exam[sample(1:20 , 5), ]
mean(math1$math)
exam[sample(1:20,5), ]
sample_n(exam, 5) 
exam[sample(1:20, 5),] 
#수학 평균 
set.seed(1111) #비밀번호 마냥 정해진 숫자 , 재현하고자 할 때 
math1<- exam[sample(1:20 , 5), ]
mean(math1$math)

#A,B 무작위 10개 비율: 3,1 
sample(c("A", "B"), 10, replace = T, prob =c(3,1) )

library(rvest)
library(stringr)
title = c()
body = c()
url_1 <- "https://www.pressian.com/pages/news-world-list?page="
for(i in 1:5){
  url <- paste(url_1, i, sep = "")
  print(url)
  
  hdoc <- read_html(url)
  
  title_part <- hdoc %>% html_nodes(".title") %>% html_text()
  body_part <- hdoc %>% html_nodes(".body") %>% html_text()
  
  title <- c(title, title_part)
  body <- c(body, body_part)
}

#news = data.frame(제목 = title, 본문 = body)
useNIADic()
news = cbind(title, body)
nouns<- extractNoun(news)
words<- unlist(nouns)
words <- words [nchar(words)>=2]
table(words)
dim(mpg)
df <- as.data.frame(table(words)) #데이터 프레임 만들기 
head(df)
df<- df %>% arrange(desc(Freq)) %>% head(20)

#그래프 
df %>% ggplot(aes(x=reorder(words,Freq),
                   y=Freq, fill=words))+geom_col()+coord_flip()+
  theme(legend.position="none")+ 
  ggtitle("세계뉴스")
#범례제거 theme(legend.position="none") , 오른쪽에 색상표 


#수강설문조사정리, 과목별 강의 만족도 , 7,9번 만족도 
survey<- read_excel("수강설문조사정리.xlsx")
names(survey)
colnames(survey)
glimpse(survey)

names(survey) = c("1","수강과목","zoom환경","강의시간","강의운영방법","중간 과제", "강사만족도","학습태도","수업진도","추가교육","관심분야")
survey$강사만족도 <- ifelse(survey$강사만족도=="매우 동의함",5,ifelse(survey$강사만족도=="동의함", 4, ifelse(survey$강사만족도=="보통",3,2)))

survey  %>% group_by(강사만족도,수강과목) %>% summarise(n=n()) %>% ggplot(
  aes(x=강사만족도,y=n, fill=수강과목))+geom_col(position="dodge")
