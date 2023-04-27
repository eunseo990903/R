install.packages("ggplot2")
library(ggplot2)
dim(diamonds) 
str(diamonds) 
install.packages("readxl")
library(readxl)
getwd()
setwd("C:/Rdata")
df_exam <-read_excel("excel_exam.xlsx")

sd(seq(1,100,3))

diamonds
library(dplyr)
exam <- read.csv("exam.csv")
filter(exam, class==1)
filter(exam, class==2)
filter(exam, class !=2)
#영어가 90점 이상 몇 명?
filter(exam, english >= 90) 
#영어가 90점 이상, 1반 몇 명?
filter(exam, english >=90 & class==1)
#pipe %>%
exam %>% filter(class==1)
exam %>% filter(english >90) %>% filter (class== 1)
#수학 90보다 작은 학생 들 중 영어 70점보다
#크고 1반이 아닌 학생 
exam %>% filter(math <90) %>% filter(english>70) %>% filter (class!=1)
#1반 또는 2반 또는 3반 학생
exam %>% filter(class==1 | class ==2 |class==3)
exam %>% filter(class %in% c(1,2,3))
library(readxl)
#1반의 수학평균
df <- exam %>% filter(class==1)
mean(df$math)
mean((exam %>% filter(class==1))$math) 
exam %>% filter(class==1) %>% summarise(mean(math))

#열 뽑기 
select(exam, class, math) 
exam %>% select (math)
exam %>% select (-math) #수학 빼고
exam %>% filter(class==1) %>% select(english,math)
exam %>%select(english,math)%>% filter(class==1)
#반대로하면 불가능 , select 에 class 를 넣어줘야댐

#정렬 arrange
exam %>%arrange(math)
exam %>%arrange(desc(math)) #내림차순
exam %>%arrange(desc(class), math) #반은 내림, 수학은 오름 

#파생변수 추가 앞에있는 칼럼을 통해 새로운 칼럼 추가
exam %>% mutate(total = math +english+science) #칼럼이름 total
exam %>% mutate(avg = (math +english+science)/3) #avg= 평균
exam %>% mutate(total = (math +english+science),
                avg = (total)/3)

#ifelse (조건, 참일때 , 거짓일때 )
exam %>%
  mutate(test= ifelse (science>60, "pass", "fail "))
#수영과 평균 구하기, 60점이상 패스 아니면 페일,내림차순
exam %>% mutate(avg = (math +english+science)/3) %>% 
  mutate(test=ifelse(avg >60,"pass", "fail"))%>%
  arrange(desc(avg)) 

#p.133 배기량에 따른 고속도로 연비 displ4이하 5이상 중 hwy 평균 
mpg_disp4 <- mpg %>% filter(displ <=4)
mpg_disp5 <- mpg %>% filter(displ >=5)
mean(mpg_disp4$hwy)
mean(mpg_disp5$hwy)
# 제조회사 아우디,토요타에 따른 도시 연비 cty
mpg_a <-mpg %>% filter(manufacturer == "audi")
mpg_b <-mpg %>% filter(manufacturer == "toyota")
mean(mpg_a$cty)
mean(mpg_b$cty)
# 쉐보레, 포드, 혼다 고속도로 연비 평균
mpg_c <- mpg %>% filter(manufacturer %in% c("chevrolet", "ford" ,"honda"))
mean(mpg_c$hwy)                                            

# p.138 class, city 변수 추출해 새로운 데이터만들기 
mpg
mpg1 <-mpg %>% select(class, cty)
mpg1
#종류에 따른 도시 연비
mpg_suv <- mpg1 %>% filter(class== "suv" ) 
mpg_compact <- mpg1 %>% filter(class== "compact" ) 
mean(mpg_compact$cty)
mean(mpg_suv$cty)

#p.141 audi중 어떤 자동차 모델이 HWY 높은지 1~5 위 출력
mpg <- as.data.frame(ggplot2::mpg)
mpg %>%
  filter(manufacturer=="audi") %>% 
  arrange(desc(hwy)) %>% head(5)

exam <-read_excel("excel_exam.xlsx")
head(exam)
mean(exam$math)
#summarise 가 소속이 다른 SELECT, MEAN 연결 
exam %>% select(math) %>%  summarise(mean(math))

#그냥 수학 평균 mean(exam$math)
#반별로 수학평균 SUMMARISE(연결), group by 활용 
class1 <- exam %>% filter(class==1)
mean(class1$math)
exam %>% group_by(class) %>%  summarise ( mean(math))
#최고점
exam %>% group_by(class) %>%  summarise (max(math))

#요약함수 sum, mean, median, sd, min,max, n <- 갯수 n은 써머라지이 안해도 써진다. 
exam %>% group_by(class)  %>% summarise (n())                                               

#영어의 반별 평균
exam %>% group_by(class) %>% summarise(mean(english),max(english),min(english) )

#table 빈도 수 , unique 부호로 빈도 비교 
table(c("A","B", "A"))
table(diamonds$cut)
unique(diamonds$cut)
#cut 별 최댓값과 최솟값
diamonds %>% group_by(cut) %>% summarise(max(price),min(price))
#color 별 
unique(diamonds$color)
diamonds %>% group_by(color) %>% summarise(max(price),min(price))
#carat 별 너무 많음 
diamonds %>% group_by(carat) %>% summarise(max(price),min(price))

library(ggplot2)
mpg
rownames(mpg)
colnames(mpg)
names(mpg) #column 에 우선순위 colnames와 같은 결과 값 
mpg$class

#p150. class  별  cty평균
mpg %>% group_by(class) %>% summarise(mean(cty))
# 위에 문제 cty 평균이 높은 순으로 출력 
mpg %>% group_by(class) %>% summarise(mean(cty)) %>% 
  arrange(desc(`mean(cty)`))
# 1옆에 작은 따옴표를 사용해야댐 mean 구할때 
#`` : backtick , ~: tilde , ^: caret

#hwy 평균 가장 높은 회사 세 곳
mpg %>% group_by(manufacturer) %>% summarize(mean(hwy)) %>% 
  arrange(desc(`mean(hwy)`)) %>% head(6)

#회사별 compact 차종생산 수 내림차순 
mpg %>% group_by(manufacturer) %>% 
 filter(class=="compact") %>% summarise(count=n()) %>% 
  arrange(desc(count))
mpg %>% filter(class=="compact") %>% 
  group_by(manufacturer)%>% summarise(count=n()) %>% 
  arrange(desc(count))

#p. 152 데이터 합치기 
test1 <- data.frame(id= c(1,2,3,4,5,6),
           midterm= c(60,80,70,90,85,90))
test2 <- data.frame(id= c(1,2,3,4,5,7),
              final= c(70,83,65,95,80,0))
test1
test2
total <- left_join(test1,test2) #왼쪽에 우선순위 
total
#left_join 겹치는 것 알아서 판단해서 조인, 지정하고싶으면 by= "__
right_join(test1,test2)#오른쪽에 우선순위
full_join(test1,test2) #그냥 합치기 

#세로 합치기 bind_rows
bind_cols(test1,test2)
bind_rows(test1,test2)

#NA (not available 결측치 )

exam
name <- data.frame(class=c(1,2,3,4,5), 
                   teacher = c("Kim","Lee","Park","Choi","Jung"))
left_join(exam,name, by='class')

#칼럼 이름바꾸기 dyplr: rename 
exam %>%  rename( 반 = class)
mpg %>% rename(city=cty, highway=hwy) %>% names()

mpg
colnames(mpg)<- c("manufacturer","model","displ","year","cyl","trans","drv",
                  "cty","hwy", "fl","class")
colnames()
exam
exam <-  exam %>% filter(id < 11)
exam <-  exam %>% select(id,class,math)                  
exam$classes <- exam$math #classes 없어서 새롭게 추가

exam <- exam %>% rename (english = classes)
exam
#sum 칼럼 만들기
exam$sum <-  exam$math + exam$english
exam
