#filter 행 추출 , select 함수 차이점
#could not find funtion '%>%' , library(dplyr)
#table(diamonds$cut) 갯수 세는것 빈도수
#(q)plot(테이블$열) 그래프 , error figure margins: 그래프 창 키우면 됨
#column 의 이름:colnames(diamonds) 
#mutate 파생변수 생성 원래있던변수이용만듬
#sumarise()는 종종 앞에 group_by()로 분리 후 계산 
#mpg에서 회사별 "suv" 차종 수 내림차순 정렬 
mpg %>% group_by(manufacturer) %>%
filter(class=="suv") %>% summarise(a=n())
%>% arrange(desc(a))
# %>% filter,select %>% group_by() %>% summarise() %>% arrange()순서 
#rm(mpg) 수정했던 내용 지우기
#rename(new = old)
#names(): 변수보기 
library(dplyr)
library(readxl)
exam <- read.csv("exam.csv")
library(ggplot2)

plot(diamonds$cut)
colnames(diamonds) 
exam <- data.frame(class= c(1,2,3,4,5),
                   subject = c("math", "english", "science","society","korean"))
name <-data.frame(class= c(1,2,3,4,5),
                  teacher = c("kim","lee","park","choi","jung")) 
exam_new <-  left_join(exam, name)
exam_new

mpg <- as.data.frame(ggplot2::mpg)
mpg

mpg_new <-  mpg #복사본 만들기 보통 new 로 작업 
mpg_new
mpg_new %>% rename(displacement= displ,fuel= fl)
table(mpg_new)

#연속적인 조건, 범주 ifelse
exam %>% 
  mutate(math2 = ifelse(math>=80,"a", 
                        ifelse(math>=60,"b", 
                               ifelse(math>=40,"c","f")))) 
exam %>% 
  mutate(math2 = ifelse(math>=80,"a", 
                        ifelse(math>=60,"b", 
                               ifelse(math>=40,"c","f")))) 
#수업 
#데이터 정제 
#빈칸 결측치 NA 비어있는 값 
df <- data.frame(gender = c("M","F",NA,"M","F"),
                 score= c(5,4,3,4,NA))
df
# 1. 결측치 확인 
is.na(df) #존재여부is, 바꿔주는 것 as
sum(is.na(df)) #결측치 갯수만 T= 1, F=0 취급 
table(is.na(df))#T, F 갯수

airquality
sum(is.na(airquality))
sum(is.na(airquality$Ozone))

summary(mpg)
summary(airquality) #summary: NA 를 칼럼별로 알수있음 

#2.결측치 처리 (지우기, 채워넣기 )
#1. 지우기
na.omit(df) #가급적 사용 금지, 데이터가 너무 많이 날라감 


#2. 채워넣기 
is.na(df$gender)
df$gender <- ifelse(is.na(df$gender),"M", df$gender)
#ifelse(is.na(df$gender), 결측치를 "M", 나머지 그대로 )
df$score <-  ifelse(is.na(df$score),5,df$score)
df

df[3,1] <- NA #값을 수정하는과정 
df[5,2] <- NA #숫자열이라 생각하고 따옴표 없음 
df[5,1] <- "F"
mean(df$score)#NA 있으면 평균 안줌 그래서 처리  
mean(df$score, na.rm = T) #NA가 있는걸 인식, 빼고 평균, na.rm <-  remove NA

airquality %>% head
mean(airquality$Ozone, na.rm=T)

is.na(df$score)
df %>% filter(is.na(df$score)) #NA것만 필터링  
df %>% filter(!is.na(df$score)) #NA가 아닌것만 필터링 

airquality <- airquality %>% rename(Solar =Solar.R) #꼭 지정 
airquality %>% select(Ozone, Solar) %>%
  summarise(mean(airquality$Ozone, na.rm = T),mean(airquality$Solar,na.rm =T))

#이상치 Outlier 나쁜것이 아닌 평균과 멈. 
outlier <- data.frame(gender = c(1,2,1,3,2,1),
                      score = c(5,4,3,4,2,10))
outlier
#이상치 확인 
boxplot(diamonds$cut) 
#boxplot() 그리기 , 수염 밖 = 이상치
#IQR (inter Quartile Range) Q3-Q1,길이 1.5*IQR 
ggplot(data=diamonds,aes(x=cut, y=price)) +geom_boxplot()
# x,y범위 주면 더 자세한 boxplot
#boxplot으로 그려지지 않는 것 table() 이용
#이상치 처리
#1. 제외 , 아예 빼버리기 
mean(outlier$score[-6]) #이상치 위치(6번째,1차원)를 알경우의 평균 
mean(outlier$score[1:5]) 
outlier %>% filter(score < 9) %>% summarise((mean(score)))

#vector: 한줄, 1차원 / vectorize, dataframe: 2차원 ,표
#1,3,5번째 [c(1,3,5)] 

#p.173 10를 결측치로 바꿔줌 , 그러고 mean 계산 
outlier$score[6] <- NA
mean(outlier$score, na.rm=T)
rm(mpg)

#p.170
mpg[c(65,124,131,153,212),"hwy"] <- NA
#1. drv, hwy 결측치 몇개?
table(is.na(mpg$drv))
table(is.na(mpg$hwy))

#2. hwy 결측치 제거후 , 어떤 drv가 hwy 평균 높은지 
mpg %>% filter(!is.na(hwy)) %>% 
  group_by(drv) %>% 
  summarise(mean(hwy))

rm(mpg)
#p.178 
mpg[c(10,14,58,93), "drv"] <- "k"
mpg[c(29,43,129,203), "cty"] <- c(3,4,39,42)
#1. drv이상치 확인 후 처리
boxplot(mpg$drv)
table(mpg$drv)
#k이상치 발견 후 처리
mpg$drv <- ifelse(mpg$drv="k",NA, mpg$drv)
table(is.na(mpg$drv))

#2. 
boxplot(mpg$cty)$stats
mpg$cty <- ifelse(mpg$cty < 9|mpg$cty >37,NA,mpg$cty)
table(mpg$cty)
#3. mean
mpg %>% filter(!is.na(drv)& !is.na(drv)) %>% 
  group_by(drv) %>% summarise(mean(cty))

#그래프 만들기 p.184
#ggplot2 : grammar of graphics 
mpg %>% select(displ,hwy) %>% data.frame
plot(mpg$displ,mpg$hwy) #데이터 파악 용 가볍게 
ggplot(data=mpg,mapping = aes(x= displ, y= hwy))+#바닥깔기 
  geom_point( color="blue", size=5)+ #점이 더해짐 
  xlim(3,6) +ylim(10,30) #범위 제한 
#선은 geom_line(), 옵션은 ()안에 color="blue", size=5 옵션 가능 

ggplot(mpg, aes(x=displ, y=hwy)) + geom_point()
#ggplot (데이터, xy축) , aes: aesthetic geom:geometry 

#p.183
#1. 산점도 <- 반드시 2차원
ggplot(mpg, aes(x=cty, y=hwy))+geom_point(color="red")
ggplot(mpg, aes(x=displ, y=hwy))+geom_point(color="red")
ggplot(mpg, aes(x=manufacturer, y=hwy))+geom_point(color="red")
ggplot(mpg, aes(x=displ, y=hwy, color =manufacturer))+geom_point(size= 4)

#drv 기준으로 색이 칠해짐(character:각각 다른색,int,num: 그라데이션) 
#str(mpg) 열이 잘 안보일때 세로로 표를 바꿔서 보여줌 
#glimpse(mpg) 가지런한 str(mpg) 가독성 좋음 (질문 )
#문자열일경우 그래프 잘안그려짐 , 그려저도 일자로 모아니면 도이니까
#unique(mpg$cty) level 표시 
str(mpg)
glimpse(mpg)
#2.막대그래프 <-  차원부터 파악 
#geom_bar() <- 1차원만  가능 
library(dplyr)
ggplot(data=mpg, aes(x=class, fill= drv)) + geom_bar()
#color= 테두리; fill 채우다 <- aes()안에무조건넣고 안에선 컬럼명, 밖은 그냥 fill= "red"  , 
#x와 fill 이 같아야 안정적 

df <- table(mpg$class) %>% as.data.frame() #2차원으로 바꿈 
df <- df %>% rename(class =Var1)
ggplot(df, aes(x= class, y=Freq))+ geom_bar(stat="identity") (질문)
#2차원일 경우 geom_bar(stat="identity") 옵션 추가 or geom_col 
df <- df %>% rename(class =Var1)
ggplot(df, aes(x= class, y=Freq))+ geom_col()

df %>% arrange (-Freq) %>%  
  ggplot(aes(x= reorder(class,-Freq), y=Freq,fill=class))+geom_col()
#막대그래프 순서정렬 reorder(x축, y축) -는 내림차순 

#주의; 2차원 막대그래프에서 x축이 숫자열일 경우 범위를 지정해야돼 
ggplot(mpg, aes(x= displ, y=hwy))+ geom_col() #잘못된 예시
#drv 구동방식 별로 고속도로 평균 연비를 구해서 그래프로 그려라. 
df1 <- mpg %>% group_by(drv) %>% summarise(연비 =mean(hwy))
ggplot(df1, aes(x=drv, y=연비))+geom_col()
ggplot(df1, aes(x=drv, y=연비,fill=drv))+geom_col()
ggplot(df1, aes(x=reorder(drv,-연비), y=연비,fill=drv))+geom_col()

#선그래프 x축 시간 
economics
glimpse(economics)
ggplot(economics, aes(x=date, y=pop))+geom_line()
ggplot(economics, aes(x=date, y=unemploy))+geom_line()
rm(economics)
str(economics)
economics$date <- as.character(economics$date)
economics$date <- as.Date(economics$date)
ggplot(economics, aes(x=date, y=unemploy))+geom_line()
#Boxplot
ggplot(mpg,aes(x=drv,y=hwy, fill=drv))+geom_boxplot()


#p.188 
ggplot(mpg,aes(x=cty,y=hwy))+geom_point()

ggplot(midwest,aes(x=poptotal, y= popasian)) +geom_point() +xlim(0,500000)+ylim(0,10000)
rm(mpg)
#p.193 
mean_city <- mpg %>% filter(class== "suv") %>% group_by(manufacturer) %>% summarise(mean_cty=mean(cty)) %>% 
  arrange(mean_cty) %>% head(5)
ggplot(mean_city, aes(x= reorder(manufacturer,- mean_cty),y= mean_cty))+ geom_col()


ggplot(mpg, aes(x= class))+geom_bar()

#195 
rm(economics)
table(economics)
head(economics,3)
ggplot(economics, aes(x=date, y=psavert))+geom_line()

rm(mpg)
mpg
car <- mpg %>% filter(class=="compact"|class== "subcompact" |class=="suv")
ggplot(car, aes(x=class, y= cty))+geom_boxplot()
