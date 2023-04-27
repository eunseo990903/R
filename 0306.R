install.packages("readxl")
library(readxl)
getwd()
setwd("C:/Rdata")
exam
exam <-read_excel("excel_exam.xlsx")
exam %>% filter(english <= 70 & class %in% c(4,5)) 
library (dplyr)
#class 별 영어수학과학 의 평균 총점
exam %>% group_by(class) %>% summarise(mean(math+english+science))
#영어 90점 이상이고 1반인 학생
exam %>% filter(english >= 90 & class==1)
#수학 90점보다 작은학생들 중에서 영어 70보다 크고 1반 아닌 학생
exam %>% filter(math <90) %>% filter(english >=70 & class !=1)
#총점과 평균칼럼 추가
 exam <- exam %>% mutate(total= (math+english+science),avg = total /3)
#통과 여부 칼럼
exam <- exam %>% mutate(통과여부 = ifelse(avg >=60,"Pass", "Fail")) 
exam$pas
rm(mpg)
#mpg에서 도시연비가 좋은 차종 6개 
library(ggplot2)
mpg
class(mpg$displ)
rm(mpg)
mpg %>% group_by(model) %>% summarise(MEAN=mean(cty)) %>% arrange(MEAN) %>% head
#어떤회사에서 생산한 평균 cty가 높은지 구하고 가장 높은 회사 다섯 곳 막대그래프 
mpg1 <- mpg %>% group_by(manufacturer) %>% summarise(MEAN=mean(cty)) %>%
  arrange(desc(MEAN)) %>% head(5) #desc 높은다섯개 
ggplot(mpg1, aes(x=reorder(manufacturer, -MEAN), y= MEAN))+geom_col()+xlab("제조사")
#x,y 두 막대그래프 geom_col , 순서정리 x= reorder(x,y)
#labs,xlab, ylab 그래프 축 이름바꾸기 
g+ labs(title="비교", x= "배기량", y="연비") 

#그래프
ggplot(data=mpg, aes(x=class))+ geom_bar(color="red",fill="skyblue"  )
ggplot(data=mpg, aes(x=class, fill=class))+ geom_bar()
rm(exam)
ggplot(mpg,aes(x=manufacturer, y=class, color=drv))+geom_point()
#범주형 변수일때 컬러 , 그라데이션이 아닌 3가지 색 
ggplot(data = mpg, aes(x=displ, y= hwy, color =drv))+geom_point(size=2)
ggplot(data = mpg, aes(x=displ, y= hwy, color =class))+geom_point(size=2)
ggplot(data = mpg, aes(x=displ, y= hwy, color =cty))+geom_point(size=2)
#cty 숫자 = 그라데이션 
#shape 모양바꾸기, color와 맞춰주기 
ggplot(data = mpg, aes(x=displ, y= hwy, shape =drv, color = drv))+geom_point()
ggplot(data = mpg, aes(x=displ, y= hwy, shape =cty, color = cty))+geom_point()
#cty 숫자 = shape X 작동안돼 
#geom_smooth() - 중간 곡선 , method="lm" >회귀분석 일직선 
ggplot(data = mpg, aes(x=displ, y= hwy, color=drv )) +geom_point()+ geom_smooth(method="lm") 
g <- ggplot(data = mpg, aes(x=displ, y= hwy, color=drv )) +geom_point()+ geom_smooth(method="lm")
g #임시저장해주면 따로 레이어 쌓기가 좋음 g + 모시기 ~ 
g+ theme_dark()  #다크 테마 
g+ theme_bw() #black n white 
library(ggthemes) #theme package
g+ theme_wsj() #wall street journal 
#facet_wrap 그룹별 단면 ~따라서 
g+facet_wrap(~drv)
g+facet_wrap(~class)


#수업 
#p.318
#데이터 뽑기 filter, select 대신 간단하게 데이터 [행, 열]
exam[ , c("class", "english")]
exam [ , c(2,3)]
#영어가 90점 이상인 행, 베이지R에선 $무조건 사용해야돼
exam [exam$english >90,  ]
exam [exam$class ==1, ]
exam$class==1 #T,F 로 나옴 그냥 치면

#p. 325 변수타입 
#범주, 카테고리, factor : 금 ,은, 동 (성별, 지역, 등수를 숫자로 표현,사칙연산X)
var1 <- c(1,2,3,4,5)
mode (var1) #mode, 데이터 타입
var2 <- as.factor(var1) #numeric > factor로 바꿈 
mode(var2)
class(var2)

str(diamonds)
# 범주 변수만 fill 가능 그래프때 

var3 <-  c("a","b","b","c","c","c")
var3 <- factor(var3)
class(var3)
#factor로 바꿔야하는 이유: CHARACTER이면 그래프가 잘 안그려짐

#p.332 데이터구조
#vector :1차원, 한줄 , data frame : 2차원 다양한 변수 (글자도 mpg)
# matrix:2차원, 한가지 변수타입 (숫자만, 표), array: 1-n 차원 ,list: 다차원(R),1차원 (PYTHON)

rm(mpg)
class(mpg)
class(mpg[ ,3]) #한줄이어도 dataframe : 한 셀 안에 숫자 다있어야 vector 
class(mpg[3, ])
class(mpg$manufacturer) #이게 한줄 :
ggplot(mpg,aes(cty))+ geom_histogram()
ggplot(mpg,aes(cty))+ geom_histogram() +geom_freqpoly(color="red")


ggplot(mpg,aes(displ,hwy))
mpg

ggplot(mpg,aes(displ, color=drv))+geom_freqpoly(binwidth=0.5)

ggplot(mpg,aes(displ, fill=drv))+geom_histogram(binwidth=0.5)
ggplot(mpg,aes(displ, fill=drv))+geom_histogram(binwidth=0.5, position="dodge")
#position="dodge" 겹치는 그래프 따로 분리
ggplot(mpg,aes(displ, fill=drv))+geom_histogram(binwidth=0.5)+facet_wrap(~drv)

ggplot(mpg,aes(manufacturer, fill=manufacturer))+geom_bar()

#geom_bar(), stat="identity , geom_col 
#피피티 다시보기 
#geom_line() with time series
ggplot(economics, aes(date,unemploy/pop))+geom_line()
economics
