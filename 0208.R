1+1
1/3
2^3
sqrt(4)
install.packages("dplyr")
library(dplyr)

a=1
print(a)
b="Hello"
print(b)
a=2
print(a)
a=c(1,2)
print(a)

var1 <- c(1,2,3,4,5)
var1
var1 <-c(1:10)
var1
var3<-seq(1,5)
var3

installed.packages()
install.packages("stringr")
paste()
str2
paste(str2)
paste0("Hello", "Good")
str <- c("Hello","world", "is", "good")
str
paste(str, collapse = ",")
sentence <- paste(str, collapse = " ")
sentence

#Package= Library 
#패키지 열기 library()
#함수에 f2 , 맘대로 변경 가능

qplot(x)
ggplot()
library(ggplot2)
qplot(x)
ggplot()
qplot()
x<- c("a","a","b","c")
x
qplot(x) #quick plot 
ggplot(data = diamonds, aes(x=carat, y= price, color= cut)) +
  +geom_point()
# help(ggplot) = ?ggplot =커서두고 f1
?ggplot

#p77
scores<- c(80, 60, 70,50,90)
mean(scores)
mean_scores <- mean(scores)
# <- alt + - assignment operator 

#  데이터 프레임 = table  표 
english <- c(90,80,60,70)
math <- c(50,60,100,20)
class <- c(1,1,2,3)
df_midterm<- data.frame(english, math,class)

#접근 대괄호 [행, 열]
df_midterm[2,3]
df_midterm[3,2]
df_midterm[3, ]
df_midterm[3,c(1,2,3) ]
df_midterm[c(2,3),c(1,2)]
df_midterm[2:3,1:2]
df_midterm [ 2 , -1] #1열 빼고 
df_midterm[2, 2:3 ] #73=74 같음
df_midterm[ ,1] 
df_midterm[ , 'english']
df_midterm$english 
#영어성적 평균
mean(df_midterm$english)

df_fruit <- data.frame(제품=c("사과","딸기","수박"),
                       가격=c(1800,1500,3000),
                       판매량= c(24,38,13))
제품 <- c("사과","딸기","수박")
가격 <- c(1800,1500,3000)
판매량 <-  c(24,38,13)
data.frame(제품, 가격, 판매량)
data.frame(제품= c("사과","딸기","수박"),
            가격 =  c(1800,1500,3000),
           판매량 =c(24,38,13))
mean(df_fruit$가격  )
mean(df_fruit$판매량)

#엑셀데이터 불러오기 
read_excel()
install.packages("readxl")
library(readxl)
read_excel("excel_exam.xlsx")
#working directory 
getwd()
setwd("C:/Rdata")
read_excel("excel_exam.xlsx")

#f1도움말 f2함수설명 
#영어평균은? 
df <- read_excel("excel_exam.xlsx")
mean(df$english)

#csv 파일 읽기
read.csv("csv_exam.csv.txt")
# 엑셀파일을 csv로 저장
write.csv(df, "exam.csv")
read.csv("exam.csv")
exam <- read.csv("exam.csv")
exam[ ,2:6] # x열 뺐음 사실은 덮어쓴거 
exam[ ,-1] # = 115 

read.table("csv_exam.txt", sep= ",")

#csv comma separated value 
#RDS R 전용파일
saveRDS(df, file = "df.rds")
readRDS("df.rds")

#데이터 파악
exam
head(exam)
head(exam,2)

tail(exam)
tail(exam,2)

View(exam) #엑셀같이보임
dim(exam)
str(exam) #structure 구조 중요
#Obs. observations 관측지=행, var. variables 변수= 열 
summary(exam)

library(ggplot2)
diamonds
 
#몇행몇열 #53940X10 
str(diamonds)

#carat 최댓값
summary(diamonds) #Max:5.0100 
#price 중앙값 : 2401, 
#price  평균: 3933  