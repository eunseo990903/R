cars
mtcars
lm(dist~speed, data=cars)
#dist(y)가 speed(x)를 따른다 
#회귀 lm(y~x) linearmodel
fit.cars <-lm(dist~speed, data=cars) 

ggplot(cars)
plot(mtcars)
pairs(cars)
pairs(~vs+am+gear+mpg,data=mtcars) 
#일부의컬럼만 
ggplot(cars, aes(x=speed, y=dist))+geom_point() 
#산점도 -> 양의 상관관계 

cor.test(cars$speed, cars$dist) 
#p-value가 매우 작으므로 유의 

corrplot.mixed(cor(cars))
#상관관계 시각화 

fit.cars <- lm( dist~speed, data=cars)
fit.cars #회귀모형 함수 lm(y~x)
#y= -17.579 + 3.932x  

summary(fit.cars)
# Pr(>|t|) 0.0123 : H0은 절편이 0이다 
#단순회귀분석에서는변수 speed의 유의확률과 회귀직선 전체의 유의확률이 같음
#𝑅2 = 0.6511 = (0.8069)=r2

cor(cars)&2


#회귀식에 대해 적합한 값들
f1 <- function(x){
  y= -17.5904+3.032409*x
  return(y)
}

x %>% f1(cars$speed)

f1(cars$speed)

cars$dist-f1(cars$speed)

f2 <-  function(x,yi){
  y_hat= -17.5904+3.032409*x
  residuals(yi-y_hat)
  return(residuals)
}


#26일때 
new= data.frame(speed=c(26))
predict(fit.cars,newdata=new)
#30,31,32
new= data.frame(speed=c(30,31,32))
predict(fit.cars,newdata=new,interval='confidence') #범위로 값을냄 confidence 신뢰도 
  
#잔차제곱의 합
sum((cars$dist- f1(cars$speed))^2) 

