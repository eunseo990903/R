a <- matrix(1:12,nrow=3,ncol=4
       ,byrow=TRUE)
a[-3,c(1:4)]


library(ggplot2)
mpg
cars
mtcars

plt.scatter(cars)

with(cars, { plot(speed ~ dist)})

ggplot(cars,aes(x=speed, y=dist))+geom_point()
with (cars, {lm(dist~speed)})

lm(dist~speed, data=cars)
plot(speed ~ dist, data=cars)
