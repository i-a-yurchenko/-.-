iris <- read.csv('C:\\1\\iris.csv',header=T, sep=',')
renv::init()
renv::install("ggplot2", "maps")
renv::snapshot()

library(ggplot2)
library(tidyverse)

head(iris)
tail(iris)

ggplot(data = iris) + 
  geom_point(mapping = aes(x = petal.length, y = petal.width, color = variety))

ggplot(data = iris)+
  geom_point(mapping = aes(x= petal.length, y = petal.width))+
  facet_wrap(~variety, nrow = 2)

#срез по комбинации 2-х переменных
ggplot(data = iris)+
  geom_point(mapping = aes(x= petal.length, y = petal.width))+
  facet_grid(sepal.length~sepal.width)

# Геометрические объекты
ggplot(data = iris)+ geom_smooth(mapping = aes(x= petal.length, y = petal.width, linetype = variety))

#на одной диаграмме графики с разными геометриями
ggplot(data = iris)+
  geom_point(mapping = aes(x = petal.length, y = petal.width, color = variety))+
  geom_smooth(mapping = aes(x= petal.length, y = petal.width))

#Многослойная диаграмма с выведением зависимости для сорта Setosa
ggplot(data = iris, mapping = aes(x = petal.length, y = petal.width))+
  geom_point(mapping = aes(color = variety))+
  geom_smooth( 
    data = filter(iris, variety == "Setosa"), se = F
  )+
  geom_smooth(
    data = filter(iris, variety == "Versicolor"), se = F
  )+
  geom_smooth(
    data = filter(iris, variety == "Virginica"), se = F
  )

ggplot(data = iris,
       mapping = aes(x = variety, y = petal.width))+
  geom_boxplot()+
  coord_flip() # оси поменены местами

ggplot(data = iris)+
  stat_summary(
    mapping = aes(x = variety, y = sepal.length),
    fun.min = min, # диаграмма размахов по категориям
    fun.max = max,
    fun = median)

# Позиционные настройки
ggplot(data = iris)+
  geom_bar(mapping = aes(x = sepal.length, fill = variety))

ggplot(data = iris)+
  geom_bar(mapping = aes(x = sepal.length, fill = variety),
           position = "fill")

i <- ggplot(data = iris)+
  geom_bar(
    mapping = aes(x = variety, fill = variety),
    show.legend = F,
    width = 1
  ) +
  theme(aspect.ratio = 1) +
  labs(x = NULL, y = NULL)
i + coord_flip()
i + coord_polar()
