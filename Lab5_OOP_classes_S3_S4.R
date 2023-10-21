#renv::init() # инициализация виртуального окружения
#renv::install() # установка библиотеки из CRAN
#renv::snapshot() # делаем снимок версий библиотек в нашем виртуальном окружении
# фиксируем этот список в .lock-файле для возможности восстановления
# renv::restore() # команда отктиться к предыдушему удачному обновления библиотек

# ------------------- 
# Лабораторная работа №5:
# Написание классов S3, S4.

# Для примера посмотрим на регрессионный анализ с использованием ф-ии lm()
?lm
x <- c(1:3)
y <- c(1,3,8)
lmout <- lm(y~x)
class(lmout)
lmout # это объект класса lm

# напишем класс S3
j <- list(
  name="Joe",
  salary=55000,
  union=T
)
class(j) <- "employee"

attributes(j)

j # вызвали метод print по умлочанию к этому классу

# -------------Напишем собственный метод для нашего класса j
print.employee <- function(wrkr){
  cat(wrkr$name, "\n")
  cat("Зарплата", wrkr$salary, "\n")
  cat("Является ли членом профсоюза?", wrkr$union, "\n")
}

# теперь любой вызов print() для объекта класса employee теперь должен обозначаться print.employee()
methods(,"employee")
print.employee

j


# ----------- Наследование
# Смысл наследования - в создании новых объектов, как специализированных версий объектов-родителей

k <- list(
  name = "Kate",
  salary = 68000,
  union = F,
  hrsthismonth = 2 # новый класс содержит одну доп переменную по сравнению с родительским
)
class(k) <- c("hrlyemployee", "employee") # эта доп переменная указывается первой
k


# Задание №1 - создать класс Домашнее животное с подклассами (кошка, собака, корова), у которых есть общие методы (наследуемые) и собственные.

c <- list(
  type = "cat",
  weight = "4",
  averageage = "12"
)
class(c) <- "Pet"
print.pet <- function(pets)
{
  cat("Тип", pets$type, "\n")
  cat("Вес", pets$weight, "\n")
  cat("Средний возраст", pets$averageage, "\n")
}
methods(,"pet")
print.pet
c
cw <- list(
  type = "cow",
  weight = "720",
  averageage = "20",
  skill = "mooing"
)
class(cw) <- c("skill","Pet")
cw
d <- list(
  type = "dog",
  weight = "5",
  averageage = "13",
  skill = "bark",
  color = "black"
    )
class(d) <- c("color","skill","Pet")
c
# Задание №2 - создать класс Автомобиль с подклассами и своими методами.
cr <-list(
  type = "car",
  age = "4"
)
class(cr) <- "cars"
print.car <- function(cars)
{
  cat("Тип", cars$type, "\n")
  cat("Возраст",cars$age, "\n")
  cat("Функция",cars$noun, "\n")
}
methods(,"car")
print.car
cr
cr1 <- list(
  type = "diesel car",
  age = "5",
  noun = "biiip"
)
class(cr1) <- c("noun","car")
cr1
cr2<- list(
  type = "electric car",
  age = "1",
  noun = "driving",
  color = "black"
)
class(cr2)<- c("color","noun","car")
cr
# S4 класс - более безопасный с точки зрения типов данных и ошибок в орфографии.
# S4  класс задается немного по-другому
setClass("employee",
         representation(
           name="character",
           salary="numeric",
           union="logical)
)
joe <- new("employee", name="Joe", salary=55000, union=T)
joe

joe@name
joe@salary
joe@union

slot(joe, "salary") <- 88000
joe@salary

# обобщенный метод для класса S4
setMethod("show", "employee",
          function(object){
            inorout <- ifelse(object@union, "is", "is not")
            cat(object@name, "has a salary of", object@salary,
                "and", inorout, "in the union", "\n")
          }
)
joe



# Практическое задание:
# 1) напишите класс "Автомобиль". Пропишите в нем базовые функции.
# Создайте несколько дочерних классов, которые наследуют общие функции родительского класса
# и при этом создают новые, которых раньше не было.

# В частности - базовая функция всех автомобилей - ехать (print("I'm driving")) и гудеть (print("biiip-biiip!!"))
# этот родительский метод будут наследовать все потомки. Плюс - добавлять свои методы.

# Например:
# Автомобиль -> Дизельный автомобиль - ехать на солярке
# Автомобиль -> Бензиновый автомобиль - ехать на бензине
# Автомобиль -> Электромобиль - ехать на электричестве

# 2) Для запуска написанных классов - напишите функцию, которая:
# 2.1) спросит пользователя - какой класс автомобилей его интересует, запомнит введенную инфу
# 2.2) в зависимости от введенной инфы - запустит создание экземпляра указанного класса
# 2.3) созданный класс должны вывести в консоль все родительские и собственные методы


# 3) сохраните это задание в виде локального Гит-репозитория, запушьте на Гитхаб, пришлите ссылку в Тимз 

setClass("cars",
                 representation(
                   name = "character",
                   year = "numeric",
                   maxspeed = "numeric")
)
volkswagen <- new("cars",name = "Volkswagen",year = "2020",  maxspeed = "208")
volkswagen
setMethod("show","cars",
          function(object){inorout <- ifelse(object@year, "is", "is not")
            cat(object@name, "год производства", object@maxspeed,
                "and", inorout, "максимальная скорость", "\n")
          }
)
setClass("cars",
                slots = list(),
                contains = "environment")
setGeneric("ехать",
                function(object)
                {
                standardGeneric("ехать")
                }
)
setClass("cars",
                slots = list(),
                contains = "environment")
setGeneric("сигналить",
                function(object)
                {
                standardGeneric("сигналить")
                }
)
setGeneric("ехать",
                function(object)
                {
                 cat("I am driving\n")
                }
)
setClass("cars",
                slots = list(),
                contains = "environment")
setGeneric("сигналить",
                function(object)
                {
                cat("biiip-biiip!!\n")
                }
)
setClass("Дизельный автомобиль",
         slot = list(),
         contains = "cars")
setMethod("ехать",
          signature(object = "Дизельный автомобиль"),
          function(object)
          {
          cat("I am driving on diesel fuel\n")
          }
)
setClass("Бензиновый автомобиль",
         slot = list(),
         contains = "cars")
setMethod("ехать",
          signature(object = "Бензиновый автомобиль"),
          function(object)
          {
          cat("I am driving on gasoline\n")
          }
)
setClass(""Бензиновый авто"Электромобиль",
         slot = list(),
         contains = "cars")
setMethod("ехать",
          signature(object = "Электромобиль"),
          {
          cat("I am driving on electricity\n")
          }
)
Запуск <- function(){
 выбор <- readline(prompt = "Какой класс автомобилей вас интересует?")
 if (tolower(выбор) == "дизельный"){
  cars <- new("Дизельный автомобиль")
 } else if (tolower(выбор) == "бензиновый"){
  cars <- new("Бензиновый автомобиль")
 } else if (tolower(выбор) == "электрический") {
  cars <- new("Электромобиль")
 } else {
 cat("Неверный выбор\n")
 return
 }
 cat("Родительские методы:\n")
 ехать(cars)
 сигналить(cars)
 cat("Собственные методы:\n")
 if (class(cars) == "Дизельный автомобиль")
 {
 cat("едет на бензине\n")
 }
 else if (class(cars) == "Электромобиль")
 {
 cat("едет на электричестве\n")
 }
}
запуск()
