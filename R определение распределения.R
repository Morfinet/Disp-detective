#устанавливаем и подключаем нужные пакеты
install.packages(c("readxl",
                   "fitdistrplus",
                   "gamlss.dist",
                   "gamlss.add"),
                 dependencies=TRUE)
library(fitdistrplus)
library(gamlss)
library(gamlss.dist)
library(gamlss.add)
library(readxl)

#получаем данные
DATA <- read_excel("C:/Users/Dell/Desktop/Распределения Маша/Test1.xlsx") #в скобочках указываем путь до файла эксель
        #рисуем эмпирическую плотность распределения
        View(DATA)
        mean(emp_dist)
        sd(emp_dist)
        
        #ЗАДАЕМ ФУНКЦИЮ def_disp(x), где x - ряд данных, для которых надо найти теоретическое распределение
        
        def_disp <- function(emp_dist){
          #запускаем fitDist - он прогоняет большинство распределений с нашим методом максимального правдоподобия и выдает наиболее похожее распределение
          fit <- fitDist(emp_dist, trace = FALSE,method = "mle") # "mle' - метод максимального правдоподобия
          ter_disp_gamlss <- fit$family[1] #получаем название распределения в ter_disp_gamlss
          #переименовываем распределения на русский
        
              #зададим функцию, в которой опишем нужные нам распределения
              perevod <- function(ter_disp_gamlss_variable){
                if (ter_disp_gamlss_variable == "NO" || ter_disp_gamlss_variable == "Skew normal type 1 (Azzalini type 1)" ) {
                  ter_dist = "Нормальное"
                } 
                else if (ter_disp_gamlss_variable == "EXP") {
                  ter_dist = "Показательное"
                } 
                else if (ter_disp_gamlss_variable == "LOGNO" || ter_disp_gamlss_variable == "LOGNO2" ) {
                  ter_dist = "Логнормальное"
                }  
                else if (ter_disp_gamlss_variable == "Gamma") {
                  ter_dist = "Гамма"
                } 
                else if (ter_disp_gamlss_variable == "WEI" || ter_disp_gamlss_variable ==  "WEI2" || ter_disp_gamlss_variable ==  "WEI3")  {
                  ter_dist = "Вейбула"
                } 
                else if (ter_disp_gamlss_variable == "Skew t (Azzalini type 1)") {
                  ter_dist = "t-распределение"
                } 
                else {
                  ter_dist = "<распределение не опознано>"}
              return(ter_dist)
                  }
            
              ter_dist = perevod(ter_disp_gamlss) #получаем наше распределение на русском
              
              #учтем, что мог произойти сбой, и проверим ближайшее найденное распределение тоже
              for (i in c(1:length(fit$fits))) {
                  if (as.numeric(fit$fits[i+1] - fit$fits[i]) < 1 && ter_dist =="<распределение не опознано>"){
                  ter_dist <- perevod(names(fit$fits)[i+1])
                   }
                  
              }
              #в ответе выводим функцию распределения, среднее и стандартное отклонение
              res_dist <- data.frame(ter_dist,fit$mu,fit$sigma)
              return(res_dist)
        }
              
        #ФУНКЦИЯ def_disp(x) ВЫДАЕТ ТИП РАСПРЕДЕЛЕНИЯ(если получилось найти) И ЗНАЧЕНИЯ СРЕДНЕЙ И СТАНДАРТНОГО ОТКЛОНЕНИЯ
        
        #проверка для нормального распределения
        
    
        print(def_disp(DATA$income[DATA$region == "краснодар"]))
        mean(DATA$income[DATA$region == "краснодар"])
        sd(emp_dist)
                                                  

#отделение по регионам и технологиям
regions <- unique(DATA$region)
techs <- unique(DATA$technolohy)


        #1. отделение только по регионам
        
        for (region in regions) {
          emp_dist_region <- DATA$income[DATA$region == region]
          disp_regions[region] <- def_disp(emp_dist_region)
            }



#Проводим дальнейшее тестирование

ks.test(x = emp_dist,gamlssML(fit$y))


mean <- mean(emp_dist)
sd <- sd(emp_dist)


plotdist(x)+plotdist(fit)
plot(fit)
fit$fits
normal_dist <- fitdist( , "norm")
?fitdist
plot(normal_dist)
exp_dist <- fitdistr(x, "exp")
exp_dist
plot(normal_dist)
lines(density(x))
library(fitdistrplus)
descdist(x, discrete = FALSE)
summary(descdist)