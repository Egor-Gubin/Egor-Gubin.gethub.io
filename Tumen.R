#Губин Егор 123 вар5
#регион 72 Tyumen
#урожайность пшеницы в 2012 году, взяв для рассчета средние суммы активных температур 
#за предыдущие 5 лет, с 30 ближайших метеостанций
setwd("D:/Tumen")
getwd()
#install.packages("tidyverse")
library (tidyverse)
#install.packages ("rnoaa")
library (rnoaa)
#station_data = ghcnd_stations()
#write.csv(station_data, "Tumen.csv")
station_data=read.csv("Tumen1.csv")
tyumen=data.frame(id = "TYUMEN", latitude = 57.1522, longitude = 65.5272)
Tyumen_around = meteo_nearby_stations(lat_lon_df = tyumen, station_data = station_data, limit = 30, var = c("PRCP", "TAVG"), year_min = 2007, year_max = 2015)
#Tyumen_around это список единственным элементом которого является таблица, содержащая идентификаторы 
#метеостанций отсортированных по их 
# удалленности от Тюмени, очевидно что первым элементом таблицы будет идентификатор метеостанции Тюмени,
#его мы и получаем
? meteo_nearby_stations
Tyumen_id = Tyumen_around[["Tyumen"]][["id"]][1]
#получение всех данных с метеостанций
summary (tyumen_id)
str(Tyumen_around)
all_Tyumen_data = meteo_tidy_ghcnd(stationid = Tyumen_id)
#2)чтобы получить таблицу всех метеостанций вокруг Тюмень нужно выбрать целиком первый объект из списка
Tyumen_table = Tyumen_around[[1]]
summary(Tyumen_table)
#в таблице Tyumen_table оказалось 16 объектов, ранжированных по расстоянию от Tyumen
#нужно убедится, что этот список включает нужные по условию задачи метеостанции

Tyumen_stations = Tyumen_table 
str(Tyumen_stations)
#Таким образом, мы сформировали список необходимых станций, посмотрим, что он содержит
Tyumen_stations$id

###Нужно создать цикл, в котором бы скачивались  нужные данные для всех метеостанций из созданного списка
#Создадим промежуточный объект, куда будем скачивать данные с конкретной метеостанции
################## 3. Скачивание погодных данных для выбранных метеостанций
#Для получения всех данных с 1 метеостанции, зная ее идентификатор, используйте #след. команду
all_Tyumen_data = meteo_tidy_ghcnd(stationid = Tyumen_id)
#посмотрим, что же скачивается
#all_Tyumen_data = meteo_tidy_ghcnd(stationid = Tyumen_stations) #не получается
?meteo_tidy_ghcnd
summary(all_Tyumen_data)
#скачиваются данные только с самой первой станции
#Подумаем, какие из этих данных нам нужны
##нам нужны среднесуточные температуры (tavg) выше 5, но ниже 30 градусов за вышеуказанный период (2007-2015 гг.)

###Нужно создать цикл, в котором бы скачивались  нужные данные для всех метеостанций из созданного списка
#Создадим промежуточный объект, куда будем скачивать данные с конкретной метеостанции
all_i = data.frame()

#Создадим объект, куда скачаем все данные всех метеостанций
all_Tyumen_meteodata = data.frame()    
#Цикл для всех метеостанций

for(i in 1:16) 
{ 
  all_i  = meteo_tidy_ghcnd(stationid =  Tyumen_around[["Tyumen"]][["id"]][i])
  
  #выберем нужные свойства 
  all_i = all_i[ ,c("id","date","tavg")] 
  #с помощью команды rbind соединяем данные, полученные на предыдущих и данном    #этапах цикла
  print(all_i)
  all_Tyumen_meteodata=rbind(all_Tyumen_meteodata, all_i)
}

#Записываем полученные результаты
#write.csv(all_Tyumen_meteodata,"all_Tyumen_meteodata.csv")

#2 часть
################## 4. Разбивка даты на составляющие(год, месяц, день года) 
# считываем данные из файла all_Tyumen_meteodata.csv
all_Tyumen_meteodata = read.csv("all_Tyumen_meteodata.csv")
#посмотрим на данные
str(all_Tyumen_meteodata)
#ищем библиотеку из tidyverse, которая может помочь с датой
library(lubridate)
# вытащить год
#проверим, что работает
y = year(all_Tyumen_meteodata$date); y
all_Tyumen_meteodata [,"year"]= year(all_Tyumen_meteodata$date)
#добавим месяц
all_Tyumen_meteodata [,"month"]= month(all_Tyumen_meteodata$date) 
#вытащить день от начала года
all_Tyumen_meteodata [,"day_of_the_year"]= yday(all_Tyumen_meteodata$date) 
#проверим результат
str(all_Tyumen_meteodata)    
#отфильтруем данные за 2007-2015
years_Tyumen_meteodata = filter (all_ufa_meteodata, year > 2006 & year < 2016 )   
#проверим результат
str(years_Tyumen_meteodata)
summary (years_Tyumen_meteodata)    
################## 5. Средняя (по годам и метеостанциям) сумма активных температур за месяц
#Изучаем формулу и видим, что единственное, что нужно расчитать
#- это сумму температур больше 5 град. по месячно, остальное в формуле-  константы

#### 1.  температурy нужно поделить на 10
years_Tyumen_meteodata[,"tavg"]= years_Tyumen_meteodata$tavg / 10
summary (years_Tyumen_meteodata)
#### 2. Превратим в нули все NA и где  5<tavg>30 

years_Tyumen_meteodata [is.na(years_Tyumen_meteodata$tavg), "tavg"] = 0
years_Tyumen_meteodata [years_Tyumen_meteodata$tavg<5, "tavg"] = 0
years_Tyumen_meteodata [years_Tyumen_meteodata$tavg>30, "tavg"] = 0

#проверяем, что температура получилась в или 0 или больше 5 градусов
summary(years_Tyumen_meteodata)
#### 3. суммарная температура за месяц за 12 лет для всех станций 
# группирую по метеостанциям, годам и месяцам
#?group_by
alldays= group_by(years_Tyumen_meteodata,id,year,month)

#функция summarize применяет некоторые действия к отдельным группам, полученным
#с помощью функции group_by
#просуммирую температуру по этим группам с помощью sum

sumT_alldays_Tyumen = summarize(alldays, tsum = sum(tavg))
#Получилось - все года, все месяца присутствуют
summary(sumT_alldays_Tyumen) 
#Сгруппирем данные по месяцам  
groups_Tyumen_months = group_by(sumT_alldays_ufa,month)
groups_Tyumen_months
#найду для всех метеостанций и ВСЕХ лет среднее по месяцам
sumT_months= summarize(groups_Tyumen_months , St = mean(tsum))
sumT_months



################## 6. Подготовка к расчету по формуле Урожая
### Ввод констант
afi=c(0.000,0.000,0.000,32.110,26.310,25.640,23.200,18.730,16.300,13.830,0.000,0.000)
bfi=c(0.000,0.000,0.000,11.300,9.260,9.030,8.160,6.590,5.730,4.870,0.000,0.000)
di=c(0.000,0.000,0.000,0.330,1.000,1.000,1.000,0.320,0.000,0.000,0.000,0.000)
# константы выше взяты из первой таблицы
y1 = 1.0 # - коэффициент для экспозиции склона - считаем, что все поля идеально ровные;
Kf = 300 # - коэффициент использования ФАР посевом;
Qj = 1600 # - калорийность урожая культуры; 
Lj = 2.2 #  - сумма частей основной и побочной продукции; 
Ej = 25 # - стандартная влажность культуры; 
# Рассчитаем Fi по месяцам
#Fi= afi+bfi∗y∗(St>5℃)
sumT_months = mutate(sumT_months, Fi = afi+bfi*y1*St)

#Рассчитаем Yi
sumT_months = mutate(sumT_months, Yi = ((Fi*di)*Kf)/(Qj*Lj*(100 - Ej)))
##  Расчитываем урожай как сумму по месяцам и думаем разумный ли он
Yield = sum(sumT_months$Yi)  
Yield
# Ответ: 16,7 ц/га
#По данным аналитики 2012 года средняя урожайность в регионе составляла 19,1 ц/га
#полученный ответ вполне адекватен, учитывая то, что отбрасывались очень жаркие дни (>30 град.)
