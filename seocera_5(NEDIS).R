library(dplyr)


######## red data

# read NEDIS data
rawdata <- read.table("/Users/jd5/Dropbox/R/seo/data.csv", header = TRUE , sep = ",", na.strings=c("NA","","."), stringsAsFactors = FALSE)
umls <- read.table("/Users/jd5/Dropbox/R/seo/umls.txt", header = TRUE, sep="\t", quote = "", strip.white = TRUE, na.strings=c("NA","","."), stringsAsFactors = FALSE)
kcdc <- read.csv("/Users/jd5/Dropbox/R/seo/kcdc7.csv", header = TRUE)


# read bestian data
best <- read.csv("/Users/jd5/Dropbox/R/seo/1602_1704_2.csv", header = TRUE, na.strings = "", stringsAsFactors = FALSE)
eradm <- read.csv("/Users/jd5/Dropbox/R/seo/1602_1704_ER_adm.csv", header = TRUE, na.strings = "")
chart_nu <- eradm$chart_nu
best[best$chart_nu %in% chart_nu, 14] <- "응급의학과"


# read number of population according to region
population <- read.csv("/Users/jd5/Dropbox/R/seo/population_city2.csv",   header = TRUE, na.strings = "", stringsAsFactors = FALSE)



################## 병원용

Sys.setlocale("LC_ALL", "English")

# read NEDIS data
rawdata <- read.table("data.csv", header = TRUE , sep = ",", na.strings=c("NA","","."), stringsAsFactors = FALSE, encoding = "UTF-8")
umls <- read.table("umls.txt", header = TRUE, sep="\t", quote = "", strip.white = TRUE, na.strings=c("NA","","."), stringsAsFactors = FALSE)
kcdc <- read.csv("kcdc7.csv", header = TRUE)


# read bestian data
best <- read.csv("1602_1704_2.csv", header = TRUE, na.strings = "", stringsAsFactors = FALSE)
eradm <- read.csv("1602_1704_ER_adm.csv", header = TRUE, na.strings = "")
chart_nu <- eradm$chart_nu
best[best$chart_nu %in% chart_nu, 14] <- "응급의학과"


# read number of population according to region
population <- read.csv("population_city2.csv",   header = TRUE, na.strings = "", stringsAsFactors = FALSE)












############## cleaning

# change variable names and class
names(rawdata)[2] <- "region"
names(rawdata)[6] <- "age"
names(rawdata)[10] <- "route"
names(rawdata)[11] <- "route_vehicle"
names(rawdata)[12] <- "cause_disease"
names(rawdata)[13] <- "cause_intention"
names(rawdata)[14] <- "cause_vector"
names(rawdata)[25] <- "result_ER"
names(rawdata)[20] <- "systolic_BP"
names(rawdata)[21] <- "diastolic_BP"
names(rawdata)[22] <- "pulse_rate"
names(rawdata)[23] <- "respiratory_rate"
names(rawdata)[24] <- "temperature"



rawdata$route <- as.factor(rawdata$route)
rawdata$route_vehicle <- as.factor(rawdata$route_vehicle)
rawdata$cause_disease <- as.factor(rawdata$cause_disease)
rawdata$cause_intention <- as.factor(rawdata$cause_intention)
rawdata$cause_vector <- as.factor(rawdata$cause_vector)
rawdata$result_ER <- as.factor(rawdata$result_ER)


# conversion 'visit date' to POSIXlt class and make another variables(year, wday, month )      # 계절은?
yymmdd <- strptime(rawdata$내원일자, "%Y%m%d")
year <- unclass(yymmdd)$year  
wday <- unclass(yymmdd)$wday 
month <- unclass(yymmdd)$mon
yearmonth <- format(yymmdd, format = "%Y-%m" )
rawdata <- cbind(rawdata, yymmdd, year, wday, month, yearmonth)
rawdata$wday <- as.factor(rawdata$wday)


# conversion 'visit_time' and make 'hour_data'
time_temp <- sprintf("%04d", rawdata$내원시간) 
visit_hour <- format(strptime(time_temp, format="%H%M"), format = "%H" )
rawdata <- cbind(rawdata, visit_hour)




# make 'age group'
age_group <- floor(rawdata$age / 10) *10
rawdata <- cbind(rawdata, age_group)
rawdata[rawdata$age_group >= 80, 86] <- "a.80"

# cleaning 'sex'       #1,3,9,5,7 -> Male,    0,2,4,6,8 -> Female

table(rawdata$성별)

sex <- rawdata[, 7]
sex[(sex == "1" | sex == "3" | sex == "9" | sex == "5" | sex == "7") ] <- "M"
sex[!(sex == "M")] <- "F"

rawdata <- cbind(rawdata, sex)


# cleaning vital data

rawdata[rawdata$systolic_BP == 999, 20] <- NA
rawdata[rawdata$diastolic_BP == 999, 21] <- NA
rawdata[rawdata$pulse_rate >= 300, 22] <- NA
rawdata[rawdata$respiratory_rate >= 100, 23] <- NA
rawdata[rawdata$temperature >= 60, 24] <- NA

boxplot(rawdata$temperature)


############### filtering burn 

#  burn data according to umls(main symptom)
burn_umls <- umls[grepl("burn", umls$english) | grepl("Burn", umls$english) | grepl("BURN", umls$english), ]
burn_umls <- burn_umls[!grepl("burning", burn_umls$english), ]
burn_umls <- burn_umls[!grepl("Burning", burn_umls$english), ]
burn_umls <- burn_umls[!grepl("Heartburn", burn_umls$english), ]
burn_umls <- burn_umls[!grepl("alopecia", burn_umls$english), ]
burn_umls <- burn_umls[!grepl("contracture", burn_umls$english), ]
burn_umls <- burn_umls[!grepl("scar", burn_umls$english), ]
burn_umls <- burn_umls[!grepl("ABDOMINAL", burn_umls$english), ]
burn_umls <- burn_umls[!grepl("Mc", burn_umls$english), ]

burn_umls_code <- burn_umls$CODE

burndata_umls <- filter(rawdata,  주증상.1 %in% burn_umls_code | 주증상.2 %in% burn_umls_code | 주증상.3 %in% burn_umls_code)


#  burn data according to kcdc7(dx code)
burn_kcdc <- kcdc[grepl("burn", kcdc$english) | grepl("Burn", kcdc$english) | grepl("BURN", kcdc$english), ]
burn_kcdc <- burn_kcdc[!grepl("due", burn_kcdc$english), ]
burn_kcdc <- burn_kcdc[!grepl("M", burn_kcdc$code), ]
burn_kcdc <- burn_kcdc[!grepl("Sequelae", burn_kcdc$english), ]
burn_kcdc <- burn_kcdc[!grepl("Z", burn_kcdc$code), ]
burn_kcdc <- burn_kcdc[!grepl("R", burn_kcdc$code), ]

burn_kcdc_code <- burn_kcdc$code

burndata_kcdc <- filter(rawdata, 응급환자퇴실진단코드01 %in% burn_kcdc_code | 응급환자퇴실진단코드02 %in% burn_kcdc_code | 응급환자퇴실진단코드03 %in% burn_kcdc_code )


#  burn data accroding to injury_vector

burndata_vector <- filter(rawdata, cause_vector == 50)  


# find unmatching data
burndata_umls_unmatch <- filter(burndata_umls, !(응급환자퇴실진단코드01 %in% burn_kcdc_code) )
burndata_kcdc_unmatch <- filter(burndata_kcdc, !(주증상.1 %in% burn_umls_code))


# merge three datasets
burndata_kcdc_umls <- merge(burndata_kcdc, burndata_umls, all = TRUE)
burndata <- merge(burndata_kcdc_umls, burndata_vector, all = TRUE)


# check duplication
sum(duplicated(burndata$X))

# check visit cause
table(burndata$cause_disease)


# make another 'vector_burn' variable to 'rawdata'
burn_X <- burndata$X
burn <- rawdata$X %in% burn_X
identical(sum(burn), nrow(burndata))
rawdata <- cbind(rawdata, burn)


########## subset 

subdata <- select(rawdata, X, burn, region, yymmdd, year, yearmonth, month, wday, visit_hour,
    age, age_group, sex, route, route_vehicle, cause_disease, cause_intention, cause_vector, 응급증상.해당유무, 
    systolic_BP, diastolic_BP, pulse_rate, respiratory_rate, temperature, result_ER, 전원.보낸.의료기관종류)



################### exploration    ggplot(mpg, aes(displ, hwy)) + ds       # 산포도 출력


# location 
location <- as.data.frame(table(rawdata$region))  
location <- arrange(location, Var1)
population <- arrange(population, 행정구역)
population_c <- as.numeric(population[,2])
location <- cbind(location, population_c)
location <- mutate(location, prevalence = (Freq /population_c*100))
location


route <- as.data.frame(table(rawdata$route))
route_c <- c("direct to ER", "trasfer from other hospital", "from OPD", "etc", "unknown")
route <- cbind(route_c, route)
route


route_vehicle <- as.data.frame(table(rawdata$route_vehicle))
route_vehicle_c <- c("ambulance 119", "ambulance of other hospital", "ambulance others", "government vehicle_police", "air transport", "other car", "walk", "etc", "unknown")
route_vehicle <- cbind(route_vehicle_c, route_vehicle)
route_vehicle


intention <- as.data.frame(table(rawdata$cause_intention))
intention_c <- c("non_intentional", "self_or_sucical attempt","violence_or_murdered","4","5","etc","unknown")
intention <- cbind(intention_c, intention)
intention


cause <- as.data.frame(table(rawdata$cause_vector))
cause_c<- read.csv("/Users/jd5/Dropbox/R/seo/cause.csv", header = TRUE)
cause <- cbind(cause_c$cause_vector_c, cause)
cause


emergency <- as.data.frame(table(rawdata$응급증상.해당유무))
emergency


result <- as.data.frame(table(rawdata$result_ER))
result_c<- read.csv("/Users/jd5/Dropbox/R/seo/result.csv", header = TRUE)
result <- cbind(result_c$result_ER_c, result)
result


department <- as.data.frame(table(rawdata$주진료과))
department_c<- read.csv("/Users/jd5/Dropbox/R/seo/department.csv", header = TRUE)
department<- merge(department, department_c, by.x = "Var1", by.y = "department", all = FALSE)
department


trans_in <- as.data.frame(table(rawdata$전원.보낸.의료기관종류))
hospital_from <- c("level1 general H", "general H", "hostpital", "clinic", "oriental clinic", "unknown", "etc")
trans_in <- cbind(hospital_from, trans_in)
trans_in


as.data.frame(table(rawdata$systolic_BP))
as.data.frame(table(rawdata$diastolic_BP))





# bestian hospital data
str(best)

as.data.frame(table(best$sex))
as.data.frame(table(best$Decade))
as.data.frame(table(best$adult_child))
as.data.frame(table(best$city1))
as.data.frame(table(best$city2))
as.data.frame(table(best$result))
as.data.frame(table(best$day_week))
as.data.frame(table(best$pt_class))
as.data.frame(table(best$department))

table(best$department, best$result)

as.data.frame(table(best$doctor))
as.data.frame(table(best$visit_course))
as.data.frame(table(best$burn_place))
as.data.frame(table(best$burn_place2))
as.data.frame(table(best$cause))
as.data.frame(table(best$cricumstance_of_injury))




names(rawdata)

