library(dplyr)
library(ggplot2)

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



############## cleaning

# change variable names
names(rawdata)[2] <- "region"
names(rawdata)[6] <- "age"
names(rawdata)[10] <- "route"
names(rawdata)[11] <- "route_vehicle"
names(rawdata)[12] <- "cause_disease"
names(rawdata)[13] <- "cause_intention"
names(rawdata)[14] <- "cause_vector"
names(rawdata)[25] <- "result_ER"

# conversion 'visit date' to POSIXlt class and make another variables(year, wday, month )      # 계절은?
yymmdd <- strptime(rawdata$내원일자, "%Y%m%d")
year <- unclass(yymmdd)$year   ; year
wday <- unclass(yymmdd)$wday ; wday
month <- unclass(yymmdd)$mon ; month
rawdata <- cbind(rawdata, yymmdd, year, wday, month)

# conversion 'visit_time' and make 'hour_data'
time_temp <- sprintf("%04d", rawdata$내원시간) 
visit_hour <- format(strptime(time_temp, format="%H%M"), format = "%H" )
rawdata <- cbind(rawdata, visit_hour)


# make 'age group'
age_group <- floor(rawdata$age / 10) *10
rawdata <- cbind(rawdata, age_group)


# cleaning 'sex'       #1,3,9,5,7 -> Male,    0,2,4,6,8 -> Female

table(rawdata$성별)

sex <- rawdata[, 7]
sex[(sex == "1" | sex == "3" | sex == "9" | sex == "5" | sex == "7") ] <- "M"
sex[!(sex == "M")] <- "F"

rawdata <- cbind(rawdata, sex)



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
vector_burn <- rawdata$X %in% burn_X
identical(sum(vector_burn), nrow(burndata))
rawdata <- cbind(rawdata, vector_burn)






################### exploration    ggplot(mpg, aes(displ, hwy)) + ds       # 산포도 출력


# location 
location <- as.data.frame(table(rawdata$region))  
location <- arrange(location, Var1)
population <- arrange(population, 행정구역)
population_c <- as.numeric(population[,2])
location <- cbind(location, population_c)
location <- mutate(location, prevalence = (Freq /population_c*100))
location


route <- as.data.frame(table(burndata$내원경로))
route_c <- c("direct to ER", "trasfer from other hospital", "from OPD", "etc", "unknown")
route <- cbind(route_c, route)
route


route_vehicle <- as.data.frame(table(burndata$내원수단))
route_vehicle_c <- c("ambulance 119", "ambulance of other hospital", "ambulance others", "government vehicle_police", "air transport", "other car", "walk", "etc", "unknown")
route_vehicle <- cbind(route_vehicle_c, route_vehicle)
route_vehicle


intention <- as.data.frame(table(burndata$내원사유.의도성여부.))
intention_c <- c("non_intentional", "self_or_sucical attempt","violence_or_murdered","4", "etc","unknown")
intention <- cbind(intention_c, intention)
intention


cause <- as.data.frame(table(burndata$내원사유.손상기전.))
cause_c<- read.csv("/Users/jd5/Dropbox/R/seo/cause.csv", header = TRUE)
cause_c <- filter(cause_c, !(cause_vector == 9))
cause <- cbind(cause_c$cause_vector_c, cause)
cause



emergency <- as.data.frame(table(burndata$응급증상.해당유무))
emergency


result <- as.data.frame(table(burndata$응급진료.결과))
result_c<- read.csv("/Users/jd5/Dropbox/R/seo/result.csv", header = TRUE)
result_c<- filter(result_c, !(result_ER == 38 | result_ER == 48))
result <- cbind(result_c$result_ER_c, result)
result


department <- as.data.frame(table(burndata$주진료과))
department_c<- read.csv("/Users/jd5/Dropbox/R/seo/department.csv", header = TRUE)
department<- merge(department, department_c, by.x = "Var1", by.y = "department", all = FALSE)



trans_in <- as.data.frame(table(burndata$전원.보낸.의료기관종류))
hospital_from <- c("level1 general H", "general H", "hostpital", "clinic", "oriental clinic", "unknown", "etc")
trans_in <- cbind(hospital_from, trans_in)
trans_in




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

