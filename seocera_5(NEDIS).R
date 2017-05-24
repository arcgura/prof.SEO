library(dplyr)

data <- read.table("/Users/jd5/Dropbox/R/seo/data.csv", header = TRUE , sep = ",", na.strings=c("NA","","."), stringsAsFactors = FALSE)

umls <- read.table("/Users/jd5/Dropbox/R/seo/umls.txt", header = TRUE, sep="\t", quote = "", strip.white = TRUE, na.strings=c("NA","","."), stringsAsFactors = FALSE)

kcdc <- read.csv("/Users/jd5/Dropbox/R/seo/kcdc7.csv", header = TRUE)



# umls(주증상) 기준 burn data
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

burndata_umls <- filter(data, 주증상.1 %in% burn_umls_code | 주증상.2 %in% burn_umls_code | 주증상.3 %in% burn_umls_code)

#kcdc7(진단코드) 기준 burn data
burn_kcdc <- kcdc[grepl("burn", kcdc$english) | grepl("Burn", kcdc$english) | grepl("BURN", kcdc$english), ]
burn_kcdc <- burn_kcdc[!grepl("due", burn_kcdc$english), ]
burn_kcdc <- burn_kcdc[!grepl("M", burn_kcdc$code), ]
burn_kcdc <- burn_kcdc[!grepl("Sequelae", burn_kcdc$english), ]
burn_kcdc <- burn_kcdc[!grepl("Z", burn_kcdc$code), ]
burn_kcdc <- burn_kcdc[!grepl("R", burn_kcdc$code), ]

burn_kcdc_code <- burn_kcdc$code

str(data)
burndata_kcdc <- filter(data, 응급환자퇴실진단코드01 %in% burn_kcdc_code | 응급환자퇴실진단코드02 %in% burn_kcdc_code | 응급환자퇴실진단코드03 %in% burn_kcdc_code )


# find unmatching data
burndata_umls_unmatch <- filter(burndata_umls, !(응급환자퇴실진단코드01 %in% burn_kcdc_code) )
burndata_kcdc_unmatch <- filter(burndata_kcdc, !(주증상.1 %in% burn_umls_code))



#merge two dataset
burndata <- merge(burndata_kcdc, burndata_umls, all = TRUE)

#check duplication
sum(duplicated(burndata$X))


# make tidy
location <- as.data.frame(table(burndata$지역))
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
