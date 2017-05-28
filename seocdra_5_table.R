################### exploration    : make table


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

