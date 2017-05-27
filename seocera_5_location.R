##install.packages("ggplot2")
library(ggplot2)
library(dplyr)

location_total <- as.data.frame(table(rawdata$region))
location_total <- mutate(location_total, percentage = Freq / sum(Freq)*100)
location_total <- arrange(location_total, Var1)
population <- arrange(population, 행정구역)
population_c <- as.numeric(population[,2])
location_total <- cbind(location_total, population_c)
location_total <- mutate(location_total, prevalence = (Freq /population_c*100))

location_total

location_burn <- as.data.frame(table(burndata$region))
location_burn <- mutate(location_burn, percentage = Freq / sum(Freq)*100)
location_burn <- arrange(location_burn, Var1)
population <- arrange(population, 행정구역)
population_c <- as.numeric(population[,2])
location_burn <- cbind(location_burn, population_c)
location_burn <- mutate(location_burn, prevalence = (Freq /population_c*100))

location_burn

#### bar plot

#ggplot(data=rawdata, aes(x=region)) +
#    geom_bar( fill="white", colour="black") 

#ggplot(data=rawdata[vector_burn==TRUE,], aes(x=region)) +
#    geom_bar( fill="white", colour="black") 
  

ggplot(data=location_total, aes(x=Var1, y=Freq)) +
    geom_bar(stat="identity", fill="white", colour="black") +
    ggtitle("Bar Chart of total visits by Region")


ggplot(data=location_burn, aes(x=Var1, y=Freq)) +
    geom_bar(stat="identity", fill="white", colour="black") +
    ggtitle("Bar Chart of BURN visits by Region")


ggplot(data=location_total, aes(x=Var1, y=prevalence)) +
    geom_bar(stat="identity", fill="white", colour="black") +
    ggtitle("Bar Chart of total prevalence by Region")


ggplot(data=location_burn, aes(x=Var1, y=prevalence)) +
    geom_bar(stat="identity", fill="white", colour="black") +
    ggtitle("Bar Chart of burn
        prevalence by Region")


######## choropleth,    use Kormaps data

require(Kormaps)
require(tmap)
library(dplyr)
library(ggplot2)
library(scales)
library(ggmap)

# 윈도우 인코딩문제 때문에 길어진 코드
tmp <- korpopmap1@data %>% select(21)
names(tmp) <- c("population")
tmp$population <- tmp$population / 10^4

# id 값을 .shp 파일과 동일하게 유지 (여기서 id는 각 시도에 대응)
kor.dat <- data.frame(NAME_1=korpopmap1$name_eng, id=korpopmap1$id)
kor.dat <- bind_cols(kor.dat, tmp)
kor.dat

View(kor.dat)

NAME_2 <- c("서울", "부산", "대구", "인천", "광주", "대전", "울산", "경기", "강원", "충북", "충남", "전북", "전남", "경북", "경남", "제주")

kor.dat <- cbind(kor.dat, NAME_2)
kor.dat <- arrange(kor.dat, NAME_2)
id<-kor.dat$id
location_total <- cbind(id, location_total)


# ggplot으로 시각화 위해 데이터프레임으로 변환
korea.shp.f <- fortify(korpopmap1, region = "id")

merge.shp.coef<-merge(korea.shp.f, location_total, by="id", all.x=TRUE)
korea.population.2010 <-merge.shp.coef[order(merge.shp.coef$order), ] 

head(korea.population.2010)



# 1 단계 : 시범 지리정보 도식화

ggplot() +
    geom_polygon(data = korea.population.2010, 
        aes(x = long, y = lat, group = group, fill = Freq), 
        color = "black", size = 0.25) + 
    coord_map()

# 2 단계 : 배포 품질 도식화
ggplot() +
    geom_polygon(data = korea.population.2010, 
        aes(x = long, y = lat, group = group, fill = Freq), 
        color = "black", size = 0.25) + 
    coord_map()+
    scale_fill_distiller(name="인구 : 단위(만명)", palette = "Greens", breaks = pretty_breaks(n = 10), direction = 1)+
    theme_nothing(legend = TRUE)+
    labs(title="ER 방문 환자 수")

ggplot() +
    geom_polygon(data = korea.population.2010, 
        aes(x = long, y = lat, group = group, fill = prevalence), 
        color = "black", size = 0.25) + 
    coord_map()+
    scale_fill_distiller(name="퍼센트 : 단위(%)", palette = "Greens", breaks = pretty_breaks(n = 10), direction = 1)+
    theme_nothing(legend = TRUE) +
    labs(title="인구 대비 ER 방문 환자 수 (%)")


# use ggmap

korea <- qmap("south korea", zoom = 7)

korea +geom_polygon(aes(x=long,y=lat, group=group, alpha=0.25), data=korea.population.2010, fill='white')+
    geom_polygon(aes(x=long,y=lat, group=group), data=korea.population.2010, color='black', fill=NA)

korea +geom_polygon(aes(x=long,y=lat, group=group,  fill=prevalence), 
    data=korea.population.2010, color="black") +
    scale_fill_gradient(low='white', high='red')

korea + geom_polygon(aes(x=long,y=lat, group=group, fill=prevalence), data=korea.population.2010, color='black') +
    scale_fill_distiller(palette='Spectral') + scale_alpha(range=c(0.5,0.5))
