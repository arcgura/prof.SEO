library(dplyr)
library(ggplot2)



best_sg <- filter(best, location_1 == "서울" | location_1 == "경기")

best_sg_city <- best_sg %>% group_by(location_1, location_2) %>% summarise(count = n())



## 각 구별 병원과의 거리 계산
library(ggmap)


city_name <- best_sg_city$location_2
city_geocode_raw<- geocode(city_name)

city_geocode <- cbind(city_name, city_geocode_raw)

guangju <- geocode("경기도 광주시")

city_geocode[5, c(2,3)] <- guangju

best_geo<- geocode("베스티안병원")

seoul_geo <- geocode("서울")

library(geosphere)
 
city_geocode <- mutate(city_geocode, dist = as.numeric(distm(city_geocode[, c(2,3)], best_geo, fun = distHaversine)))


## merge 

names(best_sg_city) <- c("region",  "city_name", "count")


best_sg_city_dist <- merge(best_sg_city, city_geocode) %>% 
    merge(population_sg)  %>% arrange(dist)  %>%
    mutate(dist_2 =round(dist/1000, digits=1) )  %>% 
    mutate(INDEX = round(count / population *100, digit =2)) %>%
    mutate(city_name_2 = paste(city_name,"-", as.character(dist_2))) %>%
    mutate(prediction = round(population/100)) %>%
    mutate(per = round(count/11940 *100, digit =2 ))

seoul <- filter(best_sg_city_dist, region == "서울")
gg <- filter(best_sg_city_dist, region == "경기")





## plot



ggplot(best_sg_city_dist, aes(x=dist_2, y=count)) + geom_point(  size = 3) + geom_line()


ggplot(seoul, aes(x=reorder(city_name_2, dist), y=count)) + geom_bar(aes(fill = dist_2), stat = "identity") + 
    theme(axis.text.x = element_text(angle = 60, hjust = 1), 
        panel.grid.major.y = element_blank(),
        panel.grid.minor.y = element_blank(),
        panel.grid.major.x = element_line(color = "grey60", linetype = "dashed")) +
    labs(y = "내원 환자 수(명)", x = "지역(구) - 거리(km)") + 
    labs(title = "서울 지역 이격 거리별 내원 환자 분포")  

    
ggplot(seoul, aes(x=reorder(city_name_2, dist), y=count/prediction*100)) + geom_bar(aes(fill = dist_2),  stat = "identity") + 
        theme(axis.text.x = element_text(angle = 60, hjust = 1), 
            panel.grid.major.y = element_blank(),
            panel.grid.minor.y = element_blank(),
            panel.grid.major.x = element_line(color = "grey60", linetype = "dashed")) +
        labs(y = "흡수정도(%) \n (실제내원환자수 / 추정 화상환자수 * 100) ", x = "지역(구) - 거리(km)") + 
        labs(title = "서울 지역 별 화상환자 흡수율 (지역별 인구 수로 환자수 추정)")  

    
ggplot(gg, aes(x=reorder(city_name_2, dist), y=count)) + geom_bar(aes(fill = dist_2), stat = "identity") + 
    theme(axis.text.x = element_text(angle = 60, hjust = 1), 
        panel.grid.major.y = element_blank(),
        panel.grid.minor.y = element_blank(),
        panel.grid.major.x = element_line(color = "grey60", linetype = "dashed")) +
    labs(y = "내원 환자 수(명)", x = "지역(시) - 거리(km)") + 
    labs(title = "경기 지역 이격 거리별 내원 환자 분포")      


ggplot(gg, aes(x=reorder(city_name_2, dist), y=count/prediction*100)) + geom_bar(aes(fill = dist_2),  stat = "identity") + 
    theme(axis.text.x = element_text(angle = 60, hjust = 1), 
        panel.grid.major.y = element_blank(),
        panel.grid.minor.y = element_blank(),
        panel.grid.major.x = element_line(color = "grey60", linetype = "dashed")) +
    labs(y = "흡수정도(%) \n (실제내원환자수 / 추정 화상환자수 * 100) ", x = "지역(시) - 거리(km)") + 
    labs(title = "경기 지역 별 화상환자 흡수율 (지역별 인구 수로 환자수 추정)")  



ggplot(seoul, aes(x=reorder(city_name_2, dist), y=prediction)) + 
    geom_bar(stat = "identity", colour = "black", fill = "light blue") +
    theme(axis.text.x = element_text(angle = 60, hjust = 1)) +
    labs(y = "예상 환자 수", x = "구 - 거리(km)") + 
    labs(title = "서울 지역 예상환자수")  +
    geom_point(data=seoul, aes(y=count), size =3, color = "red")


ggplot(gg, aes(x=reorder(city_name_2, dist), y=prediction)) + 
    geom_bar(stat = "identity", colour = "black", fill = "light blue") +
    theme(axis.text.x = element_text(angle = 60, hjust = 1)) +
    labs(y = "예상 환자 수", x = "구 - 거리(km)") + 
    labs(title = "경기 지역 예상환자수 비교")  +
    geom_point(data=gg, aes(y=count), size =3, color = "red")
    


#### 시도 별 중중도 확인  (최영환 선생님 30% 이상 자료 참조할것)



### 서울경기 지도에 내원신환명수(전체신환 중 퍼센트) 표시
require(Kormaps)
require(tmap)
library(dplyr)
library(ggplot2)
library(scales)
library(ggmap)


# korpomap shp를 data.frame 으로 변환
korea.shp.f_2 <- fortify(korpopmap2, region = "id")

#서울 지도 추출
sp_id_s <- korpopmap2@data[c(1:25), 1]
seoul_map <- filter(korea.shp.f_2, id %in% sp_id_s)
seoul_map_center <- group_by(seoul_map, id) %>% summarise(long_avr = mean(long), lat_avr = mean(lat))


# 서울 지역 표시
ggplot() +
    geom_polygon(data = seoul_map, 
        aes(x = long, y = lat, group = group), fill = "white", 
        color = "black" , size = 0.25 ) +
    coord_map() +
    geom_point(data = seoul,
        aes(x = lon, y = lat, size = count), color = "pink") +
        scale_size_area(max_size = 20, guide = FALSE) +
    geom_point(data = best_geo,
        aes(x = lon, y = lat), color = "blue", size = 5) +
    geom_text(data = seoul,
        aes(x = lon, y = lat, label = per), size = 4, vjust = 0) +
    labs(title = "서울병원 내원 환자 서울 지역별 분포(%)")  


# 서울, 경기 지도 추출
korea.shp.f_1 <- fortify(korpopmap1, region = "id")
sg_map_raw <- filter(korea.shp.f_1, id == 15 | id == 8 | id == 12)
sg_map <- filter(sg_map_raw, long > 126.5)


ggplot() +
    geom_polygon(data = sg_map, 
        aes(x = long, y = lat, group = group), fill = "white", 
        color = "black" , size = 0.25 ) +
    coord_map() +
    geom_point(data = gg,
        aes(x = lon, y = lat, size = count), color = "pink") +
    scale_size_area(max_size = 12, guide = FALSE) +
    geom_point(data = best_geo,
        aes(x = lon, y = lat), color = "blue", size = 2) +
    geom_text(data = gg,
        aes(x = lon, y = lat, label = per), size = 3, vjust = 0) +
    labs(title = "서울병원 내원 환자 경기 지역별 분포(%)")  





