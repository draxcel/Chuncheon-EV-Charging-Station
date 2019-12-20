#  🚗 춘천시 전기차 충전소 최적의 입지선정을 위한 분석

- 목표: 춘천시 전기차 충전소 최적의 입지선정을 위한 분석

- 과정:
1. 춘천시에서 제공하는 통계자료를 분석하기 알맞는 형태로 바꾼다.
2. NB클러스터 및 K-means 클러스터링을 위한 변수를 선택한다.
3. NB클러스터를 통한 최적의 군집 개수를 확인하고 K-means 클러스터링을 한다.
4. 나뉜 군집의 특성을 확인하여 우선 설치 지역으로 판단되는 군집을 선택한다.
5. 춘천시 전체 전기차 충전소 위치를 반경 1km로 하여 표시한다.
6. 기준에 미치지 못하는 충전소(충전기 대 전기차 비율 도심지역 1:4, 교외지역 1:12)는 지운다.
7. 선택한 군집에서 전기차 충전소 반경 1km 안에 포함되어있는 읍면동은 제외한다.

- 코드:
<pre><code># 필요한 패키지 설치하기
install.packages("xlsx")
install.packages("NbClust")
install.packages("dplyr")
install.packages("fpc")
install.packages("rgdal")
install.packages("ggplot2")
install.packages("ggforce")

# 필요한 패키지 불러오기
library(xlsx)
library(NbClust)
library(dplyr)
library(fpc)
library(rgdal)
library(ggplot2)
library(ggforce)

# 필요한 자료 불러오기
population <- read.xlsx("population.xlsx", sheetIndex = 1, stringsAsFactors = FALSE, encoding = "UTF-8")
business <- read.csv("business.csv", stringsAsFactors = FALSE)
v_num <- read.xlsx("v_num.xlsx", sheetIndex = 1, stringsAsFactors = FALSE, encoding = "UTF-8")
ev_num <- read.xlsx("ev_num.xlsx", sheetIndex = 1, stringsAsFactors = FALSE, encoding = "UTF-8")
ev_charger <- read.xlsx("ev_charger.xlsx", sheetIndex = 1, stringsAsFactors = FALSE, encoding = "UTF-8")
public_institution <- read.xlsx("public_institution.xlsx", sheetIndex = 1, stringsAsFactors = FALSE, encoding = "UTF-8")
parking_lot <- read.xlsx("parking_lot.xlsx", sheetIndex = 1, stringsAsFactors = FALSE, encoding = "UTF-8")

# 데이터에서 원하는 행렬만 가져오고 형태 바꿔주기
# business에서는 읍면동별 종사자수를 가져왔습니다.
business <- business[c(5:29),] %>% 
  select(읍면동별.1., X2016.2)
business$X2016.2 <- as.numeric(business$X2016.2)

# 열 이름 정해주기
names(population) <- c("District", "Population")
names(business) <- c("District", "Practician")
names(v_num) <- c("District", "V number")
names(ev_num) <- c("District", "EV number")
names(ev_charger) <- c("District", "EV Charger")
names(public_institution) <- c("District", "Public Institution")
names(parking_lot) <- c("District", "Parking lot")

# 데이터 표준화해주기
population$Population <- scale(population$Population)
business$Practician <- scale(business$Practician)
v_num$`V number` <- scale(v_num$`V number`)
ev_num$`EV number` <- scale(ev_num$`EV number`)
ev_charger$`EV Charger` <- scale(ev_charger$`EV Charger`)
public_institution$`Public Institution` <- scale(public_institution$`Public Institution`)
parking_lot$`Parking lot` <- scale(parking_lot$`Parking lot`)

# 데이터 합쳐주기
# city는 결과 저장하기 위한 파일, city.cluster는 클러스터링하기 위한 파일입니다.
city <- data.frame(District = population$District, Population = population$Population, 
                   Practican = business$Practician, V_num = v_num$`V number`,
                   EV_num = ev_num$`EV number`,
                   EV_charger = ev_charger$`EV Charger`,
                   Public_institution = public_institution$`Public Institution`,
                   Parking_lot = parking_lot$`Parking lot`)

city.cluster <- data.frame(Population = population$Population, 
                           Practican = business$Practician, V_num = v_num$`V number`,
                           EV_num = ev_num$`EV num`,
                           EV_charger = ev_charger$`EV Charger`,
                           Public_institution = public_institution$`Public Institution`,
                           Parking_lot = parking_lot$`Parking lot`)

# NB클러스트
set.seed(779)

nbc <- NbClust(city.cluster, min.nc = 2, max.nc = 7, method = "kmeans")

par(mfrow = c(1, 1))
barplot(table(nbc$Best.n[1, ]),
        xlab = "Numer of Clusters", ylab = "Number of Criteria", main = "Number of Clusters Chosen")

# NB클러스트 결과에 따른 kmeans 클러스터링
result <- kmeans(city.cluster, 2)
plotcluster(city.cluster, result$cluster, color = TRUE, shade = TRUE)

# city 변수에 Cluster 결과 추가하고 shp파일에 정렬되어있는 id 붙여주기
city$Cluster <- result$cluster
city$id <- c(0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 14, 10, 11, 12, 13, 15, 16, 17, 18, 19, 20, 21, 22, 23, 24)
city.ordered.cluster <- city %>% 
  arrange(Cluster)

# 데이터 프레임을 CSV로 저장하기
write.csv(city.ordered.cluster, file = "cluster_result.csv", row.names = FALSE)

# 시각화자료, 지리정보 불러오기
data <- read.csv("cluster_result.csv", header = TRUE)
map <- readOGR(dsn = 'map_2018', layer = 'bnd_dong_32010_2018_2018_2Q', encoding = 'CP949')

# 시각화자료, 지리정보 합치기
cln_map <- fortify(map)
cln_map$id <- as.numeric(cln_map$id)
data_merge <- merge(cln_map, data, by = 'id')

# 춘천시 군집별 지도시각화하기
data$group1 <- c(1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 0, 0, 0, 0, 0, 0, 0, 0)
data$group2 <- c(0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 1, 1, 1, 1, 1, 1, 1)
data_merge.group <- merge(cln_map, data, by = 'id')

ggplot() + geom_polygon(data = data_merge.group, aes(x = long, y = lat, group = group, fill = group1), color = "black", size = 0.5) +
  scale_fill_gradient(low = '#C8C8C8', high = '#FF9933') +
  theme(panel.background = element_rect(fill = 'white', colour = 'white'),
        axis.title.x = element_blank(),
        axis.text.x = element_blank(),
        axis.ticks.x = element_blank(),
        axis.title.y = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks.y = element_blank(),
        legend.position = "none")

ggplot() + geom_polygon(data = data_merge.group, aes(x = long, y = lat, group = group, fill = group2), color = "black", size = 0.5) +
  scale_fill_gradient(low = '#C8C8C8', high = '#009933') +
  theme(panel.background = element_rect(fill = 'white', colour = 'white'),
        axis.title.x = element_blank(),
        axis.text.x = element_blank(),
        axis.ticks.x = element_blank(),
        axis.title.y = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks.y = element_blank(),
        legend.position = "none")

# 군집결과 두 그룹 비교하는 코드 추가하기

# 춘천시 전기차대수별 전기차충전기대수 비율 확인하기
elec_compare <- read.xlsx("EV_ratio.xlsx", sheetIndex = 1, stringsAsFactors = FALSE, encoding = "UTF-8")
elec_compare_cal <- elec_compare[,c(2, 3)]
elec_compare_cal$비율 <- elec_compare_cal[,1] %/% elec_compare_cal[,2]

elec_compare_cal$비율[is.na(elec_compare_cal$비율)] <- 0
elec_compare_cal$비율[is.nan(elec_compare_cal$비율)] <- 0
elec_compare_cal$비율[is.infinite(elec_compare_cal$비율)] <- 0
elec_compare_cal$전기차수 == 0 # 3, 6, 8, 9, 12

elec_compare_cal$비율[3] <- elec_compare_cal$충전기수[3]
elec_compare_cal$비율[6] <- elec_compare_cal$충전기수[6]
elec_compare_cal$비율[8] <- elec_compare_cal$충전기수[8]
elec_compare_cal$비율[9] <- elec_compare_cal$충전기수[9]
elec_compare_cal$비율[12] <- elec_compare_cal$충전기수[12]

elec_compare_cal$District <- elec_compare$읍면동별

# 춘천시 급속충전소 표시하기(1) - 충전기 대 전기차 비율 도심지역 1:4, 교외지역 1:12 못미치는 충전소 무시하고 전체표시 
long.1 = c(1016100); lat.1 = c(1984000)
center.1 = data.frame(long.1, lat.1) # 의암빙상장
long.2 = c(1017600); lat.2 = c(1985500)
center.2 = data.frame(long.2, lat.2) # 공지천공영주차장
long.3 = c(1020000); lat.3 = c(1987400)
center.3 = data.frame(long.3, lat.3) # 소양강공영주차장
long.4 = c(1016500); lat.4 = c(1988200)
center.4 = data.frame(long.4, lat.4) # 애니메이션박물관
long.5 = c(1018800); lat.5 = c(1980000)
center.5 = data.frame(long.5, lat.5) # 김유정문학마을
long.6 = c(1021700); lat.6 = c(1987300)
center.6 = data.frame(long.6, lat.6) # 춘천극장몸짓
long.7 = c(1020300); lat.7 = c(1985600)
center.7 = data.frame(long.7, lat.7) # 한국전력공사
long.8 = c(1020000); lat.8 = c(1985900)
center.8 = data.frame(long.8, lat.8) # 한국주택공사
long.9 = c(1018800); lat.9 = c(1985000)
center.9 = data.frame(long.9, lat.9) # 이마트
long.10 = c(1020200); lat.10 = c(1983700)
center.10 = data.frame(long.10, lat.10) # 홈플러스
long.11 = c(1020150); lat.11 = c(1985800)
center.11 = data.frame(long.11, lat.11) # 현대자동차 춘천지점
long.12 = c(1018400); lat.12 = c(1985200)
center.12 = data.frame(long.12, lat.12) # 롯데마트 춘천점
long.13 = c(1008000); lat.13 = c(1981000)
center.13 = data.frame(long.13, lat.13) # 엘리시안강촌 리조트
long.14 = c(1020746); lat.14 = c(1986682)
center.14 = data.frame(long.14, lat.14) # 강원도청
long.15 = c(1020800); lat.15 = c(1986582)
center.15 = data.frame(long.15, lat.15) # 춘천시청
long.16 = c(1026500); lat.16 = c(1993500)
center.16 = data.frame(long.16, lat.16) # 소양강댐 주차장
long.17 = c(1021200); lat.17 = c(1983600)
center.17 = data.frame(long.17, lat.17) # 하나로마트 퇴계점
long.18 = c(1020600); lat.18 = c(1986382)
center.18 = data.frame(long.18, lat.18) # 춘천시청 본청
long.19 = c(1021000); lat.19 = c(1989000)
center.19 = data.frame(long.19, lat.19) # 춘천시청 농업기술센터
long.20 = c(1013200); lat.20 = c(1977500)
center.20 = data.frame(long.20, lat.20) # 남산면사무소
long.21 = c(1020000); lat.21 = c(1989500)
center.21 = data.frame(long.21, lat.21) # 신사우동사무소
long.22 = c(1021700); lat.22 = c(1987300)
center.22 = data.frame(long.22, lat.22) # 후평1동사무소
long.23 = c(1022800); lat.23 = c(1983300)
center.23 = data.frame(long.23, lat.23) # 동내면사무소

ggplot() + geom_polygon(data = data_merge.group, aes(x = long, y = lat, group = group, fill = group2), color = "black", size = 0.5) +
  scale_fill_gradient(low = '#C8C8C8', high = '#009933') +
  theme(panel.background = element_rect(fill = 'white', colour = 'white'),
        axis.title.x = element_blank(),
        axis.text.x = element_blank(),
        axis.ticks.x = element_blank(),
        axis.title.y = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks.y = element_blank(),
        legend.position = "none") +
  geom_point(aes(x = long.1, y = lat.1, group = 1), color = "red") +
  geom_circle(aes(r = 1000, x0 = long.1, y0 = lat.1), color = "red") +
  geom_point(aes(x = long.2, y = lat.2, group = 1), color = "red") +
  geom_circle(aes(r = 1000, x0 = long.2, y0 = lat.2), color = "red") +
  geom_point(aes(x = long.3, y = lat.3, group = 1), color = "red") +
  geom_circle(aes(r = 1000, x0 = long.3, y0 = lat.3), color = "red") +
  geom_point(aes(x = long.4, y = lat.4, group = 1), color = "red") +
  geom_circle(aes(r = 1000, x0 = long.4, y0 = lat.4), color = "red") +
  geom_point(aes(x = long.5, y = lat.5, group = 1), color = "red") +
  geom_circle(aes(r = 1000, x0 = long.5, y0 = lat.5), color = "red") +
  geom_point(aes(x = long.6, y = lat.6, group = 1), color = "red") +
  geom_circle(aes(r = 1000, x0 = long.6, y0 = lat.6), color = "red") +
  geom_point(aes(x = long.7, y = lat.7, group = 1), color = "red") +
  geom_circle(aes(r = 1000, x0 = long.7, y0 = lat.7), color = "red") +
  geom_point(aes(x = long.8, y = lat.8, group = 1), color = "red") +
  geom_circle(aes(r = 1000, x0 = long.8, y0 = lat.8), color = "red") +
  geom_point(aes(x = long.9, y = lat.9, group = 1), color = "red") +
  geom_circle(aes(r = 1000, x0 = long.9, y0 = lat.9), color = "red") +
  geom_point(aes(x = long.10, y = lat.10, group = 1), color = "red") +
  geom_circle(aes(r = 1000, x0 = long.10, y0 = lat.10), color = "red") +
  geom_point(aes(x = long.11, y = lat.11, group = 1), color = "red") +
  geom_circle(aes(r = 1000, x0 = long.11, y0 = lat.11), color = "red") +
  geom_point(aes(x = long.12, y = lat.12, group = 1), color = "red") +
  geom_circle(aes(r = 1000, x0 = long.12, y0 = lat.12), color = "red")+
  geom_point(aes(x = long.13, y = lat.13, group = 1), color = "red") +
  geom_circle(aes(r = 1000, x0 = long.13, y0 = lat.13), color = "red")+
  geom_point(aes(x = long.14, y = lat.14, group = 1), color = "red") +
  geom_circle(aes(r = 1000, x0 = long.14, y0 = lat.14), color = "red") +
  geom_point(aes(x = long.15, y = lat.15, group = 1), color = "red") +
  geom_circle(aes(r = 1000, x0 = long.15, y0 = lat.15), color = "red")+
  geom_point(aes(x = long.16, y = lat.16, group = 1), color = "red") +
  geom_circle(aes(r = 1000, x0 = long.16, y0 = lat.16), color = "red")+
  geom_point(aes(x = long.17, y = lat.17, group = 1), color = "red") +
  geom_circle(aes(r = 1000, x0 = long.17, y0 = lat.17), color = "red")+
  geom_point(aes(x = long.18, y = lat.18, group = 1), color = "red") +
  geom_circle(aes(r = 1000, x0 = long.18, y0 = lat.18), color = "red")+
  geom_point(aes(x = long.19, y = lat.19, group = 1), color = "red") +
  geom_circle(aes(r = 1000, x0 = long.19, y0 = lat.19), color = "red")+
  geom_point(aes(x = long.20, y = lat.20, group = 1), color = "red") +
  geom_circle(aes(r = 1000, x0 = long.20, y0 = lat.20), color = "red")+
  geom_point(aes(x = long.21, y = lat.21, group = 1), color = "red") +
  geom_circle(aes(r = 1000, x0 = long.21, y0 = lat.21), color = "red")+
  geom_point(aes(x = long.22, y = lat.22, group = 1), color = "red") +
  geom_circle(aes(r = 1000, x0 = long.22, y0 = lat.22), color = "red")+
  geom_point(aes(x = long.23, y = lat.23, group = 1), color = "red") +
  geom_circle(aes(r = 1000, x0 = long.23, y0 = lat.23), color = "red")

# 춘천시 급속충전소 표시하기(2) - 충전기 대 전기차 비율 도심지역 1:4, 교외지역 1:12 못미치는 충전소 파란색 표시
ggplot() + geom_polygon(data = data_merge.group, aes(x = long, y = lat, group = group, fill = group2), color = "black", size = 0.5) +
  scale_fill_gradient(low = '#C8C8C8', high = '#009933') +
  theme(panel.background = element_rect(fill = 'white', colour = 'white'),
        axis.title.x = element_blank(),
        axis.text.x = element_blank(),
        axis.ticks.x = element_blank(),
        axis.title.y = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks.y = element_blank(),
        legend.position = "none") +
  geom_point(aes(x = long.1, y = lat.1, group = 1), color = "blue") +
  geom_circle(aes(r = 1000, x0 = long.1, y0 = lat.1), color = "blue") +
  geom_point(aes(x = long.2, y = lat.2, group = 1), color = "red") +
  geom_circle(aes(r = 1000, x0 = long.2, y0 = lat.2), color = "red") +
  geom_point(aes(x = long.3, y = lat.3, group = 1), color = "red") +
  geom_circle(aes(r = 1000, x0 = long.3, y0 = lat.3), color = "red") +
  geom_point(aes(x = long.4, y = lat.4, group = 1), color = "red") +
  geom_circle(aes(r = 1000, x0 = long.4, y0 = lat.4), color = "red") +
  geom_point(aes(x = long.5, y = lat.5, group = 1), color = "red") +
  geom_circle(aes(r = 1000, x0 = long.5, y0 = lat.5), color = "red") +
  geom_point(aes(x = long.6, y = lat.6, group = 1), color = "red") +
  geom_circle(aes(r = 1000, x0 = long.6, y0 = lat.6), color = "red") +
  geom_point(aes(x = long.7, y = lat.7, group = 1), color = "red") +
  geom_circle(aes(r = 1000, x0 = long.7, y0 = lat.7), color = "red") +
  geom_point(aes(x = long.8, y = lat.8, group = 1), color = "red") +
  geom_circle(aes(r = 1000, x0 = long.8, y0 = lat.8), color = "red") +
  geom_point(aes(x = long.9, y = lat.9, group = 1), color = "blue") +
  geom_circle(aes(r = 1000, x0 = long.9, y0 = lat.9), color = "blue") +
  geom_point(aes(x = long.10, y = lat.10, group = 1), color = "blue") +
  geom_circle(aes(r = 1000, x0 = long.10, y0 = lat.10), color = "blue") +
  geom_point(aes(x = long.11, y = lat.11, group = 1), color = "red") +
  geom_circle(aes(r = 1000, x0 = long.11, y0 = lat.11), color = "red") +
  geom_point(aes(x = long.12, y = lat.12, group = 1), color = "blue") +
  geom_circle(aes(r = 1000, x0 = long.12, y0 = lat.12), color = "blue")+
  geom_point(aes(x = long.13, y = lat.13, group = 1), color = "red") +
  geom_circle(aes(r = 1000, x0 = long.13, y0 = lat.13), color = "red")+
  geom_point(aes(x = long.14, y = lat.14, group = 1), color = "red") +
  geom_circle(aes(r = 1000, x0 = long.14, y0 = lat.14), color = "red") +
  geom_point(aes(x = long.15, y = lat.15, group = 1), color = "red") +
  geom_circle(aes(r = 1000, x0 = long.15, y0 = lat.15), color = "red")+
  geom_point(aes(x = long.16, y = lat.16, group = 1), color = "red") +
  geom_circle(aes(r = 1000, x0 = long.16, y0 = lat.16), color = "red")+
  geom_point(aes(x = long.17, y = lat.17, group = 1), color = "blue") +
  geom_circle(aes(r = 1000, x0 = long.17, y0 = lat.17), color = "blue")+
  geom_point(aes(x = long.18, y = lat.18, group = 1), color = "red") +
  geom_circle(aes(r = 1000, x0 = long.18, y0 = lat.18), color = "red")+
  geom_point(aes(x = long.19, y = lat.19, group = 1), color = "red") +
  geom_circle(aes(r = 1000, x0 = long.19, y0 = lat.19), color = "red")+
  geom_point(aes(x = long.20, y = lat.20, group = 1), color = "red") +
  geom_circle(aes(r = 1000, x0 = long.20, y0 = lat.20), color = "red")+
  geom_point(aes(x = long.21, y = lat.21, group = 1), color = "blue") +
  geom_circle(aes(r = 1000, x0 = long.21, y0 = lat.21), color = "blue")+
  geom_point(aes(x = long.22, y = lat.22, group = 1), color = "red") +
  geom_circle(aes(r = 1000, x0 = long.22, y0 = lat.22), color = "red")+
  geom_point(aes(x = long.23, y = lat.23, group = 1), color = "red") +
  geom_circle(aes(r = 1000, x0 = long.23, y0 = lat.23), color = "red")

# 춘천시 급속충전소 표시하기(3) - 충전기 대 전기차 비율 도심지역 1:4, 교외지역 1:12 못미치는 충전소 제거
ggplot() + geom_polygon(data = data_merge.group, aes(x = long, y = lat, group = group, fill = group2), color = "black", size = 0.5) +
  scale_fill_gradient(low = '#C8C8C8', high = '#009933') +
  theme(panel.background = element_rect(fill = 'white', colour = 'white'),
        axis.title.x = element_blank(),
        axis.text.x = element_blank(),
        axis.ticks.x = element_blank(),
        axis.title.y = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks.y = element_blank(),
        legend.position = "none") +
  geom_point(aes(x = long.2, y = lat.2, group = 1), color = "red") +
  geom_circle(aes(r = 1000, x0 = long.2, y0 = lat.2), color = "red") +
  geom_point(aes(x = long.3, y = lat.3, group = 1), color = "red") +
  geom_circle(aes(r = 1000, x0 = long.3, y0 = lat.3), color = "red") +
  geom_point(aes(x = long.4, y = lat.4, group = 1), color = "red") +
  geom_circle(aes(r = 1000, x0 = long.4, y0 = lat.4), color = "red") +
  geom_point(aes(x = long.5, y = lat.5, group = 1), color = "red") +
  geom_circle(aes(r = 1000, x0 = long.5, y0 = lat.5), color = "red") +
  geom_point(aes(x = long.6, y = lat.6, group = 1), color = "red") +
  geom_circle(aes(r = 1000, x0 = long.6, y0 = lat.6), color = "red") +
  geom_point(aes(x = long.7, y = lat.7, group = 1), color = "red") +
  geom_circle(aes(r = 1000, x0 = long.7, y0 = lat.7), color = "red") +
  geom_point(aes(x = long.8, y = lat.8, group = 1), color = "red") +
  geom_circle(aes(r = 1000, x0 = long.8, y0 = lat.8), color = "red") +
  geom_point(aes(x = long.11, y = lat.11, group = 1), color = "red") +
  geom_circle(aes(r = 1000, x0 = long.11, y0 = lat.11), color = "red") +
  geom_point(aes(x = long.13, y = lat.13, group = 1), color = "red") +
  geom_circle(aes(r = 1000, x0 = long.13, y0 = lat.13), color = "red")+
  geom_point(aes(x = long.14, y = lat.14, group = 1), color = "red") +
  geom_circle(aes(r = 1000, x0 = long.14, y0 = lat.14), color = "red") +
  geom_point(aes(x = long.15, y = lat.15, group = 1), color = "red") +
  geom_circle(aes(r = 1000, x0 = long.15, y0 = lat.15), color = "red")+
  geom_point(aes(x = long.16, y = lat.16, group = 1), color = "red") +
  geom_circle(aes(r = 1000, x0 = long.16, y0 = lat.16), color = "red")+
  geom_point(aes(x = long.18, y = lat.18, group = 1), color = "red") +
  geom_circle(aes(r = 1000, x0 = long.18, y0 = lat.18), color = "red")+
  geom_point(aes(x = long.19, y = lat.19, group = 1), color = "red") +
  geom_circle(aes(r = 1000, x0 = long.19, y0 = lat.19), color = "red")+
  geom_point(aes(x = long.20, y = lat.20, group = 1), color = "red") +
  geom_circle(aes(r = 1000, x0 = long.20, y0 = lat.20), color = "red")+
  geom_point(aes(x = long.22, y = lat.22, group = 1), color = "red") +
  geom_circle(aes(r = 1000, x0 = long.22, y0 = lat.22), color = "red")+
  geom_point(aes(x = long.23, y = lat.23, group = 1), color = "red") +
  geom_circle(aes(r = 1000, x0 = long.23, y0 = lat.23), color = "red")</code></pre>
  
- 결과물:
<p><img src="https://github.com/draxcel/chuncheon_EV_charging_station/blob/master/%EA%B5%B0%EC%A7%912_%ED%91%9C%EC%8B%9C_%EC%B5%9C%EC%A0%81.png?raw=true" alt="최종결과물" width="300" height="270"></p>
최종결과는 '동면, 동내면, 석사동, 퇴계동, 강남동, 신사우동' 6곳.
해당 읍면동의 공영주차장에 우선적으로 전기차 충전소를 설치하는 것을 권장함.
