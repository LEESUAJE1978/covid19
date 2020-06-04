require(dplyr) #A fast, consistent tool for working with data frame like objects, both in memory and out of memory.
require(psych) #A general purpose toolbox for personality, psychometric theory and experimental psychology
require(ggplot2) #ggplot2 is a system for declaratively creating graphics, based on The Grammar of Graphics.
if(!require(viridis)){install.packages("viridis") #The viridis scales provide colour maps that are perceptually uniform in both colour and black-and-white.
  require(viridis)}

"### 2) Patient Data\n"
"- **PatientInfo**: Epidemiological data of COVID-19 patients in South Korea\n"
"- **PatientRoute**: Route data of COVID-19 patients in South Korea\n"

#1. 데이터 불러오기 ####

#1.1. 환자 정보####
patient_info <- readr::read_csv("C:/Users/tkpeo/Documents/covid19/dataset/Patient/Patientinfo.csv")
str(patient_info)
head(patient_info)

#1.2. 환자 이동 경로####
patient_route <- readr::read_csv("C:/Users/tkpeo/Documents/covid19/dataset/Patient/PatientRoute.csv")
str(patient_route)
head(patient_route)


#1.3. Time Series####
time_series <- readr::read_csv("C:/Users/tkpeo/Documents/covid19/dataset/Time/Time.csv")
str(time_series)
head(time_series)



#2. 범주형 데이터 EDA####
#분류: 성별, 출생연도별, 나이별, 국가별, 지역별, 도시별, 질병 유무별, 감염경로, 전달자
#결과: 접촉자수, 잠복기(confirmed date - symtom_onset_date ), 완치기간(released_date), 사망여부(deceased_date)
#데이터 변환: 잠복기, 완치기, 사망여부
patient_info$incubation <- patient_info$confirmed_date - patient_info$symptom_onset_date
patient_info$cure <- patient_info$released_date - patient_info$confirmed_date
patient_info$death <- ifelse(is.na(patient_info$deceased_date) == T , "recovery", "Death")

names(patient_info)

#Q)성별에 따른 접촉자수, 잠복기간, 완치기간, 사망여부 궁금####
#2.1. 성별 기준 데이터 셋 생성
patient_sex <- patient_info %>% select(3, 13, 19, 20, 21)

#2.2 결측 치 확인
sum(is.na(patient_sex)) #7792patient_sex$incubation
colSums(is.na(patient_sex)) #컬럼별 결측치 확인, sex:77, contate_number:2745, incubation:2909, cure:2061
#결측치 처리 방법, 성별 데이터 누락은 제거, 나머지 컬럼의 NA는 0으로 대체

patient_sex <- patient_sex %>% filter(!is.na(sex))
sum(is.na(patient_sex$sex))
patient_sex$contact_number[is.na(patient_sex$contact_number)] <-0
patient_sex$incubation[is.na(patient_sex$incubation)] <-0
patient_sex$cure[is.na(patient_sex$cure)] <-0
sum(is.na(patient_sex))

#2.3 형변환
patient_sex$incubation<- as.integer(patient_sex$incubation)
patient_sex$cure <-as.integer(patient_sex$cure)

#2.4 기술통계 확인
# https://rfriend.tistory.com/125
describe(patient_sex[,2:4])
summary(patient_sex[,2:4])
describeBy(patient_sex[,2:4], patient_sex$sex, mat = FALSE)

#2.5 시각화

# https://www.r-graph-gallery.com/boxplot.html
#https://ggplot2.tidyverse.org/reference/scale_viridis.html
a <- ggplot(patient_sex, aes(x = sex, y = cure,fill=sex))
a + geom_boxplot() +
  scale_fill_viridis_d(alpha = 0.6)+
  geom_jitter(color = "black", size = 0.4, alpha=0.9)+
  theme(
    legend.position="none",
    plot.title = element_text(size=11)
  ) +
  ggtitle("Cure Duration") +
  xlab("sex")


#3.경로 데이터 EDA####
#Q) 3.1환자들의 감염경로는 어떨까?, 지역감염, 해외감염, 접촉감염####

names(patient_info)
names(patient_route)
options(scipen = 100)#숫자 전체 표현

#3.1.감염 경로 데이터만 선별하여 데이터 셋 만들기####
route<-patient_info %>% select('patient_id',"infection_case","infection_order","infected_by")
route

#3.2.감염 경로별 발생 횟수####
route %>% group_by(infection_case) %>% 
  summarise(n = n()) %>% 
  arrange(desc(n))

names(patient_info)

#3.3. 슈퍼전파자 수####
route %>% group_by(infected_by) %>% 
  summarise(n = n()) %>% 
  arrange(desc(n))

p1 <- patient_route %>% filter(patient_id =="1000000125")


#3.3.감염자 경로 시각화####
# https://mrkevinna.github.io/R-%EC%8B%9C%EA%B0%81%ED%99%94-3/
if(!require(ggmap)){
  devtools::install_github("dkahle/ggmap", force = T);require(ggmap)}

register_google(key = 'AIzaSyBtWySLrkGxKB-cKBb4ZnPhKvoyBdobTC8')#구글 맵 API 연동
map <- get_map(location = "서울", zoom = 11, maptype = 'roadmap')
geocode("서울",output ="latlon", source ='google')
ggmap(map)+geom_point(data=patient_route,
                          aes(x=longitude,
                              y=latitude),
                          alpha=0.3,
                          color="red")
#3.4. 슈퍼 전파자 경로 시각화 ####

map_s <- get_map(location = c(lon = 127.058, lat =37.63), zoom = 11, maptype = 'hybrid')


ggmap(map_s)+geom_point(data=p1,
                      aes(x=longitude,
                          y=latitude),
                      alpha=0.7,
                      color="red")


#https://m.blog.naver.com/PostView.nhn?blogId=lool2389&logNo=220822220516&proxyReferer=https:%2F%2Fwww.google.com%2F



#4. 시계열 데이터 EDA시간의 흐름에 따른 데이터의 변화를 파악####
#test 환자수, 음성 환자수 변화, 확진사수 변화, 완치자수 변화, 사망자수 변화
str(time_series)

#4.1. test 환자수 trend ####
c1 <- ggplot(data = time_series , aes(x = date, y = test))+
  geom_line()+
  geom_point(size =1)+
  ggtitle("Covid19_test")

c1
#4.2. negative 환자수 trend ####
c2 <- ggplot(data = time_series , aes(x = date, y = negative))+
  geom_line()+
  geom_point(size =1)+
  ggtitle("Covid19_negative")

c2
#4.3. confirmed 환자수 trend ####
c3 <-ggplot(data = time_series , aes(x = date, y = confirmed))+
  geom_line()+
  geom_point(size =1)+
  ggtitle("Covid19_confirmed")

c3
#4.4. realeased 환자수 trend ####
c4 <-ggplot(data = time_series , aes(x = date, y = released))+
  geom_line()+
  geom_point(size =1)+
  ggtitle("Covid19_released")

c4

#4.5 deceased 환자수 trend####
c5 <- ggplot(data = time_series , aes(x = date, y = deceased))+
  geom_line()+
  geom_point(size =1)+
  ggtitle("Covid19_deceased")

c5

#4.5. y 축 값(분석 대상 값) 한번에 보기
if(!require(patchwork)){install.packages("patchwork")
  require(patchwork)}

if(!require(hrbrthemes)){install.packages("hrbrthemes")
  require(hrbrthemes)}

c1+c2+c3+c4+c5
