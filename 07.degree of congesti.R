#실습에필요한packages를라이브러리에등록
library(dplyr)
library(ggplot2)

#CSV형식의파일불러와서subway객체에입력하고구조확인
str(congestion)

#변수의이상치와결측기확인하고처리
summary(congestion)

#결측치개수확인
is.na(congestion)
sum(is.na(congestion))
colSums(is.na(congestion))

#결측치가있는행을제거한새로운데이터프레임생성
#6시/23시30분출발기차의결측치를제거
congestion1<-congestion[!is.na(congestion$s0600),]
colSums(is.na(congestion1))
congestion1<-congestion[!is.na(congestion$s2330),]
colSums(is.na(congestion1))
congestion1[is.na(congestion1)]<-0
colSums(is.na(congestion1))

#이상치확인
ggplot(congestion1,aes(y=s0530))+
  geom_boxplot()
summary(congestion1$s0530)

#1.지하철역의하루평균혼잡도
congestion1$day_mean <-rowMeans(congestion1[,c('s0530','s0600','s0630','s0700','s0730','s0800','s0830','s0900','s0930','s1000','s1030','s1100','s1130','s1200','s1230','s1300','s1330','s1400','s1430','s1500','s1530','s1600','s1630','s1700','s1730','s1800','s1830','s1900','s1930','s2000','s2030','s2100','s2130','s2200','s2230','s2300','s2330')])
summary(congestion1$day_mean)

#2.지하철호선별하루평균혼잡도
congestion1%>%
  group_by(line)%>%
  summarise(avg=mean(day_mean))%>%
  head(9)

#3.지하철호선별출근시간(7~9)대의평균혼잡도
congestion1 %>%
  group_by(line) %>%
  summarise(avg_s0700 = mean(s0700),
            avg_s0730 = mean(s0730),
            avg_s0800 = mean(s0800),
            avg_s0830 = mean(s0830),
            avg_s0900 = mean(s0900))
result <- congestion1 %>%
  group_by(line) %>%
  summarise(avg_s0700 = mean(s0700),
            avg_s0730 = mean(s0730),
            avg_s0800 = mean(s0800),
            avg_s0830 = mean(s0830),
            avg_s0900 = mean(s0900))
summary(result)
#3-2
congestion_summary <- congestion1 %>%
  group_by(line) %>%
  summarise(
    avg_s0700 = mean(s0700),
    avg_s0730 = mean(s0730),
    avg_s0800 = mean(s0800),
    avg_s0830 = mean(s0830),
    avg_s0900 = mean(s0900)
  )

congestion_max_time <- congestion_summary %>%
  gather(key = "time", value = "avg_congestion", -line) %>%
  group_by(line) %>%
  slice_max(order_by = avg_congestion)

max_congestion_time <- congestion_max_time$time[1]

ggplot(congestion_summary, aes(x = line, y = !!sym(max_congestion_time))) +
  geom_bar(stat = "identity", fill = "blue") +
  labs(
    title = paste("가장 높은 혼잡도 시간대 (", max_congestion_time, ")의 평균 혼잡도"),
    x = "지하철 호선",
    y = "평균 혼잡도"
  )
#3-3
# 주어진 데이터
data <- data.frame(
  line = c(1, 2, 3, 4, 5, 6, 7, 8),
  avg_s0700 = c(13.9, 19.1, 16.2, 18.5, 18.7, 13.6, 26.0, 18.3),
  avg_s0730 = c(20.7, 25.7, 21.4, 24.2, 24.9, 17.8, 33.0, 23.2),
  avg_s0800 = c(25.8, 36.3, 29.6, 33.8, 31.2, 23.9, 39.4, 31.3),
  avg_s0830 = c(26.1, 35.0, 28.9, 31.7, 27.1, 22.3, 35.9, 30.8),
  avg_s0900 = c(23.9, 32.4, 27.9, 32.8, 25.9, 21.9, 37.2, 29.8)
)

# 각 호선별로 평균 혼잡도를 모든 시간대에 대해 합산
data <- data %>%
  mutate(
    total_congestion = avg_s0700 + avg_s0730 + avg_s0800 + avg_s0830 + avg_s0900
  )

# 평균 혼잡도를 기준으로 정렬하여 상위 4개의 호선 선택
top_lines <- data %>%
  arrange(desc(total_congestion)) %>%
  head(4)

# 결과 출력
top_lines
# 상위 4개 호선 선택 
top_lines <- congestion1 %>%
  group_by(line) %>%
  summarise(avg = mean(day_mean)) %>%
  arrange(desc(avg)) %>%
  head(4) 
# 선택된 상위 4개 호선의 역별 기여도 계산 
station_contributions <- congestion1 %>%  
  filter(line %in% top_lines$line) %>%  
  group_by(line, station) %>%  
  summarise(total_contrib = sum(day_mean)) %>%  
  arrange(desc(total_contrib)) 
# 그래프 그리기 
ggplot(data = station_contributions, aes(x = reorder(station, -total_contrib), y = total_contrib, fill = line)) +  geom_bar(stat = "identity") +  labs(title = "상위 4개 호선의 역별 기여도", x = "지하철 역", y = "기여도") +  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# 각 호선별로 평균 혼잡도가 가장 높은 시간대를 찾아내기
top_times <- data %>%
  rowwise() %>%
  mutate(
    top_time = which.max(c(avg_s0700, avg_s0730, avg_s0800, avg_s0830, avg_s0900))
  )

# 상위 4개의 호선 찾기
top_lines <- top_times %>%
  arrange(desc(top_time)) %>%
  head(4) %>%
  select(-top_time)

# 결과 출력
top_lines

#출발시간18시의지하철혼잡도범주화/범주별빈도분석
congestion1 %>%
  mutate(s18_grade=ifelse(s1800<=80, "good", ifelse(s1800<=130,"normal", ifelse(s1800<=150, "caution", "bad"))))%>%
  group_by(s18_grade) %>%
  summarise(n=n())%>%
  mutate(total=sum(n), pct=round(n/total*100,1))%>%
  select(s18_grade,n,pct)%>%
  arrange(desc(n))

congestion1 %>%
  mutate(s18_grade=ifelse(s1800<=80, "good", ifelse(s1800<=130, "normal", ifelse(s1800<=150, "caution", "bad"))))%>%
  group_by(line, s18_grade) %>%
  summarise(n=n())%>%
  mutate(total=sum(n), pct=round(n/total*100,1))%>%
  filter(s18_grade=="bad")%>%
  select(line, s18_grade,n,pct)%>%
  arrange(desc(pct))%>%
  head(5)

congestion1 %>%
  group_by(line) %>%
  summarise(avg_s1800 = mean(s1800),
            avg_s1830 = mean(s1830),
            avg_s1900 = mean(s1900),
            avg_s1930 = mean(s1930),
            avg_s2000 = mean(s2000))
result <- congestion1 %>%
  group_by(line) %>%
  summarise(avg_s1800 = mean(s1800),
            avg_s1830 = mean(s1830),
            avg_s1900 = mean(s1900),
            avg_s1930 = mean(s1930),
            avg_s2000 = mean(s2000))
summary(result)

congestion_summary <- congestion1 %>%
  group_by(line) %>%
  summarise(
    avg_s1800 = mean(s1800),
    avg_s1830 = mean(s1830),
    avg_s1900 = mean(s1900),
    avg_s1930 = mean(s1930),
    avg_s2000 = mean(s2000)
  )

congestion_max_time <- congestion_summary %>%
  gather(key = "time", value = "avg_congestion", -line) %>%
  group_by(line) %>%
  slice_max(order_by = avg_congestion)

max_congestion_time <- congestion_max_time$time[1]

ggplot(congestion_summary, aes(x = line, y = !!sym(max_congestion_time))) +
  geom_bar(stat = "identity", fill = "blue") +
  labs(
    title = paste("가장 높은 혼잡도 시간대 (", max_congestion_time, ")의 평균 혼잡도"),
    x = "지하철 호선",
    y = "평균 혼잡도"
  )
congestion1$day_night<-rowMeans(congestion1[,c('s1800','s1830','s1900','s1930','s2000')])

top_lines <- congestion1 %>%
  group_by(line) %>%
  summarise(avg = mean(day_night)) %>%
  arrange(desc(avg)) %>%
  head(4) 
top_lines

station_contributions_호선 <- congestion1 %>%
  filter(line == "7" | line == "2" | line == "4" | line == "8") %>%
  group_by(line, station) %>%
  summarise(total_contrib = sum(day_work)) %>%
  arrange(desc(total_contrib))
station_contributions_호선

# 퇴근 시간대의 평균 혼잡도 계산 
congestion1$day_night <- rowMeans(congestion1[,c('s1800','s1830','s1900','s1930','s2000')]) 
# 상위 4개 호선 선택 
top_lines <- congestion1 %>%  group_by(line) %>%
  summarise(avg = mean(day_night)) %>%
  arrange(desc(avg)) %>%
  head(4) 
top_lines 
# 선택된 상위 4개 호선의 역별 기여도 계산 
# 라인이 2이거나 7이거나 4이거나 3인 역의 역별 기여도 
station_contributions_night_호선 <- congestion1 %>%
  filter(line == "2" | line == "7" | line == "4" | line == "3") %>%
  group_by(line, station) %>%
  summarise(total_contrib = sum(day_night)) %>%
  arrange(desc(total_contrib)) 
station_contributions_night_호선