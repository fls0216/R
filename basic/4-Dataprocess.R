#### 기술 통계량 ####
# min() max()
# range() : 벡터를 대상으로 범위값을 구하는 함수
# mean(), median()
# sum()
# order()
# rank()
# sd() : 표준편차, var()
# summary()
# quantile()
# table()
# sample(x,y) : x 범위에서 y만큼 샘플데이터 생성


# table()
aws = read.delim('AWS_sample.txt', sep = '#')
str(aws)
table(aws$AWS_ID)
table(aws$AWS_ID, aws$X.)

table(aws[,c("AWS_ID", 'X.')])

aws[2500:3100, 'X.'] = 'modified'
table(aws$AWS_ID, aws$X.)

# 비율 값으로 반환
prop.table(table(aws$AWS_ID))

# 비율 퍼센트 값으로 반환
prop.table(table(aws$AWS_ID)) * 100
paste0(prop.table(table(aws$AWS_ID)) * 100, '%')

# 기술통계함수의 모듈화
test <- read.csv('test.csv', header = T)
head(test)

data_proc <- function(df){
  # 각 컬럼 단위로 빈도와 최대값/ 최소값 계산
  for (idx in 1:length(df)){
    cat(idx, '번째 컬럼의 빈도 분석 결과')
    print(table(df[idx]))
    cat('\n')
  }
  
  for(idx in 1:length(df)){
    f <- table(df[idx])
    cat(idx, '번째 컬럼의 최대값/ 최소값 결과 : \t')
    cat('max = ',max(f), 'min = ', min(f), '\n')
  }
}

data_proc(test)


#### plyr, dplyr ####
#plyr
install.packages('plyr')
library(plyr)

# 데이터 병합
x <- data.frame(id=c(1,2,3,4,5,6), height = c(160,171,173,162,165,170))
y <- data.frame(id=c(1,2,3,4,5,6), weight = c(55,73,60,57,80,91))

xy <- join(x, y,by='id', type = 'left')
xy
xy <- join(x, y, by ='id', type='right')
xy
xy<- join(x, y, by='id', type='full')
xy
xy<- join(x, y, by='id', type='inner')
xy

# 다중키의 경우
x<- data.frame(key1=c(1,1,2,2,3), key2=c('a','b','c','d','e'), val = c(10,20,30,40,50))

y<- data.frame(key1=c(3,2,2,1,1), key2=c('e', 'd', 'c', 'b', 'a'), val=c(500,400,300,200,100))


xy <- join(x, y, by=c('key1', 'key2'))
xy


# 기술 통계량 : ddply(), tapply()
# tapply() : 집단 변수를 대상으로 한번에 하나의 통계치를 구할 때 사용
# ddply() : 한번에 여려 개의 통계치를 구할 때 사용

head(iris)
unique(iris$Species)

tapply(iris$Sepal.Length, iris$Species, mean)
tapply(iris$Sepal.Length, iris$Species, sd)

ddply(iris, .(Species), summarise, avg=mean(Sepal.Length))
ddply(iris, .(Species), summarise, avg=mean(Sepal.Length), std=sd(Sepal.Length), max=max(Sepal.Length), min=min(Sepal.Length))


# dplyr
install.packages('dplyr')
library(dplyr)
?package=dplyr

#filter() : 행 추출 -> subset()
#select() : 열 추출 -> data[,c()]
#arrange() : 정렬 -> order(), sort()
#mutate() : 열 추가 -> transform()
#summarize() : 통계치 산출 -> aggregate()
#groupby() : 집단별로 나누기 -> subset(), tapply(), aggregate()
#left_join() : 데이터 합치기(열) -> cbind()
#bind_rows() : 데이터 합치기(행) -> rbind()


exam <- read.csv('csv_exam.csv')
exam

# filter()

# 1반 학생들의 데이터 추출
exam[exam$class==1,]
filter(exam, class == 1)
exam %>% filter(class==1)

# 2반 이면서 영어점수가 80점 이상인 데이터 추출
exam[(exam$class==2 & exam$english >=80),]
filter(exam, class==2, english >=80)
exam %>% filter(class==2, english >=80)

# 1반 3반 5반에 해당하는 데이터 추출
exam %>% filter(class==1 | class==3 | class ==5)
exam %>% filter(class %in% c(1,3,5))

#select()
# 수학점수만 추출
exam[, 3]
exam %>% select(math)

# 반 수학점수 영어점수 추출
exam %>% select(class, math, english)

# 수학점수를 제외한 나머지 컬럼 추출
exam %>% select(-math)

# 1반 학생들의 수학점수 추출
exam %>% filter(class==1) %>% select(math) %>% head(2)


# arrange()
exam %>% arrange(math) # 오름차순
exam %>% arrange(desc(math)) # 내림차순
exam %>% arrange(class, math) # 2차정렬

#mutate()
exam$sum <- exam$math + exam$english + exam$science
head(exam)

exam <-  exam %>% mutate(total = math+english+science, mean=(math+english+science)/3)
head(exam)

#summarize()
exam %>% summarise(mean_math = mean(math))

# groupby
exam %>% group_by(class) %>% summarise(mean_math = mean(math), sum_math = sum(math), median_math = median(math), n=n())

#left_join()
test1 <- data.frame(id=c(1,2,3,4,5), midterm=c(60,70,80,90,85))
test2 <- data.frame(id=c(1,2,3,4,5), midterm=c(70,83,65,95,80))

total <- left_join(test1, test2, by = 'id')
total

test3 <- data.frame(class=c(1,2,3,4,5), teacher = c('kim','lee','park','choi','shin'))
exam_new <- left_join(exam, test3, by = 'class')
head(exam_new)

# bind_rows()
group1 <- data.frame(id=c(1,2,3,4,5), test=c(60,70,80,90,85))
group2 <- data.frame(id=c(1,2,3,4,5), test=c(70,83,65,95,80))

group_all <- bind_rows(group1, group2)
group_all

# ggplot2
install.packages('ggplot2')
library(ggplot2)

str(ggplot2::mpg)
str(ggplot2::mpg, 10)
class(ggplot2::mpg)

mpg <- as.data.frame(ggplot2::mpg)
class(mpg)

head(mpg)
tail(mpg)
names(mpg)
dim(mpg)
str(mpg)
View(mpg)


# 배기량(displ)이 4 이하인 차량의 모델명, 배리걍, 생산년도 조회

mpg_a <- mpg %>% filter(displ <=4) %>% select(model, displ, year)
mpg_a

# 통합 연비 파생 변수(total)를 만들고 통합 연비로 내림차순 정렬을 한 후에 3 개의 행만 선택해서 출력

mpg %>% mutate(total = (cty + hwy)/2) %>% arrange(desc(total)) %>% head(3)

# 회사별로 'suv' 차량의 도시 및 고속도로 통합연비 평균을 구해 내림차순으로 정렬하고 1위에서 5위까지 조회
head(mpg)
mpg %>% group_by(manufacturer) %>% filter(class == "suv") %>% mutate(total = (cty + hwy)/2) %>% summarise(mean_total = mean(total)) %>%
  arrange(desc(mean_total)) %>% head(5)

# 어떤 회사에 고속 도로 연비가 가장 높은지 알아보려고 한다. hwy 평균이 가장 높은 회사 세곳 조회
mpg %>% group_by(manufacturer) %>% summarise(hwy_mean = mean(hwy)) %>% arrange(desc(hwy_mean))  %>%head(3)

# 어떤 회사에서 compact(경차) 차종을 많이 생산하는지 알아보려고 한다. 각 회사별 경차 차종 수를 내림차순으로 조회                                                         
mpg %>% filter(class == 'compact') %>% group_by(manufacturer) %>% summarise(count=n()) %>% arrange(desc(count))

# 연료별 가격을 구해서 새로운 데이터프레임(fuel)으로 만든 후에 기존 데이터셋과 병합하여 출력
# c : cong = 2.35, d:Disel = 2.38, e:Ethanol = 2.11, p: premium=2.76, r: Regular = 2.22
unique(mpg$fl)
fuel <- data.frame(fl=c('c','d','e','p','r'), fuel=c(2.35,2.38,2.11,2.76,2.22))
fuel
mpg_ful <- left_join(mpg, fuel, by = 'fl')
mpg_ful

# 통합 연비의 기준치를 통해 합격과 불합격을 부여하는 test라는 이름의 파생변수 생성, 이때 기준은 20으로 한다.
mpg <- mutate(mpg, total = (cty + hwy)/2)
mpg$test <- ifelse(mpg$total >=20, 'pass', 'fail')
head(mpg)

# test에 대해 합격과 불합격을 받은 자동차가 각각 몇대인가
table(mpg$test)

# 통합 연비 등급을 ABC 세개의 등급으로 나누는 파생변수 추가(grade)
# 30이상 A, 20이상 B, 20미만 C
mpg$grade <- ifelse(mpg$total >=30, 'A', ifelse(mpg$total >=20, 'B', 'C'))
table(mpg$grade)
head(mpg)


# 미국 437개주 지역의 인구 통계 정보
midwest <- as.data.frame(ggplot2::midwest)
head(midwest)
names(midwest)
str(midwest)

# 전체 인구 대비 미성년 인구 백분율(ratio_child) 변수를 추가
midwest <- midwest %>% mutate(ratio_child = (poptotal-popadults)/poptotal *100)
head(midwest)

# 미성년 인구 백분율이 가장 높은 상위 5개 지역의 미성년 인구 백분율을 출력..
midwest %>% group_by('state') %>% select('state', 'ratio_child') %>%  arrange(desc(ratio_child)) %>% head(5)


# 분류표의 기준에 따라 미성년 비율 등급 변수(grade)를 추가하고 각 등급의 몇개의 지역이 있는지 조회
# 40 이상 large, 30이상 middle, 그렇지 않으면 small
midwest$grade <- ifelse(midwest$ratio_child >=40, 'large', ifelse(midwest$ratio_child >=30, 'middle', 'small'))
table(midwest$grade)

# 전체 인구 대비 아시아인 인구 백분율(ratio_asian) 변수를 추가하고 하위 10개 지역의 state, country, 아시아인 인구 백분률 출력
midwest <- midwest %>% mutate(ratio_asian = popasian/poptotal*100)
head(midwest)
midwest  %>% select('state', 'county', 'ratio_asian') %>% group_by('state') %>% arrange(desc('ratio_asian')) %>% head(10)


#### Data Preprocessing ####
# 순서 :  데이터 탐색 > 결측치 처리 > 이상치 처리 > feature Engineering

# 데이터 탐색
#   1) 변수 확인
#   2) 변수 유형(범주형, 연속형, 문자형, 숫자형, ....)
#   3) 변수의 통계량 : 평균, 최빈값, 중간값, 분포, ....
#   4) 관계, 차이 검정


# 결측치 처리
#   1) 삭제
#   2) 다른값으로 대체(평균, 최빈값, 중간값)
#   3) 예측값 :  선형회귀분석, 로지스틱 회귀분석

# 이상치 처리
#   1) 이상치 탐색 
#     - 시각적 확인 : 산포도, 박스플롯
#     - 통계적 확인 : 표준 편차, leverage, cook's D
#   2) 처리 방법
#     - 삭제
#     - 다른값으로 대체
#     - 리샘플링(케이스별로 분리해서)

#Feature Engineering
#   1) Scaling : 단위 변경
#   2) Binning : 연속형 변수를 범주형 변수로 변환
#   3) Transform : 기존 존재하는 변수의 성질을 이용해 다른 변수를 만드는 방법
#   4) Dummy : 변수형 범주를 연속형 변수로 변환

# 변수명 바꾸기

# 내장함수
df_raw <- data.frame(var1 = c(1,2,3), var2=c(2,3,2))
df_raw
df_new1 <- df_raw
names(df_new1) <- c('v1', 'v2')
df_new1


# 패키지(dplyr)
df_new2 <- df_raw
df_new2 <- rename(df_new2, v1=var1, v2=var2)
df_new2

### 결측치 처리
dataset1 <- read.csv('dataset.csv', header = T)
head(dataset1)
View(dataset1)
str(dataset1)

# resident : 1~5까지 값을 갖는 명목변수
# gender : 1~2까지
# job : 1~3 까지의 값을 갖는 명목변수, 직업을 나타냄
# position : 1~5까지의 값을 갖는 명목변수, 직위를 나타냄
# price : 양적변수(비율) : 2.1~7.9
# survey : 만족도 조사 : 1~5 까지 명목변수

y <- dataset1$price
plot(y)

# 따로 호출하지 않아도 사용 가능하도록 함
attach(dataset1)
price

# attach 제거
detach(dataset1)
price

# 결측치 확인
summary(dataset1$price)

# 결측치 제거
sum(dataset1$price)
# 결측치 빼고 계산
sum(dataset1$price, na.rm=T)
# 결측치가 포함되어있는 행을 전체 삭제
dataset1$price2 = na.omit(dataset1$price)
summary(dataset1$price2)
sum(dataset1$price2)

# 결측치 대체
# 0으로 대체
dataset1$price2 <- ifelse(!is.na(dataset1$price), dataset1$price, 0)
sum(price2)

# 평균으로 대체
dataset1$price2 <- ifelse(!is.na(dataset1$price), dataset1$price, round(mean(dataset1$price, na.rm = T), 2))
sum(dataset1$price2)


### 이상치 처리
# 질적자료
table(dataset1$gender)
pie(table(dataset1$gender))

# 양적자료
summary(dataset1$price)
length(dataset1$price)
plot(dataset1$price)
boxplot(dataset1$price)
dataset2 <- subset(dataset1, price >=2 & price <=8)
length(dataset2)


boxplot(dataset2$price)

summary(dataset2$age)
plot(dataset2$age)
boxplot(dataset2$age)




#### Feature Engineering ####
View(dataset2)

# 가독성을 위한(기술통계) 데이터 변경
dataset2$resident2[dataset2$resident ==1] <- '서울특별시'
dataset2$resident2[dataset2$resident ==2] <- '인천광역시'
dataset2$resident2[dataset2$resident ==3] <- '대전광역시'
dataset2$resident2[dataset2$resident ==4] <- '대구광역시'
dataset2$resident2[dataset2$resident ==5] <-' 시구군'

View(dataset2)


summary(dataset2)

# 척도 변경 : Binning
# 나이 변수를 청년층(30세 이하), 중년층(31-55 이하), 장년층(56-)
dataset2$age2[dataset2$age <=30] <- '청년층'
dataset2$age2[dataset2$age>30 &dataset2$age <=55] <- '중년층'
dataset2$age2[dataset2$age >=56] <- '장년층'

View(dataset2)

# 역코딩
table(dataset2$survey)
survay <- dataset2$survey
survay
csurvay <- 6-survay
dataset2$survey <- csurvay
head(dataset2$survey)


#Dummy
# 거주유형 : 단독주택 1, 다가구 주택 2, 아파트 3, 오피스텔 4
# 직업유형 : 자영업 1, 사무직 2, 서비스 3, 전문직 4, 기타
user_data <- read.csv('user_data.csv', header = T)
View(user_data)

table(user_data$house_type)


user_data$house_type_two <- ifelse(user_data$house_type==1 | user_data$house_type==2,0,1)
View(user_data)

# 데이터의 구조 변경(wide type, long type) : melt(long으로 변경), cast(wide 타입으로 변경해줌)
# reshape, reshape2, tidyr,....
install.packages('reshape2')
library(reshape2)

str(airquality)
head(airquality)

ml <- melt(airquality, id.var = c('Month', 'Day'))
head(ml)
View(ml)

m2 <- melt(airquality, id.vars=c('Month', 'Day'), variable.name ='climate_var', value.name = 'climate_val')
View(m2)

?dcast

dc1<- dcast(m2, Month + Day ~ climate_var)

head(dc1)

data<- read.csv('data.csv')
data

wide <- dcast(data, Customer_ID ~ Date)
View(wide)

wide1 <- dcast(data, Customer_ID ~ Date, mean)
wide1

long <- melt(wide, id.vars = 'Customer_ID')
long

pay_data <- read.csv('pay_data.csv')
head(pay_data)

# product type을 wide 하게
wide <- dcast(pay_data, user_id ~ product_type)
wide
View(wide)


### MySQL 연동
install.packages('rJava')
install.packages('DBI')
install.packages('RMySQL')

library(RMySQL)

#create database rtest
# create table score (
#   student_no varchar(50) primary key,
#   kor int default 0,
#   eng int default 0,
#   mat int default 0
# );
# insert into score(student_no, kor, eng, mat) values('1', 90, 80, 70);
# insert into score(student_no, kor, eng, mat) values('2', 90, 88, 70);
# insert into score(student_no, kor, eng, mat) values('3', 90, 89, 70);
# insert into score(student_no, kor, eng, mat) values('4', 90, 87, 70);
# insert into score(student_no, kor, eng, mat) values('5', 90, 60, 70);

# 세션 접속
conn <- dbConnect(MySQL(), dbname='rtest', user='root', password='1111', host='127.0.1.1')
conn
dbListTables(conn)

# 몇개의 데이터가 있는가
result <- dbGetQuery(conn, 'select count(*) from score')
result

# 테이블에 있는 전체 데이터 출력
result <- dbGetQuery(conn, 'select * from score')
result

# 데이터 테이블 필드명 조회
dbListFields(conn, 'score')

# DML
dbSendQuery(conn, 'delete from score')
result <- dbGetQuery(conn, 'select count(*) from score')
result

# 데이터를 파일로부터 읽어들여 DB에 저장
file_score <- read.csv('score.csv', header = T)
file_score

dbSendQuery(conn, 'drop table score')
dbListTables(conn)

dbWriteTable(conn, 'score', file_score, row.names =F)

result <- dbGetQuery(conn, 'select count(*) from score')
result

# 세션 종료!
dbDisconnect(conn)

### sqldf : R + SQL

install.packages('sqldf')
library(sqldf)

# 이전에 사용했던 DB 연결 완전히 끊기기
detach('package:RMySQL', unload=T)

head(iris)
sqldf('select * from iris limit 6')
sqldf('select * from iris order by Species desc limit 10')
sqldf('select sum("Sepal.Length") from iris')

unique(iris$Species)

sqldf('select distinct Species from iris')

table(iris$Species)

sqldf('select Species, count(*) from iris group by Species')
