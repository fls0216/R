library(ggplot2)
library(dplyr)
head(mpg)
mpg <- ggplot2::mpg
# 배기량이 4 이하인 차량의 모델명, 배기량, 생산년도 조회
mpg_a <-mpg %>% filter(displ <= 4) %>% select(model, displ, year)

# 통합 연비 파생 변수(total)를 만들고 통합 연비로 내림차순 정렬을 한 후에 3개의 행만 선택하여 추출
mpg %>% mutate(total = (cty+hwy)/2) %>% arrange(desc(total)) %>% head(3)

# 회사별로 'suv' 차랴의 도시 및 고속도로 통합연비 평균을 구해 내림차순으로 정렬하고 1위에서 5위까지 조회
 mpg %>% group_by(manufacturer) %>% filter(class == 'suv') %>% mutate(total = (cty+hwy)/2) %>% summarise(mean_total=mean(total)) %>% arrange(desc(mean_total)) %>% head(5)
 
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
mpg<- mutate(mpg, total=(cty+hwy)/2)
mpg$test <- ifelse(mpg$total >= 20, 'pass', 'fail') 

# test에 대해 합격과 불합격을 받은 자동차가 각각 몇대인가
table(mpg$test)

# 통합 연비 등급을 !BC 세개의 등급으로 나누는 파생변수 추가(grade)
# 30이상 A, 20이상 B, 20미만 C
mpg$grade <- ifelse(mpg$total >=30, 'A', ifelse(mpg$total >= 20, 'B','C'))

# 어떤 회사에서 생산한 suv 차종의 도시 연비가 높은지 알아보려한다. suv 차종을 대상으로 평균 cty 가 가장 높은 회사 5곳 막대그래프로 출력
df_mpg <- mpg %>% group_by(class) %>% summarise(mean_cty = mean(cty)) %>% head(5)
ggplot(df_mpg, aes(reorder(class, -mean_cty), mean_cty)) + geom_col()


midwest <- ggplot2::midwest
# 전체 인구 대비 미성년 인구 백분율(ratio child) 변수 추가
midwest<- midwest %>% mutate(ratio_child = (poptotal-popadults)/poptotal*100)
head(midwest)

# 미성년 인구 백분율이 가장 높은 상위 5개 지역의 미성년 인구 백분율 출력
midwest %>% group_by('state') %>% select('state', 'ratio_child') %>% arrange(desc(ratio_child)) %>% head(5)

# 분류표의 기준에 따라 미성년 비율 등급 변수(grade)를 추가하고 각 등급에 몇개의 지역이 있는지 조회
# 40 이상 large, 30 이상 middle, 그렇지 않으면 small
midwest$grade <- ifelse(midwest$ratio_child >=40, 'large', ifelse(midwest$ratio_child >=30, 'middle', 'small'))
table(midwest$grade)

# 전체인구 대비 아시아인 인구 백분율(ratio_asian) 변수를 추가하고 하위 10개 지역 state, contry, 아시아인 인구  백분률 출력
midwest <- midwest %>% mutate(ratio_asian = (popasian/poptotal*100))
midwest %>% select('state', 'county', 'ratio_asian') %>% group_by('state') %>% arrange(desc('ratio_asian')) %>% tail(10)


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
