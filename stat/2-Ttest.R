#### T-test ####
# 1. 정의 : 모집단의 표준편차가 알려지지 않았을 때 정규 분포의 모집단에서 모은 샘플의 평균값에 대한 가설 검정 방법
# 2. 목적 : 두 개의 집단이 같은지 다른지 비교하기 위해 사용

# Power Analysis : 적정한 표본의 갯수 산출
# cohen's d : 최소한의 표본 갯수 산출

library(plyr)
library(foreign)
library(dplyr)
library(ggplot2)
library(readxl)

ky <- read.csv('../data/KY.csv', header = T)
View(ky)
table(ky$group)

mean.1 <- mean(ky$score[ky$group == 1])
mean.2 <- mean(ky$score[ky$group == 2])

cat(mean.1, mean.2)

sd.1 <- sd(ky$score[ky$group == 1])
sd.2 <- sd(ky$score[ky$group == 2])

cat(sd.1, sd.2)

effsize <- abs(mean.1-mean.2)/sqrt((sd.1^2+sd.2^2)/2)

effsize

#install.packages('pwr')
library(pwr)

pwr.t.test(d=effsize, type = 'two.sample', alternative = 'two.sided', power = .8, sig.level = 0.05)


#### 두 그룹의 평균 비교 ####
# 조건
#   1) 결과값이 연속변수
#   2) 정규분포 여부
#   3) 등분산 여부
# 위 세가지 조건을 만족해야 t-test 사용가능(student t-test, tw sample t-test)
# 연속변수가 아닐 경우 Mann-Whitney U test, wilcoxen rank-sum test, Mann-whitney-wilcoxen test, MWW
# 정규분포가 아니라면 MWW
# 등분산이 아닐 경우 welch's t-test

install.packages('moonBook')
library(moonBook)

?acs
# 귀무가설 : 남성과 여성의 평균나이가 차이가 없다.
# 대립가설 : 남성과 여성의 평균나이가 차이가 있다.

head(acs)
str(acs)

mean.man <- mean(acs$age[acs$sex == 'Male'])
mean.woman <- mean(acs$age[acs$sex == 'Female'])

cat(mean.man, mean.woman)

moonBook::densityplot(age ~ sex, data=acs)

# 정규분포 테스트
# 귀무가설 :  정규분포가 맞다
# 대립가설 :  정규분포가 아니다.
shapiro.test(acs$age[acs$sex == 'Male'])
shapiro.test(acs$age[acs$sex == 'Female'])

# 등분산 테스트
# 귀무가설 : 등분산이 맞다
# 대립가설 : 등분산이 아니다
var.test(age ~ sex, data = acs)

wilcox.test(age~sex, data=acs)
t.test(age~sex, data=acs, var.test=T, alt = 'two.sided')
# welch's t-test 사용하기
t.test(age~sex, data=acs, var.test=F, alt = 'two.sided')

### 집단이 한개인경우
# 주제 : A회사의 건전지 수명이 1000시간일때 무작위로 뽑은 10개의 건전지 수명에 대해 샘플이 모집단과 다르다고 할 수 있는가?
# 귀무가설 : 모집단의 평균과 같다.
# 대립가설 : 모집단의 평균과 다르다.
a<- c(980, 1008, 968, 1032, 1012, 1002, 996, 1017, 990, 955)

mean.a <- mean(a)
mean.a

shapiro.test(a)

?t.test
t.test(a, mu=1000)

# 어떤 학급의 수학 평균 성적이 55점 이었다. 0교시 수업을 하고 다시 성적을 살펴보았다.
b <- c(58,49,39,99,32,88,62,30,55,65,44,55,57,53,88,42,39)
mean.b <- mean(b)
mean.b

shapiro.test(b)
t.test(b, mu =55, alt='greater')
t.test(b, mu =46, alt='greater')

### Paired Sample T-test
str(sleep)
View(sleep)
before<- subset(sleep, group == 1, extra)
after <- subset(sleep, group == 2, extra)
before

install.packages('PairedData')
library(PairedData)

sleep2 <- paired(before, after)
sleep2
plot(sleep2, type='profile') + theme_bw()

# with 함수 사용시 데이터의 이름을 반복해서 사용하지 않아도 됨
with(sleep, shapiro.test(extra[group == 1]))

# 등분산을 만족하는가? O
with(sleep, var.test(extra[group ==1], extra[group ==2]))

with(sleep, t.test(extra ~ group, data=sleep, paired=T))

# 출산률 데이터----------------------------------------------------------------------------
mydata <- read.csv('../data/independent.csv')
View(mydata)

# dummy 라는 컬럼에서 0은 군을 나타내고 1은 시를 나타낸다. 시와 군에 따라 합계 출산율의 차이가 있는지를 알아보렿나다.
# 귀무가설 : 차이가 없다.
# 대립가설 : 차이가 있다.

# 정규분포가 아니므로 wlicox 사용!
with(mydata, shapiro.test(birth_rate[dummy == 1]))
with(mydata, var.test(birth_rate[dummy == 1], birth_rate[dummy == 0]))
moonBook::densityplot(birth_rate ~ dummy, data=mydata)


with(mydata, wilcox.test(birth_rate ~ dummy, data = mydata))


#mtcars---------------------------------------------------------------------------------------
 str(mtcars)

# am : 0 은 오토, 1은 수동, 연비는 mpg
# 오토나 수동에 따라 연비가 같을까? 다를까?
with(mtcars, shapiro.test(mpg[am == 1]))
with(mtcars, var.test(mpg[am == 1], mpg[am == 0]))

with(mtcars, t.test(mpg ~ am, data = mtcars))


#pairedData-------------------------------------------------------------------------------------------
pd <- read.csv('../data/pairedData.csv')
pd

#쥐의 몸무게가 전과 후의 변화가 있는지 없는지 확인
# subset으로 뽑아도 되고(그래프 그리기가 용이함) long형으로 하나로 해도 된다(melt)
with(pd, shapiro.test(before)) #정규성
with(pd, shapiro.test(After)) #등분산성

with(pd, var.test(before, After))

with(pd, t.test(before, After, data=pd, paired=T))

#install.packages('tidyr')
library(tidyr)

pd2 <- gather(pd, key='GROUP', value='RESULT', -ID) #-컬럼 : 그룹안에 id 들어가지 않게 할 수 있다.
pd2

before <- subset(pd2, GROUP == 'before', RESULT)
after <- subset(pd2, GROUP =='After', RESULT)

pd3 <- paired(before, after)
plot(pd3, type='profile')+theme_bw()

#paired------------------------------------------------------------------------------------------------
pd <- read.csv('../data/paired.csv')
View(pd)
str(pd)

with(pd, shapiro.test(birth_rate_2015))
with(pd, shapiro.test(birth_rate_2010))

with(pd, wilcox.test(birth_rate_2015,birth_rate_2010, data=pd, paired=T))



#studendt_math------------------------------------------------------------------------------------
math <- read.csv('../data/datasets_251_561_student-mat.csv', header = T)
View(math)
str(math)
summary(math$G1)
summary(math$G2)
summary(math$G3)


table(math$sex)

# 남여 학생별로 3번의 시험에 대한 평균값조회
mean <- math %>% select(sex, G1, G2, G3)%>% group_by(sex) %>% summarise(mean_g1=mean(G1),mean_g2=mean(G2),mean_g3=mean(G3))

shapiro.test(math$G1[math$sex=='M'])
shapiro.test(math$G1[math$sex=='F'])
shapiro.test(math$G2[math$sex=='M'])
shapiro.test(math$G2[math$sex=='F'])
shapiro.test(math$G3[math$sex=='M'])
shapiro.test(math$G3[math$sex=='F'])

# 남학생이 여학생보다 수학 점수가 높은가?(단측검정) -> 반대는 greater 사용
with(math, wilcox.test(G1~sex, data=math, alt='less'))
with(math, wilcox.test(G2~sex, data=math, alt = 'less'))
with(math, wilcox.test(G3~sex, data=math, alt = 'less'))

# G1과 G3에 대해 변화가 있었는지 확인
mean <- math %>% select(G1, G3)
head(mean)

mean(mean$G1)
mean(mean$G3)

with(mean, shapiro.test(G1))
with(mean, shapiro.test(G3))

with(mean, wilcox.test(G1, G3, data=mean, paired =T))

mydata <- gather(math, key='GROUP', value='RESULT', 'G1','G3')
with(mydata, wilcox.test(RESULT ~GROUP, data = mydata, paired=T))
with(mydata, wilcox.test(RESULT ~GROUP, data = mydata, paired=T, alt = 'greater'))
# wilcox.test -> 비모수 방식(평균을 이용하지 않음), 연속변수가 아님, 데이터의 크기가 작을때 사용