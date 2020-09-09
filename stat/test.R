#### 서술형 ####
### 1. 머신러닝 기반 데이터 분석이란 고도의 정확도가 요구되는 문제를 해결하기 위해 복잡한 데이터 구조 패턴을 기계로 하여금 스스로 학습하게 하는 머신러닝 알고리즘 기술을 활용하여 현업의 데이터를 분석하고, 실제 업무에 적용하는 능력이다.

#3. 기본 통계량 설명하는 함수 8개 -----------------------------
# - mean, median, max, min, sum, var, sd, skew

# 4. 전처리에서 어떤 작업을 하는지 5개 이상--------------------
# - 정규분포 인지 연속변수인지 확인, 등분산성이 있는지 확인, 변수명을 보기 쉽게 분류, wide형 데이터를 long 형으로 변경

# 5. 컬럼명 변경-----------------------------------------------
# names(df_new1) <- c('v1', 'v2')

# df_new2 <- rename(df_new2, v1=var1, v2=var2)

# 6. 코드 해석-------------------------------------------------

# 7. 그래프 종류-----------------------------------------------
# - 산포도(상관관계 파악에 좋음), 모자이크, 박스플롯, 꽃잎 그래프, 별그래프

# 8. 용어설명--------------------------------------------------
# -분산: 그 확률변수가 기대값으로부터 얼마나 떨어진 곳에 분포하는지를 가늠하는 숫자
# -표준편차 : 분산에 제곱근을 취한 값
# -도수분포표(frequency table): 데이터를 구간으로 나누어, 각 구간의 빈도를 나타낸 표

# 9. 기본 자료구조 5가지의 특징--------------------------------
# - list : 3차원 이상, 여러 형식을 같이 묶을 수 있음
# - data.frame : 2차원, 컬럼 단위로 다른 데이터 형식 저장이 가능.
# - 배열: 행렬 형태의 데이터를 층위별로 저장한다.
# - 행렬 : 2차원, 같은 형식만 묶을 수 있음
# - 백터 : 1차원, 인덱스로 접근 가능, 동일한 자료형만 저장 가능


# 10.행렬 계산-------------------------------------------------
#열로 합치기!
#cbind(x1,x2)
#행으로 합치기!
#rbind(x1,x2)
#함수를 호출하여 행렬에 적용하기 1 = 열기준, 2 = 행기준
#apply(x, 1, max)
#apply(x, 2, max)

#### 실습형 ####
library(dplyr)
# 남성과 여성의 평균 나이차이 검정-----------------------------
mean.man <- mean(acs$age[acs$sex == 'Male'])
mean.woman <- mean(acs$age[acs$sex == 'Female'])

cat(mean.man, mean.woman)

moonBook::densityplot(age ~ sex, data=acs)

# 정규분포 확인
shapiro.test(acs$age[acs$sex == 'Male'])
shapiro.test(acs$age[acs$sex == 'Female'])
# 등분산 확인
var.test(age ~ sex, data = acs)

t.test(age~sex, data=acs, var.test=F, alt = 'two.sided')
wilcox.test(age~sex, data=acs)

# one sample t-test--------------------------------------------
### 집단이 한개인경우
# 주제 : A회사의 건전지 수명이 1000시간일때 무작위로 뽑은 10개의 건전지 수명에 대해 샘플이 모집단과 다르다고 할 수 있는가?
# 귀무가설 : 모집단의 평균과 같다.
# 대립가설 : 모집단의 평균과 다르다.
a<- c(980, 1008, 968, 1032, 1012, 1002, 996, 1017, 990, 955)

mean.a <- mean(a)
mean.a

shapiro.test(a)

?t.test
# 모집단의 평균인 1000과 표본집단의 평균은 차이가 없다.
t.test(a, mu=1000)

# 어떤 학급의 수학 평균 성적이 55점 이었다. 0교시 수업을 하고 다시 성적을 살펴보았다.
b <- c(58,49,39,99,32,88,62,30,55,65,44,55,57,53,88,42,39)
mean.b <- mean(b)
mean.b

shapiro.test(b)
# 55와는 차이가 없다고 보고 46과는 차이가 있다고 한다.
t.test(b, mu =55, alt='greater')
t.test(b, mu =46, alt='greater')

# paired sample t-test-----------------------------------------
str(sleep)
View(sleep)
before<- subset(sleep, group == 1, extra)
after <- subset(sleep, group == 2, extra)
before

# install.packages('PairedData')
library(PairedData)

sleep2 <- paired(before, after)
sleep2
plot(sleep2, type='profile') + theme_bw()

with(sleep, shapiro.test(extra[group == 1]))

# 등분산을 만족하는가? O
with(sleep, var.test(extra[group ==1], extra[group ==2]))

# 차이가 있다.
with(sleep, t.test(extra ~ group, data=sleep, paired=T))

#===
# 출산률 데이터
mydata <- read.csv('../data/independent.csv')
View(mydata)

# dummy 라는 컬럼에서 0은 군을 나타내고 1은 시를 나타낸다. 시와 군에 따라 합계 출산율의 차이가 있는지를 알아보렿나다.
# 귀무가설 : 차이가 없다.
# 대립가설 : 차이가 있다.

# 정규분포가 아니므로 wlicox 사용!
with(mydata, shapiro.test(birth_rate[dummy == 1]))
with(mydata, var.test(birth_rate[dummy == 1], birth_rate[dummy == 0]))
moonBook::densityplot(birth_rate ~ dummy, data=mydata)

# 차이가 있다!
with(mydata, wilcox.test(birth_rate ~ dummy, data = mydata))

#===
#mtcars
str(mtcars)

# am : 0 은 오토, 1은 수동, 연비는 mpg
# 오토나 수동에 따라 연비가 같을까? 다를까?
with(mtcars, shapiro.test(mpg[am == 1]))
with(mtcars, var.test(mpg[am == 1], mpg[am == 0]))

# 다르다아
with(mtcars, t.test(mpg ~ am, data = mtcars))

#===
#pairedData
pd <- read.csv('../data/pairedData.csv')
pd

#쥐의 몸무게가 전과 후의 변화가 있는지 없는지 확인
# subset으로 뽑아도 되고(그래프 그리기가 용이함) long형으로 하나로 해도 된다(melt)
with(pd, shapiro.test(before)) #정규성
with(pd, shapiro.test(After)) 

with(pd, var.test(before, After))#등분산성

# 차이가 있다.
with(pd, t.test(before, After, data=pd, paired=T))

#install.packages('tidyr')
library(tidyr)

pd2 <- gather(pd, key='GROUP', value='RESULT', -ID) #-컬럼 : 그룹안에 id 들어가지 않게 할 수 있다.
pd2

before <- subset(pd2, GROUP == 'before', RESULT)
after <- subset(pd2, GROUP =='After', RESULT)

pd3 <- paired(before, after)
plot(pd3, type='profile')+theme_bw()

#===
#paired
pd <- read.csv('../data/paired.csv')
View(pd)
str(pd)

# 정규분포가 아니다.
with(pd, shapiro.test(birth_rate_2015))
with(pd, shapiro.test(birth_rate_2010))

# 차이가 없다.
with(pd, wilcox.test(birth_rate_2015,birth_rate_2010, data=pd, paired=T))


#===
#studendt_math
math <- read.csv('../data/datasets_251_561_student-mat.csv', header = T)
View(math)
str(math)
summary(math$G1)
summary(math$G2)
summary(math$G3)


table(math$sex)

#===
# 남여 학생별로 3번의 시험에 대한 평균값조회
mean <- math %>% select(sex, G1, G2, G3)%>% group_by(sex) %>% summarise(mean_g1=mean(G1),mean_g2=mean(G2),mean_g3=mean(G3))


shapiro.test(math$G1[math$sex=='M'])
shapiro.test(math$G1[math$sex=='F'])
shapiro.test(math$G2[math$sex=='M'])
shapiro.test(math$G2[math$sex=='F'])
shapiro.test(math$G3[math$sex=='M'])
shapiro.test(math$G3[math$sex=='F'])


# 남학생이 여학생보다 수학 점수가 높은가?(단측검정) -> 반대는 greater 사용
# 전부 높다!
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

#차이가 없다아
with(mean, wilcox.test(G1, G3, data=mean, paired =T))

mydata <- gather(math, key='GROUP', value='RESULT', 'G1','G3')
with(mydata, wilcox.test(RESULT ~GROUP, data = mydata, paired=T))
with(mydata, wilcox.test(RESULT ~GROUP, data = mydata, paired=T, alt = 'greater'))
# wilcox.test -> 비모수 방식(평균을 이용하지 않음), 연속변수가 아님, 데이터의 크기가 작을때 사용


# one way anova------------------------------------------------
with(acs, shapiro.test(LDLC[Dx=="NSTEMI"])) # 귀무가설 기각o. 정규분포 아니다
with(acs, shapiro.test(LDLC[Dx=="STEMI"])) # 기각 x, 정규 분포다
with(acs, shapiro.test(LDLC[Dx=="Unstable Angina"])) # 귀무가설 기각o. 정규분포 아니다

# 정규분포인지 아닌지 한번에 구하기 (p value가 1.024e-11 이므로 정규분포 아님!)
out = aov(LDLC ~ Dx, data = acs) # ANOVA 분석
shapiro.test(resid(out)) # 잔차를 구해줌


# 등분산 검정(p value가 0.18 이므로 등분산!)
bartlett.test(LDLC ~ Dx, data = acs)

# 확인!, 차이가 없다.
oneway.test(LDLC ~ Dx, data=acs, var.equal = F)

#=====
library(pgirmess)
head(iris)
# 품종별로 Sepal.width의 평균 차이가 있는가? 만약 있다면 어느 품종과 차이가 있는가

moonBook::densityplot(Sepal.Width ~ Species, data = iris)

out = aov(Sepal.Width ~ Species, data = iris)
shapiro.test(resid(out)) # 0.323 이므로 정규분포이다!
bartlett.test(Sepal.Width ~ Species, data = iris) # 0.3515이므로 등분산 이다.

kruskalmc(iris$Sepal.Width, iris$Species)

#===
library(nparcomp)
library(userfriendlyscience)
mydata <- read.csv('../data/anova_one_way.csv')
View(mydata)
head(mydata)

# 시구군에 따라서 합계 출산율의 차이가 있는가 있다면 어느것과 차이가 있는가,

moonBook::densityplot(birth_rate ~ ad_layer, data = mydata)
out = aov(birth_rate ~ ad_layer, data = mydata)
shapiro.test(resid(out)) # 5.788e-07이므로 no정규분포...
bartlett.test(birth_rate ~ ad_layer, data = mydata) # 9.659e-05이므로 no 등분산..
kruskal.test(birth_rate ~ ad_layer, data = mydata)
posthocTGH(x = mydata$ad_layer, y = mydata$birth_rate)

# 다 차이가 있따
result <- mctp(birth_rate ~ ad_layer, data = mydata)
summary(result)






# 지현민---------------------------------------------------------------------------
-----------------------------------------------------
  실습형
-------
#1. 남성과 여성의 평균 나이 차이 검정
# install.packages('moonBook')
library(moonBook)

?acs

# 귀무가설 : 남성과 여성의 평균나이에 대해 차이가 없다.
# 대립가설 : 남성과 여성의 평균나이에 대해 차이가 있다.

head(acs)
str(acs)

mean.man <- mean(acs$age[acs$sex=='Male'])
mean.woman <- mean(acs$age[acs$sex=='Female'])
cat(mean.man, mean.woman)

moonBook::densityplot(age ~ sex, data = acs)

# 정규분포 테스트
# 귀무가설 : 정규분포가 맞다 (p-value '0.05'보다 크다)
# 대립가설 : 정규분포가 아니다
shapiro.test(acs$age[acs$sex=='Male']) # 정규분포
shapiro.test(acs$age[acs$sex=='Female']) # 정규분포 X -> t-test 못쓴다

# 등분산 테스트
# 귀무가설 : 등분산이 맞다
# 대립가설 : 등분산이 아니다
var.test(age ~ sex, data = acs)
View(acs)

wilcox.test(age ~ sex, data = acs)
# p-value '0.05'보다 작다 : 대립가설 채택
t.test(age~sex, data = acs, var.test=T, alt='less') # p-value = 1
t.test(age~sex, data = acs, var.test=T, alt='greater')

t.test(age~sex, data = acs, var.test=T, alt='two.sided')
t.test(age~sex, data = acs, var.test=F)

2. one sample t-test

### 집단이 한 개인 경우
# A회사의 건전지 수명이 1000시간일 때 무작위로 뽑은 10개의 건전지 수명에 대해 샘플이 모집단과 다르다고 할 수 있는가?
# 귀무가설 : 모집단의 평균과 같다
# 대립가설 : 모집단의 평균과 다르다

a <- c(980, 1008, 968, 1032, 1012, 1002, 996, 1017, 990, 955)

mean.a <- mean(a)
mean.a

shapiro.test(a)
?t.test
t.test(a, mu=1000)

# 어떤 학급의 수학 평균성적이 55점이었다. 0교시 수업을 하고 다시 성적을 살펴보았다
# 귀무가설 : 변화가 없다
# 대립가설 : 변화가 있다
b <- c(58, 49, 39, 99, 32, 88, 62, 30, 55, 65, 44, 55, 57, 53, 88, 42, 39)

mean.b <- mean(b)
shapiro.test(b)
t.test(b, mu=55, alt='greater')
# p-value '0.4046' 변화가 없다

3. paired sample t-test


str(sleep)
View(sleep)

before <- subset(sleep, group==1, extra)
before
after <- subset(sleep, group==2, extra)
after

# install.packages('PairedData')
library(PairedData)

sleep2 <- paired(before, after)
sleep2
plot(sleep2, type='profile') + theme_bw()

shapiro.test(sleep$extra[sleep$group==1])

with(sleep, shapiro.test(extra[group==1]))
with(sleep, shapiro.test(extra[group==2]))

with(sleep, var.test(extra[group==1], extra[group==2]))

with(sleep, t.test(extra ~ group, data=sleep, paired = T))

4. one way anova

5. one way anova

## one way anova
library(moonBook)

View(acs)
# LDLC (저밀도 콜레스테롤 수치)
# Dx(진단 결과) : STEMI, NSTEMI, unstable angina

moonBook::densityplot(LDLC ~ Dx, data = acs)

with(acs, shapiro.test(LDLC[Dx=='NSTEMI'])) # 귀무가설 기각o. 정규분포 아니다
with(acs, shapiro.test(LDLC[Dx=='STEMI']))  # 기각 x, 정규 분포다
with(acs, shapiro.test(LDLC[Dx=='Unstable Angina'])) # 귀무가설 기각o. 정규분포 아니다

out = aov(LDLC ~ Dx, data = acs)
shapiro.test(resid(out))

# 등분산 검정
bartlett.test(LDLC ~ Dx, data = acs)

# 정규분포이고 등분산일 경우
out = aov(LDLC ~ Dx, data = acs)
summary(out)

# 연속변수가 아니거나 정규분포가 아닐 경우
kruskal.test(LDLC ~ Dx, data = acs)

# 등분산이 아닐 경우
oneway.test(LDLC ~ Dx, data = acs, var.equal = F)

### 사후 검정
# aov()를 사용했을 경우 : TukeyHSD()
TukeyHSD(out)

# kruskal.test()를 사용했을 경우
# install.packages('pgirmess')
library(pgirmess)
kruskalmc(acs$LDLC, acs$Dx)

str(InsectSprays)
View(InsectSprays)

moonBook::densityplot(count ~ spray, data = InsectSprays)
kruskalmc(InsectSprays$count, InsectSprays$spray)

# install.packages('userfriendlyscience')
library(userfriendlyscience)
posthocTGH(x = InsectSprays$spray, y = InsectSprays$count,
           method = 'games-howell')

# oneway.test()를 사용했을 때 사후 검정
# install.packages('nparcomp')
library(nparcomp)

result <- mctp(LDLC ~ Dx, data = acs)
summary(result)

# -----------------------------------------------------------

head(iris)
# 품종별로 Sepal.width의 평균 차이가 있는가? 만약 있다면 어느 품종과 차이가 있는가?

##--------------------------------------------------------------##
# 직접 해본 부분
out <- aov(Sepal.Width ~ Species, data = iris) # anova
out
shapiro.test(resid(out)) # 정규분포 O

bartlett.test(Sepal.Width ~ Species, data = iris) # 등분산 O

summary(out) # '***'라서 차이가 매우 크다

TukeyHSD(out) 
## --------------------------------------------------------------##

mydata <- read.csv('../data/anova_one_way.csv')
View(mydata)

# 시, 군, 구에 따라서 합계 출산율의 차이가 있는가? 있다면 어느것과 차이가 있는가?
out <- aov(birth_rate ~ ad_layer, data = mydata)
shapiro.test(resid(out))

kruskal.test(birth_rate ~ ad_layer, data = mydata)

posthocTGH(x = mydata$ad_layer, y = mydata$birth_rate,
           method = 'games-howell')

kruskalmc(mydata$birth_rate, mydata$ad_layer)
