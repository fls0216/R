#### Chi Square Test ####
# 집단간의 비유 검정(확률로 계산)
# 1. Chi Square Test : 데이터 수가 충분할 때 사용(기대 빈도가 20% 이하일때 사용)
# 2. Fisher's exact test : 데이터 수가 부족할 때 사용(기대 빈도가 20% 이상일때 사용)
# 3. Cochran-armitage trend test : 명목변수가 서열변수 일때 사용(trend가 있을 때)
# 일원 카이제곱(차이검정), 이원 카이제곱(관계검정)

#mtcars-----------------------------------------------------------------------------
head(mtcars)
# 자동차의 실린더 수와 변속기의 관계
table(mtcars$cyl, mtcars$am)

mtcars$tm <- ifelse(mtcars$am == 0, 'auto', 'manual')
result <- table(mtcars$cyl, mtcars$tm)
result

addmargins(result)

# 데이터를 테이블 형태로 넘겨주어야함
chisq.test(result)
fisher.test(result)


#Cochran-armitage---------------------------------------------------------------------
# Cochran-armitage trend test
# 흡연자, 비흡연자, 과거 흡연자와 고혈압의 유무가 서로 연관이 있을까?

str(acs)
View(acs)
table(acs$HBP, acs$smoking)

acs$smoking <- factor(acs$smoking, level = c('Never', 'Ex-smoker', 'Smoker'))
result <- table(acs$HBP, acs$smoking)

?prop.trend.test()
# x : Number of event
# n : Number of trials

# 고혈압이 발생한 사람의 수(x)
result[2,]

#n
colSums(result)
prop.trend.test(result[2,], colSums(result))

mytable(smoking~age, data=acs)

result

# 모자이크 그래프
mosaicplot(result)
mosaicplot(result, color = c('tan1','firebrick2'))

colors()
demo('colors')

# 행과 열 바꾸기
t(result)

mosaicplot(t(result), color = c('tan1','firebrick2'), ylab = 'Hypertension', xlab = 'Smoking')

#anova_two_wat--------------------------------------------------------------------------
mydata <- read.csv('../data/anova_two_way.csv')
mydata
View(mydata)

# 시 군 구에 따라 다자녀 지원 조례 채택 여부가 연관이 있는가

result <- table(mydata$ad_layer, mydata$multichild)

prop.trend.test(result[2,], colSums(result))


