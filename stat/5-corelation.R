# 상관분석
install.packages('UsingR')
library(UsingR)
head(galton)
str(galton)

# 산포도 그리기
plot(child ~ parent, data=galton)
# 상관계수 구해보기
cor.test(galton$child, galton$parent)

# 상관계수 더 자세히 구하기
out <- lm(child~parent, data=galton)
summary(out)

abline(out, col='red')

# 정리되어있는 데이터를 흔들어서 산포도 처럼 보이게 해줌
plot(jitter(child, 5) ~ jitter(parent, 5), data=galton)



install.packages('SwissAir')
library(SwissAir)
View(AirQual)
str(AirQual)

Ox <- AirQual[ , c("ad.O3", "lu.O3", "sz.O3")] + AirQual[ , c("ad.NOx", "lu.NOx", "sz.NOx")] -
  AirQual[ , c("ad.NO", "lu.NO", "sz.NO")]
names(Ox) <- c('ad', 'lu','sz')

plot(lu~sz, data=Ox)

# 어디에 데이터가 더 많이 모여있는지를 확인하기 쉽게 해줌
install.packages('hexbin')
library(hexbin)

bin <- hexbin(Ox$lu, Ox$sz, xbin = 50)

plot(bin)

smoothScatter(Ox$lu, Ox$sz)


install.packages('IDPmisc')
library(IDPmisc)
iplot(Ox$lu, Ox$sz)

# cor.csv-------------------------------------------------------------------------
mydata <- read.csv('../data/cor.csv')
View(mydata)
head(mydata)

par("mar")

par(mar=c(5.1, 4.1, 4.1, 2.1))


# 산포도
plot(pop_growth ~ elderly_rate, data=mydata, main = 'Scatterplot')

# 상관계수 구하기
cor(mydata$pop_growth, mydata$elderly_rate, method = 'pearson')
cor(mydata$pop_growth, mydata$elderly_rate, method = 'spearman')
cor(mydata$pop_growth, mydata$elderly_rate, method = 'kendall')


# 여러개를 동시에 상관분석
x <- cbind(mydata$pop_growth, mydata$birth_rate, mydata$elderly_rate, mydata$finance, mydata$cultural_center)
cor(x)



