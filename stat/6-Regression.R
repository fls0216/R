str(women)
women

# 상관관계 시각화를 위해 산포도 그리기
plot(weight ~ height, data=women)

# 회귀선 그리기
fit <- lm(weight ~ height, data=women)
abline(fit, col='blue')

# 연구가설 : 키가 클수록 몸무게가 많이 나갈 것이다!
# 원인과 결과에 대한 예측을 해준다.

summary(fit)

fit2 <- lm(weight ~ height + I(height^2), data=women)
summary(fit2)
lines(women$height, fitted(fit2), col='red')

# Estimate = 회귀계수(기울기와 절편) -> 기울기의 방향(양이냐 음이냐)
# std. Error = 표준오차, 여러 집단의 차이 -> 우연히 발생했는가? 아닌가? 유의하다 유의하지 않다.
# t value = t-test 값, 두개 집단의 같은지 다른지, 다르다면 얼마나 다른지 확인하는 값/ 결과가 의미가 있느냐 없느냐를 확인해줌 
# R-squared = 상관계수^2 -> 원인과 결과를 설명해줌, 분산을 파악한다! 종속변수의 분산과 독립변수의 분산이 얼마나 겹쳐져있느냐 파악, 높을 수록 설명력이 있다.
# 변수를 추가하면 R-squared가 올라가지만 그렇게 되면 다중 공산성이 높아질 가능성이 높다. 과적합과 비슷함 -> 잘못된 예측선을 그릴 가능성이 높아진다.
# VIF = 분산이 어느정도 팽창했는지 확인, 다중 공산성을 확인할 수 있음

# house--------------------------------------------------------------------------------------

house <- read.csv('../data/kc_house_data.csv')

View(house)
str(house)


attach(house)

x <- cbind(sqft_living, bathrooms, sqft_lot, floors)
# 독립변수간의 상관관계 비교를 통해 다중 공산성 확인
cor(x)

# 종속변수와 독립변수 상관비교
cor(x, price)


# 가장 상관관계가 높은 변수인 거실의 크기와 집값 비교(유의하다!)
reg1 <- lm(price~sqft_living, data=house)
summary(reg1)

#  층수 변수 추가 -> 층수의 경우에는 pvalue 값이 높음.
reg2 <- lm(price~sqft_living+floors, data=house)
summary(reg2)

# 조절변수(교호작용)을 통해 수치를 높여줌
reg2_1 <- lm(price~sqft_living+floors+sqft_living*floors, data=house)
summary(reg2_1)
# -> p value 값은 높아졌으나, 기울기의 방향이 음수로 변경됨, 따라서 다중공산성이 사료된다.

install.packages('car')
library(car)
vif(reg2_1)


x = cbind(floors, sqft_above, sqft_basement)
cor(x)

cor(x, price)

x = cbind(floors,bedrooms)
cor(x)

cor(x, price)

reg3 <- lm(price~floors+bedrooms, data=house)
summary(reg3)

vif(reg3)


reg4 <- lm(price~floors+bedrooms+waterfront, data=house)
summary(reg4)

vif(reg4)


reg5 <- lm(price~floors+bedrooms+waterfront+bedrooms*waterfront, data=house)
summary(reg5)

vif(reg5)


reg6 <- lm(price~floors+bedrooms+waterfront+floors*waterfront, data=house)
summary(reg6)

vif(reg6)


#정규성(잔차), 독립성, 선형성, 등분산성---------------------------------------------------------------------------
fit <- lm(weight ~ height, data=women)
plot(fit)

# 그래프를 한번에!
par(mfrow=c(2,2))
plot(fit)



#regression---------------------------------------------------------------------------------
mydata <- read.csv('../data/regression.csv')
View(mydata)
str(mydata)

# 유치원이 많으면 합계출산률이 높아진다! -> 유의하지만 설명력도 낮고 기울기도 애매함

y <- cbind(mydata$birth_rate)
x <- cbind(mydata$kindergarten)

fit <- lm(y~x, data=mydata)
summary(fit)
plot(fit)

# 로그화를 통해 유의하게 만들기
fit2 <- lm(log(y)~log(x), data=mydata)
summary(fit2)

plot(fit2)

# 잔차확인!
shapiro.test(resid(fit))
shapiro.test(resid(fit2))


# 시군구
y <- cbind(mydata$birth_rate)
x2 <- cbind(mydata$dummy)

fit3 <- lm(y~x2, data=mydata)
summary(fit3)
plot(fit3)


#다중회귀----------------------------------------------------------------------
class(state.x77)
states <- as.data.frame(state.x77[, c("Murder", "Population", "Illiteracy", "Income", "Frost")])
states


fit <- lm(Murder~.,data=states)
summary(fit)

vif(fit)

# 다중공산성에는 문제가 없다.
sqrt(vif(fit))

# 이상치처리
# 1) 이상치(outlier) : 표준잔차 2배이상 ~ -2배 이상 크거나 작은값 -> 의심할 필요가 있음
# 2) 큰 지레점(High leverage points) : 절편을 포함한 인수들의 숫자/n 값이 2~3배 이상되는 관측치
# 3) 영향관측치(Influential Observation, Cook's D) : 독립변수의 수/샘플수-독립변수의수-1 의 값보다 큰 값들은 이상치로 의심


# 큰 지레점을 기준으로 이상치를 확인해주는 그래프
influencePlot(fit, id=list(method = "identify"))

# 정규성(잔차), 독립성, 선형성, 등분산 확인해보기!
par(mfrow=c(2,2))
plot(fit)

shapiro.test(resid(fit))


# 회귀 모형의 교정
# -1, -`, -0.5, 0, 0.5, 1,2`
summary(powerTransform(states$Murder))
# 0.6을 제곱 희귀모형을 교정해봐라

# 선형성의 교정
# 이미 정규분포를 이루고 있으므로 유의하지 않음
boxTidwell(Murder~Population+Illiteracy, data=states)

# 등분산성 교정
ncvTest(fit)
spreadLevelPlot(fit)


# 모형선택 ------------------------------------------------------------------------------
# Backward Stepwise Regression(모든 변수를 일단 넣어서 돌린 후 하나씩 제거하면서 성능 측정),  Forward Stepwise Regression(변수를 하나씩 추가하면서 성능 관찰)
# AIC(Akaike's Information Critterion) : 모델 중에 어떤 모델이 더 잘 맞는지 확인해줌
fit1 <- lm(Murder~.,data=states)
summary(fit1)

fit2 <- lm(Murder ~ Population + Illiteracy, data=states)
summary(fit2)

AIC(fit1, fit2)

# Backward
full.model <- lm(Murder~., data=states)
reduce.model <- step(full.model, direction = 'backward')

summary(reduce.model)

min.model <- lm(Murder~1, data=states)
fwd.model <- step(min.model, direction='forward', scope=(Murder~Population +Illiteracy+Income+Frost))

install.packages('leaps')
library(leaps)

leaps <- regsubsets(Murder~Population +Illiteracy+Income+Frost, data=states, nbest = 4)
# 기존의 R^2을 보완해 성능 측정
plot(leaps, scale='adjr2')

#실습문제----------------------------------------------------------------------------------
# 다중회귀로!
mydata <- read.csv('../data/regression.csv')
head(mydata)

plot(birth_rate ~ tris, data=mydata)

fit <- lm(birth_rate~dummy+cultural_center+social_welfare+doctors+active_firms+pop+kindergarten+tris,data=mydata)
summary(fit)

vif(fit)

fit2 <- lm(birth_rate~dummy + active_firms+tris,data=mydata)
summary(fit2)

vif(fit2)

# 다중공산성에는 문제가 없다.
sqrt(vif(fit2))

fit3 <- lm(birth_rate~kindergarten+pop,data=mydata)
summary(fit3)

vif(fit3)

# 다중공산성에는 문제가 없다.
sqrt(vif(fit3))
