library(moonBook)
mean.m <- mean(acs$age[acs$sex == 'Male'])
mean.w <- mean(acs$age[acs$sex == 'Female'])

# 정규분포 확인 -> 남성은 0.2098로 정규분포이나 여성은 6.32e-07로 정규분포가 아님
shapiro.test(acs$age[acs$sex == 'Male'])
shapiro.test(acs$age[acs$sex == 'Female'])
# 등분산 확인 -> 등분산 O
var.test(age ~ sex, data = acs)

# p value 2.2e-16으로 차이가 있다.
t.test(age~sex, data=acs, var.test=T, alt = 'two.sided')


a <- c(980, 1008, 968, 1032, 1012, 1002, 996, 1017, 990, 955)

mean.a <- mean(a)
mean.a

# p value 0.9781로 등분산 이다.
shapiro.test(a)

# 모집단의 평균인 1000과 표본집단의 평균은 차이가 없다.
t.test(a, mu=1000)



library(PairedData)

# 정규분포를 만족하는가? O -> 등분산 검증
shapiro.test(sleep$extra[sleep$group ==1])
shapiro.test(sleep$extra[sleep$group ==2])


# 등분산을 만족하는가? O -> t-test
with(sleep, var.test(extra[group ==1], extra[group ==2]))

# 차이가 있다.
with(sleep, t.test(extra ~ group, data=sleep, paired=T))




# 정규분포인지 아닌지 확인 (p value가 1.024e-11 이므로 정규분포 아님!)
out = aov(LDLC ~ Dx, data = acs) # ANOVA 분석
shapiro.test(resid(out)) # 잔차를 구해줌


# 등분산 검정(p value가 0.18 이므로 등분산!)
bartlett.test(LDLC ~ Dx, data = acs)

# 확인!, 차이가 있다.
oneway.test(LDLC ~ Dx, data=acs, var.equal = T)


library(pgirmess)


out = aov(Sepal.Width ~ Species, data = iris)
shapiro.test(resid(out)) # 0.323 이므로 정규분포이다!
bartlett.test(Sepal.Width ~ Species, data = iris) # 0.3515이므로 등분산 이다.

summary(out) # 차이가 있다.

kruskalmc(iris$Sepal.Width, iris$Species) # 전체적으로 평균의 차이가 있다.

