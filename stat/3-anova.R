#### ANOVA(분산분석)####
# 연속변수가 아니라면 kruskal-wallis H test
# 정규분포가 아니라면 kruskal-wallis H test
# 등분산이 아니라면 welch's anova
# 모두 만족시에 ANOVA 사용
# 사후 검정 : Tukey

#one way anova----------------------------------------------
library(moonBook)
library(pwr)

View(acs)
# LDLC(저밀도 콜레스테롤 수치)
# Dx(진단 결과) : STEMI, NSTEMI, unstable angina

moonBook::densityplot(LDLC ~ DX, data = acs)

# 정규분포 검정
with(acs, shapiro.test(LDLC[Dx=="NSTEMI"])) # 귀무가설 기각o. 정규분포 아니다
with(acs, shapiro.test(LDLC[Dx=="STEMI"])) # 기각 x, 정규 분포다
with(acs, shapiro.test(LDLC[Dx=="Unstable Angina"])) # 귀무가설 기각o. 정규분포 아니다

# 정규분포인지 아닌지 한번에 구하기 (p value가 1.024e-11 이므로 정규분포 아님!)
out = aov(LDLC ~ Dx, data = acs) # ANOVA 분석
shapiro.test(resid(out)) # 잔차를 구해줌

# 등분산 검정(p value가 0.18 이므로 등분산!)
bartlett.test(LDLC ~ Dx, data = acs)

# 정규분포이고 등분산일 경우(IF!)
out=aov(LDLC ~ Dx, data = acs)
summary(out)

# 연속변수가 아니거나 정규분포가 아닐 경우
kruskal.test(LDLC ~ Dx, data=acs)


# 등분산이 아닐 경우(var.equal = F 로 설정)
oneway.test(LDLC ~ Dx, data=acs, var.equal = F)

#### 사후 검정 ####
# aov()를 사용했을 경우 : TukeyHSD()
TukeyHSD(out)


# kruskal.test()를 사용했을 경우
install.packages('pgirmess')
library(pgirmess)
kruskalmc(acs$LDLC, acs$Dx)

str(InsectSprays)
View(InsectSprays)

moonBook::densityplot(count ~ spray, data = InsectSprays)
kruskalmc(InsectSprays$count, InsectSprays$spray)

# True False가 아닌 다양한 값으로 반환해 주어 어느정도 차이가 나는지 확인할 수 있음
install.packages('userfriendlyscience')
library(userfriendlyscience)
posthocTGH(x=InsectSprays$spray, y=InsectSprays$count, method = 'games-howell')

# oneway.test()를 사용했을 때의 사후검정
install.packages('nparcomp')
library(nparcomp)
result <- mctp(LDLC ~ Dx, data = acs)
summary(result)


#iris---------------------------------------------------------------------------
head(iris)
# 품종별로 Sepal.width의 평균 차이가 있는가? 만약 있다면 어느 품종과 차이가 있는가

moonBook::densityplot(Sepal.Width ~ Species, data = iris)

out = aov(Sepal.Width ~ Species, data = iris)
shapiro.test(resid(out)) # 0.323 이므로 정규분포이다!
bartlett.test(Sepal.Width ~ Species, data = iris) # 0.3515이므로 등분산 이다.

kruskalmc(iris$Sepal.Width, iris$Species)

#one way. csv-----------------------------------------------------------

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

result <- mctp(birth_rate ~ ad_layer, data = mydata)
summary(result)


#### two way anova ####
mydata <- read.csv('../data/anova_two_way.csv')
View(mydata)

out <- aov(birth_rate ~ ad_layer + multichild + ad_layer:multichild, data= mydata)
shapiro.test(resid(out))

summary(out)

TukeyHSD(out)


#### one way Repeated Measures Anova ####
# 연속변수나 종속변수가 정규분포가 아닐 경우 : friedman test


mydata <- read.csv('../data/anova_two_way.csv')
View(mydata)

out <- aov(birth_rate ~ ad_layer + multichild + ad_layer:multichild, data=mydata)
shapiro.test(resid(out))

summary(out)

TukeyHSD(out)





### one way Repeated Measures Anova
# 연속 변수나 정규분포가 아닐 경우 : friedman test

?friedman.test

RoundingTimes <-
  matrix(c(5.40, 5.50, 5.55,
           5.85, 5.70, 5.75,
           5.20, 5.60, 5.50,
           5.55, 5.50, 5.40,
           5.90, 5.85, 5.70,
           5.45, 5.55, 5.60,
           5.40, 5.40, 5.35,
           5.45, 5.50, 5.35,
           5.25, 5.15, 5.00,
           5.85, 5.80, 5.70,
           5.25, 5.20, 5.10,
           5.65, 5.55, 5.45,
           5.60, 5.35, 5.45,
           5.05, 5.00, 4.95,
           5.50, 5.50, 5.40,
           5.45, 5.55, 5.50,
           5.55, 5.55, 5.35,
           5.45, 5.50, 5.55,
           5.50, 5.45, 5.25,
           5.65, 5.60, 5.40,
           5.70, 5.65, 5.55,
           6.30, 6.30, 6.25),
         nrow = 22,
         byrow = TRUE,
         dimnames = list(1 : 22,
                         c("Round Out", "Narrow Angle", 
                           "Wide Angle")))


View(RoundingTimes)


# long형으로 변환
# install.packages('reshape')
library(reshape)

rt1 <- reshape::melt(RoundingTimes)
View(rt1)

out <- aov(value ~ X2, data = rt1)
shapiro.test(resid(out)) # 정규분포 X

boxplot(value ~ X2, data = rt1)

friedman.test(RoundingTimes)


# https://www.r-statistics.com/2010/02/post-hoc-analysis-for-friedmans-test-r-code/

friedman.test.with.post.hoc <- function(formu, data, to.print.friedman = T, 
                                        to.post.hoc.if.signif = T,  
                                        to.plot.parallel = T, 
                                        to.plot.boxplot = T, signif.P = .05, 
                                        color.blocks.in.cor.plot = T, jitter.Y.in.cor.plot =F)
{
  # formu is a formula of the shape: 	Y ~ X | block
  # data is a long data.frame with three columns:    [[ Y (numeric), X (factor), block (factor) ]]
  
  # Note: This function doesn't handle NA's! In case of NA in Y in one of the blocks, then that entire block should be removed.
  
  
  # Loading needed packages
  if(!require(coin))
  {
    print("You are missing the package 'coin', we will now try to install it...")
    install.packages("coin")
    library(coin)
  }
  
  if(!require(multcomp))
  {
    print("You are missing the package 'multcomp', we will now try to install it...")
    install.packages("multcomp")
    library(multcomp)
  }
  
  if(!require(colorspace))
  {
    print("You are missing the package 'colorspace', we will now try to install it...")
    install.packages("colorspace")
    library(colorspace)
  }
  
  
  # get the names out of the formula
  formu.names <- all.vars(formu)
  Y.name <- formu.names[1]
  X.name <- formu.names[2]
  block.name <- formu.names[3]
  
  if(dim(data)[2] >3) data <- data[,c(Y.name,X.name,block.name)]	# In case we have a "data" data frame with more then the three columns we need. This code will clean it from them...
  
  # Note: the function doesn't handle NA's. In case of NA in one of the block T outcomes, that entire block should be removed.
  
  # stopping in case there is NA in the Y vector
  if(sum(is.na(data[,Y.name])) > 0) stop("Function stopped: This function doesn't handle NA's. In case of NA in Y in one of the blocks, then that entire block should be removed.")
  
  # make sure that the number of factors goes with the actual values present in the data:
  data[,X.name ] <- factor(data[,X.name ])
  data[,block.name ] <- factor(data[,block.name ])
  number.of.X.levels <- length(levels(data[,X.name ]))
  if(number.of.X.levels == 2) { warning(paste("'",X.name,"'", "has only two levels. Consider using paired wilcox.test instead of friedman test"))}
  
  # making the object that will hold the friedman test and the other.
  the.sym.test <- symmetry_test(formu, data = data,	### all pairwise comparisons
                                teststat = "max",
                                xtrafo = function(Y.data) { trafo( Y.data, factor_trafo = function(x) { model.matrix(~ x - 1) %*% t(contrMat(table(x), "Tukey")) } ) },
                                ytrafo = function(Y.data){ trafo(Y.data, numeric_trafo = rank, block = data[,block.name] ) }
  )
  # if(to.print.friedman) { print(the.sym.test) }
  
  
  if(to.post.hoc.if.signif)
  {
    if(pvalue(the.sym.test) < signif.P)
    {
      # the post hoc test
      The.post.hoc.P.values <- pvalue(the.sym.test, method = "single-step")	# this is the post hoc of the friedman test
      
      
      # plotting
      if(to.plot.parallel & to.plot.boxplot)	par(mfrow = c(1,2)) # if we are plotting two plots, let's make sure we'll be able to see both
      
      if(to.plot.parallel)
      {
        X.names <- levels(data[, X.name])
        X.for.plot <- seq_along(X.names)
        plot.xlim <- c(.7 , length(X.for.plot)+.3)	# adding some spacing from both sides of the plot
        
        if(color.blocks.in.cor.plot)
        {
          blocks.col <- rainbow_hcl(length(levels(data[,block.name])))
        } else {
          blocks.col <- 1 # black
        }
        
        data2 <- data
        if(jitter.Y.in.cor.plot) {
          data2[,Y.name] <- jitter(data2[,Y.name])
          par.cor.plot.text <- "Parallel coordinates plot (with Jitter)"
        } else {
          par.cor.plot.text <- "Parallel coordinates plot"
        }
        
        # adding a Parallel coordinates plot
        matplot(as.matrix(reshape(data2,  idvar=X.name, timevar=block.name,
                                  direction="wide")[,-1])  ,
                type = "l",  lty = 1, axes = FALSE, ylab = Y.name,
                xlim = plot.xlim,
                col = blocks.col,
                main = par.cor.plot.text)
        axis(1, at = X.for.plot , labels = X.names) # plot X axis
        axis(2) # plot Y axis
        points(tapply(data[,Y.name], data[,X.name], median) ~ X.for.plot, col = "red",pch = 4, cex = 2, lwd = 5)
      }
      
      if(to.plot.boxplot)
      {
        # first we create a function to create a new Y, by substracting different combinations of X levels from each other.
        subtract.a.from.b <- function(a.b , the.data)
        {
          the.data[,a.b[2]] - the.data[,a.b[1]]
        }
        
        temp.wide <- reshape(data,  idvar=X.name, timevar=block.name,
                             direction="wide") 	#[,-1]
        wide.data <- as.matrix(t(temp.wide[,-1]))
        colnames(wide.data) <- temp.wide[,1]
        
        Y.b.minus.a.combos <- apply(with(data,combn(levels(data[,X.name]), 2)), 2, subtract.a.from.b, the.data =wide.data)
        names.b.minus.a.combos <- apply(with(data,combn(levels(data[,X.name]), 2)), 2, function(a.b) {paste(a.b[2],a.b[1],sep=" - ")})
        
        the.ylim <- range(Y.b.minus.a.combos)
        the.ylim[2] <- the.ylim[2] + max(sd(Y.b.minus.a.combos))	# adding some space for the labels
        is.signif.color <- ifelse(The.post.hoc.P.values < .05 , "green", "grey")
        
        boxplot(Y.b.minus.a.combos,
                names = names.b.minus.a.combos ,
                col = is.signif.color,
                main = "Boxplots (of the differences)",
                ylim = the.ylim
        )
        legend("topright", legend = paste(names.b.minus.a.combos, rep(" ; PostHoc P.value:", number.of.X.levels),round(The.post.hoc.P.values,5)) , fill =  is.signif.color )
        abline(h = 0, col = "blue")
        
      }
      
      list.to.return <- list(Friedman.Test = the.sym.test, PostHoc.Test = The.post.hoc.P.values)
      if(to.print.friedman) {print(list.to.return)}
      return(list.to.return)
      
    }	else {
      print("The results where not significant, There is no need for a post hoc test")
      return(the.sym.test)
    }
  }
  
  # Original credit (for linking online, to the package that performs the post hoc test) goes to "David Winsemius", see:
  # http://tolstoy.newcastle.edu.au/R/e8/help/09/10/1416.html
}

friedman.test.with.post.hoc(value ~ X2 | X1, rt1)


# 본페로니 검정

# 그룹별 반복 측정. 유의수준을 비교하는 갯수로 나눔. 
# 3개의 그룹으로 비교한다면 3개의 짝이 만들어짐.
# ex) 유의수준 : 0.05 / 3 = 0.017

# 정하는 가설의 숫자가 늘어나면 귀무가설이 기각될 확률이 증가하는 (즉 귀무가설이 옳은데도 기각하는) 제 1종 오류의 가능성을 보정하기 위해 통계적 유의확률을 0.05에서 훨씬 낮추는 방법 


ow <- read.csv('../data/onewaySample.csv', header=T)
View(ow)

ow <- ow[, 2:6]
View(ow)

means <- c(mean(ow$score0),
           mean(ow$score1),
           mean(ow$score3),
           mean(ow$score6))
means

# install.packages('gplots')
library(gplots)

plotCI(x=means, type = 'l', ylab='score', xlab='month', main='One Way Test')


multimodel <- lm(cbind(ow$score0, ow$score1, ow$score3, ow$score6) ~ 1)
trials <- factor(c('score0', 'score1', 'score3', 'score6'), ordered = F)

# install.packages('car')
library(car)
model1 <- Anova(multimodel, idata = data.frame(trials), idesign = ~trials, type = 'III')

summary(model1, multivariate=F)

library(tidyr)
owlong <- gather(ow, key='ID', value='score')
View(owlong)

owlong <- owlong[8:35, ]

out <- aov(score~ID, data=owlong)
shapiro.test(resid(out))

summary(out)

TukeyHSD(out)





# Telco-Customer-Churn 데이터
library(dplyr)
library(moonBook)

telco <- read.csv('../data/Telco-Customer-Churn.csv', header=T)
View(telco)
str(telco)


## PaymentMethod에 따라 TotalCharges 차이 비교
# one way anova 방식
unique(telco$PaymentMethod)
telco %>% select(PaymentMethod, TotalCharges) %>% group_by(PaymentMethod) %>% summarise(n=n(), mean=mean(TotalCharges, na.rm=T))

moonBook::densityplot(TotalCharges ~ PaymentMethod, data=telco)

# 표본의 개수가 너무 많은 경우에는 shapiro 테스트로 정규성 확인이 의미가 없음
# 데이터가 많으면 몇몇 이상치 때문에 정규분포가 아니라고 판단함.
#out = aov(TotalCharges ~ PaymentMethod, data=telco)
#shapiro.test(resid(out))

# 분리해서는 검사가능
with(telco, shapiro.test(TotalCharges[PaymentMethod == 'Bank transfer (automatic)'])) # 정규분포가 아니다. (2.2e-16) -> R에서 표현할 수있는 가장 작은 값
with(telco, shapiro.test(TotalCharges[PaymentMethod == 'Credit card (automatic)'])) # 정규분포가 아니다. (2.2e-16)
with(telco, shapiro.test(TotalCharges[PaymentMethod == 'Electronic check'])) # 정규분포가 아니다. (2.2e-16)
with(telco, shapiro.test(TotalCharges[PaymentMethod == 'Mailed check'])) # 정규분포가 아니다. (2.2e-16)

# 샘플을 추출해서 정규분포 확인
x = telco$TotalCharges[telco$PaymentMethod == 'Bank transfer (automatic)']
x1 <- sample(x, 30 , replace = F)
shapiro.test(x1)

# 앤더슨 달링 테스트 (댜량의 데이터 정규분포 확인)
nortest::ad.test(x)

bartlett.test(TotalCharges ~ PaymentMethod, data=telco) #등분산 아님 (2.2e-16)

#welch's anova
oneway.test(TotalCharges ~ PaymentMethod, data=telco, var.equal = F)

library(nparcomp)
result <- mctp(TotalCharges ~ PaymentMethod, data=telco)
summary(result)
# 계좌이체와 신용카드 지불만 차이가 없고 나머지는 다 차이가 있다.

# kruscal test
library(pgirmess)
kruskal.test(TotalCharges ~ PaymentMethod, data=telco)
kruskalmc(telco$TotalCharges, telco$PaymentMethod)


#### two way repeated measured anova ####

acne <- read.csv('../data/10_rmanova.csv')
acne
library(reshape2)
ac1 <- reshape(acne, direction = 'long', varying = 3:6, sep ='')
ac1

ac2 <- reshape2::melt(acne, id=c('group', 'id'), variable.name='time', value.name='month', measure.vars=c('month0', 'month1', 'month3', 'month6'))
ac2

# 변수를 카테고리 형으로 변경
str(ac1)
class(ac1$group)
ac1$group <- factor(ac1$group)
ac1$id <- factor(ac1$id)
ac1$time <- factor(ac1$time)

interaction.plot(ac1$time, ac1$group, ac1$month)
str(ac1)

# 두개의 변수를 묶어서 분석
out <- aov(month ~ group*time, data = ac1)
summary(out)

# 변수 분리해서 각각 t-test로 검정
ac_0 <- ac1[ac1$time == '0', ]
ac_1 <- ac1[ac1$time == '1', ]
ac_3 <- ac1[ac1$time == '3', ]
ac_6 <- ac1[ac1$time == '6', ]

ac_0
ac_1

t.test(month ~ group, data=ac_0)
t.test(month ~ group, data=ac_1)
t.test(month ~ group, data=ac_3)
t.test(month ~ group, data=ac_6)
# 본페로니 검정
0.05/6
