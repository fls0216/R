#### 변수 ####

# 실행시에는 ctrl + enter
# 콘솔창 정리는 ctrl + l
# Ctrl+shift +c = 전체 주석처리
goods = '냉장고'

# 변수 사용시 객체형태로 사용하는 것을 권장
goods.name = '냉장고'
goods.code = 'ref001'
goods.price = 600000

goods.name

# 값을 대입할 때에는  = 보다는 <-를 사용하자!

goods.name <- '냉장고'
goods.code <- 'ref001'
goods.price <- 600000

# 데이터 타입 확인할 때
class(goods.name)
class(goods.price)
mode(goods.name)
mode(goods.price)

# 도움말 활용
help(sum)
?(sum)
# 인자 값만 콘솔에 반환
args(sum)
# 활용예제를 콘솔에 반환
example(sum)

#### 데이터 타입 ####
# 스칼라(0차원), 벡터(1차원), 행렬(2차원, 같은 형식만 묶을 수 있음), 데이터프레임(2차원, 여러 형식을 같이 묶을 수 있음), 
# 배열(3차원 이상, 같은 형식끼리만 묶을 수 있음), 리스트(3차원 이상, 여러 형식을 같이 묶을 수 있음)

# 벡터
# 1) 기본 자료 구조
# 2) 1차원 배열
# 3) 인덱스로 접근 (1 부터 시작)
# 4) 동일한 자료형만 저장 가능
# 5) 벡터 생성 함수 : c(), seq(), rep()

v<- c(1,2,3,4,5)
v
mode(v)
class(v)

# 변수를 저장하면서 동시에 콘솔에 출력하기
(v<- c(1,2,3,4,5))

# 범위 연산자 n:m 사용 가능
mode(c(1:5))

# 벡터는 같은 자료형만 사용 가능하기 때문에 변수를 문자형으로 변경함
class(c(1,2,3,4,'5'))

# seq() : 순차적으로 변수를 만들어줌
(seq(from=1, to = 10, by = 2)) # = seq(1,10,2)

# rep() : 반복해서 생성
rep(1:3,3)

# 벡터에 접근
c<- c(1:50)
c[10:45]
c[10:(length(c)-5)]
c[10:length(c)-5]

b<-c(13,-5,20:32,12,-2:3)
b
b[1]
# 반드시 벡터형식으로 넘겨주어야 인덱싱 가능
b[c(2,4)]
b[c(4,5:8,7)]
# -n 는 n 번째의 벡터를 제외하고 호출함
b[-1]
b[-2]
b[c(-2,-4)]

# 집합 연산
x <- c(1,3,5,7)
y <- c(3,5)
# 합집합, 차집합, 교집합
# ; 은 한 문장이 끝나고 다른 문장이 시작한다는 의미임
union(x,y);setdiff(x,y);intersect(x,y)

# 컬럼명 지정
age <- c(30,35,40)
age
names(age) <- c('홍길동','임꺽정','신돌석석')
age

# 특정변수의 데이터 제거
age <- NULL
age

# 연속적인 데이터의 경우 c 함수 생략 가능
x <- (1:5)
x

#=============================================================================================

#### 팩터(factor)####
# 1) 범주형 데이터 타입(명목형, 순서형)
gender <- c('man', 'women','women', 'man', 'man')
gender
class(gender)
is.factor(gender)
# 그래프 그리기
plot(gender)

# 형태 변환하기
ngender <- as.factor(gender)
ngender
# 클래스 이름(추상적인 데이터 형) 확인
class(ngender)
# 물리적인 데이터의 형식
mode(ngender)
is.factor(ngender)
plot(ngender)
# 각 명목 변수의 개수 변환
table(ngender)

ofactor <- factor(gender, levels = c('women', 'man'), ordered=TRUE)
ofactor

#==========================================================================================
#### 기본 자료형 ####
# numeric, character, factor, logical
# 특수 자료형
# NA, NULL, NaN, Inf

#=========================================================================================
##### Matrix ####
#1) 행과 열의 2차원배열
#2) 동일한 데이터 타입만 저장 가능
#3) metrix(), rbind(), cbind()

m <- matrix(c(1:5))
m

# 행을 2개로 강제로 맞춰줌
m<- matrix(c(1:11), nrow = 2)
m


# 데이터를 행 방향으로 2행 만듬
m <- matrix(c(1:11), nrow=2, byrow=T)
m

class(m)
mode(m)

# 행 열을 합쳐서 생성
x1 <- c(3,4, 50:52)
x2 <- c(30,5, 6:8, 7, 8)

x1
x2

mr <- rbind(x1, x2)
mr

mc <- cbind(x1,x2)
mc

# 외부 데이터 읽어오기
aws <- read.delim('../data/AWS_sample.txt', sep='#')
head(aws)

# 행렬[행,열]
x1 <- aws[1:3,2:4]
x2 <- aws[9:11, 2:4]


# 열로 합치기!
cbind(x1,x2)
# 행으로 합치기!
rbind(x1,x2)

class(cbind(x1,x2))

# matrix 활용
x<- matrix(c(1:9), ncol=3)
x
length(x);nrow(x);ncol(x);dim(x)

# 컬럼명 지정
colnames(x) <- c('one','two','three')
x

# 함수를 호출하여 행렬에 적용하기 1 = 열기준, 2 = 행기준
apply(x, 1, max)
apply(x, 2, max)
apply(x, 1, mean)
apply(x, 2, mean)

#=================================================================================================
##### data.frame ####
#1) DB의 테이블과 유사한 구조 제공
#2) R에서 가장 많이 사용되는 구조
#3) 컬럼 단위로 서로 다른 데이터 타입 사용이 가능
#4) data.frame(), read.csv(), read.delim(), read.table()...

np <- c(1,2,3)
name <- c('hong', 'lee', 'kim')
pay <- c(150,250,300)

emp <- data.frame(No = np, Name = name, Payment = pay)
emp

# 현재 작업 위치 확인
getwd()
# 현재 위치 변경하기
setwd('../data')
# 외부파일을 이용하여 데이터 프레임 제작
txtemp <-  read.table('emp.txt', header = T, sep = ' ')
emp
class(txtemp)
csvemp = read.csv('emp.csv')
csvemp

csvemp2 = read.csv('emp.csv', header = T, col.names =c('사번', '이름','급여')) 
csvemp2

csvemp3 = read.csv('emp2.csv', header = F, col.names = c('사번','이름','급여'))
csvemp3

# 데이터 프레임을 보기 편하게 표 형식으로 반환
View(csvemp3)

# 접근하기
csvemp3$사번
class(csvemp3$사번)

csvemp3[,1]

# 데이터프레임의 구조
str(csvemp3)

# 기본 통계량 확인
summary(csvemp3)


#apply()
df <- data.frame(x=c(1:5), y=seq(2,10,2),z=c('a','b','c','d','e'))
df

apply(df[,c(1,2)], 2, sum)
apply(df[,c(1,2)], 1, sum)

# 데이터의 일부 추출
x1 <- subset(df, x>=3)
x1

x2 <- subset(df, x>=2 & y<=6)
x2

# 병합
height <- data.frame(id=c(1,2), h=c(180,175))
height
weight <- data.frame(id=c(1,2), w=c(80,75))
weight

user<-merge(height, weight, by.x='id', by.y='id')
user

#================================================================
#### array ####
# 1) 행 열 면의 3차원 배열 형태의 객체 생성
# 2) array()

v<-c(1:12)
arr <- array(v, c(3,2,3))
arr
arr[,,1]
arr[2,2,1]
arr[2,1,2]

#================================================================================
##### list ####
#1) key 와 value가 한쌍으로 구성
#2) 파이썬에서의 dict와 유사
#3) list()

x0 <- 1
x1 <- data.frame(var1 = c(1,2,3), var2 = c('a','b','c'))
x2 <- matrix(c(1:12), ncol=2)
x3 <- array(1:20, dim = c(2,5,2))

x4 <- list(c1 = x0, c2=x1, c3=x2, c4=x3)
x4

x4$c1
x4$c2

list1 <- list('lee', '이순신',95)
list1
list1[1]
list1[[1]]

# 리스트를 해제하여 벡터 형식으로 반환
un <- unlist(list1)
class(un)

# apply : lapply(), sapply()
a <- list(c(1:5))
b <- list(c(6:10))
a
b

c <- c(a,b)
c
class(c)

x <- lapply(c, max)
x
y <- sapply(c, max)
y

#=================================================================================
#### 기타 자료형 및 함수들 ####
# 날짜형
# 현재 날짜 반환
Sys.Date()
# 현재 시간 반환
Sys.time()

a <- '20/7/13'
class(a)
b <- as.Date(a)
class(b)
c <- as.Date(a, '%y/%m/%d')
c

# 문자열 처리 함수
#stringr
install.packages('stringr')
# library(패키지명), require(패키지명)
library(stringr)

str1 <- '홍길동35이순신45임꺽정25'
str1
str_extract(str1, '\\d{2}')
str_extract_all(str1, '\\d{2}')
str_extract_all(str1, '[1-9]{2}')

str2 <- c('hongkd105leess1002you25TOM400강감찬2005')
# 세자리 문자만 추출
str_extract_all(str2, '[a-zA-Z가-힣]{3}')
str_extract_all(str2,'[a-zA-Z가-힣]{3,}')


str_length(str2)
length(str2)
str_locate(str2, '강감찬')
str_c(str2,'유비55')
str2


str3 <- c('hongkd105','leess1002','you25','TOM400','강감찬2005')
str_split(str3, ',')

# 기본함수
sample = data.frame(cl=c('abc_sdfsdf', 'abc_KKDFSfsfs', 'ccd'), c2=1:3)
sample

# 문자열의 갯수 반환
nchar(sample[1,1])
which(sample[,1] == 'ccd')

toupper(sample[1,1])
tolower(sample[2,1])
substr(sample[,1], start=1, stop=2)

# 문자열 분리, 병합
install.packages('splitstackshape')
library(splitstackshape)

cSplit(sample, splitCols = 'cl', sep = '_')

paste0(sample[,1], sample[,2])
paste(sample[,1], sample[,2], sep = '@@')
