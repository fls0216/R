# 난수 발생
x <- runif(1) # 0~1까지의 균일분포내에서의 난수생성, rnorm() : 정규분포 내에서 난수생성
x

# x가 0보다 크면 절대값(abs())으로 값 출력
if (x > 0){(abs(x))}

# x가 0.5보다 작으면 1-x를 출력하고 그렇지 않으면 x 출력
if (x<0.5){(1-x)} else {(x)}

ifelse (x<0.5, 1-x, x)

#### 조건문 ####
# 다중 조건
avg <- scan()
if (avg >90){
  print('당신의 학점은 A 입니다.')
    }else if (avg > 80){('당신은 B 학점 입니다.')
    }else if(avg > 70){('당신은 C 학점 입니다.')
    }else if (avg > 60){('당신은 D 학점 입니다.')
    }else {('당신의 학점은 F입니다.')}

# switch(비교문, 실행문, 실행문, 실행문.....)
a<- '중1'
switch(a, '중1'= print('14살'), '중2'= print('15살'))
switch(a, '중1'= '14살', '중2'= '15살')

# 인덱스도 이용 가능
b <- 3
switch(b, '14살', '15살', '16살')

empname <- scan(what = '')
switch(empname, hong=250, kim = 200, kang = 400)

avg <- scan()%%10
result <- switch(as.character(avg), '10'='A', '9'='A', '8'='B', '7'='C', '6'='D', 'F')
cat('당신의 학점은', result,' 입니다.')


# which() 
x <- c(2:10)
x

which(x == 3)
x[which(x==3)]


m <- matrix(1:12, 3,4)
m
which(m%%3==0)
which(m%%3==0, arr.ind = T)

no <- c(1:5)
name <- c('홍길동', '유비', '관우', '장비', '조자룡')
score <- c(85,78,89,90,74)
exam<- data.frame(학번=no, 이름=name, 성적=score)
exam

which(exam[2]=='장비')
which(exam$이름=='장비')

data(trees)
head(trees)
tail(trees)

# Hight 컬럼이 70 미만인 행 검색
which(trees$Height<=70)
trees[which(trees$Height<=70),]

which.max(trees$Height)
trees[31, ]


#any(), all()
x <- runif(5)
x

# x 값 중에서 0.8 이상이 있는가?
any(x >=0.8)

# x의 값이 모두 0.9 이하인가
all(x <=0.9)


#### 반복문! ####
# for()
sum <- 0
for(i in seq(1,10)){if (i%%2 == 0){ sum <- sum + i}}
sum



#### 함수 ####
# 인자 없는 함수
test1 <-function(){x <- 10
          y<-10
          return (x*y)}

test1()


# 인자가 있는 경우

test2 <-function(x, y){xx <- x
yy<-y
return (sum(xx,yy))}
test2(10,20)
test2(x=20, y=30)

# 가변길이 : ...

test3 <- function(...){
 arge <-  list(...)
 for (i in arge){print(i)}
  
}

test3(20)
test3(20,30)
test3(40,50,60)
test3(7,'홍',8)

test3 <- function(...){
  arge <-  list(...)
  for (i in arge){print(i)}
  
}
# 중첩함
test4 <- function(a,b,...){
  print(a)
  print(b)
  print('-----------------')
  test3(...)
  
}
test4(10,20,30,40,50)
