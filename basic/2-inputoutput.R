#### 키보드 입력 ####
# scan() : 벡터로 입력
# edit() : 데이터 프레임으로 입력받음

# 정수형만 입력가능
a <- scan()

# 문자 입력
b <- scan(what=character())


df <-data.frame()
df <- edit(df)
df


#### 파일 입력 ####
# read.csv() : csv 형식의 파일을 읽어옴
# read.table() : csv가 아닌 형식의 파일을 읽어옴
# read.xlsx() : 엑셀파일을 불러옴

student <- read.table("student.txt")
student

# 제목이 있는 파일 불러오기
student1 <- read.table('student1.txt', header = T)
student1

#파일을 직접 선택하기
student2<- read.table(file=file.choose(), header = T, sep = ';')
student2

# 결측치가 있는 파일 열기
student3 <- read.table(file.choose(), header = T, sep = '', na.strings = '-')
student3

# 결측치가 여러개인 경우
student3<- read.table(file.choose(), header = T, sep = '', na.strings = c('-', '+', '&'))
student3

# read.xlsx()
install.packages('xlsx')
library(rJava)
library(xlsx)

studentx <-read.xlsx(file.choose(), sheetIndex=1, encoding = 'UTF-8')
studentx

# 시트에 이름이 있을 경우 시트 이름을 통해 접근 가능
studentx2 <-read.xlsx(file.choose(), sheetName = 'emp2', encoding = 'UTF-8')
studentx2


#### 웹문서 읽기 ####
install.packages('httr')
install.packages('XML')
library(httr)
library(XML)

url = 'https://ssti.org/blog/useful-stats-capita-personal-income-state-2010-2015'

get_url <- GET(url)
get_url

html_content <- readHTMLTable(rawToChar(get_url$content))


class(html_content)
str(html_content)

df <- as.data.frame(html_content)
df


#### 화면 출력 ####
# 변수명
# ()
# cat()
# print()

x<- 10
y<- 20
z<- x+y

z
(z<- x+y)

print(z)

# 함수 내의 변수들을 연결해서 출력 가능
cat('x+y의 결과는', as.character(z), '입니다.')


#### 파일 출력 ####
# write.table()
# write.csv()

studentx <-read.xlsx(file.choose(), sheetIndex=1, encoding = 'UTF-8')
studentx
class(studentx)

write.table(studentx, 'stud1.txt')
# 인덱스 이름 없에기
write.table(studentx, 'stud2.txt', row.names = F)
# 큰따옴표 없이 저장
write.table(studentx, 'stud3.txt', row.names = F, quote = F)
# csv 파일로 저장
write.csv(studentx, 'stud4.csv', row.names = F, quote = F)

library(rJava)
library(xlsx)

write.xlsx(studentx, 'stud5.xlsx')




#### rda 파일 ####
# save()
# load()

save(studentx, file = 'stud6.rda')
# 메모리에서 변수 삭제
rm(studentx)
studentx
load('stud6.rda')
studentx


#### sink() ####
data("iris")
head(iris)
str(iris)

# 내가하는 모든 작업이 파일로 저장됨
sink('iris.txt')
head(iris)
tail(iris)
str(iris)
# 작업 종료
sink()

head(iris)
