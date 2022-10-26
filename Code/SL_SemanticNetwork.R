





#기존의 데이터를 모두 제거
rm(list=ls())




# 데이터 불러오기
#경로 확인및 지정
getwd()
setwd("/Users/seongminmun/Desktop/Class/Data")
dir()

dataSN <- read.csv("DataForSN.csv", header=TRUE, fileEncoding="UTF-8"); summary(dataSN)
head(dataSN)
summary(dataSN)
summary(as.factor(dataSN$targetWord))

#파일을 2개로 나누기
kkamkkam <- subset(dataSN,targetWord=="깜깜")
summary(as.factor(kkamkkam$targetWord))

etwup <- subset(dataSN,targetWord=="어두"|targetWord=="어둡")
summary(as.factor(etwup$targetWord))











#KoNLP - 한글 자연어 처리 툴킷

#한글깨짐현상
#한글 인코딩 문제 해결
#맥
Sys.setlocale(category = "LC_CTYPE", locale = "ko_KR.UTF-8")
#윈도우
#options(encoding = "UTF-8")


##JRE install
#http://www.oracle.com/technetwork/java/javase/downloads/jre9-downloads-3848532.html
#https://www.oracle.com/java/technologies/downloads/
dyn.load('/Library/Java/JavaVirtualMachines/jdk1.8.0_241.jdk/Contents/Home/jre/lib/server/libjvm.dylib')


#전희원 선생님: https://github.com/haven-jeon/KoNLP
#카이스트 CI연구소 - 한나움 형태소 분석기(http://semanticweb.kaist.ac.kr/home/index.php/HanNanum)

# install.packages("remotes")
library(remotes)

# install.packages("rJava")
library(rJava)

# remotes::install_github('haven-jeon/KoNLP', upgrade = "never", INSTALL_opts=c("--no-multiarch"))
library(KoNLP)

# install.packages("stringr")
library(stringr)



text <- kkamkkam$Sentence[1]

#형태소 분석하기
MorphAnalyzer(text)

#22가지 분류코드로 분류하기
SimplePos22(text)

#9가지 분류코드로 분류하기
SimplePos09(text)







#형태소 태그 형태로 변환하기
#22가지 분류코드로 분류하기
# kkamkkam vs etwup

#빈 변수 생성하기
kkamkkamPOS <- NULL
for (i in 1: length(kkamkkam$Sentence)){
  input <- kkamkkam$Sentence[i]
  
  textPos22 <- SimplePos22(input); textPos22
  textPOS <- NULL
  for (j in 1:length(textPos22)){
    #리스트의 형태로 분석하기 (품사태깅)
    process1<-textPos22[j]
    #리스트를 문자 그대로의 형태로 변환하기
    process2<-as.character(process1);process2
    #품사 사이의 +기호 제거하기
    process3<-gsub("\\+"," ",process2);process3
    #텍스트의 형태로 이어 붙이기
    textPOS <- paste(textPOS,process3,sep=" ")
  }
  #앞 뒤의 공간 제거하기
  textPOS <- gsub("^\\s","",textPOS)
  textPOS <- gsub("\\s$","",textPOS)
  #빈 벡터에 하나씩 품사 태깅된 문장 저장하기
  kkamkkamPOS <- c(kkamkkamPOS,textPOS)
}
head(kkamkkamPOS)
# write.table(kkamkkamPOS,"kkamkkamPOS.txt",fileEncoding = "UTF-8")








#빈 변수 생성하기
etwupPOS <- NULL
for (i in 1: length(etwup$Sentence)){
  input <- etwup$Sentence[i]
  
  textPos22 <- SimplePos22(input); textPos22
  textPOS <- NULL
  for (j in 1:length(textPos22)){
    #리스트의 형태로 분석하기 (품사태깅)
    process1<-textPos22[j]
    #리스트를 문자 그대로의 형태로 변환하기
    process2<-as.character(process1);process2
    #품사 사이의 +기호 제거하기
    process3<-gsub("\\+"," ",process2);process3
    #텍스트의 형태로 이어 붙이기
    textPOS <- paste(textPOS,process3,sep=" ")
  }
  #앞 뒤의 공간 제거하기
  textPOS <- gsub("^\\s","",textPOS)
  textPOS <- gsub("\\s$","",textPOS)
  #빈 벡터에 하나씩 품사 태깅된 문장 저장하기
  etwupPOS <- c(etwupPOS,textPOS)
}
head(etwupPOS)
# write.table(etwupPOS,"etwupPOS.txt",fileEncoding = "UTF-8")












#문맥 단어만 추출하기; 동사, 형용사, 명사
#명사와 용언만 추출하기
# POS태그 정보
# https://www.sketchengine.eu/wp-content/uploads/Original-HanNanum-manual.pdf
# 3.4 태그 집합

# kkamkkam vs etwup

kkamkkamN_P <- NULL
for (i in 1: length(kkamkkamPOS)){
  input <- kkamkkamPOS[i]
  if (input!=""){
    inputWords <- strsplit(input, split=" ")
    inputWords <- unlist(inputWords)
    cleanedInput <- NULL
    for (j in 1:length(inputWords)){
      if(inputWords[j]!=""){
        if(str_detect(inputWords[j],'([A-Z가-힣]+)/N')==TRUE | str_detect(inputWords[j],'([A-Z가-힣]+)/P')==TRUE){
          cleanedInput <- paste(cleanedInput, inputWords[j], sep=" ")
        }
      }
    }
  }
  kkamkkamN_P <- c(kkamkkamN_P, cleanedInput)
}
head(kkamkkamN_P)
# write.table(kkamkkamN_P,"kkamkkamN_P.txt",fileEncoding = "UTF-8")




etwupN_P <- NULL
for (i in 1: length(etwupPOS)){
  input <- etwupPOS[i]
  if (input!=""){
    inputWords <- strsplit(input, split=" ")
    inputWords <- unlist(inputWords)
    cleanedInput <- NULL
    for (j in 1:length(inputWords)){
      if(inputWords[j]!=""){
        if(str_detect(inputWords[j],'([A-Z가-힣]+)/N')==TRUE | str_detect(inputWords[j],'([A-Z가-힣]+)/P')==TRUE){
          cleanedInput <- paste(cleanedInput, inputWords[j], sep=" ")
        }
      }
    }
  }
  etwupN_P <- c(etwupN_P, cleanedInput)
}
head(etwupN_P)
# write.table(etwupN_P,"etwupN_P.txt",fileEncoding = "UTF-8")








# 기호 제거하기
#kkamkkam
kkamkkamN_P <- gsub("[^가-힣]", " ",kkamkkamN_P)
kkamkkamN_P <- gsub("\\s+", " ",kkamkkamN_P)
kkamkkamN_P <- gsub("^\\s", "",kkamkkamN_P)
kkamkkamN_P <- gsub("\\s$", "",kkamkkamN_P); head(kkamkkamN_P)



etwupN_P <- gsub("[^가-힣]", " ",etwupN_P)
etwupN_P <- gsub("\\s+", " ",etwupN_P)
etwupN_P <- gsub("^\\s", "",etwupN_P)
etwupN_P <- gsub("\\s$", "",etwupN_P); head(etwupN_P)













# word matrix생성하기
#tm패키지 설치
#install.packages("tm")
library(tm)

# kkamkkam vs etwup

#코퍼스데이터 생성(말뭉치)
kkamkkamN_P_corpus <- Corpus(VectorSource(kkamkkamN_P))
kkamkkamN_P_corpus

#TermDocumentMatrix를 활용하여 수치형 데이터로 형변환 
kkamkkamN_P_Tdm <- TermDocumentMatrix(kkamkkamN_P_corpus, control = list(wordLengths = c(2, Inf)))
kkamkkamN_P_Tdm



# 연관 단어 분석
#10번 이상 출현한 명사
findFreqTerms(kkamkkamN_P_Tdm, lowfreq = 10)
#20번 이상 출현한 명사
findFreqTerms(kkamkkamN_P_Tdm, lowfreq = 20)
#30번 이상 출현한 명사
findFreqTerms(kkamkkamN_P_Tdm, lowfreq = 30)




#'깜깜하'과 10% 연관성있는 명사 (상관계수값)
findAssocs(kkamkkamN_P_Tdm, "깜깜하", 0.1)
#'깜깜해지'과 10% 연관성있는 명사
findAssocs(kkamkkamN_P_Tdm, "깜깜해지", 0.1)















#단어 빈도수에 기반한 워드 크라우드
#패키지 설치
# install.packages("wordcloud")

#불러오기
library(wordcloud)

#단어의 출현빈도를 기반으로 생성된 Tdm데이터를 매트릭스형으로 변환
kkamkkamN_P_Tdm_M <- as.matrix(kkamkkamN_P_Tdm)

#단어들의 출현 빈도를 합한다.
kkamkkamN_P_wordFreq <- sort(rowSums(kkamkkamN_P_Tdm_M), decreasing = TRUE); kkamkkamN_P_wordFreq

#워드클라우드 색상지정
pal <- brewer.pal(8, "Dark2")
#pal <- brewer.pal(8, "Accent")#검 분 파 보초
#pal <- brewer.pal(12, "Paired")#갈노보초파
#pal <- brewer.pal(9, "Pastel1")#회노초파분
#pal <- brewer.pal(8, "Pastel2")#회노초주녹
#pal <- brewer.pal(9, "Set1")#회갈주초파빨
#pal <- brewer.pal(8, "Set2")#회노초파주녹
#pal <- brewer.pal(12, "Set3")#노회초빨보노녹
#pal <- brewer.pal(7,"Greens")#진초-연초
#출현횟수가 최소 10인 단어들을 대상으로 워드 클라우드 생성

#pdf("total_ggplot.pdf")

wordcloud(words = names(kkamkkamN_P_wordFreq), freq = kkamkkamN_P_wordFreq, min.freq = 4, random.order = F, rot.per = 0.1, colors = pal)

#dev.off()


# # 한글깨짐
# # install.packages("extrafont") 
# library(extrafont) 
# # font_import()
# 
# theme_set(theme_grey(base_family='NanumGothic'))

#그래프 폰트 깨짐 수정
par(family = "AppleGothic")

wordcloud(words = names(kkamkkamN_P_wordFreq), freq = kkamkkamN_P_wordFreq, min.freq = 4, random.order = F, rot.per = 0.1, colors = pal)

















# kkamkkam vs etwup

#코퍼스데이터 생성(말뭉치)
etwupN_P_corpus <- Corpus(VectorSource(etwupN_P))
etwupN_P_corpus

#TermDocumentMatrix를 활용하여 수치형 데이터로 형변환 
etwupN_P_Tdm <- TermDocumentMatrix(etwupN_P_corpus, control = list(wordLengths = c(2, Inf)))
etwupN_P_Tdm



# 연관 단어 분석
#10번 이상 출현한 명사
findFreqTerms(etwupN_P_Tdm, lowfreq = 10)
#20번 이상 출현한 명사
findFreqTerms(etwupN_P_Tdm, lowfreq = 20)
#30번 이상 출현한 명사
findFreqTerms(etwupN_P_Tdm, lowfreq = 30)




#'어둡'과 10% 연관성있는 명사 (상관계수값)
findAssocs(etwupN_P_Tdm, "어둡", 0.1)

#'어두웠'과 10% 연관성있는 명사
findAssocs(etwupN_P_Tdm, "어두웠", 0.1)





# 워드 클라우드
#단어의 출현빈도를 기반으로 생성된 Tdm데이터를 매트릭스형으로 변환
etwupN_P_Tdm_M <- as.matrix(etwupN_P_Tdm)

#단어들의 출현 빈도를 합한다.
etwupN_P_wordFreq <- sort(rowSums(etwupN_P_Tdm_M), decreasing = TRUE); etwupN_P_wordFreq

#워드클라우드 색상지정
pal <- brewer.pal(8, "Dark2")

#그래프 폰트 깨짐 수정
par(family = "AppleGothic")

wordcloud(words = names(etwupN_P_wordFreq), freq = etwupN_P_wordFreq, min.freq = 4, random.order = F, rot.per = 0.1, colors = pal)


















# kkamkkam vs etwup


#"어둡" vs "깜깜하" 유사도 구하기
#두 단어와 동시 출현 빈도가 높은 단어 추출

kkamkkamCoWords <- names(unlist(head(etwupN_P_wordFreq,100)))
etwupCoWords <- names(unlist(head(kkamkkamN_P_wordFreq,100)))




#합집합: 단어의 리스트를 중복없이 결합 -> 이 단어들을 기준으로 상호 유사도를 계산
totalWords <- union(kkamkkamCoWords, etwupCoWords)


kkamkkamCoList <- NULL
etwupCoList <- NULL
for (i in 1: length(totalWords)){
  
  tryCatch(
    kkamkkamCoList <- c(kkamkkamCoList,as.numeric(head(kkamkkamN_P_wordFreq,100)[totalWords[i]])),
    error = function(e){
      kkamkkamCoList <- c(kkamkkamCoList,NA)
    })
  
  tryCatch(
    etwupCoList <- c(etwupCoList,as.numeric(head(etwupN_P_wordFreq,100)[totalWords[i]])),
    error = function(e){
      etwupCoList <- c(etwupCoList,NA)
    })
}


kkamkkamCoList[is.na(kkamkkamCoList)] <- 0; kkamkkamCoList
etwupCoList[is.na(etwupCoList)] <- 0; etwupCoList

A <- kkamkkamCoList
B <- etwupCoList


#"깜깜하"와 "어둡"의 코사인 유사도
sum(A*B)/sqrt(sum(A^2)*sum(B^2))



















#install_github("christophergandrud/d3Network")

library(d3Network)

#데이터 생성하기
Source<-c("A","A","A","A","B","B","C","C","D")
Target<-c("B","C","D","J","E","F","G","H","I")
NetworkData<-data.frame(Source,Target)
head(NetworkData)

#네트워크 시각화 실행을 위한 함수
ericOpenHtml <- function(filename){
  if(Sys.info()["sysname"]=="windows"){
    shell.exec(filename)
  }else{
    system(paste("open",filename))
  }
}

# 네트워크 생성하기
d3SimpleNetwork(NetworkData,width=800,height=700,file="test1.html")
ericOpenHtml("test1.html")

#Sys.setenv(JAVA_HOME='C:/program Files/Java/jre-10')

#노드와 링크에 대한 색상 변경 
d3SimpleNetwork(NetworkData, width = 800, height = 700,textColour = "#D95F0E", linkColour = "#FEC44F",nodeColour = "red", opacity = 0.3,file="test2.html")
ericOpenHtml("test2.html")

# #charge(노드간의 인력과 척력)의 값을 음수로 약하게 지정
# d3SimpleNetwork(NetworkData, width = 400, height = 250,textColour = "#D95F0E", linkColour = "#FEC44F",nodeColour = "#D95F0E", opacity = 0.9,charge = -50, fontsize = 12,file="test3.html")
# ericOpenHtml("test3.html")














#두 단어와 동시 출현 빈도가 높은 단어 추출

kkamkkamCoWords <- names(unlist(head(etwupN_P_wordFreq,100)))
etwupCoWords <- names(unlist(head(kkamkkamN_P_wordFreq,100)))


#합집합 교집합 차집합
#합집합
union(kkamkkamCoWords, etwupCoWords)
#교집합
intersect(kkamkkamCoWords, etwupCoWords)

#차집합 difference of sets
#어둡에는 있고 깜깜하에는 없는 단어
setdiff(kkamkkamCoWords, etwupCoWords)

#깜깜하에는 있고 어둡에는 없는 단어
setdiff(etwupCoWords, kkamkkamCoWords)







#install_github("christophergandrud/d3Network")

library(d3Network)

#데이터 생성하기
Source<-c("어둡","어둡","어둡","어둡","어둡","어둡","어둡","어둡","깜깜하","깜깜하","깜깜하","깜깜하","깜깜하","깜깜하","깜깜하","깜깜하")
Target<-c("보이","빛","밤","느끼","표정","공간","등잔","그림자","보이","빛","밤","느끼","눈앞","동굴","소식","주위")
NetworkData<-data.frame(Source,Target)
head(NetworkData)


# 네트워크 생성하기
d3SimpleNetwork(NetworkData,width=1000,height=1000,file="outcome.html")
ericOpenHtml("outcome.html")






