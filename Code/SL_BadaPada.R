
#기존의 데이터를 모두 제거
rm(list=ls())







# 데이터 불러오기
#경로 확인및 지정
getwd()
setwd("/Users/seongminmun/Desktop/Development/Rprogram/R_Script/ChosunUniv/강의/2022/특강(바다-파다)/Data")
dir()

KoCorpus <- read.csv("KoCorpus_9_A2P03F.csv", header=FALSE, fileEncoding="UTF-8"); summary(wordSegOutcomes)
head(KoCorpus)
summary(KoCorpus)

#길이가 가장 긴 문장 찾기
maxIndex <- 0
maxNum <- 0
for (i in 1:length(KoCorpus$V3)){
  if(maxNum < nchar(KoCorpus$V3[i])){
    maxNum = nchar(KoCorpus$V3[i])
    maxIndex = i
  }
}

maxNum
maxIndex

text <- KoCorpus$V3[222]; text














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


#명사 추출하기
extractNoun(text)

# POS태그 정보
# https://www.sketchengine.eu/wp-content/uploads/Original-HanNanum-manual.pdf
# 3.4 태그 집합


#형태소 분석하기
MorphAnalyzer(text)

#22가지 분류코드로 분류하기
SimplePos22(text)

#9가지 분류코드로 분류하기
SimplePos09(text)

textPos09 <- SimplePos09(text)


# 명사 확인하기
textN <- str_extract_all(textPos09,'([A-Z가-힣]+)/N'); textN

# 용언 확인하기
textP <- str_extract_all(textPos09,'([A-Z가-힣]+)/P'); textP


#한글을 자음모음으로 나타내기 
convertHangulStringToJamos(text)

#한글을 키보드 획으로 나타내기 
convertHangulStringToKeyStrokes(text)

#자음 모음 조합 벡터로 생성하기
str <- convertHangulStringToJamos(text); str

#빈공간없이 정제
str2 <-paste(str, collapse=""); str2






#자모로 이루어진 행렬 만들기
jaList <- c("ㄱ", "ㄲ", "ㅋ", "ㄷ", "ㄸ", "ㅌ", "ㅈ", "ㅉ", "ㅊ", "ㅁ", "ㅍ", "ㅂ", "ㅃ", "ㅎ", "ㄴ", "ㄹ", "ㅇ", "ㅅ", "ㅆ")
moList <- c("ㅏ", "ㅐ", "ㅑ", "ㅒ", "ㅓ", "ㅔ", "ㅕ", "ㅖ", "ㅗ", "ㅘ", "ㅙ", "ㅚ", "ㅛ", "ㅜ", "ㅝ", "ㅞ", "ㅟ", "ㅠ", "ㅡ", "ㅢ", "ㅣ")

totalLength <- length(jaList) + length(moList)

jamoMatrix <- matrix(0,nrow=totalLength,ncol=totalLength); jamoMatrix

jamoList <- c(jaList, moList)

colnames(jamoMatrix) <- jamoList; jamoMatrix

rownames(jamoMatrix) <- jamoList; jamoMatrix








# 문자열 자모 단위로 출력하기
eachEle <- strsplit(str2, split="")

eachEle[[1]][1]



# 연속되는 자모의 조합 빈도 저장하기
currentNum <- 1
for (i in eachEle[[1]]){
  
  #현재의 문자가 마지막 문자가 아닌 경우
  if(currentNum != nchar(str2)){
    
    #인풋으로 들어오는 자모가 빈칸이 아닐 경우
    if (eachEle[[1]][currentNum]!=" " && eachEle[[1]][currentNum+1]!=" "){
      jamoMatrix[eachEle[[1]][currentNum],eachEle[[1]][currentNum+1]] = jamoMatrix[eachEle[[1]][currentNum],eachEle[[1]][currentNum+1]] + 1
    }
  }
  currentNum <- currentNum + 1
}






# 조합의 최대값 찾기
jamoMatrix
max(jamoMatrix)

maxNum <- max(jamoMatrix)


# 자모 조합의 사용빈도가 최대인 자모 검색하기
#행
for (i in 1:length(jamoMatrix[,1])){
  #열
  for (j in 1:length(jamoMatrix[1,])){
   
    if (jamoMatrix[i,j] == maxNum){
      
      cat(names(jamoMatrix[1,])[i],names(jamoMatrix[,1])[j],"\n")
    }
  }
}


jamoMatrix["ㅇ","ㅣ"]




#말뭉치에서 해당 자모 조합이 사용된 단어 찾기
str

#단어 단위로 나누어져 있지 않다

# 이전에 생성한 자모 조합을 단어 단위로 변경하기 -> str

words <- NULL
word <- NULL
for (i in 1:length(str)){
  if(str[i]==" "){
    words <- c(words,word)
    word <- NULL
  } else {
    word <- paste(word,str[i],sep="")
  }
}
words



targetString <- 'ㅇㅣ'
wordList <- NULL
for (i in 1:length(words)){
  if(str_detect(words[i],targetString)==TRUE){
    wordList <- c(wordList,words[i])
  }
}

wordList
text

#다시 한글로 재 배열
for (i in 1:length(wordList)){
  print(HangulAutomata(wordList[i]))
}




