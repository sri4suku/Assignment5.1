stNms<-rownames(USArrests)

getVowelCnt<- function(x) {
  vCnt = 0
  for ( i in 1:nchar(x)) {
    px <- isVowel(substr(x,i,i)) 
    if (px == TRUE )  {
      vCnt <- vCnt + 1
    }
  }
  print (paste(x , vCnt ))
}

isVowel <- function(vchr) {
  gRes<-grep (pattern = vchr,x="aeiou",ignore.case = T)
  if (length(gRes) > 0 ){
    isVowel = TRUE
  }else{
    isVowel = FALSE
  }
  return (isVowel)
}

l<-lapply(stNms,function(x) getVowelCnt(x))

l
l<-as.numeric(lapply(stNms,function(x) getVowelCnt(x)))
#hist(l)
l
barplot(l,main="Vowel distribution",xlab="State Names", ylab="No of Vowels")
