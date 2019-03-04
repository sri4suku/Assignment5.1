vowelLtr<-c("a","e","i","o","u")
vowelCnt<-c(0,0,0,0,0)
df <- data.frame(vowelCnt)

getVowelCnt<- function(x) {
  for ( i in 1:nchar(x)) {
    v <- substr(x,i,i)
      for ( j in 1:length(vowelLtr)) {
        if (toupper(vowelLtr[j]) == toupper(v)) {
          vowelCnt[j] <- vowelCnt[j] + 1
          break
        }
      }
  }
  return (vowelCnt)
}
mainFunc <- function(){
  stNms<-rownames(USArrests)
  df<- cbind(df,lapply(stNms,function(x) vowelCnt<- getVowelCnt(x)))
  print(df)
  vowelCnt<-apply(df,1,sum)
  per <- round(vowelCnt/sum(vowelCnt)*100)
  lbls<-paste(vowelLtr, per, "%", sep =" ")
  pie(vowelCnt, labels=lbls, main="Vowel Distribution")
}

mainFunc()




