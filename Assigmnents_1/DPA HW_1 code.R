V <- 99:10000
AB <- sample(V)
top <- "Right Left"
gga<- "Right"
setdiff(top,gga)
gsub(". ","",top)
grep(" ","",top)
strsplit(top, split)
strsplit(top, split, fixed = FALSE, perl = FALSE, useBytes = FALSE)
str_split(top, " ")

kap<-unlist(strsplit(top, " "))
n<-1547



s<-'abbb'
length(s)
n<-nchar(s)
fo<-NULL
for (i in 1:(n-1)){
  fo = append(fo,substr(s,i,i+1))
}
print(table(fo))
fffo = substr(s,1,3)
chr<-strsplit(s, "")
chars<-unlist(chr)
count<-0
for (x in 1:n){
  for (y in n:1){
    if (chars[x] == chars[y] && y >=x) {
        ft<-x
        lt<-y
        x<-x+1
        count<-count+1
        if (x == y){
          break
        }
      }
  }
  if(count>0){
    pla<-substring(s,ft,lt)
    pal<-unique(chars)
    pal<-append(pal,pla)
    
  }
}
pal<-unique(chars)
pal<-append(pal,"k")
for (y in n:1){
  print(chars[y])
}


datafram$IncomeLevel[levels()]
levels(datafram$IncomeLevel)
plot(datafram$`Hours-Per-Week`,levels(datafram$IncomeLevel))

fo<-table(datafram$IncomeLevel)
fo<-NULL

length(bigListOfWords)
for (i in 1:length(bigListOfWords)){
  
}
strsplit()
[[:alnum:]]
grepl("c[[:alnum:]]",s)

temp<-NULL
lapply(bigListOfWords, tou)
for (k in 1:length(allQwords)){
  if (grepl("qu",allQwords[2])){
    print(allQwords[k])
    temp<-temp+1
  }
}  


grepl("q[^[:alnum:] ]","abquijhq@t")

ttr<-strsplit(allQwords[1], "")
unlist(allQwords[1])
paste("ab","dc")
c(s)
s[]

grepl("q","Que",ignore.case = TRUE)
chars
is.character(".")
is.l