df<-read.csv("C:/Users/Toh Wei Zhong/Desktop/adult.txt",stringsAsFactors=T,sep=",")
table(df$workclass)

ss<-df[,c("workclass","marital.status")]
table(ss)

whichcmpclasses<-c("Federal-gov","Local-gov")
which(ss[,"workclass"] == whichcmpclasses)