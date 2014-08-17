ucb<-read.csv("C:/Users/Toh Wei Zhong/Desktop/ucb.txt",
		  sep=",",
		  header=T,
		  stringsAsFactors=F)

#empty data frame
ucbMelted<-NULL
ucbMelted<-data.frame(Admit=character(),
		   Gender=character(),
		   Dept=character(),
		   stringsAsFactors=F)

#melting
for(i in seq(nrow(ucb))){
	num<-ucb$Freq[i]
	for(j in seq(num)){
		ucbMelted<-rbind(ucbMelted,ucb[i,-4])
	}
}

str(ucbMelted)

write.table(ucbMelted,
		"C:/Users/Toh Wei Zhong/Desktop/ucbMelted.csv",
		row.names=F,
		sep=",")