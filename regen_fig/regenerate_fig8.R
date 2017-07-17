

df <- read.csv("regen_fig/probabilities.txt", sep = "\t")

rownames(df) <- c(" Adm-clerical", " Craft-repair")

barplot(as.matrix(df), beside = TRUE,
        main = "income ~ occupation",
        ylab = paste("Pr(income ==  >=50K)"),
        col = terrain.colors(2), ylim = c(0,1),
        cex.axis = 1.2, cex.names = 1.2)
legend("topright", rownames(df), cex=0.8, 
       fill=terrain.colors(2))

pv <- c(0,0)

pv.stars<-sapply(pv,FUN=function(pv){
  stars<-""
  if(is.na(pv)) return("")
  if(pv<0.05)  stars<-paste(stars,"*",sep="")
  if(pv<0.01)  stars<-paste(stars,"*",sep="")
  if(pv<0.001) stars<-paste(stars,"*",sep="")
  stars
})

for(i in seq(2)){
  if(i == 1){incre<-1}
  else{incre<-incre+2}
  text(x=i+incre,y=max(df[,i])+0.1,pv.stars[i], cex = 1.2)
}
