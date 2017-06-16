
library("ggplot2")
readdata<-read.csv(file.choose())
readdata
rows<-length(readdata$Date)
processdata<-matrix(nrow=rows,ncol=4)
colnames(processdata)<-c("month","year","openvalue","closevalue")
processdata
j<-1
#readdata$Date[1]
#----------------------
#creating data to be used
#----------------------
#i = readdata$Date[1]
for(i in readdata$Date){

  open<-readdata[which(readdata[,"Date"] == i),c("Open.Price")]
  close<-readdata[which(readdata[,"Date"] == i),c("Close.Price")]
  i<-strsplit(as.character(i),"-")
  i<-unlist(i)
  processdata[j,]<-c(i[2],i[3],open,close)
  j<-j+1

}
print(j)
#----------------------
#proceesing the data
#----------------------

#months<-unique(processdata[,1])
#print(months)
years<-unique(processdata[,2])
#print(years)
k<-1
for(i in years){
  if(!is.na(i)){
  interm<-processdata[which(processdata[,"year"] == i),]
  #print(interm)
  months<-unique(interm[,"month"])
  intermdata<-matrix(nrow=length(unique(interm[,"month"])),ncol=4)
  colnames(intermdata)<-c("month","year","averageopenvalue","averageclosevalue")
  #print(intermdata)                   
  for(j in months){
    solution<-interm[which(interm[,"month"] == j),]
    #print(solution)
    avgopenprice<-mean(as.numeric(solution[,"openvalue"]))
    #print(avgopenprice)
    avgcloseprice<-mean(as.numeric(solution[,"closevalue"]))
    intermdata[k,]<-c(j,i,avgopenprice,avgcloseprice)
    k<-k+1
  }
  writeLines("\n")
  print("-------------------")
  writeLines(paste("YEAR","-->>",i))
  print("-------------------")
  writeLines("\n")
  print(intermdata)
  write.csv(intermdata,"~/sharesintermdata.csv")
  #print(intermdata[,"month"])
  #print(intermdata[,"averageopenvalue"])
  #x<-factor(intermdata[,"month"],levels=unique(intermdata[,"month"]))
  intermdata<-read.csv("~/sharesintermdata.csv")
  intermdata<-intermdata[nrow(intermdata):1,]
  print(intermdata)
  xval<-factor(intermdata[,"month"],levels=unique(intermdata[,"month"]))
  print(length(xval))
  line<-ggplot(intermdata,aes(x = xval,group=1))
  print(line+geom_path(aes( y = as.numeric(intermdata[,"averageopenvalue"]),colour="Red",group=1),stat="identity",linejoin="round")+geom_path(aes( y = as.numeric(intermdata[,"averageclosevalue"]),colour="Blue",group=1),stat="identity",linejoin="round"))
  writeLines(paste("AVERAGE OPEN PRICE FOR THE YEAR-----",mean(as.numeric(intermdata[,"averageopenvalue"]))))
  writeLines(paste("AVERAGE CLOSE PRICE FOR THE YEAR-----",mean(as.numeric(intermdata[,"averageclosevalue"]))))
  readline(prompt = "do you want to continue:")
  k<-1
  xval<-0
  }
}

