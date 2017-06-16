library("quantmod")

triggeranalysis<-function(){
filename<-"~/first.txt"

con<-file(filename,open = "r+")
readit<-readLines(con,n = 1)
readit




from.dat <- as.Date("01/01/08", format="%m/%d/%y") 
to.dat <- as.Date("07/30/13", format="%m/%d/%y") 
#getSymbols("^GSPC", src="yahoo", from = from.dat, to = to.dat)
#getSymbols("^GSPC", src="yahoo", from = from.dat, to = to.dat)
t<-getQuote("GOOG",src = "yahoo")
#getSymbols("GOOG",from = "2010-01-01")

#getSymbols.yahoo(readit,from="2007-01-01",to="2009-01-02",env=details,verbose=TRUE,return.class = 'xts',auto.assign = T)
getSymbols(readit, src="yahoo", from = from.dat, to = to.dat)
g<-get(readit)
rows<-length(g[,1])
processdata<-matrix(nrow=rows,ncol=4)
colnames(processdata)<-c("month","year","openvalue","closevalue")

j<-1

i<-index(g[,0])

openvalue<-as.vector(g[,1])
closevalue<-as.vector(g[,4])
datevalue<-index(g[,0])

#----------------------
#creating data to be used
#----------------------

for(k in 1:length(i)){
  open<-openvalue[k]
  close<-closevalue[k]
  st<-as.character(datevalue[k])
  datextract<-strsplit(st,"-")
  datextract<-unlist(datextract)
  processdata[j,]<-c(datextract[2],datextract[1],open,close)
  j<-j+1
} 

years<-unique(processdata[,2])
yearwisedata<-matrix(nrow = length(years),ncol = 3)
colnames(yearwisedata)<-c("year","AverageOpenValue","AverageCloseValue")
#print(years)
k<-1
l<-1
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
    meanopenprice <- mean(as.numeric(intermdata[,"averageopenvalue"]),na.rm = TRUE)
    meancloseprice <- mean(as.numeric(intermdata[,"averageclosevalue"]),na.rm = TRUE)
    yearwisedata[l,] <- c(i,meanopenprice,meancloseprice)
    writeLines(paste("AVERAGE OPEN PRICE FOR THE YEAR-----",mean(as.numeric(intermdata[,"averageopenvalue"]),na.rm = TRUE)))
    writeLines(paste("AVERAGE CLOSE PRICE FOR THE YEAR-----",mean(as.numeric(intermdata[,"averageclosevalue"]),na.rm = TRUE)))
    readline(prompt = "do you want to continue:")
    k<-1
    xval<-0
  }
  l<-l+1
}
write.csv(yearwisedata,"~/yearwisedata.csv")
close(con)

}

