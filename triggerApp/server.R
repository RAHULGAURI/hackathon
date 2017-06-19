
# This is the server logic for a Shiny web application.
# You can find out more about building applications with Shiny here:
#
# http://shiny.rstudio.com
#

library(shiny)
filename<-"C:\\wamp64\\www\\hackathon\\trigger.txt"
connn<-file(filename,open = "r")

line <- readLines(connn,n = 1)
lines<-""
v<-""
close(connn)

shinyServer(function(input, output) {
  output$distPlot <- renderText({
    invalidateLater(1000,session=getDefaultReactiveDomain())
    timestampp()
  })
})


timestampp<-function(){
  
  #print(showConnections(all = FALSE))
  closeAllConnections()
  filename<-"C:\\wamp64\\www\\hackathon\\trigger.txt"
  conn<-file(filename,open = "r")
  
  liness <- readLines(conn,n = 1)
  #snapshot <- fileSnapshot(dir, timestamp = tempfile("timestamp"), md5sum=TRUE)
  close(conn)
  if(line == liness){
    v1<-"NOT CHANGED"
    closeAllConnections()
    v<<-v1
    print(paste(v1,line))
    v<-paste(v1,line,",",Sys.time())
    #print(isOpen(connn))
  }
  else{
    v1<-"CHANGED"
    print(paste(v1,liness))
    v<<-v1
    v<-paste(v1,liness,",",Sys.time())
    line<<-liness
    beforesleepfunc()
    
    Sys.sleep(5);
    
    aftersleepfunc()
    #baz <- function(){
    #  assign(line, lines, envir = .GlobalEnv)
    #}
    #baz()
  }
}

beforesleepfunc<-function(){
  print("beforesleepfunc called")
  
  library("quantmod")
  library("ggplot2")
  library(jsonlite)
    filename<-"C:\\wamp64\\www\\hackathon\\trigger.txt"
    
    con<-file(filename,open = "r")
    readit<-readLines(con,n = 1)
    readit
    close(con)
    
    ticker<-read.csv("C:\\wamp64\\www\\hackathon\\ticker.csv")
    for(i in ticker$COMPANY){
      tickeread<-ticker[which(ticker[,"COMPANY"] == readit),]
    }
    tickeread<-as.character(tickeread$TICKER)
    
    from.dat <- as.Date("01/01/08", format="%m/%d/%y") 
    to.dat <- as.Date("06/30/17", format="%m/%d/%y") 
    from1.dat <- as.Date("05/01/17", format="%m/%d/%y") 
    to1.dat <- as.Date(Sys.Date(), format="%y-%m-%d") 
    #getSymbols("^GSPC", src="yahoo", from = from.dat, to = to.dat)
    #getSymbols("^GSPC", src="yahoo", from = from.dat, to = to.dat)
    prefernceqoutes = processFile()
    #getSymbols("GOOG",from = "2010-01-01")
    print(prefernceqoutes)
    preferncejson<-toJSON(prefernceqoutes,pretty = TRUE)
    write(preferncejson,"C:\\wamp64\\www\\hackathon\\CompanyPreference.json")
    
    
    #quoteconec<-file("D:\\Users\\H223838\\Documents\\triggerApp\\preferencecom.txt",open = "r")    
    #countl<-length(count.fields(quoteconec,sep="\n"))
    #for(i in 1:countl){
    #  print(i)
    #  readprefer<-readLines(quoteconec,n = -1)
    #  print(readprefer)
    #}
    #close(quoteconec)
    #t<-getQuote(readit,src = "yahoo")
    #getSymbols("GOOG",from = "2010-01-01")
    
    #getSymbols.yahoo(readit,from="2007-01-01",to="2009-01-02",env=details,verbose=TRUE,return.class = 'xts',auto.assign = T)
    getSymbols(tickeread, src="yahoo", from = from.dat, to = to.dat)
    g<-get(tickeread)
    getSymbols(tickeread, src="yahoo", from = from1.dat, to = to1.dat)
    g1<-get(tickeread)
    
    
    yeardata<-processing(g,1)
    write.csv(yeardata,"C:\\wamp64\\www\\hackathon\\yearwisedata.csv",row.names = FALSE)
    yeardata<-toJSON(yeardata,pretty = TRUE)
    write(yeardata,"C:\\wamp64\\www\\hackathon\\yeardata.json")
    monthdata<-processing(g1,2)
    write.csv(monthdata,"C:\\wamp64\\www\\hackathon\\monthdata.csv",row.names = FALSE)
    monthdata<-toJSON(monthdata,pretty = TRUE)
    write(monthdata,"C:\\wamp64\\www\\hackathon\\monthdata.json")
    
}    
    #----------------------
    #creating data to be used
    #----------------------
processing<-function(g,value){
  
  rows<-length(g[,1])
  
  if(value == 1){
    processdata<-matrix(nrow=rows,ncol=4)
    colnames(processdata)<-c("month","year","openvalue","closevalue")
    
  }
    
    else{
      processdata1<-matrix(nrow=rows,ncol=5)
    colnames(processdata1)<-c("date","month","year","openvalue","closevalue")
    }
    j<-1
  
    
    i<-index(g[,0])
    openvalue<-as.vector(g[,1])
    closevalue<-as.vector(g[,4])
    datevalue<-index(g[,0])
    datemonth<-""
  
    for(k in 1:length(i)){
      open<-openvalue[k]
      close<-closevalue[k]
      st<-as.character(datevalue[k])
      datextract<-strsplit(st,"-")
      datextract<-unlist(datextract)
      if(value == 1){
      processdata[j,]<-c(datextract[2],datextract[1],open,close)
      }
      else{
      processdata1[j,]<-c(datextract[3],datextract[2],datextract[1],open,close)
      }
      j<-j+1
    } 
    if(value == 1){
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
        write.csv(intermdata,"~/sharesintermdata.csv",row.names = TRUE)
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
        #readline(prompt = "do you want to continue:")
        k<-1
        xval<-0
      }
      l<-l+1
      #write.csv(yearwisedata,"C:\\wamp64\\www\\hackathon\\yearwisedata.csv")
    }
    return(yearwisedata)
    }
    else{
      years<-unique(processdata1[,3])
      for(i in years){
      if(!is.na(i)){
        interm<-processdata1[which(processdata1[,"year"] == i),]
      }  
        return(interm)
      }  
    }
}
    
  

aftersleepfunc<-function(){
  print("aftersleepfunc called")
}


processFile = function() {
  con = file("C:\\wamp64\\www\\hackathon\\preferencecom.txt", "r")
  index<-1
  len<-length(count.fields("C:\\wamp64\\www\\hackathon\\preferencecom.txt",sep = "\n"))
  processprefdata<-matrix(nrow=len,ncol=6)
  while ( TRUE ) {
    line = readLines(con, n = 1)
    if ( length(line) == 0 ) {
      break
    }
    
    
    ticker<-read.csv("C:\\wamp64\\www\\hackathon\\ticker.csv")
    for(i in ticker$COMPANY){
      tickeread<-ticker[which(ticker[,"COMPANY"] == line),]
    }
    tickeread<-as.character(tickeread$TICKER)
    t<-getQuote(tickeread,src = "yahoo")
    #print(line)
    print(t)
    openvalue<-as.vector(t[,5])
    currentvalue<-as.vector(t[,2])
    volume<-as.vector(t[,8])
    perodicchnage<-as.vector(t[,3])
    daychange<-as.vector(t[,4])
    if(!is.na(openvalue) && !is.na(currentvalue) && !is.na(volume) && !is.na(perodicchnage) && !is.na(daychange)){
      processprefdata[index,]<-c(line,openvalue,currentvalue,volume,perodicchnage,daychange)
      index<-index+1
    }
    
  }
  print(processprefdata)
  #return(processprefdata)
  #print(processprefdata)
  close(con)
}
