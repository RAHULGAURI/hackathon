
# This is the server logic for a Shiny web application.
# You can find out more about building applications with Shiny here:
#
# http://shiny.rstudio.com
#

library(shiny)
filename<-"C:\\Users\\Mahe\\Documents\\trigger application\\trigger.txt"
con=file(filename,open = "r+")

line <- readLines(con,n = 1)
lines<-""
close(con)
shinyServer(function(input, output) {
  
  obs<-1
  
  print(obs)
  
  
  
  output$distPlot <- renderText({
    invalidateLater(1000,session=getDefaultReactiveDomain())
   timestampp()
  })
 
  
})
timestampp<-function(){
  
  changedFiles(snapshot)
  changedFiles(snapshot)$changes
  filename<-"C:\\Users\\Mahe\\Documents\\trigger application\\trigger.txt"
  conn=file(filename,open = "r+")
  
  lines <- readLines(conn,n = 1)
  #snapshot <- fileSnapshot(dir, timestamp = tempfile("timestamp"), md5sum=TRUE)
  
  if(line == lines){
    v<-"NOT CHANGED"
    print(v)
  }
  else{
    v<-"CHANGED"
    print(paste(v,lines))
    baz <- function(){
      assign(line, lines, envir = .GlobalEnv)
    }
    baz()
  }
 
  
  
  
  close(conn)
  v<-paste(v,line,",",Sys.time())
}


