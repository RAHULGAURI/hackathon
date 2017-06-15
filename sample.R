
f<-function(){
  
  filename<-"C:\\Users\\Mahe\\Documents\\trigger application\\trigger.txt"
  con=file(filename,open = "r")
  
  line <- readLines(con,n = 1)
 
}
lines <- "yo"

if(line == lines){
  print("true")
}else{
  print("false")
}