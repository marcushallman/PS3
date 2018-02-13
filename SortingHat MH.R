student <- function(name){
  if (!is.character(name)) stop("Name must be character")
  courage<-sample(1:100,size=1)
  ambition<-sample(1:100,size=1)
  intelligence<-sample(1:100,size=1)
  effort<-sample(1:100,size=1)
  
  structure(list(name,courage,ambition,intelligence,effort), class="student")
  }

ian<-student("Ian Davis")
marcus<-student("Marcus Hallman")

sort.student<- function(x){
  useMethod(sort)
}

sortMatrix<-diag(1,nrow=4,ncol=4)

sort.student<-function(x,matrix){
attrib<-c(x[[2]],x[[3]],x[[4]],x[[5]])
 matrix=t(matrix)
 houses<-c("Gryffindor","Slytherin","Ravenclaw","Hufflepuff")
 monty=(attrib %*% matrix)
 print(monty)
 for (i in 1:4){if (max(monty)==monty[i]){
   print(houses[i])
   class(x)<-houses[i]
 }}
  }
sort.student(marcus,sortMatrix)  
ian

