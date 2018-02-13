door <- function(x){ #S3 create door class and object
  if (x!=1&x!=2&x!=3) stop("Pick 1,2,or3")
  structure(list(x), class="door")
}
PlayGame<-function(x){   #Create generic
  UseMethod("PlayGame", x)
}
PlayGame.door<-function(x){ #create door specific method
  
  prize<-sample(1:3,size=1)
  if (x[[1]]==prize){print("You win a car. Congrats!!!")}
  else {print("Life is hard sometimes. I hope you do better next time")}
}
test<-door(3)  #It works!
PlayGame(test)


setClass(Class="Door",     #Setting doors as an extant s4 class
         representation = representation(
           door="numeric"
         ),
         prototype = prototype(
           door = c()
         )
)

setValidity("Door", function(object){  #testing the validity of door objects. they should be 1,2 or 3

  test1<-all(object@door==1)
  test2<-all(object@door==2)
  test3<-all(object@door==3)
  if(!test1 & !test2 &!test3){return("@door is not a valid value")}
})
setMethod("initialize", "Door", function(.Object, ...){ #Initializing Doors
  value = callNextMethod()
  validObject(value)
  return(value)
})

new("Door", door=3)

setGeneric("getDoor",      #Generic Getter for class Door
           function(object="Door") {
             standardGeneric("getDoor")
           })

setMethod("getDoor", "Door",  #Cereating the Door specific method for getDoor
          function(object){
            return(object@door)
          } )


Monty<-new("Door",door=3) #test door named Monty
getDoor(Monty)  #It works! Yay!


gutsOfPlay<-function(thing="Door"){ #telling R to do something specific to doors with the PlayGame method
  standardGeneric("PlayGame")  
}

setGeneric("PlayGame",gutsOfPlay) #Setting the generic
setMethod("PlayGame","Door", #creating the method
          function(thing){
            prize<-sample(1:3,size=1) #Prize is the number against which doors are checked
           if (prize==thing@door){print("You Win. Congrats!")}
           if(prize!=thing@door){print("You lose. Better luck next time!")}                        
                                   })
PlayGame(Monty) #Test
