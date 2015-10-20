require(R6)

PriorityQueue <- R6Class("PriorityQueue",
  public = list(
   length = function() base::length(private$keys),
   
   push = function(key, value) {
     temp <- c(private$keys, key)
     ord <- order(temp)
     private$keys <- temp[ord]
     private$values <- c(private$values, list(value))[ord]
     invisible(self)
   },
   
   pop = function() {
     if (self$length() == 0) return(NULL)
     key <- private$keys[[1]]
     value <- private$values[[1]]
     private$keys <- private$keys[-1]
     private$values <- private$values[-1]
     list(key, value)
   }
  ),
  
  private = list(
   keys = c(),
   values = list()
  )
)

evaluate_value<-function(value){
  tryCatch(
    {
      abs(parse(text=value))
    }, 
    error = function(err) value)
}
