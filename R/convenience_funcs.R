#' generate arrivals at specific times (generator convenience function)
#'
#' @param times a (vector) of times at which to initiate an arrival
#'
#' @return returns a generator function
#' @seealso \link{add_generator}
#' @export
#'
#' @examples
#' env <- simmer("SuperDuperSim") %>%
#'   add_resource("nurse", 1) %>%
#'   add_resource("doctor", 2) %>%
#'   add_resource("administration", 1) %>%
#'   add_generator("patient", t0, at(c(0,10,30)))
at<-function(times){
  i<-0
    function(){
      if(i<length(times)){
        i<<-i+1
        return(times[[i]])
      } else {
        return(-1)
      }
    }
}