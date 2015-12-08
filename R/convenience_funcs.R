#' generate arrivals at specific times (generator convenience function)
#'
#' @param ... a vector or multiple paramters of times at which to initiate an arrival
#'
#' @return returns a generator function
#' @seealso \link{add_generator}
#' @export
#'
#' @examples
#' t0 <- create_trajectory("my trajectory") %>%
#'   ## add an intake activity
#'   seize("nurse", 1) %>%
#'   timeout(function() rnorm(1, 15)) %>%
#'   release("nurse", 1) %>%
#'   ## add a consultation activity
#'   seize("doctor", 1) %>%
#'   timeout(function() rnorm(1, 20)) %>%
#'   release("doctor", 1) %>%
#'   ## add a planning activity
#'   seize("administration", 1) %>%
#'   timeout(function() rnorm(1, 5)) %>%
#'   release("administration", 1)
#'   
#' env <- simmer("SuperDuperSim") %>%
#'   add_resource("nurse", 1) %>%
#'   add_resource("doctor", 2) %>%
#'   add_resource("administration", 1) %>%
#'   add_generator("patient", t0, at(c(0,10,30)))
at<-function(...){
  time_vec<-unlist(list(...))
  time_diffs<-c(time_vec[1], diff(time_vec))
  i<-0
    function(){
      if(i<length(time_vec)){
        i<<-i+1
        return(time_diffs[i])
      } else {
        return(-1)
      }
    }
}