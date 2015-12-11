#' Arrivals at specific times
#' 
#' Generator convenience function to generate arrivals at specific times.
#'
#' @param ... a vector or multiple parameters of times at which to initiate an arrival.
#'
#' @return Returns a generator function.
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
#'   add_generator("patient", t0, at(0, c(1,10,30), 40, 43))
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


#' Generate arrivals starting at a specified time
#' 
#' Generator convenience function to generate inter arrivals with a specified initial start time.
#'
#' @param start_time the time at which to launch the initial arrival
#' @param dist a function modelling the interarrival times (returning a negative value stops the generator).
#' @param arrive if set to \code{TRUE} (default) the first arrival will be generated at \code{start_time} and will follow \code{dist} from then on. If set to \code{FALSE}, will initiate \code{dist} at \code{start_time} (and the first arrival will most likely start at a time later than \code{start_time}).
#'
#' @return Returns a generator function.
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
#'   add_generator("patient", t0, from(5, function() rnorm(1, 10)))
from<-function(start_time, dist, arrive=TRUE){
  started<-FALSE
  function(){
    if(!started){
      started<<-TRUE
      if(arrive){
        return(start_time)
      } else {
        return(start_time + dist())
      }
    } else {
      return(dist())
    }
  }
}
