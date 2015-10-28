#' auxiliary function
#' 
#' does nothing
#' @param arg accepts a single parameter
#' @export
yield <- function(arg){}

#' Simmer.make_closure
#' 
#' convert a SimPy-like process into an R closure
#' @param is_gen must be TRUE if the process is a generator (has a while loop), FALSE otherwise
#' @param expr the function to convert
#' @return a process implemented as an R closure
#' @export
Simmer.make_closure <- function(is_gen, expr) {
  flag <- T
  
  # expr to character
  expr <- as.character(substitute(expr))[2]
  
  # split and head
  expr <- strsplit(expr, "\n")[[1]]
  expr[[1]] <- paste0(expr[[1]], "\ni <- 1\n function() {")
  if (!is_gen)
    expr[[1]] <- paste0(expr[[1]], "\nswitch(i, {")
  
  # substitute yields, ignore while
  for (i in 1:length(expr)) {
    if (flag && is_gen && grepl("while", expr[[i]])) {
      expr[[i]] <- paste0(expr[[i]], "\nswitch(i, {")
      flag <- F
    }
    expr[[i]] <- gsub("(yield\\()([[:print:]]*)(\\))", 
                      paste0("\\2", "\n i <<- i + 1\n return(1) }, {"),
                      expr[[i]])
  }
  
  # tail
  if (is_gen)
    expr[[length(expr)-1]] <- paste0("i <<- 1 }, return(0)) } ", expr[[length(expr)-1]])
  else
    expr[[length(expr)]] <- paste0("return(0) }) } ", expr[[length(expr)]])
  
  eval(parse(text=expr))
}

### Prototypes
#
# arrival <- function(env, ...) {
#   i <- 1
#   function() {
#     switch(i, {
#       #code
#       i <<- i + 1
#       return(1)
#     }, {
#       ...
#       i <<- i + 1
#       return(1)
#     }, {
#       #code
#       i <<- i + 1
#       return(1)
#     },
#     return(0))
#   }
# }
# 
# generator <- function(env, ...) {
#   i <- 1
#   function() {
#     while (T) {
#       switch(i, {
#         #code
#         i <<- i + 1
#         return(1)
#       }, {
#         #code
#         i <<- 1
#       },
#       return(0))
#     }
#   }
# }
