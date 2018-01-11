tmp <- file.path(getwd(), "working_dir/tmp")
tags <- git2r::tags()[-c(1:7)] # from 3.3.0 on

## load saved data and filter out existing tags
db <- file.path(tmp, "db.RData")
benchmark <- NULL
if (file.exists(db)) load(db)
tags <- tags[!(names(tags) %in% benchmark$expr)]

## install dependencies if necessary
deps <- file.path(tmp, "deps")
dir.create(deps, showWarnings=FALSE, recursive=TRUE)
.libPaths.bkp <- .libPaths()
.libPaths(c(deps, .Library))
if (!requireNamespace("Rcpp", quietly=TRUE))
  install.packages(c(
    tools::package_dependencies("simmer")$simmer,
    "microbenchmark"
  ))

## resolve tags and create dirs if necessary
tags <- sapply(names(tags), function(tag) {
  path <- file.path(tmp, tag)
  dir.create(path, showWarnings=FALSE)
  list(
    name = tag,
    hash = tags[[tag]]@sha,
    date = as(tags[[tag]]@author@when, "POSIXct"),
    paths = c(path, deps)
  )
}, simplify=FALSE)

## install tags if necessary
parallel::mclapply(tags, function(tag) {
  .libPaths(c(tag$paths, .Library))
  if (!requireNamespace("simmer", quietly=TRUE))
    remotes::install_github("r-simmer/simmer", ref=tag$hash)
}, mc.cores=2L)

## benchmark
benchmark <- rbind(do.call(rbind, parallel::mclapply(tags, function(tag) {
  message("running... ", tag$name)

  .libPaths(c(tag$paths, .Library))
  library(simmer)

  mm1 <- {
    if (exists("trajectory")) trajectory()
    else create_trajectory()
  } %>%
    seize("server", 1) %>%
    timeout(function() rexp(1, 66)) %>%
    release("server", 1)

  gen <- function() rexp(100, 60)

  test <- function(t) {
    set.seed(1234)
    simmer(verbose=F) %>%
      add_resource("server", 1) %>%
      add_generator("customer", mm1, gen, mon=F) %>%
      run(t)
  }

  res <- microbenchmark::microbenchmark(test(1000), times=10L)
  res$date <- tag$date
  res$expr <- tag$name
  res
}, mc.cores=1L)), benchmark)

save(benchmark, file=db)

## plot results
.libPaths(.libPaths.bkp)
library(ggplot2)

cpu <- readLines("/proc/cpuinfo")
cpu <- strsplit(cpu[grep("model name", cpu)][[1]], "\t*: ")[[1]][[2]]
benchmark$minor <- sub("\\.[0-9]$", "\\.x", benchmark$expr)

ggplot(benchmark, aes(date, time/1e9)) + theme_classic() +
  ggtitle("Historical performance", paste("on", cpu)) + ylab("time [s]") +
  geom_jitter(alpha=.2) + geom_smooth() +
  stat_summary(fun.y=mean, geom="point", aes(color=minor), size=3) +
  stat_summary(fun.y=mean, geom="point", color="grey90", size=1)
