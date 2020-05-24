## setup
options(repos=c(CRAN="https://cran.rstudio.com"))
options(Ncpus=parallel::detectCores(logical=FALSE))

install_required <- function(pkgs) {
  pkgs <- pkgs[!sapply(pkgs, requireNamespace, quietly=TRUE)]
  if (length(pkgs)) install.packages(pkgs)
}

install_required(c("git2r", "ggplot2"))

tmp <- file.path(getwd(), "working_dir/tmp")
tags <- git2r::tags()
version <- package_version(gsub("v", "", names(tags)))
tags <- tags[version >= "3.3.0"]

## load saved data and filter out existing tags
benchmark <- NULL
db <- file.path(tmp, "db.RData")
if (file.exists(db)) load(db)
tags <- tags[!(names(tags) %in% benchmark$expr)]

## install dependencies if necessary
deps <- file.path(tmp, "deps")
dir.create(deps, showWarnings=FALSE, recursive=TRUE)
.libPaths.bkp <- .libPaths()
.libPaths(deps)
install_required(c(
  tools::package_dependencies("simmer")$simmer,
  "microbenchmark", "remotes", "callr"))

## resolve tags and create dirs if necessary
tags <- sapply(names(tags), function(tag) {
  path <- file.path(tmp, tag)
  dir.create(path, showWarnings=FALSE)
  list(
    name = tag,
    hash = tags[[tag]]$sha,
    date = as.POSIXct(tags[[tag]]$author$when),
    paths = c(path, deps)
  )
}, simplify=FALSE)

## install tags if necessary
parallel::mclapply(tags, function(tag) {
  .libPaths(tag$paths)
  if (!dir.exists(file.path(.libPaths()[1], "simmer")))
    callr::r(function(ref)
      remotes::install_github("r-simmer/simmer", ref=ref), list(tag$hash))
}, mc.cores=getOption("Ncpus", 1L))

## benchmark
benchmark <- rbind(do.call(rbind, lapply(tags, function(tag) {
  message("running... ", tag$name)

  .libPaths(tag$paths)
  res <- callr::r(function() {
    library(simmer)

    message(exists("trajectory"))

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

    microbenchmark::microbenchmark(test(1000), times=10L)
  })

  res$date <- tag$date
  res$expr <- tag$name
  res
})), benchmark)

save(benchmark, file=db)

## plot results
.libPaths(.libPaths.bkp)
library(ggplot2)

cpu <- readLines("/proc/cpuinfo")
cpu <- strsplit(cpu[grep("model name", cpu)][[1]], "\t*: ")[[1]][[2]]
benchmark$minor <- sub("\\.[0-9]$", "\\.x", benchmark$expr)
cooksd <- cooks.distance(lm(time ~ 1, benchmark))

ggplot(benchmark[cooksd < 4*mean(cooksd, na.rm=T),], aes(date, time/1e9)) +
  ggtitle("Historical performance", paste("on", cpu)) + ylab("time [s]") +
  theme_classic() + geom_jitter(alpha=.2) + geom_smooth() +
  stat_summary(fun.y=mean, geom="point", aes(color=minor), size=3) +
  stat_summary(fun.y=mean, geom="point", color="grey90", size=1)
