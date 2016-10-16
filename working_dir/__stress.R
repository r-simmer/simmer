library(simmer)
library(testthat)

wd <- getwd()
setwd("tests")
for (i in 1:100000) {
  message(i)
  capture.output(test_check("simmer")) %>% invisible
}
setwd(wd)
