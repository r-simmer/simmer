library(simmer)

t <- trajectory() %>% timeout(1)

print(head(t))
