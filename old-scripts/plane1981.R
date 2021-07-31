library(tidyverse)
library(migest)

d1 <- read_csv("./data-raw/plane1981-tab1.csv")
d2 <- read_csv("./data-raw/plane1981-tab2.csv")

d2$`Net Migration`

m1 <- d1 %>%
  slice(-10) %>%
  pivot_longer(cols = 2:10, names_to = "dest", values_to = "flow") %>%
  select(orig, dest, flow) %>%
  mutate(orig = fct_inorder(orig), 
         dest = fct_inorder(dest)) %>%
  xtabs(formula = flow ~ orig + dest, data = .) 

n <- d2 %>%
  slice(-10) %>%
  select(orig, `Net Migration`) %>%
  deframe()

t2 <- sum(d2$`Out Migration`, na.rm = TRUE)
y <- cm_net_tot(net_tot = n, m = m1, tot = t2)
addmargins(y$n)

# does not quite match.. i think because his model 
# has a distance parameter as well
588685-592765


