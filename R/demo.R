library(tidyverse)
library(rioja)
library(ggpalaeo)
data(SWAP)

performance_as_tibble <- function(performance, cv = TRUE) {
  performance <- if(cv){
    performance$crossval
  } else{
     performance$object
  } 
  performance  %>% 
    as_tibble(rownames = "model")
}

spp <- SWAP$spec
env <- SWAP$pH
mod <- WA(spp, env, mono = TRUE)
mod_loo <- crossval(mod, cv.method = "loo")
mod_boot <- crossval(mod, cv.method  = "bootstrap", nboot = 1000)

performance(mod_loo)$crossval
performance(mod_boot)$crossval

#R2 is higher!

tibble(
  env = env,
  loo = mod_loo$residuals.cv[, 1],
  boot = mod_boot$residuals.cv[, 1]
) %>% pivot_longer(-env) %>% 
  ggplot(aes(x = env, y = value, colour = name)) + 
  geom_point() + 
  geom_smooth()

autoplot(mod_loo, residuals = TRUE)


# try other transfer function methods, other training sets


