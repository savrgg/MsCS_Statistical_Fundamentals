# 1) load libraries and functions -----------------------------------------
library(tidyverse)

load("fundamentos-final/data/x.RData")
muestra <- x

crear_log_p <- function(x){
  log_p <- function(pars){
    media = pars[1]
    desv_est = pars[2]
    z <- (x - media) / desv_est
    log_verosim <- -(log(desv_est) +  0.5 * mean(z^2))
    log_verosim
  }  
  log_p
}

simular_modelo <- function(n, media, sigma){
  rnorm(n, media, sigma)
}

rep_boot <- function(rep, crear_log_p, est_mle, n){
  muestra_bootstrap <- simular_modelo(length(muestra), 
                                      est_mle["media", "estimador"], 
                                      est_mle["sigma", "estimador"])
  log_p_boot <- crear_log_p(muestra_bootstrap)
  # optimizamos
  res_boot <- optim(c(0, 0.5), log_p_boot, 
                    control = list(fnscale = -1, maxit = 1000), method = "Nelder-Mead")
  try(if(res_boot$convergence != 0) stop("No se alcanzó convergencia."))
  tibble(parametro = c("media", "sigma"), estimador_boot = res_boot$par) 
}

# 2) Find MLE of sample ---------------------------------------------------

log_p <- crear_log_p(muestra)
res <- optim(c(0, 0.5), log_p, control = list(fnscale = -1, maxit = 1000), method = "Nelder-Mead")
res$convergence

est_mle <- tibble(parametro = c("media", "sigma"), estimador = res$par) %>% 
  column_to_rownames(var = "parametro")


# 3) replicas bootstrap ---------------------------------------------------

reps_boot <- map_dfr(1:5000, ~ rep_boot(.x, crear_log_p, est_mle, 
                                        n = length(muestra)), rep = ".id") 

error_est <- reps_boot %>% 
  group_by(parametro) %>% 
  summarise(ee_boot = sd(estimador_boot)) 

bind_cols(est_mle, error_est) %>% 
  mutate(across(where(is.numeric), round, 3)) %>% 
  select(parametro, estimador, ee_boot)

reps_boot %>% 
  ggplot(aes(x = estimador_boot)) +
  geom_histogram()+
  facet_wrap(~parametro, scales = "free")+
  theme_minimal()






