library(tidyverse)
library(patchwork)
## Lee los datos
tips <- read_csv("tips.csv")
glimpse(tips)

## Recodificar nombres y niveles
propinas <- tips %>% 
  rename(cuenta_total = total_bill, 
         propina = tip, sexo = sex, 
         fumador = smoker,
         dia = day, momento = time, 
         num_personas = size) %>% 
  mutate(sexo = recode(sexo, Female = "Mujer", Male = "Hombre"), 
         fumador = recode(fumador, No = "No", Si = "Si"),
         dia = recode(dia, Sun = "Dom", Sat = "Sab", Thur = "Jue", Fri = "Vie"),
         momento = recode(momento, Dinner = "Cena", Lunch = "Comida")) %>% 
  select(-sexo) %>% 
  mutate(dia  = fct_relevel(dia, c("Jue", "Vie", "Sab", "Dom")))
propinas


## 1. Calcula percentiles de la variable propina
## junto con mínimo y máxio
quantile(propinas$propina, probs = seq(0,1, .01))
  
## 2. Haz una gráfica de cuantiles de la variable propina
propinas <- propinas %>% 
  mutate(orden_propina = rank(cuenta_total, ties.method = "first"), 
         f = orden_propina / n(),
         cuartil = cut(propina,
                         breaks = quantile(propina, probs = seq(0,1, .25)), include.lowest = T, )
         ) 


(exercise2 <-
  propinas %>%
  count(cuartil) %>% 
  ggplot(aes(x = cuartil, y = n, fill = cuartil)) +
  geom_bar(stat = "identity")+
  scale_fill_brewer(palette = "Set2") +
  theme_minimal()+
  labs(
    y = "Cantidad de registros",
    x = "Cuartil",
    title = "Cantidad de registro por cuartil", 
    subtitle = "El total de registros son: 244",
    caption = "Dataset utilizado: tips.csv") +
  theme(legend.position = "bottom"))


# ggplot

## 3. Haz un histograma de la variable propinas
## Ajusta distintos anchos de banda
library(gridExtra)

gridExtra::grid.arrange(
  propinas %>% 
    ggplot(aes(x = propina)) +
    geom_histogram(binwidth = 1) ,
  propinas %>% 
    ggplot(aes(x = propina)) +
    geom_histogram(binwidth = 2),
  propinas %>% 
    ggplot(aes(x = propina)) +
    geom_histogram(binwidth = 3), ncol =1
) 

## 4. Haz una gráfica de cuenta total contra propina
propinas %>% 
  ggplot(aes(x=cuenta_total, y = propina, color = factor(num_personas))) +
  geom_point() +
  theme_minimal()  +
  labs(
    y = "Cantidad de registros",
    x = "Cuartil",
    title = "Cantidad de registro por cuartil", 
    subtitle = "El total de registros son: 244",
    caption = "Dataset utilizado: tips.csv",
    color = "Numero de personas") +
  theme(legend.position = "bottom")


## 5. Calcula propina en porcentaje de la cuenta total
## calcula algunos cuantiles de propina en porcentaje
propinas <- propinas %>% 
  mutate(pct_propina = propina/cuenta_total,
         cnt_propina = cut(x = pct_propina, breaks = quantile(pct_propina, probs = c(0,.2,.4,.6,.8,1)), include.lowest = T)
         )
           
quantile(propinas$pct_propina, probs = c(0,.2,.4,.6,.8,1))
  
## 6. Haz un histograma de la propina en porcentaje. Prueba con
##  distintos anchos de banda. 

gridExtra::grid.arrange(
  propinas %>% 
    ggplot(aes(x = pct_propina)) +
    geom_histogram(binwidth = .05),
  propinas %>% 
    ggplot(aes(x = pct_propina)) +
    geom_histogram(binwidth = .1),
  propinas %>% 
    ggplot(aes(x = pct_propina)) +
    geom_histogram(binwidth = .15)
)

## 7. Describe la distribución de propina en pct. ¿Hay datos atípicos?
propinas %>% 
  ggplot(aes(x=pct_propina)) +
  geom_boxplot() +
  coord_flip()

# si, hay 4 datos atipicos, para encontrarlos podemos ocupar la funcion de IQR:
propinas %>% 
  filter(pct_propina > quantile(propinas$pct_propina, .75)+1.5*IQR(propinas$pct_propina))

##8. Filtra los casos con porcentaje de propina muy altos. 
## ¿Qué tipos de cuentas son? ¿Son cuentas grandes o chicas?
propinas %>% 
  filter(pct_propina > quantile(propinas$pct_propina, .75)+1.5*IQR(propinas$pct_propina))

# son cuentas pequenas con una o dos personas

## 9. Haz una diagrama de caja y brazos para 
## propina en dolares dependiendo del momento (comida o cena)
## ¿Cuál parece más grande? ¿Por qué? Haz otras gráficas si es necesario.
propinas %>% 
  ggplot(aes(x = momento, y=propina)) +
  geom_boxplot()

propinas %>% count(momento)

library(scales)
propinas %>% 
  count(momento, num_personas) %>% ungroup() %>% 
  group_by(momento) %>%
  mutate(n = n/sum(n)) %>% 
  ggplot(aes(x = num_personas, y = n, color = momento)) +
  geom_line()+
  geom_point()+
  scale_y_continuous(labels = scales::percent_format())


# Parece mas grande la dispersion de la cena, esto puede deberse a que en la cena 
# un mayor porcentaje de las visitas contienen 3 o 4 personas, entonces esto hace  
# que la dispersion por que la propina esta generalmente ligada al monto e la cuenta
