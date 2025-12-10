install.packages("dplyr")
install.packages("ggplot2")

library(dplyr)
library(ggplot2)

hd_data <- read.csv("Cleveland_hd.csv")

hd_pr <- hd_data %>% 
  mutate(sex = as.factor(ifelse(sex == 0, "mujer", "hombre")), 
         hd = ifelse(class > 0, 1, 0), 
         hd_etiqueta = as.factor(ifelse(hd == 0, "No enfermedad", "Enfermedad")))  %>% 
  relocate(c(hd, hd_etiqueta), .after = sex) %>% 
  select(age:hd_etiqueta, trestbps, chol, thalach)


#CHI CUADRADO PARA UNA VARIABLE
n = 100
moneda <- sample(c(0,1), n, replace = TRUE)
table(moneda)
chisq.test(table(moneda))
datos <- as.data.frame(table(moneda)) %>% #convierte la tabla en dataframe
  rename("res" = moneda, "freq" = Freq)

datos %>% 
  ggplot(aes(x = res, y = freq, fill = res)) +
  geom_bar(stat = "identity", width = 0.6, color = "black") +
  geom_hline(yintercept = n/2, color = "red", linetype = "dashed", size = 1) +
  scale_fill_manual(values = c("skyblue", "salmon")) +
  labs(title = "Resultados de 100 lanzamientos",
       subtitle = expression("Hipótesis nula: " ~ H[0] * ": p = 0.5"),
       x = "",
       y = "Frecuencia observada") +
  theme_minimal(base_size = 14) +
  theme(legend.position = "none")

# obtener valores de la variable 'hd'
vals_hd <- hd_pr %>% pull(hd_etiqueta)

# obtener valores de la variable 'sex'
vals_sex <- hd_pr %>% pull(sex)

# test de independencia
hd_by_sex <-  chisq.test(vals_sex, vals_hd)
hd_by_sex


hd_pr %>% 
  ggplot(aes(x = hd_etiqueta, fill = sex)) +
  geom_bar(position = "fill") + 
  labs(x = "", y= "porcentaje %")

## con dos variables: 

##Examina la variable ‘chol’ entre los grupos enfermedad/sanos.

condition <- hd_pr %>% 
  pull(hd_etiqueta)

colesterol <- hd_pr %>% 
  pull(chol)    

chol_by_condition <- t.test(colesterol ~ condition)
chol_by_condition


CCCC

##1. Examina la variable ‘age’ entre los grupos enfermedad/sanos

age <- hd_pr %>% pull(age)
age_by_condition <- t.test(age ~ condition )
print(age_by_condition

  hd_pr %>% 
        ggplot(aes(x = hd_etiqueta, y = age, col = hd_etiqueta)) + 
        geom_boxplot(show.legend = FALSE, outlier.shape = NA) + 
        geom_jitter(width = 0.3, alpha = 0.4) + 
        labs(x = "", y = "edad") +
        theme(legend.position = "none")
  
## Examina la variable ‘thalach’ entre los grupos enfermedad/sanos
  
  tension <- hd_pr %>% pull(thalach)
  
  thalach_by_condition <- t.test(age ~ condition )
  
  print(thalach_by_condition)

  hd_pr %>% 
    ggplot(aes(x = hd_etiqueta, y = age, col = hd_etiqueta)) + 
    geom_boxplot(show.legend = FALSE, outlier.shape = NA) + 
    geom_jitter(width = 0.3, alpha = 0.4) + 
    labs(x = "", y = "maximum heart rate") +
    theme(legend.position = "none")
  
## Examina la variable ‘trestbps’ entre los grupos enfermedad/sanos
  
  bps <- hd_pr %>% pull(trestbps)
  
  bps_by_condition <- t.test(age ~ condition )
  
  print(bps_by_condition)
  
  hd_pr %>% 
    ggplot(aes(x = hd_etiqueta, y = trestbps, col = hd_etiqueta)) + 
    geom_boxplot(show.legend = FALSE, outlier.shape = NA) + 
    geom_jitter(width = 0.3, alpha = 0.4) + 
    labs(x = "", y = "blood pressure (mm Hg)") +
    theme(legend.position = "none")
  
  ¿ Las medias de los grupos son o no son similares ?
    
    hd_pr %>% 
    ggplot(aes(x = hd_etiqueta, y = trestbps, col = hd_etiqueta)) + 
    geom_boxplot(show.legend = FALSE, outlier.shape = NA) + 
    geom_jitter(width = 0.3, alpha = 0.4) + 
    labs(x = "", y = "blood pressure (mm Hg)") +
    theme(legend.position = "none") + 
    facet_wrap(~ sex)
  
  ##Examina la variable ‘chol’ entre los grupos hombre/mujer
  chol <- hd_pr %>% pull(chol)
  
  sex <- hd_pr %>% pull(sex)
  
  chol_by_sex <- t.test(chol ~ sex)
  chol_by_sex
  
  hd_pr %>% 
    ggplot(aes(x = sex, y = chol, col = sex)) + 
    geom_boxplot(show.legend = FALSE, outliers = F) + 
    geom_jitter(width = 0.3, alpha = 0.4) + 
    labs(x = "", y = "chol (mg/dl)") +
    theme(legend.position = "none")
  
## Examina la variable ‘trestbps’ entre los grupos hombre/mujer
  
  trestbps_by_sex <- t.test(hd_pr$trestbps ~ hd_pr$sex)
  
  trestbps_by_sex
  
  hd_pr %>% 
    ggplot(aes(x = sex, y = trestbps, col = sex)) + 
    geom_boxplot(show.legend = FALSE, outliers = F) + 
    geom_jitter(width = 0.3, alpha = 0.4) + 
    ggtitle("Blood pressure") + 
    labs(y = "mm Hg", x = "") + 
    theme(legend.position = "none")
  
  
  
  ## Examina la variable ‘trestbps’ entre los grupos hombre/mujer ENFERMOS
  enfermedad <- hd_pr %>% 
    filter(hd_etiqueta == "Enfermedad")
  
  bps_enfermedad <- enfermedad %>% pull(trestbps)
  
  trestbps_by_sex_enfermedad <- t.test(enfermedad$trestbps ~ enfermedad$sex)
  trestbps_by_sex_enfermedad
  
  enfermedad %>% 
    ggplot(aes(x = sex, y = trestbps, col = sex)) + 
    geom_boxplot(outliers = F) + 
    geom_jitter(width = 0.3, alpha = 0.4) + 
    ggtitle("Blood pressure in hd disease") + 
    labs(y = "mm Hg", x = "", col= "")
  
  ##Examina la variable ‘thalach’ entre los grupos hombre/mujer
  thalach_by_sex <- t.test(hd_pr$thalach ~ hd_pr$sex)
  
  thalach_by_sex
  
  hd_pr %>% 
    ggplot(aes(x = sex, y = thalach, col = sex)) + 
    geom_boxplot(show.legend = FALSE, outliers = F) + 
    geom_jitter(width = 0.3, alpha = 0.4) + 
    ggtitle("Maximum heart rate") + 
    labs(y = "ppm", x = "") + 
    theme(legend.position = "none")
  ##Examina la variable ‘thalach’ entre los grupos hombre/mujerENFERMOS
  thalach_enfermedad <- enfermedad %>% pull(thalach)
  
  thalach_by_sex_enfermedad <- t.test(enfermedad$thalach ~ enfermedad$sex)
  thalach_by_sex_enfermedad
  
  enfermedad %>% 
    ggplot(aes(x = sex, y = thalach, col = sex)) + 
    geom_boxplot(outliers = F) + 
    geom_jitter(width = 0.3, alpha = 0.4) + 
    ggtitle("Maximum heart rate in hd disease") + 
    labs(y = "ppm", x = "", col= "")