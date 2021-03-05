# Librerias====
library(tidyverse)
library(RCT)
library(stargazer)
library(kableExtra)
library(devtools)
library(mfx)
library(EnvStats)
library(magrittr)

# Lectura ====
base_original <- haven::read_stata("data/raw/Names.dta") %>% 
  janitor::clean_names()

# Limpieza====

summary_statistics(base_original) %>% 
  as.data.frame() %>% 
  stargazer(type = "text", summary = FALSE)

## Pregunta 2====

funciones = list(
  "media" = ~mean(.x, na.rm = T), 
  "max" = ~max(.x, na.rm = T), 
  "min" = ~min(.x, na.rm = T),
  "d.e." = ~sd(.x, na.rm = T)
)

# AED de años de experciencia 
base_original %>% 
  group_by(firstname) %>% 
  summarise(across(.cols = yearsexp, .fns = funciones,
                   .names = "{.fn}")) %>% 
  mutate(across(-firstname, ~round(.x, digits = 2))) %>%  
  as.data.frame() %>% 
  stargazer(type = "text", summary = F, title = "Años de experiencia", header = F,
            table.placement = "H")

# AED de calidad de CV
base_original %>% 
  group_by(firstname) %>% 
  summarise(across(.cols = high, .fns = funciones,
                   .names = "{.fn}")) %>% 
  mutate(across(-firstname, ~round(.x, digits = 2))) %>%  
  as.data.frame() %>% 
  stargazer(type = "text", summary = F,title = "Calidad del CV", header = F,
            table.placement = "H")


#  AED (medias de la algunas variables por nombre)

# Opción 1
# base_original %>% 
#   group_by(firstname) %>% 
#   summarise(callback = mean(call_back), Female = mean(female), Black = mean(black), 
#             HQresume = mean(high), Chicago = mean(chicago), Yearsexp = mean(yearsexp), 
#             College = mean(college),Email = mean(email)) %>%  
#   as.data.frame() %>% 
#   stargazer(type = "text", summary = F,title = "Calidad del CV", header = F,
#             table.placement = "H")

# Opción 2
base_original %>% 
  group_by(firstname) %>% 
  summarise(Callback = mean(call_back), Female = mean(female), Black = mean(black), 
            HQresume = mean(high), Chicago = mean(chicago), Yearsexp = mean(yearsexp), 
            College = mean(college),Email = mean(email)) %>% 
  kbl(booktabs = T,
      caption = "Características del grupo por nombre",
      digits = c(0, 2, 0, 0, 2, 2, 2, 2, 2)) %>% 
  kable_styling(position = "center", latex_options = "striped",
                full_width = F)

# Tabla de balance
base_original %>% 
  dplyr::select(female, black, high, call_back, chicago) %>%
  balance_table(treatment = "black") %>% 
  as.data.frame() %>% 
  stargazer(type = "text", summary = F)

# Pregunta 3====

# Estimador de Neyman
attach(base_original)

yiT <- call_back[black == 1] 
yiC <- call_back[black == 0] 

myiT <- mean(yiT)
myiC <- mean(yiC)

tau <- abs(myiT - myiC)

vartau <- var(yiT) / length(yiT) + var(yiC) / length(yiC)
tstat <- tau / sqrt(vartau)

valorp <- 2 * (1 - pnorm(tstat)) 

b <- structure(c(-tau, sqrt(vartau), tstat, valorp ), .Dim = c(4L, 1L),
                .Dimnames = list(c("black", "se", "Estadístico t", "Valor-p"),
                                 c(" ")))
stargazer(b, type = "text", summary = F)

# Estimación de OLS con errores heterocedásticos 

reg1 <- lm(call_back ~ black)
summary(reg1, robust = T)
robust_se1 <- as.vector(summary(reg1, robust = T)$coefficients[, "Std. Error"])

stargazer(reg1, type = "text", se = list(robust_se1))


# Estimación de OLS agregando controles

reg2 <- lm(call_back ~ black + female + I(black * female) +
             high + I(black * high) + chicago)

stargazer(reg1, reg2, type ="text", summary = F)

# Probit sin controles 

probit <- glm(call_back ~ black, family = binomial(link = "probit"))
mfx <- probitmfx(call_back ~ black, base_original)

stargazer(b, reg1, reg2, probit, type ="text", summary = F,
          title = "Comparación de modelos", digits = 3, 
          column.labels = c("OLS EH", "OLS CONT", "Probit"))

stargazer(mfx$mfxest, type = "text",coef = list(NULL, mfx$mfxest[,1]),
          se = list(NULL, mfx$mfxest[,2]))

## Pregunta 4 ====

x <- call_back[black == 1]
y <- call_back[black == 0]

Fisher_test <- twoSamplePermutationTestLocation(y, x,
                                   alternative ="two.sided",
                                   mu1.minus.mu2 = 0.01)
Fisher_test[["p.value"]]


## Pregunta 5====

tibble("HChiMa", "HChiTrans", "HChiBan", "HChiTra", "HChiBus", "HChiOth", "HChiMiss", "HBosMa", "HBosTrans", "HBosBan", "HBosTra", "HBosBus", "HBosth",
       "HBosMiss", "MChiMa", "MChiTrans", "MChiBan", "MChiTra", "MChiBus", "MChiOth", "MChiMiss", "MBosMa", "MBosTrans", "MBosBan", "MBosTra", "MBosBus", "MBosth",
       "MBosMiss") %>% 
  pivot_longer(everything(), values_to = "valores", names_to = "Estratos") %>% 
  pull(Estratos) %>% 
    kbl(booktabs = T, linesep = "",  caption = "Estratos",
        col.names = "Estratos") %>% 
  kable_styling(position = "center", latex_options = "stripped",
                full_width = F, font_size = 12)
  



# Estratificación

base_original %<>% 
  mutate(d = case_when(black == 1 ~ 1, TRUE ~ 0))

ey0.1 <- base_original %>% 
  filter(d == 1) %>% 
  pull(call_back) %>% 
  mean()

ey0.2 <- base_original %>% 
  filter(d == 0) %>% 
  pull(call_back) %>% 
  mean()

gate <- ey0.1 -  ey0.2




base_original %<>%
  mutate(s = case_when(female == 0 & chicago == 1 &  manuf == 1 ~ 1,
                       female == 0 & chicago == 1 &  transcom == 1 ~ 2,
                       female == 0 & chicago == 1 &  bankreal == 1 ~ 3,
                       female == 0 & chicago == 1 &  trade == 1 ~ 4,
                       female == 0 & chicago == 1 &  busservice == 1 ~ 5,
                       female == 0 & chicago == 1 &  othservice == 1 ~ 6,
                       female == 0 & chicago == 1 &  missind == 1 ~ 7,
                       female == 0 & chicago == 0 &  manuf == 1 ~ 8,
                       female == 0 & chicago == 0 &  transcom == 1 ~ 9,
                       female == 0 & chicago == 0 &  bankreal == 1 ~ 10,
                       female == 0 & chicago == 0 &  trade == 1 ~ 11,
                       female == 0 & chicago == 0 &  busservice == 1 ~ 12,
                       female == 0 & chicago == 0 &  othservice == 1 ~ 13,
                       female == 0 & chicago == 0 &  missind == 1 ~ 14,
                       female == 1 & chicago == 1 &  manuf == 1 ~ 15,
                       female == 1 & chicago == 1 &  transcom == 1 ~ 16,
                       female == 1 & chicago == 1 &  bankreal == 1 ~ 17,
                       female == 1 & chicago == 1 &  trade == 1 ~ 18,
                       female == 1 & chicago == 1 &  busservice == 1 ~ 19,
                       female == 1 & chicago == 1 &  othservice == 1 ~ 20,
                       female == 1 & chicago == 1 &  missind == 1 ~ 21,
                       female == 1 & chicago == 0 &  manuf == 1 ~ 22,
                       female == 1 & chicago == 0 &  transcom == 1 ~ 23,
                       female == 1 & chicago == 0 &  bankreal == 1 ~ 24,
                       female == 1 & chicago == 0 &  trade == 1 ~ 25,
                       female == 1 & chicago == 0 &  busservice == 1 ~ 26,
                       female == 1 & chicago == 0 &  othservice == 1 ~ 27,
                       female == 1 & chicago == 0 &  missind == 1 ~ 28,
                       T ~ 0))

ey11 <- base_original %>% 
  filter(s == 1 & d == 1) %$%
  mean(call_back)
ey10 <- base_original %>% 
  filter(s == 1 & d == 0) %$%
  mean(call_back)

ey21 <- base_original %>% 
  filter(s == 2 & d == 1) %$%
  mean(call_back)
ey20 <- base_original %>% 
  filter(s == 2 & d == 0) %$%
  mean(call_back)

ey31 <- base_original %>% 
  filter(s == 3 & d == 1) %$%
  mean(call_back)
ey30 <- base_original %>% 
  filter(s == 3 & d == 0) %$%
  mean(call_back)

ey41 <- base_original %>% 
  filter(s == 4 & d == 1) %$%
  mean(call_back)
ey40 <- base_original %>% 
  filter(s == 4 & d == 0) %$%
  mean(call_back)

ey51 <- base_original %>% 
  filter(s == 5 & d == 1) %$%
  mean(call_back)
ey50 <- base_original %>% 
  filter(s == 5 & d == 0) %$%
  mean(call_back)

ey61 <- base_original %>% 
  filter(s == 6 & d == 1) %$%
  mean(call_back)
ey60 <- base_original %>% 
  filter(s == 6 & d == 0) %$%
  mean(call_back)

ey71 <- base_original %>% 
  filter(s == 7 & d == 1) %$%
  mean(call_back)
ey70 <- base_original %>% 
  filter(s == 7 & d == 0) %$%
  mean(call_back)

ey81 <- base_original %>% 
  filter(s == 8 & d == 1) %$%
  mean(call_back)
ey80 <- base_original %>% 
  filter(s == 8 & d == 0) %$%
  mean(call_back)

ey91 <- base_original %>% 
  filter(s == 9 & d == 1) %$%
  mean(call_back)
ey90 <- base_original %>% 
  filter(s == 9 & d == 0) %$%
  mean(call_back)

ey101 <- base_original %>% 
  filter(s == 10 & d == 1) %$%
  mean(call_back)
ey100 <- base_original %>% 
  filter(s == 10 & d == 0) %$%
  mean(call_back)

ey111 <- base_original %>% 
  filter(s == 11 & d == 1) %$%
  mean(call_back)
ey110 <- base_original %>% 
  filter(s == 11 & d ==  0) %$%
  mean(call_back)

ey121 <- base_original %>% 
  filter(s == 12 & d == 1) %$%
  mean(call_back)
ey120 <- base_original %>% 
  filter(s == 12 & d == 0) %$%
  mean(call_back)

ey131 <- base_original %>% 
  filter(s == 13 & d == 1) %$%
  mean(call_back)
ey130 <- base_original %>% 
  filter(s == 13 & d == 0) %$%
  mean(call_back)

ey141 <- base_original %>% 
  filter(s == 14 & d == 1) %$%
  mean(call_back)
ey140 <- base_original %>% 
  filter(s == 14 & d == 0) %$%
  mean(call_back)

ey151 <- base_original %>% 
  filter(s == 15 & d == 1) %$%
  mean(call_back)
ey150 <- base_original %>% 
  filter(s == 15 & d == 0) %$%
  mean(call_back)

ey161 <- base_original %>% 
  filter(s == 16 & d == 1) %$%
  mean(call_back)
ey160 <- base_original %>% 
  filter(s == 16 & d == 0) %$%
  mean(call_back)

ey171 <- base_original %>% 
  filter(s == 17 & d == 1) %$%
  mean(call_back)
ey170 <- base_original %>% 
  filter(s == 17 & d == 0) %$%
  mean(call_back)

ey181 <- base_original %>% 
  filter(s == 18 & d == 1) %$%
  mean(call_back)
ey180 <- base_original %>% 
  filter(s == 18 & d == 0) %$%
  mean(call_back)

ey191 <- base_original %>% 
  filter(s == 19 & d == 1) %$%
  mean(call_back)
ey190 <- base_original %>% 
  filter(s == 19 & d == 0) %$%
  mean(call_back)

ey201 <- base_original %>% 
  filter(s == 20 & d == 1) %$%
  mean(call_back)
ey200 <- base_original %>% 
  filter(s == 20 & d == 0) %$%
  mean(call_back)

ey211 <- base_original %>% 
  filter(s == 21 & d == 1) %$%
  mean(call_back)
ey210 <- base_original %>% 
  filter(s == 21 & d == 0) %$%
  mean(call_back)

ey221 <- base_original %>% 
  filter(s == 22 & d == 1) %$%
  mean(call_back)
ey220 <- base_original %>% 
  filter(s == 22 & d == 0) %$%
  mean(call_back)

ey231 <- base_original %>% 
  filter(s == 23 & d == 1) %$%
  mean(call_back)
ey230 <- base_original %>% 
  filter(s == 23 & d == 0) %$%
  mean(call_back)

ey241 <- base_original %>% 
  filter(s == 24 & d == 1) %$%
  mean(call_back)
ey240 <- base_original %>% 
  filter(s == 24 & d == 0) %$%
  mean(call_back)

ey251 <- base_original %>% 
  filter(s == 25 & d == 1) %$%
  mean(call_back)
ey250 <- base_original %>% 
  filter(s == 25 & d == 0) %$%
  mean(call_back)

ey261 <- base_original %>% 
  filter(s == 26 & d == 1) %$%
  mean(call_back)
ey260 <- base_original %>% 
  filter(s == 26 & d ==  0) %$%
  mean(call_back)

ey271 <- base_original %>% 
  filter(s == 27 & d == 1) %$%
  mean(call_back)
ey270 <- base_original %>% 
  filter(s == 27 & d == 0) %$%
  mean(call_back)

ey281 <- base_original %>% 
  filter(s == 28 & d == 1) %$%
  mean(call_back)
ey280 <- base_original %>% 
  filter(s == 28 & d ==  0) %$%
  mean(call_back)

# Diferencias por estrato

diff1 <- ey11 - ey10
diff2 <- ey21 - ey20
diff3 <- ey31 - ey30
diff4 <- ey41 - ey40
diff5 <- ey51 - ey50
diff6 <- ey61 - ey60
diff7 <- ey71 - ey70
diff8 <- ey81 - ey80
diff9 <- ey91 - ey90
diff10 <- ey101 - ey100
diff11 <- ey111 - ey110
diff12 <- ey121 - ey120
diff13 <- ey131 - ey130
diff14 <- ey141 - ey140
diff15 <- ey151 - ey150
diff16 <- ey161 - ey160
diff17 <- ey171 - ey170
diff18 <- ey181 - ey180
diff19 <- ey191 - ey190
diff20 <- ey201 - ey200
diff21 <- ey211 - ey210
diff22 <- ey221 - ey220
diff23 <- ey231 - ey230
diff24 <- ey241 - ey240
diff25 <- ey251 - ey250
diff26 <- ey261 - ey260
diff27 <- ey271 - ey270
diff28 <- ey281 - ey280

 EDE <- tibble(diff1, diff2, diff3, diff4, diff5, diff6, diff7, diff8, diff9, diff10, diff11, diff12, diff13,
       diff14, diff15, diff16, diff17, diff18, diff19, diff20, diff21, diff22, diff23, diff24, diff25,
       diff26, diff27, diff28) %>% 
   pivot_longer(diff1: diff28, values_to = "valores", names_to = "Estratos")
 
 
EDE %>% 
  kbl(booktabs = T, linesep = , caption = "Efecto de discriminación por estratos") %>% 
  kable_styling(position = "center", latex_options = "stripped",
                full_width = F, font_size = 12)


EDE %>% 
  ggplot + 
  geom_point(aes(x = Estratos, y = valores, color = Estratos), size = 3) + 
  ggtitle("Efecto de discriminación por estratos") +
  ggthemes::theme_clean() +
  theme(legend.position = 'top', text = element_text(family = 'rob', size = 30)) +
   ggsave('figs/ED_porestratos.png', height = 5.5, width = 7.5) 
  
       
# Estimación de los ponderadores

obs = nrow(base_original)

wt1 <- base_original %>% 
  filter(s == 1 & d == 0) %$%
  nrow(.) / obs

wt2 <- base_original %>% 
  filter(s == 2 & d == 0) %$%
  nrow(.) / obs

wt3 <- base_original %>% 
  filter(s == 3 & d == 0) %$%
  nrow(.) / obs

wt4 <- base_original %>% 
  filter(s == 4 & d == 0) %$%
  nrow(.) / obs

wt5 <- base_original %>% 
  filter(s == 5 & d == 0) %$%
  nrow(.) / obs

wt6 <- base_original %>% 
  filter(s == 6 & d == 0) %$%
  nrow(.) / obs

wt7 <- base_original %>% 
  filter(s == 7 & d == 0) %$%
  nrow(.) / obs

wt8 <- base_original %>% 
  filter(s == 8 & d == 0) %$%
  nrow(.) / obs

wt9 <- base_original %>% 
  filter(s == 9 & d == 0) %$%
  nrow(.) / obs

wt10 <- base_original %>% 
  filter(s == 10 & d == 0) %$%
  nrow(.) / obs

wt11 <- base_original %>% 
  filter(s == 11 & d == 0) %$%
  nrow(.) / obs

wt12 <- base_original %>% 
  filter(s == 12 & d == 0) %$%
  nrow(.) / obs

wt13 <- base_original %>% 
  filter(s == 13 & d == 0) %$%
  nrow(.) / obs

wt14 <- base_original %>% 
  filter(s == 14 & d == 0) %$%
  nrow(.) / obs

wt15 <- base_original %>% 
  filter(s == 15 & d == 0) %$%
  nrow(.) / obs

wt16 <- base_original %>% 
  filter(s == 16 & d == 0) %$%
  nrow(.) / obs

wt17 <- base_original %>% 
  filter(s == 17 & d == 0) %$%
  nrow(.) / obs

wt18 <- base_original %>% 
  filter(s == 18 & d == 0) %$%
  nrow(.) / obs

wt19 <- base_original %>% 
  filter(s == 19 & d == 0) %$%
  nrow(.) / obs

wt20 <- base_original %>% 
  filter(s == 20 & d == 0) %$%
  nrow(.) / obs

wt21 <- base_original %>% 
  filter(s == 21 & d == 0) %$%
  nrow(.) / obs

wt22 <- base_original %>% 
  filter(s == 22 & d == 0) %$%
  nrow(.) / obs

wt23 <- base_original %>% 
  filter(s == 23 & d == 0) %$%
  nrow(.) / obs

wt24 <- base_original %>% 
  filter(s == 24 & d == 0) %$%
  nrow(.) / obs

wt25 <- base_original %>% 
  filter(s == 25 & d == 0) %$%
  nrow(.) / obs

wt26 <- base_original %>% 
  filter(s == 26 & d == 0) %$%
  nrow(.) / obs

wt27 <- base_original %>% 
  filter(s == 27 & d == 0) %$%
  nrow(.) / obs

wt28 <- base_original %>% 
  filter(s == 28 & d == 0) %$%
  nrow(.) / obs

wate = diff1 * wt1 + diff2 * wt2 + diff3 * wt3 + diff4 * wt4 + diff5 * wt5 + diff6 * wt6 + diff7 * wt7 +
  diff8  * wt8 + diff9 * wt9 + diff10 * wt10 + diff11 * wt11 + diff12 * wt12 + diff13 * wt13 + diff14 * wt14 +
  diff15 * wt15 + diff16 * wt16 + diff17 * wt17 + diff18 * wt18 + diff19 * wt19 + diff20 * wt20 +
  diff21 * wt21 + diff22 * wt22 + diff23 * wt23 + diff24 * wt24 + diff25 * wt25 + diff26 * wt26 +
  diff27 * wt27 + diff28 * wt28



kbl(c(wate,gate), booktabs = T, 
    linesep = , caption = "Diferencia entre el estimador considerando los estratos vs el estimados de Neyman(3)",
    col.names = "Estimador de Neyman") %>% 
  kable_styling(position = "center", latex_options = "stripped",
                full_width = F, font_size = 12)


## Pregunta 6====


base_modif <- base_original %>% 
  mutate(total_req = as.numeric(expreq + compreq + comreq + orgreq + educreq))
  

attach(base_modif)

probit_06 <- glm(call_back ~ (black + total_req) + I(black * total_req), family = binomial(link = "probit"))

mfx_06 <- probitmfx(call_back ~ (black + total_req) + I(black * total_req), base_modif) 

stargazer(mfx_06$mfxest, type = "text",coef = list(NULL, mfx_06$mfxest[,1]),
          se = list(NULL, mfx_06$mfxest[,2]))



## Pregunta 7====

probit_07 <- glm(call_back ~ black + yearsexp + I(black * yearsexp) + I(yearsexp * yearsexp) + I(black * yearsexp * yearsexp), family = binomial(link = "probit"))
probitmfx(call_back ~ black + yearsexp + I(black * yearsexp)+ I(yearsexp * yearsexp), base_original)


ols7 <- lm(call_back ~ black + yearsexp + I(black * yearsexp) + I(yearsexp * yearsexp) + I(black * yearsexp * yearsexp) )
stargazer(ols7, probit_07, type = "text")



## Pregunta 8====

# Inciso a
mue_com <- var(call_back)
gamma <- 0.5
phi <- 0.85
prim_parte <- pnorm(phi, mean = 0, sd = 1, lower.tail = T, log.p = F)
seg_parte <- pnorm(1 - (0.01 / 2), mean = 0, sd = 1, lower.tail = T, log.p = F)

power.t.test(d = -tau, sig.level = 0.01, power = 0.85, sd = sqrt(mue_com), alternative = "two.sided")

# Inciso b

# primera forma
#funcion_poder = function(x){pnorm(sqrt(3768 * (tau ^ 2 / var(call_back)) * x * (1 - x)) - qnorm(1 - (0.01 / 2)))}
#curve(funcion_poder)

gamma1 <- seq(from = 0, to = 1, by = 0.1)
nuevo <- (gamma1 * (1 - gamma1) * 3768 * (tau^{2} / mue_com))^{1/2} - seg_parte

#Se buscaron los datos en el vector nuevo en las tablas 
distacum <- c(0.2005, 0.5557, 0.6808, 0.7454, 0.7764, 0.7881, 0.7764, 0.7454, 0.6808, 0.5557, 0.2005)
conj <- tibble(gamma1, nuevo, distacum)


conj %>%
  ggplot() +
  geom_line(aes(x = gamma1, y = distacum), color = "#9d0208", size = 1.5) +
  labs(fill = 'am',
       x = 'Proporcion de tratamiento',
       y = 'Poder estadístico',
       title = 'Tradeoff entre poder estadítico y proporción de tratamiento y control') +
  ggthemes::theme_clean() +
  ggsave('figs/Tradeoff.png', height = 5.5, width = 7.5) 



