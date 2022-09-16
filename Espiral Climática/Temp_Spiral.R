# Bibliotecas #####
## tidyverse para manipulação e visualização
## gganimate para criar a animação final

library(tidyverse)
library(gganimate)
library(readxl)

# Importação dos Dados #####

## Tabela Inicial
Dados <-
  read.csv(file = "Espiral Climática/dados_83696_D_1961-01-01_2021-12-31.csv",
          sep = ";", dec = ".", header = T, quote = "",
          skip = 10, na.strings = "null") %>%
  dplyr::select("Data.Medicao", "TEMPERATURA.MINIMA..DIARIA..C.", "TEMPERATURA.MAXIMA..DIARIA..C.",
         "TEMPERATURA.MEDIA.COMPENSADA..DIARIA..C.", "PRECIPITACAO.TOTAL..DIARIO.mm.",
         "UMIDADE.RELATIVA.DO.AR..MEDIA.DIARIA...") %>%
  rename("Data" = "Data.Medicao",
         "Tmin" = "TEMPERATURA.MINIMA..DIARIA..C.",
         "Tmax" = "TEMPERATURA.MAXIMA..DIARIA..C.",
         "Tmed" = "TEMPERATURA.MEDIA.COMPENSADA..DIARIA..C.",
         "Prec" =  "PRECIPITACAO.TOTAL..DIARIO.mm.",
         "Umid" = "UMIDADE.RELATIVA.DO.AR..MEDIA.DIARIA...")

## Criando vetor de datas para colar na base de dados
Data <- seq.Date(from = as.Date("1961/01/01"),
                 to = as.Date("2021/12/31"),
                 by = "day")

## Importação de tabela de médias compensadas 1961-1990
T_Med_Esp <-
  read_excel("Espiral Climática/Temperatura-Media-Compensada_NCB_1961-1990.xlsx",
             sheet = "Temperatura_Media", skip = 3, na = "-")

T_Med_Esp <-
  T_Med_Esp[207, -c(1:3,16)] %>% pivot_longer(cols = everything(),
                                              names_to = "MesN",
                                              values_to = "Tmed") %>%
  mutate("Mes" = 1:12)

# Manipulação dos dados #####

## Imputação

Dados_pmm <- mice::complete(mice::mice(Dados, method = "pmm"))

## Organizando

## Juntar "Dados" e "Data";
## Selecionar apenas colunas desejadas

Dados_pmm_org <-
  tibble(Data = Data,
         Ano = lubridate::year(Data),
         Mes = lubridate::month(Data),
         Tmed = Dados_pmm$Tmed)

## Temperaturas Médidas Mensais Observadas

T_Med_Obs <-
  Dados_pmm_org %>%
  group_by(Ano, Mes) %>%
  summarise(Tmed = mean(Tmed, na.rm = T),
            Tsd = sd(Tmed, na.rm = T)) %>%
  ungroup()

## Calculando as anomalias em um novo objeto

Dif_T_Med <-
  data.frame(
    Ano =       T_Med_Obs$Ano,
    Mes =       T_Med_Obs$Mes,
    T_Med_Obs_ = T_Med_Obs$Tmed,
    T_Med_Esp_ = rep(T_Med_Esp$Tmed, 61),
    Dif_T_M =   T_Med_Obs$Tmed - rep(T_Med_Esp$Tmed, 61)
  )


# Visualização #####

## Rápida visualização da Temperatura Média Esperada
ggplot(T_Med_Esp) +
  aes(x = Mes, y = Tmed, color =  Tmed) +
  geom_line(size = 1.3, alpha = 0.7) +
  geom_point(size = 3, alpha = 0.5) +
  labs(title = "Temperatura Média Compensada 1961-1990 Santa Maria Madalena",
       x = "",
       y = "Temperatura Média Esperada Mensal")+
  scale_x_continuous(breaks = 1:12,
                     labels = toupper(month.abb)) +
  scale_color_viridis_c(guide = "none") +
  theme_minimal()
ggsave("Temp_Esp.png", width = 2000, height = 1600, units = "px", bg = "white")

## Rápida visualização comparando Observado com Esperado
ggplot() +
  geom_line(data = T_Med_Obs,
            mapping = aes(x = Mes, y = Tmed,
                          color = Tmed, group = Ano),
            alpha = 0.35, size = 0.2) +
  geom_errorbar(data = T_Med_Obs,
                mapping = aes(x = Mes, y = Tmed,
                              ymin = Tmed - Tsd,
                              ymax = Tmed + Tsd,
                              color = Tmed, group = Ano),
                width = 0.1, alpha = 0.35) +
  geom_line(data = T_Med_Esp,
            mapping = aes(x = Mes, y = Tmed),
            color = "Black", size = 1) +
  labs(title = "Temperatura Média Compensada 1961-1990 vs Temperatura Média Observada",
       x = "",
       y = "Temperatura Média Observada Mensal")+
  scale_x_continuous(breaks = 1:12,
                     labels = toupper(month.abb)) +
  scale_color_viridis_c(guide = "none") +
  theme_minimal()
ggsave("Temp_Obs.png", width = 2000, height = 1600, units = "px", bg = "white")

## Visualizando evolução da diferença através dos anos
ggplot(Dif_T_Med,
       aes(x = Mes, y = Dif_T_M, group = Ano, color = Ano)) +
  geom_line(alpha = 0.35, size = 0.2) +
  scale_color_viridis_c() +
  scale_x_continuous(breaks = 1:12,
                     labels = toupper(month.abb)) +
  scale_y_continuous(breaks = -5:5, limits = c(-5,5)) +
  theme_minimal() +
  labs(x = "",
       y = "Anormalidades na Temperatura Média Mensal")
ggsave("Dif_Anos_L.png", width = 2000, height = 1600, units = "px", bg = "white")

ggplot(Dif_T_Med) +
  geom_boxplot(aes(x = Ano, y = Dif_T_M, group = Ano)) +
  labs(y = "Anormalidades na Temperatura Média Mensal") +
  scale_y_continuous(breaks = -5:5, limits = c(-5,5)) +
  theme_minimal()
ggsave("Dif_Anos_Bx.png", width = 2000, height = 1600, units = "px", bg = "white")

ggplot(Dif_T_Med,
       aes(x = Ano, y = Dif_T_M, color = Dif_T_M)) +
  geom_point() +
  geom_smooth(method = "lm",se = F, color = "red") +
  ggpmisc::stat_poly_eq(formula = y ~ x,
                        aes(label = paste(..eq.label.., ..rr.label.., ..p.value.label.., sep = "*`,`~")),
                        parse = TRUE,
                        label.x.npc = "right",
                        vstep = 0.05) +
  labs(y = "Anormalidades na Temperatura Média Mensal") +
  scale_y_continuous(breaks = -5:5, limits = c(-5,5)) +
  scale_color_viridis_c(guide = "none") +
  theme_minimal()
ggsave("Dif_Anos_Points.png", width = 2000, height = 1600, units = "px", bg = "white")

# Confecção da animação ######

## Temos que resolver um problema para o gráfico
## circular: A conexção dezembro-janeiro.
## Para isso, vamos criar um mês "zero" em cada ano,
## que representa o mês de dezembro do ano anterior.
## Depois é só plotar o gráfico e definr a escala x
## para cortar o escesso.

T_Med_0 <-
Dif_T_Med %>%
  filter(Mes == 12) %>%
  mutate(Ano = Ano + 1,
         Mes = 0)

## Outro problema enfrentado é a falta de um vetor
## data para adicionar na função transition_reveal()
## de forma a animar o gráfico mês a mês. Para solu-
## cionar a questão, vamos criar um vetor fictício i
## com uma sequencia de numeros, ele funcinará como
## contagem de frames.

i <- 1:793

## Unir tabela M0 e coluna i nos dados

Dados_Pl <-
  Dif_T_Med %>%
  rbind(T_Med_0) %>%
  arrange(Ano, Mes) %>%
  cbind(i)

# Grafico

Pl <-
  ggplot(Dados_Pl,
         aes(x = Mes, y = Dif_T_M,
             group = Ano, color = Dif_T_M)) +
  geom_path(size = 1.2, alpha = 0.7) +
  # O geom_label não quer funcionar, não sei o motivo
  # geom_label(aes(x = 1, y = -8, label = i),
             # size = 10, fill = "black", label.size = 0) +
  scale_color_viridis_c(guide = "none") +
  scale_x_continuous(breaks = 1:12,
                     labels = toupper(month.abb)) +
  scale_y_continuous(limits = c(-6, 6)) +
  coord_polar(start = -2*pi/12) +
  # Tema
  theme_void() +
  theme(
    panel.background = element_rect(fill = "Black"),
    plot.background = element_rect(fill = "Black"),
    panel.grid = element_blank(),
    axis.text.x = element_text(color = "yellow", size = 15),
    axis.text.y = element_blank(),
    axis.title.y = element_blank(),
    axis.title.x = element_blank(),
    axis.ticks = element_blank(),
    axis.title = element_text(color="white", size = 13),
    plot.title = element_text(color="white", hjust = 0.5,size = 15)) +
  transition_reveal(i)

animate(plot = Pl, nframes = 300, fps = 6, res = 200) %>%
anim_save(filename = "Gif.gif", animation = .,
          width = 2000, height = 2000, units = "px")
