# Bibliotecas #####
## tidyverse para manipulação e visualização
## gganimate para criar a animação final
## readxl para importar normal dos dados

library(tidyverse)
library(gganimate)
library(readxl)
library(ggthemes)

# Importação dos Dados #####

## Tabela Inicial
Dados <-
  read.csv(file = "Espiral Climática/Dados/Dados.csv",
             sep = ";", dec = ",", header = T, skip = 10, na.strings = "null") %>%
  select(!X) %>%
  rename(Data = "Data.Medicao",
         Prec = "PRECIPITACAO.TOTAL..DIARIO.mm.",
         Tmax = "TEMPERATURA.MAXIMA..DIARIA..C.",
         Tmed = "TEMPERATURA.MEDIA.COMPENSADA..DIARIA..C.",
         Tmin = "TEMPERATURA.MINIMA..DIARIA..C.") %>%
  separate(col = Data, into = c("Ano", "Mes", "Dia"), sep = "-")


## Importação de tabela de médias normais 1961-1990
T_Med_Esp <-
  read_excel("Espiral Climática/Dados/Temperatura-Media-Compensada_NCB_1961-1990.xlsx",
             sheet = "Temperatura_Media", skip = 3, na = "-")
    # Limpando os dados: separando apenas a linha de interesse e pivotando para formatar
T_Med_Esp <-
  T_Med_Esp[197, -c(1:3,16)] %>% pivot_longer(cols = everything(),
                                              names_to = "MesN",
                                              values_to = "Tmed") %>%
  mutate("Mes" = 1:12)

# Manipulação dos dados #####

## Imputação

# Dados_pmm <- mice::complete(mice::mice(Dados, method = "pmm"))
# Dados <- Dados_pmm

## Organizando

## Temperaturas Médidas Mensais

T_Med_Obs <-
  Dados %>%
  group_by(Ano, Mes) %>%
  summarise(TmedM = mean(Tmed, na.rm = TRUE),
         TsdM = sd(Tmed, na.rm = TRUE)) %>% ungroup() %>%
  mutate(Ano =       as.numeric(Ano),
         Mes =       as.numeric(Mes))

## Calculando as anomalias em um novo objeto

Dif_T_Med <-
  data.frame(
    Ano =       as.numeric(T_Med_Obs$Ano),
    Mes =       as.numeric(T_Med_Obs$Mes),
    T_Med_Obs_ = T_Med_Obs$TmedM,
    T_Med_Esp_ = rep(T_Med_Esp$Tmed, 61),
    Dif_T_M =   T_Med_Obs$TmedM - rep(T_Med_Esp$Tmed, 61)
  )


# Visualização #####

## Rápida visualização da Temperatura Média Normal 1961-1990
ggplot(T_Med_Esp) +
  aes(x = Mes, y = Tmed, color =  Tmed) +
  geom_line(size = 1.3, alpha = 0.7) +
  geom_point(size = 3, alpha = 0.5) +
  scale_x_continuous(breaks = 1:12,
                     labels = toupper(month.abb)) +
  scale_color_viridis_c(guide = "none") +
  labs(x = "", y = "Normais Temperatura Média Mensal") +
  theme_clean()
ggsave("Temp_Esp.png", width = 2000, height = 1600, units = "px", bg = "white")

## Rápida visualização comparando temperaturas normais x temperaturas médias
ggplot() +
  geom_line(data = T_Med_Obs,
            mapping = aes(x = Mes, y = TmedM,
                          color = Ano, group = Ano),
            alpha = 0.35, size = 1) +
  geom_errorbar(data = T_Med_Obs,
                mapping = aes(x = Mes, y = TmedM,
                              ymin = TmedM - TsdM,
                              ymax = TmedM + TsdM,
                              color = Ano, group = Ano),
                width = 0.1, alpha = 0.35) +
  geom_line(data = T_Med_Esp,
            mapping = aes(x = Mes, y = Tmed),
            color = "Black", size = 1) +
  scale_x_discrete(labels = toupper(month.abb)) +
  scale_color_viridis_c() +
  labs(x = "", y = "Temperatura Média Mensal") +
  theme_clean()
ggsave("Temp_Obs.png", width = 2000, height = 1600, units = "px", bg = "white")

## Visualizando evolução da diferença através dos anos
ggplot(Dif_T_Med,
       aes(x = Mes, y = Dif_T_M, group = Ano, color = Ano)) +
  geom_line(alpha = 0.35, size = 1) +
  geom_point(alpha = 0.5) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  scale_color_viridis_c() +
  scale_x_continuous(breaks = 1:12,
                     labels = toupper(month.abb)) +
  scale_y_continuous(breaks = -5:5, limits = c(-5,5)) +
  theme_clean() +
  labs(x = "", y = "Anormalidades de Temperatura")
ggsave("Anomalias_Temp_L.png", width = 2000, height = 1600, units = "px", bg = "white")

ggplot(Dif_T_Med) +
  geom_boxplot(aes(x = Ano, y = Dif_T_M, group = Ano, color = Ano)) +
  scale_y_continuous(breaks = -5:5, limits = c(-5,5)) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  scale_color_viridis_c() +
  theme_clean()
ggsave("Anomalias_Temp_Bx.png", width = 2000, height = 1600, units = "px", bg = "white")

ggplot(Dif_T_Med,
       aes(x = Ano, y = Dif_T_M, color = Dif_T_M)) +
  geom_point() +
  geom_smooth(method = "lm",se = F, color = "red") +
  ggpmisc::stat_poly_eq(formula = y ~ x,
                        aes(label = paste(..eq.label.., ..rr.label.., ..p.value.label.., sep = "*`,`~")),
                        parse = TRUE,
                        label.x.npc = "right",
                        vstep = 0.05) +
  scale_y_continuous(breaks = -5:5, limits = c(-5,5)) +
  scale_color_viridis_c(guide = "none") +
  theme_clean()
ggsave("Anomalias_Temp_Points.png", width = 2000, height = 1600, units = "px", bg = "white")

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

# Pl <-
  ggplot(Dados_Pl)+
    aes(x = Mes, y = Dif_T_M,
             group = Ano, color = Dif_T_M) +
    geom_line(size = 0.6, alpha = 0.7) +
    geom_point(alpha = 0.3) +
  # O geom_label não quer funcionar, não sei o motivo
    geom_label(aes(x = 0, y = -8, label = rep(1961:2021, each = 13)),
             size = 10, fill = "black", label.size = 0) +
    scale_color_viridis_c(guide = "none") +
    scale_x_continuous(breaks = 1:12,
                     labels = toupper(month.abb)) +
    scale_y_continuous(limits = c(-8, 6)) +
    coord_polar(start = -2*pi/12) +
  # theme_clean() +
    labs(x = "") +
  # Tema
  # theme_void() +
  theme(
    # panel.background = element_blank(),
    plot.background = element_rect(fill = "white"),
    # panel.grid = element_rect(fill = "gray"),
    # axis.text.x = element_text(color = "yellow", size = 15),
    axis.text.y = element_blank(),
    axis.title.y = element_blank(),
    axis.title.x = element_blank(),
    axis.ticks = element_blank()#,
  #   axis.title = element_text(color="white", size = 13),
  #   plot.title = element_text(color="white", hjust = 0.5,size = 15)
    ) +
  transition_manual(i, cumulative = T)

animate(plot = Pl, nframes = 300, fps = 6, res = 200) %>%
anim_save(filename = "Gif.gif", animation = .,
          width = 2000, height = 2000, units = "px")

# Grafico interativo?

library(plotly)

Dif_T_Med %>%
  mutate(raio = Dif_T_M + 8,
         theta = 2 * pi * (Mes - 1) / 12,
         x = raio * sin(theta),
         y = raio * cos(theta),
         i = 1:732) %>%
plot_ly(data = .,
        x = ~x, y = ~y, z = ~i,
        type = "scatter3d",
        mode = "lines", sizes = 3,
        line = list(width = 6, color = ~Dif_T_M,
                    colorscale = list(c(-4.5293, "blue"), c(3.3903, "red")))
        )

