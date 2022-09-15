# Bibliotecas
## tidyverse para manipulação e visualização
## gganimate para criar a animação final

library(tidyverse)
library(gganimate)
library(readxl)

# Dados

## Importação do banco imputado
Dados <- read.csv(file = file.choose(), sep = ";",
               dec = ".", header = T, quote = "",
               skip = 10, na.strings = "null")

## Criando vetor de datas para colar na base de dados
Data <- seq.Date(from = as.Date("1961/01/01"),
                 to = as.Date("2021/12/31"),
                 by = "day")

## Importação do vetor de médias compensada (?)
T_Med_Esp <-
  read_xls()

## Juntar "Dados" e "Data";
## Selecionar apenas colunas desejadas
Dados <-
  tibble(Data = Data,
         Ano = lubridate::year(Data),
         Mes = lubridate::month(Data),
         Tmed = Dados$`TEMPERATURA.MEDIA.COMPENSADA..DIARIA..C.`)

# Imputação?
#

# Manipulação dos dados
## A visualização que desejamos criar ilustra a
## diferença entre a temperatura mensal esperada
## e a observada. Para saber qual a temperatura
## mensal esperada, vamos fazer um cálculo de
## médias por mes em nosso banco de dados.

T_Med_Esp <-
  Dados %>%
    group_by(Mes) %>%
    summarise(T_Med_Esp_M = mean(Tmed, na.rm = T),
              T_Med_Esp_SD = sd(Tmed, na.rm = T))

## Rápida e simples visualização
ggplot(T_Med_Esp) +
  aes(x = Mes, y = T_Med_Esp_M, color = T_Med_Esp_M) +
  geom_line(size = 1.3) +
  geom_errorbar(aes(ymin = T_Med_Esp_M - T_Med_Esp_SD,
                    ymax = T_Med_Esp_M + T_Med_Esp_SD),
                width = 0.1) +
  labs(x = "",
       y = "Temperatura Média Esperada Mensal")+
  scale_x_continuous(breaks = 1:12,
                     labels = toupper(month.abb)) +
  scale_color_viridis_c(guide = "none") +
  theme_minimal()
ggsave("Temp_Esp.png", width = 1000, height = 800, units = "px", bg = "white")
## Vamos agora achar as temperaturas médias mensais
## em cada ano e então calcular a diferença entre
## elas e as temperaturas esperadas.

T_Med_Obs <-
  Dados %>%
  group_by(Ano, Mes) %>%
  summarise(T_Med_Obs_M = mean(Tmed, na.rm = T),
            T_Med_Obs_SD = sd(Tmed, na.rm = T)) %>%
  ungroup() %>%
  filter(Ano != 2021)

## Rápida visualização comparando Observado com Esperado
ggplot() +
  geom_line(
    data = T_Med_Obs,
    mapping = aes(
      x = Mes,
      y = T_Med_Obs_M,
      color = T_Med_Obs_M,
      group = Ano
      ),
    alpha = 0.35
    ) +
  geom_errorbar(
    data = T_Med_Obs,
    mapping = aes(
      x = Mes,
      y = T_Med_Obs_M,
      ymin = T_Med_Obs_M - T_Med_Obs_SD,
      ymax = T_Med_Obs_M + T_Med_Obs_SD,
      color = T_Med_Obs_M,
      group = Ano
      ),
    width = 0.1,
    alpha = 0.35
    ) +
  geom_line(
    data = T_Med_Esp,
    mapping = aes(
      x = Mes,
      y = T_Med_Esp_M
      ),
    color = "Black",
    size = 1.4
    ) +
  geom_errorbar(
    data = T_Med_Esp,
    mapping = aes(
      x = Mes,
      y = T_Med_Esp_M,
      ymin = T_Med_Esp_M - T_Med_Esp_SD,
      ymax = T_Med_Esp_M + T_Med_Esp_SD
      ),
    width = 0.1,
    color = "black") +
  labs(x = "",
       y = "Temperatura Média Observada Mensal")+
  scale_x_continuous(breaks = 1:12,
                     labels = toupper(month.abb)) +
  scale_color_viridis_c(guide = "none") +
  theme_minimal()
ggsave("Temp_Obs.png", width = 1000, height = 800, units = "px", bg = "white")
## Calculando as diferenças em um novo objeto

Dif_T_Med <-
  tibble(
    Ano =       T_Med_Obs$Ano,
    Mes =       T_Med_Obs$Mes,
    Dif_T_M =   T_Med_Obs$T_Med_Obs_M -
                rep(T_Med_Esp$T_Med_Esp_M, 60)
  )


# Confecção da animação

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

i <- 1:780

## Unir tabela M0 e coluna i nos dados

Dados_Pl <-
  Dif_T_Med %>%
  rbind(T_Med_0) %>%
  arrange(Ano, Mes) %>%
  cbind(i)

## Precisamos de um objeto com valores para serem
## inseridos na função geom_label para servirem de
## marcação da diferença de temperatura.

Leg_Raio <-
  data.frame(
    x = 1,
    y = seq(from = -2, to = 2, by = 1),
    labels = c("-2\u00B0C", "-1\u00B0C", "0\u00B0C",
               "1\u00B0C", "2\u00B0C"
               )
  )

## Plot

Pl <-
  ggplot(
    data = Dados_Pl,
    mapping = aes(
      x = Mes,
      y = Dif_T_M,
      group = Ano,
      color = Ano
      )
    ) +

    # Linhas nas marcações -2 a +2
    geom_hline(yintercept = -2, color = "Red", size = 1.3) +
    geom_hline(yintercept = -1, color = "Yellow", size = 1.3) +
    geom_hline(yintercept = 0, color = "White", size = 1.3) +
    geom_hline(yintercept = 1, color = "Yellow", size = 1.3) +
    geom_hline(yintercept = +2, color = "Red", size = 1.3) +

    # Legenda nas marcações -2 a +2
    geom_label(
      data = Leg_Raio,
      mapping = aes(
        x = x,
        y = y,
        label = labels
        ),
      inherit.aes = F,
      color = c("Red", "Yellow", "White", "Yellow", "Red"),
      fill = "Black", label.size = 0, size = 5
      ) +

    # Legenda "Ano" no centro o gráfico
    # geom_label(
    #   mapping = aes(
    #     x = 1,
    #     y = -8,
    #     label = Ano
    #     ),
    #   size = 10,
    #   fill = "black",
    #   label.size = 0
    #   ) +

    # Linhas do gráfico
    geom_path(
      size = 1.2,
      alpha = 0.7
      ) +

    # Escala de cores
    scale_color_viridis_c(guide = "none") +

    # Definindo limites de x, y e gráfico redondo
    scale_x_continuous(
      breaks = 1:12,
      labels = toupper(month.abb)
      ) +
    scale_y_continuous(
      limits = c(-8, 3)
      ) +
    coord_polar(
      start = -2*pi/12
      ) +

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
    transition_manual(i, cumulative = T, )


# Pl

anim_save(Pl, filename = "Temp_Pl.gif")
#anim_save(
#  animate(Pl, fps = 12,
#          width = 5, height = 5,
#          units = "in", res = 500),
#  filename = "Temp_Pl.gif")

