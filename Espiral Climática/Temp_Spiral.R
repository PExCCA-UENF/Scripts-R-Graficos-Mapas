# Bibliotecas #####

library(tidyverse) ## tidyverse para manipulação e visualização
library(gganimate) ## gganimate para criar a animação final
library(readxl) ## readxl para importar normal dos dados
library(ggthemes) ## ggthemes para alguns temas

# Importação dos Dados #####

## Tabela Inicial
Dados <-
  read.csv(file = "Espiral Climática/Dados/Dados.csv", # Seleciona o arquivo
             sep = ";", dec = ",", header = T, skip = 10, na.strings = "null") %>% # Configuração da importação
  select(!X) %>% # Remove coluna X extra (Ela esta presente devido a formatação dos dados)
  rename(Data = "Data.Medicao", # Renomeando
         Prec = "PRECIPITACAO.TOTAL..DIARIO.mm.",
         Tmax = "TEMPERATURA.MAXIMA..DIARIA..C.",
         Tmed = "TEMPERATURA.MEDIA.COMPENSADA..DIARIA..C.",
         Tmin = "TEMPERATURA.MINIMA..DIARIA..C.") %>%
  separate(col = Data, into = c("Ano", "Mes", "Dia"), sep = "-") # Divide coluna Data em 3


## Importação de tabela de médias normais 1961-1990
T_Med_Normal <-
  read_excel("Espiral Climática/Dados/Temperatura-Media-Compensada_NCB_1961-1990.xlsx",
             sheet = "Temperatura_Media", skip = 3, na = "-")

## Limpando os dados: separando apenas a linha de interesse e pivotando para formatar
T_Med_Normal <-
  T_Med_Normal[197, -c(1:3,16)] %>% # Seleciona linha referente a campos e colunas Janeiro-Dezembro
  # Pivotar a tabela para ficar com uma coluna de meses e uma com as temperaturas normais
  pivot_longer(cols = everything(), # Informa que desejamos todas as colunas
               names_to = "MesN", # Informa o nome da coluna com os meses
               values_to = "Tmed") %>% # Informa o nome da coluna com os valores
  mutate("Mes" = 1:12) # Adiciona uma coluna com o número dos meses

# Manipulação dos dados #####

## Imputação

Dados <- mice::complete(mice::mice(Dados, method = "pmm"))

#
## Organizando

## Temperaturas Médidas Mensais

T_Med_Mensal <-
  Dados %>%
  group_by(Ano, Mes) %>%
  summarise(TmedM = mean(Tmed, na.rm = TRUE),
         TsdM = sd(Tmed, na.rm = TRUE)) %>%
  ungroup() %>%
  mutate(Ano = as.numeric(Ano),
         Mes = as.numeric(Mes))

## Calculando as anomalias em um novo objeto

T_Med_Anomalias <-
  data.frame(
    Ano =       as.numeric(T_Med_Mensal$Ano),
    Mes =       as.numeric(T_Med_Mensal$Mes),
    T_Med_Mensal_ = T_Med_Mensal$TmedM,
    T_Med_Normal_ = rep(T_Med_Normal$Tmed, 61),
    Dif_T_M =   T_Med_Mensal$TmedM - rep(T_Med_Normal$Tmed, 61)
  )


# Visualização #####

## Rápida visualização da Temperatura Média Normal 1961-1990

ggplot(T_Med_Normal) +
  aes(x = Mes, y = Tmed, color =  Tmed) +
  geom_line(size = 1.3, alpha = 0.7) +
  geom_point(size = 3, alpha = 0.5) +
  scale_x_continuous(breaks = 1:12,
                     labels = toupper(month.abb)) +
  labs(x = "", y = "Normais Temperatura Média Mensal") +
  scale_color_gradient2(low = "blue", high = "red",
                        guide = "none", midpoint = 24) +
  theme(
    panel.background = element_rect(fill = "Gray10"),
    plot.background = element_rect(fill = "Gray10"),
    panel.grid = element_line(color = "#026ec3"),
    axis.text.x = element_text(color = "yellow", size = 10),
    axis.text.y = element_text(color = "yellow", size = 10),
    axis.title.y = element_text(color = "yellow", size = 10),
    axis.line = element_line(color = "yellow"),
    axis.ticks = element_line(color = "yellow"),
    panel.grid.minor = element_blank(),
    )

ggsave("Temp_Normal.png", width = 2000, height = 1600, units = "px", bg = "white")

## Rápida visualização comparando temperaturas normais x temperaturas médias
ggplot() +
  geom_line(data = T_Med_Mensal,
            mapping = aes(x = Mes, y = TmedM,
                          color = Ano, group = Ano),
            alpha = 0.35, size = 1) +
  geom_errorbar(data = T_Med_Mensal,
                mapping = aes(x = Mes, y = TmedM,
                              ymin = TmedM - TsdM,
                              ymax = TmedM + TsdM,
                              color = Ano, group = Ano),
                width = 0.1, alpha = 0.35) +
  geom_line(data = T_Med_Normal,
            mapping = aes(x = Mes, y = Tmed),
            color = "Black", size = 1) +
  scale_x_discrete(labels = toupper(month.abb)) +
  labs(x = "", y = "Temperatura Média Mensal") +
scale_color_gradient(low = "blue", high = "red") +
  theme(
    panel.background = element_rect(fill = "Gray10"),
    plot.background = element_rect(fill = "Gray10"),
    panel.grid = element_line(color = "#026ec3"),
    axis.text.x = element_text(color = "yellow", size = 10),
    axis.text.y = element_text(color = "yellow", size = 10),
    axis.title.y = element_text(color = "yellow", size = 10),
    axis.line = element_line(color = "yellow"),
    axis.ticks = element_line(color = "yellow"),
    panel.grid.minor = element_blank(),
  )

ggsave("Temp_Obs.png", width = 2000, height = 1600, units = "px", bg = "white")

## Visualizando evolução da diferença através dos anos

ggplot(T_Med_Anomalias) +
  geom_boxplot(aes(x = Ano, y = Dif_T_M, group = Ano), color = "yellow", fill = "yellow4") +
  scale_y_continuous(breaks = -5:5, limits = c(-5,5)) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  # scale_color_gradient2(low = "blue", high = "red",
                        # guide = "none", midpoint = 24) +
  theme(
    panel.background = element_rect(fill = "Gray10"),
    plot.background = element_rect(fill = "Gray10"),
    panel.grid = element_line(color = "#026ec3"),
    axis.text.x = element_text(color = "yellow", size = 10),
    axis.title.x = element_text(color = "yellow", size = 10),
    axis.text.y = element_text(color = "yellow", size = 10),
    axis.title.y = element_text(color = "yellow", size = 10),
    axis.line = element_line(color = "yellow"),
    axis.ticks = element_line(color = "yellow"),
    panel.grid.minor = element_blank(),
  )

ggsave("Anomalias_Temp_Bx.png", width = 2000, height = 1600, units = "px", bg = "white")

ggplot(T_Med_Anomalias,
       aes(x = Ano, y = Dif_T_M, color = Dif_T_M)) +
  geom_point() +
  geom_smooth(method = "lm",se = F, color = "white") +
  ggpmisc::stat_poly_eq(formula = y ~ x, color = "white",
                        aes(label = paste(..eq.label.., ..rr.label.., ..p.value.label.., sep = "*`,`~")),
                        parse = TRUE,
                        label.x.npc = "right",
                        vstep = 0.05) +
  scale_y_continuous(breaks = -5:5, limits = c(-5,5)) +
  # scale_color_viridis_c(guide = "none") +
  # theme_clean() +
  scale_color_gradient(low = "blue", high = "red", guide = "none") +
  theme(
    panel.background = element_rect(fill = "Gray10"),
    plot.background = element_rect(fill = "Gray10"),
    panel.grid = element_line(color = "#002240"),
    axis.text.x = element_text(color = "yellow", size = 10),
    axis.text.y = element_text(color = "yellow", size = 10),
    axis.title.y = element_text(color = "yellow", size = 10),
    # panel.grid.minor = element_blank(),
    # panel.grid.major.y = element_line(colour = "#002240"),
    # panel.grid.major.x = element_blank()
    )

ggsave("Anomalias_Temp_Points.png", width = 2000, height = 1600, units = "px", bg = "white")

# Confecção da animação ######

## Temos que resolver um problema para o gráfico
## circular: A conexção dezembro-janeiro.
## Para isso, vamos criar um mês "zero" em cada ano,
## que representa o mês de dezembro do ano anterior.
## Depois é só plotar o gráfico e definr a escala x
## para cortar o escesso.

T_Med_0 <-
T_Med_Anomalias %>%
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
  T_Med_Anomalias %>%
  rbind(T_Med_0) %>%
  arrange(Ano, Mes) %>%
  cbind(i)

Legenda <-
  data.frame(
    x = 1,
    y = seq(from = -2, to = 2, by = 1),
    labels = c("-2\u00B0C", "-1\u00B0C", "0\u00B0C",
               "1\u00B0C", "2\u00B0C"))

# Grafico

# Pl <-
  ggplot(Dados_Pl)+
    aes(x = Mes, y = Dif_T_M,
             group = Ano, color = Dif_T_M) +
    geom_hline(yintercept = -2, color = "#326290", size = 1.3, alpha = 0.5) +
    geom_hline(yintercept = -1, color = "#6ea2ca", size = 1.3, alpha = 0.5) +
    geom_hline(yintercept = 0, color = "White", size = 1.3, alpha = 0.5) +
    geom_hline(yintercept = 1, color = "#ec822f", size = 1.3, alpha = 0.5) +
    geom_hline(yintercept = +2, color = "#a54122", size = 1.3, alpha = 0.5) +
    geom_label(data = Legenda,
               aes(x = x, y = y, label = labels),
               inherit.aes = F,
               color = c("Red", "Yellow", "White", "Yellow", "Red"),
               fill = "Black", label.size = 0, size = 3, alpha = 0.5) +
    # geom_label(aes(x = 0, y = -10, label = rep(1961:2021, each = 13)),
    #          size = 10, fill = "black", label.size = 0) +
    geom_line(size = 1.2) +
    geom_point() +
    scale_color_gradient2(low = "blue", high = "red",
                          midpoint = 0, guide = "none") +
    scale_x_continuous(breaks = 1:12,
                       labels = toupper(month.abb)) +
    scale_y_continuous(limits = c(-10, 4)) +
    coord_polar(start = -2*pi/12) +
    labs(x = "",
         color = "Anomalias de Temperatura") +
    theme(
    panel.background = element_rect(fill = "Gray10"),
    plot.background = element_rect(fill = "Gray10"),
    panel.grid = element_line(color = "#002240"),
    axis.text.x = element_text(color = "yellow", size = 15),
    axis.text.y = element_blank(),
    axis.title.y = element_blank(),
    axis.title.x = element_blank(),
    axis.ticks = element_blank()
    ) +
  transition_manual(i, cumulative = T)

animate(plot = Pl, nframes = 300, fps = 6, res = 200) %>%
anim_save(filename = "Gif.gif", animation = .,
          width = 2000, height = 2000, units = "px")
# ggsave("Anomalias_Temp_Spiral.png", width = 2000, height = 1600, units = "px", bg = "Gray10")


# Grafico interativo?

library(plotly)

T_Med_Anomalias %>%
  mutate(raio = Dif_T_M + 8,
         theta = 2 * pi * (Mes - 1) / 12,
         x = raio * sin(theta),
         y = raio * cos(theta),
         i = 1:732) %>%
  unite(col = "Data", c(Ano, Mes), sep = "-") %>%
  mutate(Data = lubridate::ym(Data)) %>%
  plot_ly(data = .,
        x = ~x, y = ~y, z = ~i,
        # frame = ~i, # Temtiva de animação
        type = "scatter3d",
        mode = "lines", sizes = 3,
        line = list(width = 6, color = ~Dif_T_M,
                    colorscale = list(c(-4.5293, "blue"), c(3.3903, "red")))
        )




