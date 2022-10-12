# Bibliotecas #####

library(tidyverse) ## tidyverse para manipulação e visualização
library(readxl) ## readxl para importar normal dos dados
library(ggthemes) ## ggthemes para alguns temas
library(magick) ## utilizado para animação
library(animation) ## utilizado para animação
library(magrittr) ## utilizado para manipular dados, pipe %$%

# Importação dos Dados #####

## Tabela Inicial

Dados <-
  read.csv(file = "Campos.csv", # Seleciona o arquivo
           sep = ";", dec = ",", header = T, skip = 10, na.strings = "null") %>% # Configuração da importação
  # Remove coluna X extra (Ela esta presente devido a formatação dos dados)
  select(!X) %>%
  # Renomeando todas as colunas
  rename(Data = "Data.Medicao",
         Prec = "PRECIPITACAO.TOTAL..DIARIO.mm.",
         Tmax = "TEMPERATURA.MAXIMA..DIARIA..C.",
         Tmed = "TEMPERATURA.MEDIA.COMPENSADA..DIARIA..C.",
         Tmin = "TEMPERATURA.MINIMA..DIARIA..C.",
         Evap = "EVAPORACAO.DO.PICHE..DIARIA.mm.",
         Inso = "INSOLACAO.TOTAL..DIARIO.h.",
         UmrMe = "UMIDADE.RELATIVA.DO.AR..MEDIA.DIARIA...",
         UmrMi = "UMIDADE.RELATIVA.DO.AR..MINIMA.DIARIA...",
         VVel = "VENTO..VELOCIDADE.MEDIA.DIARIA.m.s.") %>%
  # Divide coluna Data em 3, dia, mes e ano
  separate(col = Data, into = c("Ano", "Mes", "Dia"), sep = "-", remove = F) %>%
  mutate(Ano = as.numeric(Ano),
         Mes = as.numeric(Mes),
         Dia = as.numeric(Dia))


## Importação de tabela de médias normais 1961-1990
T_Med_Normal <-
  read_excel("Normais.xls",
             sheet = 1, skip = 3, na = "-")

## Limpando os dados: separando apenas a linha de interesse e pivotando para formatar
T_Med_Normal <-

  # Seleciona a tabela
  T_Med_Normal %>%
  # Filtra apenas a linha da estação de Campos
  filter(Código == 83698) %>%
  # Seleciona apenas colunas de janeiro a dezembro
  select(Janeiro:Dezembro) %>%
  # Pivotando a tabela para obter duas colunas, uma referente ao Mes e uma à Temperatura Média
  pivot_longer(cols = everything(), # Informa que desejamos todas as colunas
               names_to = "MesN", # Informa o nome da coluna com os meses
               values_to = "Tmed") %>% # Informa o nome da coluna com os valores
  # Adiciona uma coluna extra com o número dos meses, 1 a 12
  mutate("Mes" = 1:12)

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
         Mes = as.numeric(Mes)) %>%
  unite(col = "Data", Ano:Mes, sep = "-", remove = F) %>%
  mutate(Data = lubridate::ym(Data))

## Calculando as anomalias em um novo objeto

T_Med_Anomalias <-
  data.frame(
    Ano =       as.numeric(T_Med_Mensal$Ano),
    Mes =       as.numeric(T_Med_Mensal$Mes),
    Data = T_Med_Mensal$Data,
    T_Med_Mensal_ = T_Med_Mensal$TmedM,
    T_Med_Normal_ = rep(T_Med_Normal$Tmed, diff(range(Dados$Ano))+1 ),
    Dif_T_M =   T_Med_Mensal$TmedM - rep(T_Med_Normal$Tmed, diff(range(Dados$Ano))+1 )
  )


# Visualização #####

## Rápida visualização da Temperatura Média Normal 1961-1990

ggplot(T_Med_Normal) +
  aes(x = Mes, y = Tmed) +
  geom_line(size = 1.3, alpha = 0.7) +
  geom_point(size = 3, alpha = 0.5) +
  scale_x_continuous(breaks = 1:12,
                     labels = toupper(month.abb)) +
  labs(x = "", y = "Normais Climatológicas\nTemperatura Média Mensal") +
  ggpubr::theme_pubr()
ggsave("NormaisClimatologicas.png", width = 2000, height = 1600, units = "px", bg = "white")

## Rápida visualização comparando temperaturas normais x temperaturas médias
ggplot() +
  geom_line(
    data = T_Med_Mensal,
    aes(x = Mes, y = TmedM, group = Ano, color = Ano),
    alpha = 0.5, size = 1) +
  geom_errorbar(
    data = T_Med_Mensal,
    aes(x = Mes, y = TmedM, group = Ano, color = Ano,
        ymin = TmedM - TsdM, ymax = TmedM + TsdM),
    width = 0.1, alpha = 0.5) +
  geom_line(
    data = T_Med_Normal,
    aes(x = Mes, y = Tmed),
    color = "red", size = 2) +
  scale_x_discrete(labels = toupper(month.abb)) +
  scale_color_viridis_c() +
  labs(x = "", y = "Anomalias Climatológicas\nTemperatura Média Mensal") +
  ggpubr::theme_pubr()
ggsave("Anomalias_Normais.png", width = 2000, height = 1600, units = "px", bg = "white")

## Visualizando evolução da diferença através dos anos

ggplot(T_Med_Anomalias,
       aes(x = Ano, y = Dif_T_M, group = Ano)) +
  geom_boxplot() +
  scale_y_continuous(breaks = -5:5, limits = c(-5,5)) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  labs(y = "Anomalias Climatológicas Temperatura") +
  ggpubr::theme_pubr()
ggsave("Anomalias_Temp_Bx.png", width = 2000, height = 1600, units = "px", bg = "white")

ggplot(T_Med_Anomalias,
       aes(x = Ano, y = Dif_T_M)) +
  geom_point() +
  geom_smooth(method = "lm", color = "Red") +
  ggpmisc::stat_poly_eq(formula = y ~ x, color = "Red",
                        aes(label = paste(..eq.label.., ..rr.label.., ..p.value.label.., sep = "*`,`~")),
                        parse = TRUE,
                        label.x.npc = "right") +
  ggpubr::theme_pubr()
ggsave("Anomalias_Temp_Points.png", width = 2000, height = 1600, units = "px", bg = "white")

# Confecção da animação ######

## Preparação dos dados e suportes ####
# Agradecimento à Pat Schloss pelos insights sobre a replicação de um gráfico como esse.
#https://riffomonas.org/code_club/2022-06-09-nasa-animation

# Temos que resolver um problema para o gráfico
# circular: A conexção dezembro-janeiro.
# Para isso, vamos criar um mês "zero" em cada ano,
# que representa o mês de dezembro do ano anterior.
# Depois é só plotar o gráfico e definr a escala x
# para cortar o escesso.

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

frames <- 1:sum(c(nrow(T_Med_Anomalias), nrow(T_Med_0)))

## Unir tabela M0 e coluna frames nos dados

Dados_Pl <-
  T_Med_Anomalias %>%
  rbind(T_Med_0) %>%
  arrange(Ano, Mes) %>%
  cbind(frames)

## Data frame -2 a +2 para legenda
Legenda <-
  data.frame(
    x = 1,
    y = seq(from = -2, to = 2, by = 1),
    labels = c("-2\u00B0C", "-1\u00B0C", "0\u00B0C",
               "+1\u00B0C", "+2\u00B0C"))

# Vetor de meses para legenda
Meses <-
  c("jan", "fev", "mar",
    "abr", "mai", "jun",
    "jul", "ago", "set",
    "out", "nov", "dez")

# Define uma pasta onde serão plotados todos os frames
setwd("./Anim")

# Precisamos remover o "Mes 0" criado anteriormente, na conexão
# dezembro-janeiro. Caso contrário, todo mês dezembro, teríamos 2
# frames repetidos plotados. Esta solução acha no vetor frames
# do objeto Dados_Pl, aqueles multiplos de 13, essencialmente,
# os frames extas, e remove eles.

frames <-
  data.frame(
    frames,
    multiplo13 = if_else(frames %% 13 == 0,
                         print("True"),
                         print("False")))
frames %>%
  filter(multiplo13 != "True") %>%
  filter(frames >= 13) %$%
  print(frames) -> frames

## Plotando os frames ####
# ATENÇÃO ESTA ETAPA DEMANDA TEMPO

for(i in frames){
  alpha = c(rep(0.15, times = i - 11), seq(0.15, 1, by = 1/12))
  ggplot(Dados_Pl %>% filter(frames <= i),
         aes(x = Mes, y = Dif_T_M,
             group = Ano, color = Dif_T_M)) +
    geom_hline(yintercept = -2, color = "DarkBlue", size = 1.3, alpha = 0.5) +
    geom_hline(yintercept = -1, color = "#ad84ff", size = 1.3, alpha = 0.5) +
    geom_hline(yintercept = 0, color = "White", size = 1.3, alpha = 0.5) +
    geom_hline(yintercept = 1, color = "#ffa286", size = 1.3, alpha = 0.5) +
    geom_hline(yintercept = +2, color = "DarkRed", size = 1.3, alpha = 0.5) +
    geom_label(data = Legenda,
               aes(x = x, y = y, label = labels),
               inherit.aes = F,
               color = c("DarkBlue", "#ad84ff", "White", "#ffa286", "DarkRed"),
               fill = "Black", label.size = 0, size = 3, alpha = 0.5) +
    geom_label(aes(x = 0, y = -10, label = Ano),
               size = 10, fill = "black", label.size = 0) +
    geom_line(size = 1.2, alpha = alpha) +
    geom_point(alpha = alpha) +
    scale_color_gradient2(low = "darkblue", high = "darkred",
                          midpoint = 0, guide = "none") +
    scale_x_continuous(breaks = 1:12,
                       labels = toupper(Meses)) +
    scale_y_continuous(limits = c(-10, 4)) +
    coord_polar(start = -2*pi/12) +
    theme(
      panel.background = element_rect(fill = "Gray10"),
      plot.background = element_rect(fill = "Gray10"),
      panel.grid = element_line(color = "#002240"),
      axis.text.x = element_text(color = "yellow", size = 15),
      axis.text.y = element_blank(),
      axis.title.y = element_blank(),
      axis.title.x = element_blank(),
      axis.ticks = element_blank()
    )
  ggsave(filename = paste("Spiral_Frame_", i+100, # Somo 100 ao i para evitar problemas no ordenamento das strings com nome dos frames
                          ".png", sep = ""),
         width = 1600, height = 1600,
         units = "px", bg = "Gray10")
}

## Animando ####
# Agradecimento à Jason Mercer pelo script abaixo.
# https://wetlandscapes.github.io/blog/blog/making-movie-in-r-from-still-images/

# Devemos ter instalado no computador o ffmpeg e o ImageMagick para o devido
# funcionamento desta etapa. Não tenho certeza sobre a necessidade do ImageMagick,
# mas como ele já estava instalado em minha máquina, recomendo sua instalação.

# Indicando o caminho do FFmpeg para o pacote animation
animation::ani.options(ffmpeg = shortPathName("C:/Program Files (x86)/FFMPEG/bin/ffmpeg.exe"))

# Criando um vetor com o nome dos arquivos. Todos seguem o padrão "Spiral_Frame_".
Frames <- list.files(pattern = "Spiral_Frame_", all.files = TRUE, recursive = F)

# Extraíndo parâmetros dos frames que seram utilizados no processo.
img.height <- magick::image_info(image_read(Frames[1]))$height
img.width <- magick::image_info(image_read(Frames[1]))$width
img.type <- magick::image_info(image_read(Frames[1]))$format

# Definindo alguns parametros para o funcionamento adequado do FFmpeg.

animation::ani.options(interval = 0.1, # Intervalo entre os frames em segundos
                       ani.height = img.height, # Dimensões das imagens
                       ani.width = img.width, # Dimensões das imagens
                       ani.dev = tolower(img.type), # Definindo a 'engine' à ser utilizada
                       ani.type = tolower(img.type)) # Definindo a 'engine' à ser utilizada

# Definindo vetor de configuração para ampliar a resolução das imagens.
# Utilizaremos na função saveVideo()
opts <- paste("-s ", img.height*2.5, "x", img.width*2.5, sep = "")

animation::saveVideo(
  # Loop pelos Frames, adicionando um a cada vez.
  for(i in 1:length(Frames)){
    # Lê o Frame.
    Frame <- magick::image_read(Frames[i])
    # Plota o Frame.
    plot(Frame)
  },
  # Definindo o nome do arquivo
  video.name = "Animacao.avi",
  # Define os parametros extas salvos anteriormente
  other.opts = opts
)


# EXTRA: Grafico interativo em html ####

library(plotly)

T_Med_Anomalias %>%
  mutate(raio = Dif_T_M + 10,
         theta = 2 * pi * (Mes - 1) / 12,
         x = raio * sin(theta),
         y = raio * cos(theta)) %>%
  plot_ly(data = .,
          x = ~x, y = ~y, z = ~Data,
          type = "scatter3d",
          mode = "lines", sizes = 3,
          line = list(width = 6, color = ~Dif_T_M,
                      colorscale = list(c(min(T_Med_Anomalias$Dif_T_M)+10, "blue"),
                                        c(max(T_Med_Anomalias$Dif_T_M)+10, "red")))
  )




