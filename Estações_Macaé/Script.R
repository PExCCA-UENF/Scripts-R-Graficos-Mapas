# Carregando pacotes necessários ####

if(!require(pacman)){install.packages("pacman")}
pacman::p_load(tidyverse, janitor, lubridate, rvest, ggthemes, gganimate, showtext)

setwd("Tabelas/")

# Setup ####

# Definindo estações que serão buscadas
# Nome e código respectivamente.
Estação = c("Aroeira", "Serra", "Glória", "Imbetiba", "Mirante")
Código = c("IMACA28", "IMACA13", "IMACA15", "IMACA27", "IMACA7")

# Definindo Data desejada
Data = c("2022-11-30")

# Baixando dados ####

# String com urls
www <- paste("https://www.wunderground.com/dashboard/pws/", Código, "/table/", Data,"/", Data, "/daily", sep = "")

# Loop que acessa os sites, baixa os dados, limpa e seleciona colunas desejadas.
for(i in 1:length(www)){

  # Lendo o site
  pg <- read_html(www[i])
  # Localização da tabela no código fonte do site
  xpath <- '//*[@id="main-page-content"]/div/div/div/lib-history/div[2]/lib-history-table/div/div/div/table'

  df <-
    pg %>%
    rvest::html_nodes(xpath = xpath) %>% # Extrai a tabela da localização
    rvest::html_table() %>%
    dplyr::bind_rows() %>%
    janitor::clean_names() %>% # Limpa o nome das colunas
    transmute(
      Estação = Estação[i],
      Tempo = parse_time(time),
      Temperatura = (parse_number(temperature)-32)*5/9, # Conversão de unidade
      Precip.Ac = parse_number(precip_accum)*25.4 # Conversão de unidade
    )

  write_csv(x = df,
            file = paste(Estação[i], ".csv", sep = ""))

}
#####

dados <-
  list.files(path = getwd()) %>%
  lapply(read_csv) %>%
  bind_rows

########

font_add(family = "Consola", regular = "Consola.ttf")
showtext_auto()

dados %>%
  mutate(Hora = hour(Tempo),
         Minutos = minute(Tempo)) %>%
  filter(Minutos == 59) %>%
  mutate(Hora = Hora + 1,
         Min = "00") %>%

  ggplot() +
  aes(x = Hora, y = Precip.Ac, color = Estação) +
  geom_line(linewidth = 2) +
  labs(title = paste("Chuva acumulada em 24 horas (", Data, ") Macaé/RJ", sep = ""),
       x = "Hora",
       y = "Chuva (mm)",
       color = "Estações") +
  scale_x_continuous(breaks = 1:24, limits = c(1,24)) +
  scale_color_manual(values = c("#124653", "#00a08f",
                                "#fee074", "#ff8850",
                                "#ffbfbf")) +
  theme_tufte(base_size = 27) +
  theme(
    text = element_text(family = "Consola"),
    axis.ticks = element_line(linewidth = 0.6),
    axis.title.y = element_text(vjust = +3),
    panel.grid.major = element_line(color = "gray90", linewidth = 0.5),
    legend.box.margin = margin(r = 50),
    legend.text = element_text(size = 23),
    legend.margin = margin(t = 10)
  ) +
  transition_reveal(along = Hora) -> p

animate(plot = p,
        nframes = 500,
        end_pause = 50,
        width = 1600,
        height = 1000,
        units = "px"
) -> anim
av::av_encode_video(input = anim, output = paste(Data,"_Anim.mp4", sep = "")

# # ggsave(filename = "precipitação.png",
# #        width = 2600, height = 2600,
# #        units = "px", bg = "#e9f3f2")
