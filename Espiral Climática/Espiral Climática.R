#=============================================================================================================================
# PROJETO DE EXTENSÃO "PROCESSAMENTO E ANÁLISE DE DADOS AMBIENTAIS COM R"
# E-mail: pexcca.lamet@uenf.br
#=============================================================================================================================

##### ESPIRAL CLIMÁTICA #####
# Elaboração: Nícolas C. Nogueira
# Revisão: Profa. Eliane B. Santos
# Atualização: 28/10/2022

## Para instalar as bibliotecas necessárias, use os comandos abaixo:
for (p in c("magrittr", "tidyverse", "readxl", "ggthemes", "magick", "animation")) {
  if (!require(p, character.only = T)) {
    install.packages(p, character = T)
  }
  library(p, quietly = T, character.only = T)
}

### Importação e organização dos dados ####

# Vamos utilizar os dados mensais de Campos-RJ (Código 83698) obtidos no Banco de Dados Meteorológicos para Ensino e Pesquisa (BDMEP) do INMET. 
# O BDMEP/INMET disponibiliza os dados no link: https://bdmep.inmet.gov.br/

file1 = "dados_83698_M_1961-01-01_2021-12-31.csv"

dados <- file1 %>% # Arquivo que será importado.
  read.csv(sep = ";", dec = ",", header = T, skip = 10, na.strings = "null") %>%   # Configuração da importação de dados.
  
  select(Data.Medicao, # Selecionando as colunas com as datas e a temperatura média compensanda. 
         TEMPERATURA.MEDIA.COMPENSADA..MENSAL..C.)  %>%   
  rename(Data = "Data.Medicao",   # Renomeando as colunas.
         Tmc = "TEMPERATURA.MEDIA.COMPENSADA..MENSAL..C.") %>%
  separate(col = Data, into = c("Ano", "Mes", "Dia"), sep = "-", remove = F) %>%   # Dividindo a coluna Data em 3: Ano, Mes e Dia.
  mutate(Ano = as.numeric(Ano),
         Mes = as.numeric(Mes),
         Dia = as.numeric(Dia))

## Importação e organização das normais climatológicas (1961-1990) da temperatura média compensada (Tmc). 
file2 = "Temperatura-Media-Compensada_NCB_1961-1990.xls"

Tmc.NC <- file2 %>% 
  read_excel(sheet = 1, skip = 3, na = "-") %>% 
  filter(Código == 83698) %>%    # Selecionando os dados de Campos-RJ (Código 83698).
  select(Janeiro:Dezembro) %>%   # Selecionando as colunas de janeiro a dezembro.
  
  # Pivotando os dados para obter duas colunas: Mês e Temperatura.
  pivot_longer(cols = everything(), # Selecionando todas as colunas.
               names_to = "MesN",   # Nome da coluna com os meses.
               values_to = "Tmc") %>%   # Nome da coluna com as temperaturas. 
  mutate("Mes" = 1:12, .before = Tmc)   # Adicionando uma coluna extra com o número dos meses, 1 a 12.

### Criando um data frame com as anomalias de temperatura.
dados.anomalias <- data.frame(Ano = as.numeric(dados$Ano),
                    Mes = as.numeric(dados$Mes),
                    Tmc.mensal = dados$Tmc,
                    Tmc.normal = Tmc.NC$Tmc,
                    Tmc.anomalias = dados$Tmc - Tmc.NC$Tmc)
                              
### Visualização - Espiral climática ### 

# Primeiro vamos criar uma conexão dezembro-janeiro. 
# Para isso, vamos criar um mês "zero" em cada ano, que representa o mês de dezembro do ano anterior.

Tmc.mes0 <- dados.anomalias %>%
  filter(Mes == 12) %>%
  mutate(Ano = Ano + 1, Mes = 0)

# Em seguida, crie um vetor com um sequência de números para funcionar como contagem de frames.

v.seq <- 1:sum(c(nrow(dados.anomalias), nrow(Tmc.mes0)))

# Agora vamos Unir os dados.

dados.mes0 <- dados.anomalias %>%
  rbind(Tmc.mes0) %>%
  arrange(Ano, Mes) %>%
  cbind(v.seq)

# Criando um data frame com os valores (-2 a +2) para legenda.
Legenda <- data.frame(
  x = 1,
  y = seq(from = -2, to = 2, by = 1),
  labels = c("-2\u00B0C", "-1\u00B0C", "0\u00B0C", "+1\u00B0C", "+2\u00B0C"))

# Criando um vetor com os meses para legenda.
Meses <- c("jan", "fev", "mar", "abr", "mai", "jun",
           "jul", "ago", "set", "out", "nov", "dez")

# Defina uma pasta onde serão plotados todos os frames.
setwd("./Anim")

# Agora vamos remover o Mes 0 criado anteriormente.
frames <- data.frame(v.seq,
                     multiplo13 = if_else(v.seq %% 13 == 0,
                                          print("True"),
                                          print("False")))
frames %>%
  filter(multiplo13 != "True") %>%
  filter(v.seq >= 13) %$%
  print(frames) -> frames.f

## Plotando os frames ##

# ATENÇÃO! Esta etapa demanda tempo.

for(i in frames.f){
  alpha = c(rep(0.15, times = i - 11), seq(0.15, 1, by = 1/12))
  ggplot(dados.mes0 %>% filter(frames.f <= i),
         aes(x = Mes, y = Tmc.anomalias, group = Ano, color = Tmc.anomalias)) +
    geom_hline(yintercept = -2, color = "DarkBlue", size = 1.3, alpha = 0.5) +
    geom_hline(yintercept = -1, color = "#ad84ff", size = 1.3, alpha = 0.5) +
    geom_hline(yintercept = 0, color = "White", size = 1.3, alpha = 0.5) +
    geom_hline(yintercept = 1, color = "#ffa286", size = 1.3, alpha = 0.5) +
    geom_hline(yintercept = +2, color = "DarkRed", size = 1.3, alpha = 0.5) +
    geom_label(data = Legenda, aes(x = x, y = y, label = labels),
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

## Animando o gráfico ##

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


