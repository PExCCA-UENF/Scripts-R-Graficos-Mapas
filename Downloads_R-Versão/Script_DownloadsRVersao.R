#==============================================================================#
#          EXTENSÃO UNIVERSITÁRIA EM CIÊNCIAS CLIMÁTICAS E AMBIENTAIS          #
#          PROJETO "PROCESSAMENTO E ANÁLISE DE DADOS AMBIENTAIS COM R"         #
#                        Contato: pexcca.lamet@uenf.br                         #
#                       https://linktr.ee/pexcca.lamet                         #
#==============================================================================#

#---------------NÚMERO DE DOWNLOADS DA LINGUAGEM DE PROGRAMAÇÃO R--------------#
#                    Elaboração: Nícolas Chenquel Nogueira                     #
#                      Revisão: Profa. Eliane B. Santos                         #
#                      Script Atualizado em 29/04/2024                         #
#------------------------------------------------------------------------------#

# Introdução ----
# O objetivo deste script é produzir um gráfico que ilustre o número de downloads
# do programa R ao longo dos anos. A "The Compreensive R Archive Network", conhecida
# como "CRAN", mantém o registro dos downloads de R desde outubro de 2012. Nessa 
# época, a versão vigente era a 2.x.x . Vamos acessar o registro de downloads e 
# # utilizá-los para produzir um gráfico com o número de downloads. Este script é
# original, mas o gráfico foi inspirado na ilustração de Cédric Scherer.

# ATENÇÃO! Dependendo da capacidade do computador, o download e o processamento 
# dos dados pode demorar um tempo para ser concluído. Em uma máquina, por exemplo,
# Intel Core i7-7700HQ - RAM 16GB, o processo é concluído em torno de 1 hora.


# Bibliotecas e preparações gerais ----

## Para instalar e carregar as bibliotecas necessárias, use os comandos abaixo:
packages <- 
  c("tidyverse", "future.apply", "parallel", "ggtext", 
    "showtext", "scales", "magick", "devtools")

for (p in packages) {
  if (!require(p, character.only = T)) {
    install.packages(p, character = T)
  }
  library(p, quietly = T, character.only = T)
}

## O pacote ggsankey está disponível apenas no github:
if(!require("ggsankey")) {
  devtools::install_github("davidsjoberg/ggsankey")
}
library(ggsankey)

## Carregando o Google Font:
font_add_google(name = "Roboto Mono", family = "Roboto Mono")
font_add_google(name = "Comfortaa", family = "Comfortaa")
font_add_google(name = "Quicksand", family = "Quicksand")

showtext_auto()

## Criando diretórios/pastas para organizar os arquivos:
if(!dir.exists("data/")){dir.create("data/")}
if(!dir.exists("data_csv/")){dir.create("data_csv/")}
if(!dir.exists("results/")){dir.create("results/")}

# Download dos dados ----

## Sequência de datas para download dos dados:
inicio <- as.Date('2012-10-01')
hoje <- Sys.Date()-1

todos_os_dias <- seq.Date(inicio, hoje, by = 'day')

## Agora, vamos criar um vetor com as URLs dos registros. 
## O padrão para o download é o seguinte:
## http://cran-logs.rstudio.com/_ANO_/_DATACOMPLETA_-r.csv.gz

urls <- paste0('http://cran-logs.rstudio.com/', 
               year(todos_os_dias), '/', todos_os_dias, '-r.csv.gz')

## Iniciando o download e processamento dos dados em paralelo para acelerar o script.

## Preparação - definindo o número de workers com base no número de núcleos do processador da máquina:
plan(multisession, workers = parallel::detectCores())

## Função para baixar os arquivos:
baixar_arquivo <- function(url, destino, i) {
  download.file(url = url, destfile = destino)
  if(file.info(destino)$size == 65){
    file.remove(destino)
    print("Arquivo removido. Dia sem registros.")
  }
  cat("Arquivo baixado com sucesso!", url)
}

## Usando a função future_lapply() para baixar os arquivos em paralelo:
future_lapply(
  X = seq_along(urls),
  FUN = function(i) {
    tryCatch({
      destino <- paste0("data/", todos_os_dias[i], ".gz")
      baixar_arquivo(urls[i], destino, i)
    },
    error = function(e) print(e))
  })

# Processamento dos Dados ----

## Nesta etapa, vamos abrir os arquivos .gz individualmente, ler o conteúdo e,
## em seguida, escrever um csv na pasta data_csv.
plan(multisession, workers = parallel::detectCores())

descompactar_arquivos <- function(arquivo){
  con <- gzfile(description = paste0("data/", arquivo), open = "rb")
  dados <- readr::read_csv(con)
  readr::write_csv(dados, file = paste0("data_csv/", arquivo, ".csv"))
  close(con)
}

ls_arquivos <- list.files(path = "data/")

## Vamos usar novamente a função future_lapply() para acelerar o script:
future_lapply(
  X = ls_arquivos,
  FUN = function(x){
    tryCatch({
      descompactar_arquivos(arquivo = x)
    },
    error = function(e) print(e))
  }
)

## Agora, vamos unir todos os arquivos para prosseguir com a ilustração:

## Função para leitura de arquivo .csv:
ler_csv <- function(arquivo) {
  read_csv(arquivo)
}

## Obtendo a lista de arquivos .csv:
arquivos_csv <- list.files("data_csv/", full.names = TRUE)

## Dividindo a lista de arquivos em lotes menores:
batch_size <- 100  # Defina o tamanho do lote adequado.
batches <- split(arquivos_csv, ceiling(seq_along(arquivos_csv)/batch_size))

## Inicializando uma lista para armazenar os resultados:
resultados <- list()

## Processamento paralelo dos lotes de arquivos .csv:
for (batch in batches) {
  result_batch <- future_lapply(batch, read_csv)
  resultados <- c(resultados, result_batch)
}

## Combinando os resultados em um único data frame:
final_df <- do.call(rbind, resultados)

## Exportando o resultado em uma planilha .csv:
write_csv(final_df, file = "df_final.csv")

# Análise dos dados ----

glimpse(final_df)

## Nesta etapa, vamos estudar os dados que coletamos. Podemos perceber que houve
## um total de 8 grandes versões do R. Para gerar o gráfico, vamos considerar apenas
## as versões principais como 2.x.x, 3.x.x, 4.x.x. Assim, vamos criar uma nova
## coluna chamada V.x que receba apenas o primeiro caractere da coluna "version".
## Em seguida, realizamos uma contagem de quantos registros encontramos em cada
## versão, ou seja, quantos downloads foram realizados em cada mês.

df_f <-
  final_df %>%
  mutate(V.x = str_sub(version, start = 1, end = 1)) %>%
  group_by(V.x, year(date), month(date)) %>%
  summarise(
    n = n()
  ) %>%
  unite(col = "Data",  c(`year(date)`, `month(date)`), sep = "-") %>%
  mutate(Data = ym(Data)) %>%
  filter(V.x %in% c("2", "3", "4")) %>%
  mutate(V.x = factor(V.x, levels = c("2", "3", "4")))
df_f

## Preparação para os rótulos do gráfico:
n_downloads <-
  df_f %>%
  group_by(V.x) %>%
  summarise(n = sum(n))

seq_ldates <-
  seq.Date(
    from = as.Date("2013-01-01"),
    to = Sys.Date(),
    by = "6 month"
  )
ldates <-
  tibble(
    data = seq_ldates,
    yf = c(rep(-200000, 15), rep(-500000, length(seq_ldates)-15)),
  ) 

# Plotando o gráfico ----
pl <-
  df_f %>%
  ## Mapeamento global inicial:
  ggplot(mapping = aes(
    x = Data,
    value = n,
    ## Argumentos para o ggsankey:
    node = V.x,
    fill = V.x,
    group = V.x
  )) +
  
  ## Desenho manual do grid personalizado:
  ## Foi feito de forma manual devido ao recuo dos primeiros anos no grid.
  geom_segment(
    data = ldates,
    mapping =
      aes(
        x = data,
        y = 0,
        xend = data,
        yend = yf,
      ),
    color = rep(c("gray45", "gray60"), length.out = length(seq_ldates)),
    linetype = "dotted",
    inherit.aes = F
  ) +
  geom_text(
    data = ldates,
    mapping = aes(
      x = data,
      y = yf,
      label = paste0(month(data), "/", year(data)),
    ),
    color = rep(c("gray45", "gray60"), length.out = length(seq_ldates)),
    size = 20,
    angle = 90,
    nudge_y = -40000,
    inherit.aes = F
  ) +
  
  ## Geometria principal do gráfico: 
  geom_sankey_bump(space = 5000,
                   smooth = 6) +
  
  ## Anotações do número acumulado de downloads: 
  geom_curve(
    mapping = aes(
      x = as.Date("2022-09-01"),
      y = 200000,
      xend = as.Date("2022-01-01"),
      yend = 300000
    ),
    color = "#382a54",
    linetype = "dotted",
    linewidth = 1.3
  ) +
  geom_richtext(
    mapping = aes(
      x = as.Date("2021-03-01"),
      y = 310000,
      label =
        paste0(
          "<p>",
          comma(
            x =
              n_downloads %>%
              filter(V.x == 4) %>%
              pull(n),
            big.mark = ".",
            decimal.mark = ","
          ) %>% as.character(),
          "<br>downloads<br>acumulados</p>"
        ),
      family = "Roboto Mono"
    ),
    fill = NA,
    size = 30,
    color = "#382a54",
    label.padding = grid::unit(rep(0, 4), "pt"),
    lineheight = 0.1,
    label.color = NA
  ) +
  
  geom_curve(
    mapping = aes(
      x = as.Date("2017-01-01"),
      y = 0,
      xend = as.Date("2017-04-01"),
      yend = 90000
    ),
    color = "#357ba2",
    linetype = "dotted",
    linewidth = 1.3,
    curvature = -0.4
  ) +
  geom_richtext(
    mapping = aes(
      x = as.Date("2017-06-01"),
      y = 120000,
      label =
        paste0(
          "<p>",
          comma(
            x =
              n_downloads %>%
              filter(V.x == 3) %>%
              pull(n),
            big.mark = ".",
            decimal.mark = ","
          ) %>% as.character(),
          "<br>downloads<br>acumulados</p>"
        ),
      family = "Roboto Mono"
    ),
    fill = NA,
    size = 20,
    color = "#357ba2",
    label.padding = grid::unit(rep(0, 4), "pt"),
    lineheight = 0.1,
    label.color = NA
  ) +
  
  geom_curve(
    mapping = aes(
      x = as.Date("2013-01-01"),
      y = 0,
      xend = as.Date("2013-03-01"),
      yend = -57000
    ),
    curvature = 0.4,
    color = "#60ceac",
    linetype = "dotted",
    linewidth = 1.3
  ) +
  geom_richtext(
    mapping = aes(
      x = as.Date("2013-07-01"),
      y = -66000,
      label =
        paste0(
          "<p>",
          comma(
            x =
              n_downloads %>%
              filter(V.x == 2) %>%
              pull(n),
            big.mark = ".",
            decimal.mark = ","
          ) %>% as.character(),
          "<br>downloads<br>acumulados</p>"
        ),
      family = "Roboto Mono"
    ),
    fill = NA,
    size = 20,
    color = "#60ceac",
    label.padding = grid::unit(rep(0, 4), "pt"),
    lineheight = 0.1,
    label.color = NA
  ) +
  
  ## Anotação com explicação da geometria: 
  geom_segment(
    mapping = aes(
      x = as.Date("2015-04-01"),
      y = -61000 / 2,
      xend = as.Date("2015-04-01"),
      yend = 73000 / 2
    ),
    arrow = arrow(
      type = "closed",
      length = unit(0.4, "cm"),
      ends = "both"
    ),
    color = "#b2b2b2",
    inherit.aes = F
  ) +
  geom_richtext(
    mapping = aes(
      x = as.Date("2015-04-01"),
      y = -72500,
      label = "<p>Largura representa<br>o número mensal<br> de downloads</p>",
      family = "Comfortaa",
      fontface = "bold"
    ),
    fill = "#e5e5e5",
    size = 20,
    color = "#b2b2b2",
    label.padding = grid::unit(rep(0, 4), "pt"),
    lineheight = 0.1,
    label.color = NA,
  ) +
  
  ## Anotação da versão do R: 
  geom_text(
    mapping = aes(
      x = as.Date("2013-11-01"),
      y = 80000,
      label = "R versão 2.x.x",
      family = "Comfortaa"
    ),
    size = 30,
    color = "#60ceac"
  ) +
  geom_text(
    mapping = aes(
      x = as.Date("2016-09-01"),
      y = 5000,
      label = "R versão 3.x.x",
      family = "Comfortaa"
    ),
    size = 30,
    color = "white"
  ) +
  geom_text(
    mapping = aes(
      x = as.Date("2022-02-01"),
      y = 5000,
      label = "R versão 4.x.x",
      family = "Comfortaa"
    ),
    size = 30,
    color = "white"
  ) +
  geom_segment(
    mapping = aes(
      x = as.Date("2013-01-01"),
      xend = as.Date("2013-01-01") ,
      y = 0,
      yend = 65000
    ),
    color = "#60ceac" ,
    linetype = "dotted"
  ) +
  
  ## Anotações textuais no gráfico: 
  geom_textbox(
    mapping = aes(
      x = as.Date("2013-01-01"),
      y = -400000,
      family = "Quicksand",
      hjust = 0.03,
      halign = 0,
      fill = NA,
      label = "Os registros de downloads do programa R do CRAN (Comprehensive R Archive Network), foram coletados por métodos automatizados e utilizados para calcular o total mensal de downloads. Apenas as versões principais do R foram consideradas. Ressaltamos que o número de downloads não representa um quantitativo de usuários ativos diretamente, mas pode ser um bom indicativo do crescimento da linguagem."
    ),
    size = 25,
    inherit.aes = F,
    lineheight = 0.2,
    width = unit(22, "cm"),
    text.color = "gray50",
    box.padding = grid::unit(rep(0, 4), "pt"),
    box.color = NA
  ) +
  
  ## Paleta de cores: 
  scale_fill_viridis_d(
    option = "G",
    direction = -1,
    begin = .2,
    end = .8
  ) +
  
  ## Rótulos: 
  labs(title = 'Linguagem de Programação R: Downloads Acumulados',
       caption = "Elaboração: Nícolas Chenquel Nogueira (2024) • Fonte dos dados: http://cran-logs.rstudio.com/") +
  
  ## Tema: 
  theme(
    axis.title = element_blank(),
    axis.text = element_blank(),
    axis.ticks = element_blank(),
    axis.line = element_blank(),
    
    legend.position = "none",
    
    panel.grid = element_blank(),
    panel.background = element_rect(fill = "gray90"),
    
    plot.background = element_rect(fill = "gray90"),
    plot.title =
      element_textbox_simple(
        family = "Quicksand",
        face = 'bold',
        color = "#382a54",
        size = 160,
        margin = margin(b = -200, t = 70, l = -25),
        width = 0.85,
        lineheight = 0.3
      ),
    plot.caption =
      element_text(
        family = "Quicksand",
        color = "gray50",
        size = 60,
        hjust = 1
      )
  )

# Salvando o gráfico:
ggsave(
  plot = pl,
  filename = "results/DownloadsRVersao.png",
  width = 5000,
  height = 5000,
  units = "px"
)

## Adicionando logos do projeto: 

gr      <- image_read('results/DownloadsRVersao.png')
qrcode  <- image_read('images/qrcode.png')  |> image_scale("450")
pexcca  <- image_read('images/pexcca.png')  |> image_scale("450")
proambr <- image_read('images/proambr.png') |> image_scale("450")

logos <- image_append(image = c(qrcode, pexcca, proambr), stack = F)

image_composite(gr, logos, gravity = "SouthWest") |>
  image_write(path = "results/GráficoFinal.png",
              format = "png",
              quality = 200)

