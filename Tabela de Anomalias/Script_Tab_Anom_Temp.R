#==============================================================================#
#          EXTENSÃO UNIVERSITÁRIA EM CIÊNCIAS CLIMÁTICAS E AMBIENTAIS          #
#          PROJETO 'PROCESSAMENTO E ANÁLISE DE DADOS AMBIENTAIS COM R'         #
#                        Contato: pexcca.lamet@uenf.br                         #
#                       https://linktr.ee/pexcca.lamet                         #
#==============================================================================#

#-------------------TABELA DE ANOMALIAS DE TEMPERATURA DO AR-------------------#
#                    Elaboração: Nícolas Chenquel Nogueira                     #
#                      Revisão: Profa. Eliane B. Santos                        #
#                      Script Atualizado em 19/10/2024                         #
#------------------------------------------------------------------------------#

# A proposta do presente script é mostrar como criar uma visualização em formato 
# de tabela com dados de temperatura do ar. Os dados utilizados neste script 
# foram obtidos do Instituto Nacional de Meteorologia (INMET).

# Bibliotecas e outras configurações-------------------------------------------#
## Para instalar e carregar as bibliotecas necessárias, use os comandos abaixo:
for (p in c('tidyverse',
            'showtext',
            'ggtext',
            'patchwork',
            'gt',
            'gtExtras')) {
  if (!require(p, character.only = T)) {
    install.packages(p, character = T)
  }
  library(p, quietly = T, character.only = T)
}

## Os comandos acima irão verificar se os pacotes (tidyverse, showtext, ggtext, 
## patchwork, gt e gtExtras) estão instalados no sistema e, quando não estiver,
## será realiza a instalação automaticamente. Em seguida, será carregado todos os 
## pacotes, garantindo que suas funções estejam disponíveis para uso posterior no código.

## Para atualizar versões dos pacotes, podemos usar a função update.packages(): 
## update.packages(ask = FALSE)   # Atualizar todos os pacotes instalados sem a pergunta de confirmação.
 
update.packages(oldPkgs = 'rlang')   # Atualizar um pacote específico ('rlang').
packageVersion('rlang')              # Verificando a versão do pacote 'rlang'.

## Agora, utilizando a função font_add_google() do pacote 'showtext' vamos adicionar
# a fonte "Comfortaa" diretamente do Google Fonts, garantindo uma tipografia
# consistente e estilizada nas figuras.

sysfonts::font_add_google('Comfortaa', 'Comfortaa')

## Para ativar a renderização de fontes customizadas vamos usar o showtext_auto(), 
## o que assegura que a fonte escolhida seja aplicada corretamente em todas as visualizações gráficas.

showtext::showtext_auto()

# Importação e organização dos dados ------------------------------------------#

## Vamos utilizar neste script dois arquivos CSV obtidos do INMET: 
#  1º: Série temporal da temperatura média mensal do município de Campos dos Goytacazes-RJ.
#      Obtidos no link: https://bdmep.inmet.gov.br/

## Esses dados foram obtidos do INMET (https://bdmep.inmet.gov.br/) e estão 
## disponíveis no GitHub do PExCCA-UENF:
url_dados <- ''

Tmed_Campos <-
  readr::read_delim(
    file = "dados_83698_M_1961-01-01_2024-10-19.csv",
    delim = ";",
    escape_double = FALSE,
    locale = locale(decimal_mark = ",", grouping_mark = "."),
    col_types = 
      cols(`Data Medicao` = col_date(format = "%Y-%m-%d")),
    na = "null",
    trim_ws = TRUE,
    skip = 10
  )
Tmed_Campos

 
locale = locale(decimal_mark = ",", grouping_mark = ".")


## Com os comandos acima importamos o arquivo "dados_83698_M_2000-01-01_2024-09-30.csv"
## usando a função read_delim(). Essa função permite a leitura de arquivos delimitados
## por ponto e vírgula, tratando corretamente as colunas e valores ausentes (NA)
## designados como 'null'. Além disso, definimos a formatação adequada para a coluna
## de datas (Data Medicao) e removemos eventuais espaços em branco antes e depois dos
## valores, garantindo uma importação limpa e estruturada dos dados. O parâmetro
## skip = 10 ignora as primeiras 10 linhas do arquivo, que contêm informações de
## cabeçalho. Esse processo resulta em um dataframe com as medições históricas da
## temperatura média mensal do município de Campos dos Goytacazes-RJ.

#  2º: Normais climatológicas da temperatura média compensada mensal e anual (°C).
## Vamos fazer o download dos dados do INMET. Link para baixar os dados:
url.NClim <- 'https://portal.inmet.gov.br/uploads/normais/Normal-Climatologica-TMEDSECA.xlsx'

## Use a função download.file() para baixar o arquivo conforme descrito por url da Internet.
download.file(url.NClim,              # URL do arquivo a ser baixado.
              'NClim_Campos.xlsx',    # Nome e extensão do arquivo que será gerado.
              mode = 'wb')            # Modo de gravação do arquivo: 'wb' para Windows.

## Obs.: Quando o caminho não é especificado para salvar o arquivo, o arquivo é salvo no diretório.
dir()   # Verifique os arquivos do diretório de trabalho.

## Leitura e seleção das normais climatológicas da estação meteorológica de Campos 
## dos Goytacazes-RJ (Código 83698):
NClim_Campos <-
  readxl::read_excel(path = 'NClim_Campos.xlsx', skip = 2, na = '-') |>
  filter(Código == 83698) |>
  select(Janeiro:Dezembro) |>
  pivot_longer(cols = Janeiro:Dezembro)
NClim_Campos

# Visualização com pacote 'gt' ------------------------------------------------#

## Vamos criar uma visualização tabular das anomalias de temperatura média mensal 
## do município de Campos dos Goytacazes-RJ, utilizando o pacote 'gt'.

## Inicialmente, vamos preparar a base de dados, calculando as anomalias a partir 
## da diferença entre as temperaturas médias mensais observadas e as normais climatológicas 
## previamente importadas. 

## Para facilitar a leitura dos resultados, os dados serão agrupados por mês e ano.
## Primeiro, vamos unir as informações das temperaturas médias mensais observadas 
## com as normais climatológicas. Em seguida, transformamos a tabela para um formato 
## mais adequado, em que cada coluna representa um mês específico.

df_gt <-
  Tmed_Campos |>
  mutate(
    normais = rep(NClim_Campos$value, length.out = n()),
    anomalias = `TEMPERATURA MEDIA COMPENSADA, MENSAL(°C)` - normais,
    ...3 = NULL,
    Ano = year(`Data Medicao`),
    Mês = month(`Data Medicao`)
  ) |>
  rename(Temp = `TEMPERATURA MEDIA COMPENSADA, MENSAL(°C)`) |>
  mutate(Temp = round(Temp, 2), anomalias = round(anomalias, 3)) |>
  select(Ano, Mês, Temp, anomalias) |>
  pivot_wider(names_from = Mês,
              values_from = c(Temp, anomalias)) |>
  rename(
    Jan = anomalias_1,
    Fev = anomalias_2,
    Mar = anomalias_3,
    Abr = anomalias_4,
    Mai = anomalias_5,
    Jun = anomalias_6,
    Jul = anomalias_7,
    Ago = anomalias_8,
    Set = anomalias_9,
    Out = anomalias_10,
    Nov = anomalias_11,
    Dez = anomalias_12
  )

## Para criar a tabela de anomalias, vamos utilizar funções do pacote 'gt', que 
## nos permite gerar uma estrutura organizada e estilizada. 

tabela <-
  df_gt |>
  filter(Ano >= 2011) |>
  gt(rowname_col = "Ano") |>
  tab_header(
    title = md(
      "Anomalias de Temperaturas Médias Mensais de Campos Dos Goytacazes - RJ"),
    subtitle = 'Climatologia de Referência: 1991-2010') |>
  tab_style(
    style = list(
      cell_text(size = px(17), weight = "bold")
    ),
    locations = cells_title("title")  
  ) |> 
  tab_style(
    style = list(
      cell_text(size = px(15))
    ),
    locations = cells_title("subtitle")  
  )|> 
  tab_source_note("Fonte dos dados: INMET• Elaborado por Nícolas Chenquel Nogueira (Projeto: @Proamb.R)") |>
  tab_source_note(html(
                    "<div style='text-align: center;'>
        <div style='margin-bottom: 3px;'>Legenda:</div>
        <div style='display: flex; align-items: center; justify-content: center;'>
          <span style='margin-right: 10px;'>-3°C</span>
          <div style='position: relative; width: 300px; height: 20px; background: linear-gradient(to right, #67001f, #d6604d, #f7f7f7, #4393c3, #053061); border: 1px solid #808080;'>
            <div style='position: absolute; left: 50%; top: 0; bottom: 0; width: 2px; background-color: #808080;'></div>
          </div>
          <span style='margin-left: 10px;'>3°C</span>
        </div>
        <div style='text-align: center; margin-top: 3px;'>0°C</div>
      </div>"
                  )) |> 
  tab_stubhead(label = md("Ano")) |>
  data_color(
    columns = c(Jan, Fev, Mar, Abr, Mai, Jun, Jul, Ago, Set, Out, Nov, Dez),
    direction = "column",
    target_columns =  c(Jan, Fev, Mar, Abr, Mai, Jun, Jul, Ago, Set, Out, Nov, Dez),
    method = "numeric",
    domain =  seq(from = -3, to = 3, by = 0.5),
    palette = "RdBu",
    reverse = T,
    na_color = "gray50"
  ) |>
  cols_hide(
    columns = c(
      Temp_1,
      Temp_2,
      Temp_3,
      Temp_4,
      Temp_5,
      Temp_6,
      Temp_7,
      Temp_8,
      Temp_9,
      Temp_10,
      Temp_11,
      Temp_12
    )
  ) |>
  gt::cols_align(align = "center", columns = everything()) |>
  gt::cols_nanoplot(
    columns = c(14:25),
    plot_type = "bar",
    autohide = F,
    new_col_label = md("Anomalias"),
    options = nanoplot_options(
      data_bar_fill_color = "#a60a0a",
      data_bar_stroke_color = "#a60a0a",
      data_bar_negative_fill_color = "#054b72",
      data_bar_negative_stroke_color = "#054b72",
      data_point_radius = 0
    )
  )

tabela

## Agora, vamos salvar a tabela gerada como uma imagem (Anomalias_CamposRJ_gt.png).

tabela |> gt_theme_nytimes() |> 
  gtsave(filename = "Anomalias_CamposRJ_gt.png")

#------------------------https://linktr.ee/pexcca.lamet------------------------#

# Para mais informações do projeto e acompanhar conteúdos relacionados a linguagem 
# de programação R, siga nossas redes sociais. Sugestões, dúvidas, elogios ou
# críticas, envie e-mail para: pexcca.lamet@uenf.br .



