Código = "IMACA28" # Da estação
Data = "2022-11-1" # Data que deseja
Modo = "daily" # daily ou monthly

Leitura <- function(Código, Data, Modo){

  # Pacotes
  if(!require(pacman)){install.packages("pacman")}
  pacman::p_load(tidyverse, janitor, lubridate, rvest)

  # Constantes
  xpath <- '//*[@id="main-page-content"]/div/div/div/lib-history/div[2]/lib-history-table/div/div/div/table'

  if(Modo == "monthly"){

    # URL
    www <- paste("https://www.wunderground.com/dashboard/pws/", Código, "/table/", Data,"/", Data, "/", Modo, sep = "")

    # Leitura
    df <-
      read_html(www) %>%
      rvest::html_nodes(xpath = xpath) %>%
      rvest::html_table() %>%
      dplyr::bind_rows() %>%
      janitor::clean_names() %>%
      filter(!row_number() %in% 1) %>%

      # Correção nos nomes
      rename(
        Data = x1,
        Temperature_High = temperature_2,
        Temperature_Avg = temperature_3,
        Temperature_Low = temperature_4,
        DewPoint_High = dew_point_5,
        DewPoint_Avg = dew_point_6,
        DewPoint_Low = dew_point_7,
        Humidity_High = humidity_8,
        Humidity_Avg = humidity_9,
        Humidity_Low = humidity_10,
        Speed_High = speed_11,
        Speed_Avg = speed_12,
        Speed_Low = speed_13,
        Pressure_High = pressure_14,
        Pressure_Low = pressure_15,
        PrecipAccum = precip_accum
      ) %>%

      # Extrair valores
      mutate(
        Data = mdy(Data),
        Temperature_High = parse_number(Temperature_High),
        Temperature_Avg = parse_number(Temperature_Avg),
        Temperature_Low = parse_number(Temperature_Low),
        DewPoint_High = parse_number(DewPoint_High),
        DewPoint_Avg = parse_number(DewPoint_Avg),
        DewPoint_Low = parse_number(DewPoint_Low),
        Humidity_High = parse_number(Humidity_High),
        Humidity_Avg = parse_number(Humidity_Avg),
        Humidity_Low = parse_number(Humidity_Low),
        Speed_High = parse_number(Speed_High),
        Speed_Avg = parse_number(Speed_Avg),
        Speed_Low = parse_number(Speed_Low),
        Pressure_High = parse_number(Pressure_High),
        Pressure_Low = parse_number(Pressure_Low),
        PrecipAccum = parse_number(PrecipAccum)
      ) %>%

      # Correção de escalas
      mutate(
        Temperature_High = (Temperature_High-32)*5/9,
        Temperature_Avg = (Temperature_High-32)*5/9,
        Temperature_Low = (Temperature_High-32)*5/9,
        DewPoint_High = (DewPoint_High-32)*5/9,
        DewPoint_Avg = (DewPoint_High-32)*5/9,
        DewPoint_Low = (DewPoint_High-32)*5/9,
        Speed_High = Speed_High*1.609,
        Speed_Avg = Speed_Avg*1.609,
        Speed_Low = Speed_Low*1.609,
        Pressure_High = Pressure_High*33.864,
        Pressure_Low = Pressure_High*33.864,
        PrecipAccum = PrecipAccum*25.4
      )

    print(df)

  }else{

    # URL
    www <- paste("https://www.wunderground.com/dashboard/pws/", Código, "/table/", Data,"/", Data, "/", "daily", sep = "")

    # Leitura
    df <-
      read_html(www[1]) %>%
      rvest::html_nodes(xpath = xpath) %>%
      rvest::html_table() %>%
      dplyr::bind_rows() %>%
      janitor::clean_names() %>%

      # Correção de Nomes
      rename(
        Time = time,
        Temperature = temperature,
        DewPoint = dew_point,
        Humidity = humidity,
        Wind = wind,
        Speed = speed,
        Gust = gust,
        Pressure = pressure,
        PrecipRate = precip_rate,
        PrecipAccum = precip_accum,
        Uv = uv,
        Solar = solar
        ) %>%

      # Extraindo Numeros
      mutate(
        Time = parse_time(Time),
        Temperature = parse_number(Temperature),
        DewPoint = parse_number(DewPoint),
        Humidity = parse_number(Humidity),
        Speed = parse_number(Speed),
        Gust = parse_number(Gust),
        Pressure = parse_number(Pressure),
        PrecipRate = parse_number(PrecipRate),
        PrecipAccum = parse_number(PrecipAccum),
        Solar = parse_number(Solar)
      ) %>%

      # Correção de escala
      mutate(
        Temperature = (Temperature-32)*5/9,
        DewPoint = (DewPoint-32)*5/9,
        Speed = Speed*1.609,
        Gust = Gust*1.609,
        Pressure = Pressure*33.864,
        PrecipRate = PrecipRate*25.4,
        PrecipAccum = PrecipAccum*25.4
      )

    print(df)

  }

}
