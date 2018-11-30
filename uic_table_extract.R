# Загружка библиотек
library(tabulizer)
library(dplyr)
library(stringr)

# Чтение pdf (самое долгое)
system.time(uic_table <- tabulizer::extract_tables("./Election_analysis/Adres_yic_on_marth_election/spisok_iu_adm_spb.pdf",
                            encoding = "UTF-8"))
# Сохраняем, чтоб дальше не было долгих ожиданий
saveRDS(uic_table, file = "uic_table.rds")
# Считываем с уже записанных данных
uic_table <- readRDS(file = "./Election_analysis/uic_table.rds")

# Создаем одну матрицу с данными

system.time(clear_uic_matrix <-  do.call(rbind,lapply(uic_table, 
                            function(x) if(dim(x)[2] == 6) x[,c(1,2,5)] else x[,c(1,2,6)])))

# Подготовливаем переменные для разбиения 

street_identifer <- c("ул\\.", "улица", "бул\\.", "бульвар", "шос\\.", "шоссе",
                      "шосс\\.", "наб\\.","ш\\.", "набережная", "квартал", "линия",
                      "пер\\.", "переулок", "пл\\.", "площадь", "пр\\.", "проспект",
                      "аллея", "канал", "остров", "проезд", "пр-т" ,"шос", "б- р",
                      "ул", " по ", "участок")

street_name_pattern <- paste0("[^,\"]*(",paste(street_identifer, collapse = "|"),
                              ")[^,:(]*", collapse = "")

# Паттерн для пригородов
suburb_name <-c("пос\\.", "посёлок", "поселок", "п\\.", "Пос\\.", "г\\.")
suburb_pattern <- paste0("^(", paste0(suburb_name, collapse = "|"),")\\W?[^,\\d%(]+" )


# Паттерн с убираемыми словами из заголовка колонок
stop_word <- c("Адрес помещения для", "Адрес помещения для",
               "работы участковой", "избирательной комиссии",
               "\\(наименование объекта\\),", "телефон" )
stop_word_patern <- paste0(stop_word, collapse = "|")

# Функция по разделению адресов и записи все в датаврейм
uic_reader <- function(matrix_uic, stop_word_patern, street_name_pattern, suburb_pattern){
  gg <- data.frame(uic = character(2500), 
                   start_position = integer(2500),
                   end_position = integer(2500),
                   adres = character(2500),
                   street_name = character(2500),
                   house_number = character(2500),
                   house_corpus = character(2500),
                   house_liter = character(2500),
                   suburb_name = character(2500),
                   stringsAsFactors = F)
  i <- 1
  k <- 1
  G <- FALSE
  while(i <= length(matrix_uic[,1])){
    if(grepl("ТИК", matrix_uic[i,1]) & G == FALSE){
      g <-   i
      G <- TRUE
      i <- i+1
    }
    if(grepl("ТИК", matrix_uic[i,1]) & G == TRUE){
      gg[k,1] <- matrix_uic[g,2]
      gg[k,2] <- g
      gg[k,3] <- i-1
      adres_string <- matrix_uic[g:(i-1),3]
      adres_string <- stringr::str_replace_all(stringr::str_replace_all(adres_string, stop_word_patern, ""), "\\r", " ")
      full_adres_string <- paste(adres_string[nzchar(adres_string)], collapse = "%%")
      gg[k,4] <- full_adres_string
      # опрределям название улицы
      gg[k,5] <- stringr::str_extract(stringr::str_to_lower(full_adres_string),street_name_pattern)
      gg[k,6] <- stringr::str_extract(full_adres_string,"(д\\.|дом|(д No))\\W*(No)?\\W*\\d{0,3}(\\/\\d{0,3})?")
      gg[k,7] <- stringr::str_extract(full_adres_string, "(к\\.|корпус)\\W*\\d{0,3}")
      gg[k,8] <- stringr::str_extract(full_adres_string,
                             "((лит\\.|литера|литер)(\\W*)(А|Б|В|Г))|(\\d(а|б|в|г|А|Б|В|Г))|(\\d[ ](а|б|в|г)\\W)")
      gg[k,9] <- stringr::str_extract(full_adres_string, suburb_pattern)
      g <- i
      k <- k+1
    }
    # Анализ последней записи
    if(i == length(matrix_uic[,1])){
      gg[k,1] <- matrix_uic[g,2]
      gg[k,2] <- g
      gg[k,3] <- i-1
      adres_string <- matrix_uic[g:(i-1),3]
      adres_string <- stringr::str_replace_all(adres_string, stop_word_patern, "")
      full_adres_string <- paste(adres_string[nzchar(adres_string)], collapse = "%%")
      gg[k,4] <- full_adres_string
      # опрределям название улицы
      gg[k,5] <- stringr::str_extract(stringr::str_to_lower(full_adres_string),street_name_pattern)
      gg[k,6] <- stringr::str_extract(full_adres_string,"(д\\.|дом)\\W*(No)?\\W*\\d{0,3}(\\/\\d{0,3})?")
      gg[k,7] <- stringr::str_extract(full_adres_string, "(к\\.|корпус)\\W*\\d{0,3}")
      gg[k,8] <- stringr::str_extract(full_adres_string,
                             "((лит\\.|литера|литер)(\\W*)(А|Б|В|Г))|(\\d(а|б|в|г|А|Б|В|Г))|(\\d[ ](а|б|в|г)\\W)")
      gg[k,9] <- stringr::str_extract(full_adres_string, suburb_pattern)
    }
    i <- i +1
  }
  
  # Ручное добивание того, что не считалось
  gg$street_name[which(gg$uic == "139" | gg$uic == "140" )] <- "ул.остоумова"
  gg$street_name[which(gg$uic == "1254")] <- "ул. восточная"
  gg$street_name[which(gg$uic == "1905")] <- "пр. авиаконструкторов"
  gg$street_name[which(gg$uic == "158")] <- "ул. наличная"
  # Решение проблемы большой монетной 29
  gg$street_name[which(gg$uic == "1634")] <- "большая монетная"
  # Слипание пригорода и улицы
  gg$street_name[which(gg$uic == "1701")] <- "ул. ульяновская"
  gg$street_name[which(gg$uic == "1725" | gg$uic == "1724")] <- "советская ул."
  # Дан только номер школы
  gg$street_name[which(gg$uic == "1146")] <- "ул. чекистов"
  gg$house_number[which(gg$uic == "1146")] <- "д. 18"
  gg$house_number[which(gg$uic == "1982")] <- "д. 12"
  gg$house_number[which(gg$uic == "2000" | gg$uic == "2001")] <- "д. 19"
  gg$house_liter[which(gg$uic == "2000" | gg$uic == "2001")] <- "А"
  
  return(gg)
}


# Функция по очистке и правке полученных адресов
cleaning_uic_table <- function(uic_dataframe){
  # убрать нижнюю ненужную часть, а так же строки без номера УИК
  clean_dt <- uic_dataframe[which(nzchar(uic_dataframe[,1])),]
  
  #номера домов без указания что это дом, просто дан номер
  bad_position_house <- which(is.na(clean_dt$house_number) == T & is.na(clean_dt$street_name) != T)
  for (i in bad_position_house){
    finding_pattern <- paste0(paste0("(", clean_dt$street_name[i], collapse = ""),
                              ")\\W*\\d{0,3}(\\/\\d{0,3})?")
    clean_dt[i,6] <- stringr::str_remove_all(stringr::str_extract(stringr::str_to_lower(clean_dt$adres[i]), finding_pattern),
                                    clean_dt$street_name[i])
  }
  # улицы без идентификатора, что это улицы или проспекты
  empty_street_position <- which(is.na(clean_dt$street_name) == T & is.na(clean_dt$suburb_name) == T & is.na(clean_dt$house_number) != T)
  for (i in empty_street_position){
    street_empty_pattern <- paste0("[^,\\.\\d]*\\W*(",
                                   paste0(str_replace(clean_dt$house_number[i], "\\.", "\\\\." ),")", collapse = ""))
    clean_dt[i,5] <- stringr::str_remove_all(stringr::str_extract(stringr::str_to_lower(clean_dt$adres[i]), street_empty_pattern),
                                             stringr::str_replace(clean_dt$house_number[i], "\\.", "\\\\." ))
  }
  
  # Чистка посёлков от названия улиц и наоборот
  bad_suburb_position <- which(apply(clean_dt[,c('street_name','suburb_name')], 1,function(x) stringr::str_detect(stringr::str_to_lower(x[2]), x[1])))
  for( i in bad_suburb_position){
    suburb_double_pattern <- paste0(".*(",
                                     paste0(stringr::str_replace(clean_dt$street_name[i], "\\.", "\\\\." ),")", collapse = ""))
    clean_dt[i,9] <-  stringr::str_remove_all(stringr::str_extract(stringr::str_to_lower(clean_dt$suburb_name[i]), suburb_double_pattern),
                                              stringr::str_replace(clean_dt$street_name[i], "\\.", "\\\\." ))
  }
  
  
  # знаки %% в адресах
  #обозначение цифры в графе литеры
  # лишние обозначения домов в строке улицы
  clean_dt <- dplyr::mutate(clean_dt,house_number = stringr::str_remove_all(house_number,"%|No"),
                     house_corpus = stringr::str_remove_all(house_corpus, "%"),
                     house_liter = stringr::str_remove_all(house_liter, "((%)|(\\d))"),
                     street_name = stringr::str_remove_all(street_name, "(д\\.|дом|дом no)\\W*\\d.*|(т\\.)?\\d+-\\d+.*"))
  
  clean_dt <- dplyr::mutate(clean_dt, 
                     house_number = stringr::str_remove_all(house_number, ","),
                     street_name = stringr::str_replace_all(street_name, "%%", " "))
  return(clean_dt)
}

# "Разворачивание" идентификаторов улиц
street_identifer_dictinary <- list(list(c("бул\\.","б- р", "\\bбул\\b", "бульвар"), "бульвар"), 
                                   list(c("ул\\.","\\bул\\b", "улица"), "улица"),
                                   list(c("шос\\.", "шосс\\.", "ш\\.", "\\bшос\\b", "шоссе"), "шоссе"),
                                   list(c("пр-т","пр\\." ), "проспект"),
                                   list(c("кан\\.", "канал"), "канал"),
                                   list("\\bр\\.", "реки"),
                                   list(c("наб\\.", "набережная"), "набережная"),
                                   list("квартал", "квартал"),
                                   list("линия", "линия"),
                                   list("пер\\.", "переулок"),
                                   list("пл\\.", "площадь"),
                                   list("площадь", "площадь"),
                                   list("аллея", "аллея"),
                                   list("\\bостров\\b", "остров"), 
                                   list("\\bпроезд\\b", "проезд"),
                                   list("\\bпо\\b", "почтовое отделение"), 
                                   list("участок", "участок"),
                                   list("переулок", "переулок"))

suburb_identifer_dictionary <- list(list(c("\\bп\\.", "\bпос\\.", "\\bПос\\.", "поселок", "\\bпос\\b"), "посёлок"),
                                    list("г\\.", "город"))

# разворачиваем всякие сокращения в идентификаторах
identifer_unfold <- function(input_data, dictionary_list_street, dictionary_list_suburb){
  input_data$street_identifer <- NA
  for (i in dictionary_list_street){
    pattern <- paste0(i[[1]], collapse = "|")
    position_street <- which(stringr::str_detect(input_data$street_name, pattern) == T)
    input_data$street_name[position_street] <- stringr::str_replace(input_data$street_name[position_street], pattern, i[[2]])
    # Отдельно записываем идентификатор, так как при сборке запроса в геокодер
    # иногда приходится переставлять местами название и идентификатор
    input_data$street_identifer[position_street] <- i[[2]]
    # Убираемся после себя
    space_pattern <- paste0("(\\b",i[[2]], "\\B|\\B", i[[2]], "\\b)")
    input_data$street_name[position_street] <- stringr::str_remove_all(stringr::str_replace_all(input_data$street_name[position_street], space_pattern, " \\1 "),"^ | $")
  }
  # input_data$suburb_identifer <- NA
  for (i in dictionary_list_suburb){
    pattern <- paste0(i[[1]], collapse = "|")
    position_suburb <- which(stringr::str_detect(input_data$suburb_name, pattern) == T)
    # Удаляем этот идентификатор вообще
    input_data$suburb_name[position_suburb] <- stringr::str_remove_all(input_data$suburb_name[position_suburb], pattern)
    # убираемся, это всякие пустые места в начале
    input_data$suburb_name[position_suburb] <- stringr::str_remove_all(input_data$suburb_name[position_suburb], "^[ ]*\\.*[ ]*")
  }
  return(input_data)
}

# Снова чистим
cleaning_little_uic_table <- function(input_data){
  # Удаляем префикс указателя что это дом/д. и т.д
  # удаляем указание что это корпуc
  # Удаляем указание что это это литера
  # Удаляем всякие запятые, кавычки в строке улиц
  input_data <- dplyr::mutate(input_data, 
                              house_number = stringr::str_remove_all(house_number,"[^[:digit:]/]*"),
                              house_corpus = stringr::str_remove_all(house_corpus, "[^[:digit:]/]*"),
                              house_liter = stringr::str_to_upper(
                                stringr::str_remove(house_liter,"(лит\\.|литера|литер|)\\W*[ ]?")),
                              street_name = stringr::str_remove_all(street_name, "[^\\.\\w -]*|^[ ]*|\\W*$"),
                              uic = stringr::str_remove_all(uic, "[^\\d]*"))
  # Немного костылей
  return(dplyr::mutate(input_data, 
                       street_name = stringr::str_remove_all(street_name, "\\W*$"),
                       street_name = stringr::str_remove_all(street_name, "^[ ]*"),
                street_name = stringr::str_replace(street_name, "в\\.о$","в.о.")
                ))
         
  }




# вызов всех этих функций

system.time(uic_dt <- uic_reader(clear_uic_matrix, stop_word_patern, street_name_pattern, suburb_pattern))
system.time(uic_dt <- cleaning_uic_table(uic_dt))
system.time(uic_dt <- identifer_unfold(uic_dt, street_identifer_dictinary, suburb_identifer_dictionary))
system.time(uic_dt <- cleaning_little_uic_table(uic_dt))

# Сохраняем дата фрейм
saveRDS(uic_dt, file = "./Election_analysis/uic_td.rds")

# Записываем часть этого дата фрейма как вполне себе готовый резульат

write.table(uic_dt[,c(1,5,6,7,8,9)],"./Election_analysis/uic_tab.csv", sep = ";", fileEncoding = "UTF-8")
