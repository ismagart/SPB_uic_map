# Загрузка библиотек
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
                            function(x) if(dim(x)[2] == 6) x[,c(1,2,6)] else x[,c(1,2,7)])))

# Подготовливаем переменные для разбиения 

street_identifer <- c("ул\\.", "улица", "бул\\.", "бульвар", "шос\\.", "шоссе",
                      "шосс\\.", "наб\\.","ш\\.", "набережная", "квартал", "линия",
                      "пер\\.", "переулок", "пл\\.", "площадь", "пр\\.", "проспект",
                      "аллея", "канал", "остров", "проезд", "пр-т" ,"шос", "б- р",
                      "ул", " по ", "участок")

street_name_pattern <- paste0("[^,\"]*(",paste(street_identifer, collapse = "|"),
                              ")[^,:(]*", collapse = "")

# Паттерн для пригородов
suburb_name <-c("пос\\.", "посёлок", "поселок", "п\\.", "Пос\\.", "г\\.", "Колпин") # костыль для колпино
suburb_pattern <- paste0("^(", paste0(suburb_name, collapse = "|"),")\\W?[^,\\d%(]+" )


# Паттерн с убираемыми словами из заголовка колонок
stop_word <- c("Адрес помещения для", "Адрес помещения для",
               "голосования \\(наименование", "объекта\\), телефон",
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
      # определям название улицы
      gg[k,5] <- stringr::str_extract(stringr::str_to_lower(full_adres_string),street_name_pattern)
      gg[k,6] <- stringr::str_extract(full_adres_string,"(д\\.|дом|(д No))\\W*(No)?\\W*\\d{0,3}(\\/\\d{0,3})?")
      gg[k,7] <- stringr::str_extract(full_adres_string, "(к\\.|корпус)\\W*\\d{1,3}")
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
      # определям название улицы
      gg[k,5] <- stringr::str_extract(stringr::str_to_lower(full_adres_string),street_name_pattern)
      gg[k,6] <- stringr::str_extract(full_adres_string,"(д\\.|дом)\\W*(No)?\\W*\\d{0,3}(\\/\\d{0,3})?")
      gg[k,7] <- stringr::str_extract(full_adres_string, "(к\\.|корпус)\\W*\\d{1,3}")
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
  gg$street_name[which(gg$uic == "110")] <- "Большой пр."
  gg$house_number[which(gg$uic == "145")] <- "д.15"
  gg$house_number[which(gg$uic == "1990")] <- "д.14" 
  gg$house_liter[which(gg$uic == "1990")] <- "А"
  gg$house_number[which(gg$uic == "1997")] <- "д.30"
  gg$suburb_name[which(gg$uic == "1997")] <- "пос. Шушары"
  # Слипание пригорода и улицы
  gg$street_name[which(gg$uic == "1701")] <- "ул. ульяновская"
  # Дан только номер школы
  gg$street_name[which(gg$uic == "1146")] <- "ул. чекистов"
  gg$house_number[which(gg$uic == "1146")] <- "д. 18"
  gg$house_number[which(gg$uic == "1982")] <- "д. 12"
  gg$house_number[which(gg$uic == "2000" | gg$uic == "2001")] <- "д. 19"
  gg$house_liter[which(gg$uic == "2000" | gg$uic == "2001")] <- "А"
  gg$street_name[which(gg$uic == "1386")] <- "Краснопутиловская ул."
  gg$house_number[which(gg$uic == "1386")] <- "д. 60"
  
  
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
  # empty_street_position <- which(is.na(clean_dt$street_name) == T & is.na(clean_dt$suburb_name) == T & is.na(clean_dt$house_number) != T)
  empty_street_position <- which(is.na(clean_dt$street_name) == T &  is.na(clean_dt$house_number) != T)
  for (i in empty_street_position){
    street_empty_pattern <- paste0("[^,\\.\\d]*\\W*(",
                                   paste0(str_replace(clean_dt$house_number[i], "\\.", "\\\\." ),")", collapse = ""))
    # str_remove(clean_dt$adres[i], clean_dt$suburb_name[i])
    replace_string <- ifelse(is.na(clean_dt$suburb_name[i]) == T, clean_dt$adres[i], str_remove(clean_dt$adres[i], clean_dt$suburb_name[i]))
    extract_string <- str_extract(str_to_lower(replace_string),street_empty_pattern)
    clean_string <- str_remove_all(extract_string,stringr::str_replace(clean_dt$house_number[i], "\\.", "\\\\." ))
    clean_dt[i,5] <- clean_string
  }
  
  # Чистка посёлков от названия улиц 
  bad_suburb_position <- which(apply(clean_dt[,c('street_name','suburb_name')], 1,function(x) stringr::str_detect(stringr::str_to_lower(x[2]), x[1])))
  for( i in bad_suburb_position){
    suburb_double_pattern <- paste0(".*(",
                                     paste0(stringr::str_replace(clean_dt$street_name[i], "\\.", "\\\\." ),")", collapse = ""))
    clean_dt[i,9] <-  stringr::str_remove_all(stringr::str_extract(stringr::str_to_lower(clean_dt$suburb_name[i]), suburb_double_pattern),
                                              stringr::str_replace(clean_dt$street_name[i], "\\.", "\\\\." ))
  }
  # чистка улиц от попадания туда поселков
  # bad_street_position <- which(apply(clean_dt[,c('street_name','suburb_name')], 1,function(x) stringr::str_detect(stringr::str_to_lower(x[1]), str_to_lower(x[2]))))
  # for( i in bad_street_position){
  #   clean_dt[i,5] <-  stringr::str_remove_all(clean_dt$street_name[i], str_to_lower(clean_dt$suburb_name[i]))
  # }
  
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
                                   list("\\bканал\\b", "канал"), #особенности канала и канал
                                   list(c("кан\\.", "канала"), "канала"),
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
abbreviation_dictionary <- list(list("\\bд\\.\\W*","демьяна "),
                                list("\\bб\\.\\W*", c("большая ", "большой ")),
                                list("\\bак\\.\\W*", "академика "),
                                list("\\bс\\.\\W*", "софьи "),
                                list("\\bм\\.\\W*б", "маршала б"),
                                list("\\bп\\.\\W*", "пограничника "),
                                list("\\bл\\.\\W*т", "льва т"),
                                list("\\bм\\.\\W*р", "малая р"),
                                list("\\bкр\\.\\W*к", "красного к"),
                                list("\\bа\\.\\W*т", "алексея т"),
                                list("\\bпроф\\.W*", "профессора "),
                                list("\\bл\\.\\W*ш", "лейтенанта ш"))

# Словарь для улиц с буквами Ё

e_yo_dictionary <- list(list(c("непокоренных", "непокорённых"),
                        c("пискаревский", "пискарёвский"),
                        c("\\bлени\\b", "лёни"),
                        c("летчика", "лётчика"),
                        #это на уровне пригорода
                        c("звездная", "звёздная"),
                        c("взлетная", "взлётная"),
                        c("дегтярева", "дегтярёва"),
                        c("королева", "королёва"),
                        c("березовая", "берёзовая"),
                        c("новоселов", "новосёлов")),
                        list(c("Молодежное", "Молодёжное"),
                             c("Саперный", "Сапёрный")))


e_yo_subburb_dictionary <- list(c("Молодежное", "Молодёжное"),
                                c("Саперный", "Сапёрный")) 


# Словарь для улиц на васильевском острове там линии
line_dictionary <- list(list(c("\\b1\\b-?\\w? линия($|( в\\.о\\.))"), "1-я линия в.о."),
                        list(c("\\b2\\b-?\\w? линия($|( в\\.о\\.))", "\\b3\\b-?\\w? линия($|( в\\.о\\.))"), "2-3-я линии в.о."),
                        list(c("\\b4\\b-?\\w? линия($|( в\\.о\\.))", "\\b5\\b-?\\w? линия($|( в\\.о\\.))?"), "4-5-я линии в.о."),
                        list(c("\\b6\\b-?\\w? линия($|( в\\.о\\.))"), "6-я линия в.о."),
                        list(c("\\b7\\b-?\\w? линия($|( в\\.о\\.))"), "7-я линия в.о."),
                        list(c("\\b8\\b-?\\w? линия($|( в\\.о\\.))"), "8-я линия в.о."),
                        list(c("\\b9\\b-?\\w? линия($|( в\\.о\\.))"), "9-я линия в.о."),
                        list(c("10-?\\w? линия($|( в\\.о\\.))?", "11-?\\w? линия($|( в\\.о\\.))"), "10-11-я линии в.о."),
                        list(c("12-?\\w? линия($|( в\\.о\\.))", "13-?\\w? линия($|( в\\.о\\.))"), "12-13-я линии в.о."),
                        list(c("14-?\\w? линия($|( в\\.о\\.))"), "14-я линия в.о."),
                        list(c("15-?\\w? линия($|( в\\.о\\.))"), "15-я линия в.о."),
                        list(c("16-?\\w? линия($|( в\\.о\\.))"), "16-я линия в.о."),
                        list(c("17-?\\w? линия($|( в\\.о\\.))"), "17-я линия в.о."),
                        list(c("18-?\\w? линия($|( в\\.о\\.))", "19-?\\w? линия($|( в\\.о\\.))"), "18-19-я линии в.о."),
                        list(c("20-?\\w? линия($|( в\\.о\\.))", "21-?\\w? линия($|( в\\.о\\.))"), "20-21-я линии в.о."),
                        list(c("22-?\\w? линия($|( в\\.о\\.))", "23-?\\w? линия($|( в\\.о\\.))"), "22-23-я линии в.о."),
                        list(c("24-?\\w? линия($|( в\\.о\\.))?", "25-?\\w? линия($|( в\\.о\\.))?"), "24-25-я линии в.о."),
                        list(c("26-?\\w? линия($|( в\\.о\\.))", "27-?\\w? линия($|( в\\.о\\.))"), "26-27-я линии в.о."))

# Улицы которые без улиц и нужно сделать указатель
street_wh_iden_dictionary <- list(c("вавилова", "улица вавилова"),
                                  c("костюшко", "улица костюшко"),
                                  c("таврическая", "таврическая улица"))

# Другие ручные ошибки слипания улиц с номерами и лишние окончания для цифр
some_miswriting_dictionary <- list(c("(\\d)([[:alpha:]])", "\\1-\\2"),
                                   c("(\\d-)[[:alpha:]]([[:alpha:]])", "\\1\\2"))


# Для улиц где есть только их имя, добавляем идентификатор
add_street_identifer <- function(input_data, ident_dictionary){
  for( i in ident_dictionary){
    line_position <- which(str_detect(input_data$street_name, i[1]) == T)
    input_data$street_name[line_position] <- i[[2]]
}
return(input_data)
}


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
    # input_data$suburb_identifer[position_suburb]   <- i[[2]]
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

abbreviation_expand <- function(input_data,abb_dictionary ){
  for( i in abb_dictionary){
    abbr_position <- which(stringr::str_detect(input_data$street_name, i[[1]]) == T)
    if(length(i[[2]]) == 2){
      replace_vector <- dplyr::if_else(stringr::str_detect(input_data$street_name[abbr_position], "проспект")  == T, i[[2]][2], i[[2]][1])
    }else{
      replace_vector <- rep(i[[2]], length(abbr_position))
    }
    replace_value <- stringr::str_replace_all(input_data$street_name[abbr_position], i[[1]], replace_vector)
    input_data$street_name[abbr_position] <- replace_value
  }
  return(input_data)
}

line_street_cleaner <- function(input_data, line_dictionary){
  for( i in line_dictionary){
    line_pattern <- paste0("(",paste0(i[[1]], collapse  = "|"),")")
    line_position <- which(str_detect(input_data$street_name, line_pattern) == T)
    input_data$street_name[line_position] <- i[[2]]
  }
  return(input_data)
}

liter_yo_cleaner <- function(input_data, yo_dictionary){
  for(i in yo_dictionary[[1]]){
    prep_osition <- which(str_detect(input_data$street_name, i[1]) == T)
    input_data$street_name[prep_osition] <- str_replace(input_data$street_name[prep_osition], i[1], i[2])
  }
  for(i in yo_dictionary[[2]]){
    prep_osition <- which(str_detect(input_data$suburb_name, i[1]) == T)
    input_data$suburb_name[prep_osition] <- str_replace(input_data$suburb_name[prep_osition], i[1], i[2])
  }
  
  return(input_data)
}

numerical_street_name_cleaner <- function(input_data, num_dictionaty){
  for( i in num_dictionaty){
    line_position <- which(str_detect(input_data$street_name, i[1]) == T)
    input_data$street_name[line_position] <- str_replace(input_data$street_name[line_position],i[1],i[2])
  }
  return(input_data)
}

# вызов всех этих функций

system.time(uic_dt <- uic_reader(clear_uic_matrix, stop_word_patern, street_name_pattern, suburb_pattern))
system.time(uic_dt <- cleaning_uic_table(uic_dt))
system.time(uic_dt <- add_street_identifer(uic_dt, street_wh_iden_dictionary))
system.time(uic_dt <- identifer_unfold(uic_dt, street_identifer_dictinary, suburb_identifer_dictionary))
system.time(uic_dt <- cleaning_little_uic_table(uic_dt))
system.time(uic_dt <- abbreviation_expand(uic_dt,abbreviation_dictionary))
system.time(uic_dt <- line_street_cleaner(uic_dt,line_dictionary))
system.time(uic_dt <- liter_yo_cleaner(uic_dt,e_yo_dictionary))
system.time(uic_dt <- numerical_street_name_cleaner(uic_dt,some_miswriting_dictionary))

# Сохраняем дата фрейм
saveRDS(uic_dt, file = "./Election_analysis/uic_td.rds")

# Записываем часть этого дата фрейма как вполне себе готовый резульат

write.table(uic_dt[,c(1,5,6,7,8,9)],"./Election_analysis/uic_tab.csv", sep = ";", fileEncoding = "UTF-8")

# Ручное обновление данных, из-за особенностей осм
manual_data <- list(list(1, c("14", NA,NA,"лермонтовский проспект", NA, "проспект")),
                    list(5, c("15", NA,NA,"лермонтовский проспект", NA, "проспект")),
                    list(11, c("4-6", NA, NA, "набережная реки пряжки", NA, "набережная")),
                    list(19, c("38/4", NA,NA, "вознесенский проспект", NA, "проспект")),
                    list(24, c("1", NA, NA, "средняя подьяческая улица", NA, "улица")),
                    list(25, c("1", NA, NA, "средняя подьяческая улица", NA, "улица")),
                    list(40, c("17", NA, NA, "малодетскосельский проспект", NA, "проспект")),
                    list(62, c("3", NA, NA, "кадетская линия в.о.", NA, "линия")),
                    list(63, c("3", NA, NA, "кадетская линия в.о.", NA, "линия")),
                    list(66, c("52", NA, NA, "7-я линия в.о.", NA, "линия")),
                    list(68, c("31", NA, NA, "большой проспект в.о.", NA, "проспект")),
                    list(69, c("1/15", NA, NA, "набережная лейтенанта шмидта", NA, "набережная")),
                    list(72, c("1/15", NA, NA, "набережная лейтенанта шмидта", NA, "набережная")),
                    list(79, c("52", NA, NA, "7-я линия в.о.", NA, "линия")),
                    list(92, c("3", "А", NA, "улица шевченко", NA, "улица")),
                    list(96, c("5", NA, NA, "шкиперский проток", NA, "проток")),
                    list(97, c("26", NA, NA, "улица шевченко", NA, "улица")),
                    list(98, c("26", NA, NA, "улица шевченко", NA, "улица")),
                    list(135, c("2", NA, NA, "переулок каховского", NA, "переулок")),
                    list(136, c("2", NA, NA, "переулок каховского", NA, "переулок")),
                    list(306, c("12/2", NA, NA, "проспект непокорённых", NA, "проспект")),
                    list(415, c("1", NA, NA, "средняя подьяческая улица", NA, "улица")),
                    list(471, c("32", NA, NA, "ЗСД", NA, NA)),
                    list(472, c("32", NA, NA, "ЗСД", NA, NA)),
                    list(549, c("30", NA, "2", "улица стойкости", NA, "улица")),
                    list(551, c("30", NA, "2", "улица стойкости", NA, "улица")),
                    list(677, c("8", "А", NA, "большая пороховская улица", NA, "улица")),
                    list(678, c("8", "А", NA, "большая пороховская улица", NA, "улица")),
                    list(814, c("40", NA, NA, "улица лётчика пилютова", NA, "улица")),
                    list(924, c("5", NA, NA, "улица лебедева", "кронштадт", "улица")),
                    list(925, c("5", NA, NA, "улица лебедева", "кронштадт", "улица")),
                    list(926, c("4/11", NA, NA, "улица рошаля", "кронштадт", "улица")),
                    list(927, c("4/11", NA, NA, "улица рошаля", "кронштадт", "улица")),
                    list(952, c("140", NA, NA, "2-й проезд", "песочный", NA)),
                    list(953, c("53", NA, NA, " улица ленинградская", "Песочный", "улица")),
                    list(957, c("5", NA, NA, "улица лесная", "серово", "улица")),
                    list(958, c("9", NA, NA, "проспект красных командиров", "Сестрорецк", "проспект")),
                    list(1195, c("10", NA, NA, "проспект пятилеток", NA, "проспект")),
                    list(1196, c("10", NA, NA, "проспект пятилеток", NA, "проспект")),
                    list(1197, c("10", NA, NA, "проспект пятилеток", NA, "проспект")),
                    list(1234, c("8", NA, "2", " улица кибальчича", NA, "улица")),
                    list(1275, c("36/73", NA, NA, "каменноостровский проспект", NA, "проспект")),
                    list(1277, c("5/7", NA, NA, "сытнинская площадь", NA, "площадь")),
                    list(1278, c("2", NA, NA, "улица большая монетная", NA, "улица")),
                    list(1281, c("3", NA, NA, "троицкая площадь п.с.", NA, "площадь")),
                    list(1282, c("5", NA, NA, "большая посадская улица", NA, "улица")),
                    list(1284, c("5", NA, NA, "большая посадская улица", NA, "улица")),
                    list(1292, c("69", NA, NA, "каменноостровский проспект", NA, "проспект")),
                    list(1294, c("42", NA, NA, "каменноостровский проспект", NA, "проспект")),
                    list(1299, c("4", "Б", NA, "пудожская улица", NA, "улица")),
                    list(1302, c("29/2", NA, NA, "большой проспект п.с.", NA, NA)),
                    list(1315, c("4", "Б", NA, "пудожская улица", NA, "улица")),
                    list(1317, c("25", "А", NA, "чкаловский проспект", NA, "проспект")),
                    list(1324, c("102", "А", NA, "санкт-петербургское шоссе", NA, "шоссе")),
                    list(1342, c("10", NA, "1", "улица шахматова", "петергоф", "улица")),
                    list(1343, c("10", NA, "1", "улица шахматова", "петергоф", "улица")),
                    list(1348, c("40", "Б", NA, "дворцовый проспект", "Ломоносов", "проспект")),
                    list(1350, c("8", "А", NA, "улица сафронова", "Ломоносов", "улица")),
                    list(1351, c("8", "А", NA, "улица сафронова", "Ломоносов", "улица")),
                    list(1406, c("7", NA, "3", "яхтенная улица", NA, "улица")),
                    list(1407, c("7", NA, "3", "яхтенная улица", NA, "улица")),
                    list(1408, c("7", NA, "3", "яхтенная улица", NA, "улица")),
                    list(1431, c("16", NA, "2", "новосибирская улица", NA, "улица")),
                    list(1432, c("16", NA, "2", "новосибирская улица", NA, "улица")),
                    list(1542, c("4", NA, "4", "новоколомяжский проспект", NA, "проспект")),
                    list(1543, c("4", NA, "4", "новоколомяжский проспект", NA, "проспект")),
                    list(1544, c("4", NA, "4", "новоколомяжский проспект", NA, "проспект")),
                    list(1545, c("4", NA, "4", "новоколомяжский проспект", NA, "проспект")),
                    list(1546, c("52", NA, NA, "поклонногорская улица", NA, "улица")),
                    list(1767, c("8", NA, NA, "гродненский переулок", NA, "переулок")),
                    list(1791, c("9/27", NA, NA, "кузнечный переулок", NA, "улица"))                    )

for(i in manual_data){
  uic_dt[i[[1]],c(6,8,7,5,9,10)] <- i[[2]]
}
# Пустые значения заменяем на NA
# возможно метод оченб плох
system.time( uic_dt[uic_dt==""]<-NA)

write.table(uic_dt[,c(1,5,6,7,8,9)],"./Election_analysis/uic_tab.csv", sep = ";", fileEncoding = "UTF-8")
saveRDS(uic_dt, file = "./Election_analysis/uic_td.rds")

street_empty_pattern <- paste0("[^,\\.\\d]*\\W*(",
                               paste0(str_replace(uic_dt$house_number[1076], "\\.", "\\\\." ),")", collapse = ""))

test <- stringr::str_remove_all(stringr::str_extract(stringr::str_to_lower(str_remove(uic_dt$adres[1076], uic_dt$suburb_name[1076])), street_empty_pattern),
                                         stringr::str_replace(uic_dt$house_number[1076], "\\.", "\\\\." ))
uic_dt$adres[1076]
str_extract(str_to_lower(uic_dt$adres[1076]), "[^,\\.\\d]*\\W*(д\\. 62)")
ifelse( str_remove(uic_dt$adres[1076], ifelse(is.na(uic_dt$suburb_name[1076]) == T, "", uic_dt$suburb_name[1076])))
replace_string <- ifelse(is.na(uic_dt$suburb_name[1076]) == T, uic_dt$adres[1076], str_remove(uic_dt$adres[1076], uic_dt$suburb_name[1076]))
str_extract(str_to_lower(replace_string), "[^,\\.\\d]*\\W*(д\\. 62)")
