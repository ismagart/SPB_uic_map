# Загрузка библиотек
library(tabulizer)
library(dplyr)
library(stringr)
library(urltools)
library(XML)
library(httr)
library(xml2)
# Считываем таблицу
ggt <- readRDS("./Election_analysis/uic_td.rds")
# параметры запроса к nominatim
# просим формат json с ним удобнее работать, он сразу как лист распознается
# просим возвращать детали адресса
# не проси возвращать границы полигонов здания
# берем только первый ответ
qury_param <- "&format=jsonv2&addressdetails=1&polygon=0&limit=1"

# нужно сдлеать функцию, которая бы создавала запрос
# Можно выбрать какие строчки будут давать запрос
# а так же можно менять написание улиц
# Прорабоать случай когда длины index_vector, street_preposition могут оказаться различными
# или сделать их парными в одном листе
# Лучше в качетве параметра задачать сразу свой параметр полной строчки
creator_query <- function(input_df,maiun_url, parameters,liter = TRUE,corpus = TRUE ,index_vector, street_preposition){
  if(missing(index_vector)){
    # Если упущен вектор позиций, значит работает со всеми и запуск впервыи
    input_df$adres_clear <- NA
    input_df$nomit_query <- NA
    index_vector <- 1:nrow(input_df)
  }
  if(missing(street_preposition)){
    street_preposition <- input_df$street_name[index_vector]
  }
  if(liter == FALSE){
    house_num_liter <- input_df$house_number[index_vector]
  }else{
    house_num_liter <- paste0(input_df$house_number[index_vector], ifelse(is.na(input_df$house_liter[index_vector]), "", input_df$house_liter[index_vector]))
  }
  if(corpus == FALSE){
    house_corpus <- rep("",length(index_vector))
  }else{
  house_corpus <- ifelse(is.na(input_df$house_corpus[index_vector]),"", paste0("к", input_df$house_corpus[index_vector]))
  }
  house_number <- str_remove(paste(house_num_liter,house_corpus), "[ ]$")
  street_name <- ifelse(is.na(street_preposition), "", street_preposition)
  suburb_name <- ifelse(is.na(input_df$suburb_name[index_vector]), "", input_df$suburb_name[index_vector])
  city_name <- "Санкт-Петербург"
  
  adres <- str_remove_all(paste(house_number, street_name, suburb_name, city_name, sep = ", "), " ,")
  input_df$adres_clear[index_vector] <- adres
  # nominatim_start <- "https://nominatim.openstreetmap.org/search?q="
  # qury_param <- "&format=jsonv2&addressdetails=1&polygon=0&limit=1"
  query <- paste0(maiun_url, url_encode(enc2utf8(adres)),parameters)
  input_df$nomit_query[index_vector] <- query
  return(input_df)
}

# Создали первый вариант запросов
ggt <- creator_query(ggt,nominatim_start,qury_param)

# Фунция которая дает задержу 1 cек, а не то забанят
# Нужен стопер мало ли там все завислл на запросе одном
nomunatim_geocode <- function(request_vecror){
  curr_value <- httr::GET(request_vecror)
  Sys.sleep(1)
  return(curr_value)
}


# создаем новый лист который содерижит ответы на запрос, число элементов в нем
# равно числу запросов, даже если ответ NULL 
system.time(test_list <- lapply(ggt$nomit_query, function(x) nomunatim_geocode(x)))
# Сохраняем парсинг
saveRDS(test_list, file = "./Election_analysis/parse_list.rds")
# Специальано сохранили как parse_list_first_run.rds чтоб можно было откатиться

test_result <- lapply(test_list, function(x) unlist(content(x, as = "parsed",type = "application/json", encoding = "UTF-8"), recursive = FALSE))
# Проверяем ранк места, чтоб понять дом это ли это (30) или нечто большее
# NULL гововрит что не удалось найти
qual <- sapply(test_result, function(x) unlist(x$place_rank),USE.NAMES = FALSE)

# качество около 63 процентов
length(qual[qual == "30"])/length(qual)
head(is.null(qual))
# Qual лист чтоб сохранять NULL
null_qual <- sapply(qual, function(x) is.null(x))
# Позиция где NULL для них возможно переделать запрос
which(null_qual == T)
# Просто позиции которые не нашлись правильно переделываем запрос
not_match_qual <- sapply(qual, function(x) ifelse(is.null(x), TRUE,x !="30"))
length(which(not_match_qual == T))

# Создаем функцию, которая меняет местами улицу и адрес
alternative_street_query <- function(input_data, index_vector){
  search_street_name <- input_data$street_name[index_vector]
  street_identifer <- input_data$street_identifer[index_vector]
  not_identifer <- str_remove_all(search_street_name,street_identifer)
  shift_pattern <- sapply(1:length(index_vector),function(x) 
    paste0("(",street_identifer[x],"|",not_identifer[x],")(",not_identifer[x],"|",street_identifer[x],")"))
  # shift_pattern <- paste0("(",street_identifer,"|",not_identifer,")(",not_identifer,"|",street_identifer,")")
  new_street_name <- str_replace_all(search_street_name,shift_pattern, "\\2 \\1")
  new_street_name <- str_remove_all(new_street_name, "^[ ]|[ ]$")
  
  return(new_street_name)
}

replace_street <- alternative_street_query(ggt, which(not_match_qual == T))
# Запросы к nominatim изменились
gg <- creator_query(gg,nominatim_start,qury_param, which(not_match_qual == T), replace_street )

head(which(null_qual == T))
system.time(update_list <- lapply(gg$nomit_query[which(not_match_qual == T)], function(x) nomunatim_geocode(x)))

# Обновляем наш лист запросов
test_list[which(not_match_qual == T)] <- update_list
# Обновляем лист json
requir_json <- lapply(test_list, function(x) unlist(content(x, as = "parsed",type = "application/json", encoding = "UTF-8"), recursive = FALSE))
# Новый фактор качества
qual_swap_street <- sapply(requir_json, function(x) unlist(x$place_rank))
# Качество стало 75 %
length(qual_swap_street[qual_swap_street == "30"])/length(qual_swap_street)
# Смотрим что не заматчило и вектор из FALSE чтоб потом сравнить с начальной 
# и выделить то, что не помогло
not_match_qual_swap_street <- sapply(qual_swap_street, function(x) ifelse(is.null(x), TRUE,x !="30"))
# незаматчилось 453
length(which(not_match_qual_swap_street == T))
# Так как изначально меняли только то что не определилось, то 
# Есть переход от F -> T, значит новые вектор мисматчей сразу же используем
# Для новых модификаций
# Вопрос о порядке их открыт

# Создаем функцию, которая будет убирать литеру в строке адреса
# Просто добавили параметр в функцию построения запроса, чтоб одна строчка менялась

# Делаем новый запрос, улицы будут как в первом, но литера уберутся
gg <- creator_query(gg,nominatim_start,qury_param,liter = FALSE, which(not_match_qual_swap_street == T) )
system.time(update_list <- lapply(gg$nomit_query[which(not_match_qual_swap_street == T)], function(x) nomunatim_geocode(x)))
# Обновляем наш лист запросов
test_list[which(not_match_qual_swap_street == T)] <- update_list
# Обновляем лист json
requir_json <- lapply(test_list, function(x) unlist(content(x, as = "parsed",type = "application/json", encoding = "UTF-8"), recursive = FALSE))
# Новый фактор качества
qual_drop_liter <- sapply(requir_json, function(x) unlist(x$place_rank))
# Качество стало 88 %
length(qual_drop_liter[qual_drop_liter == "30"])/length(qual_drop_liter)

# Совмещаем два подхода
not_match_qual_drop_liter <- sapply(qual_drop_liter, function(x) ifelse(is.null(x), TRUE,x !="30"))
length(which(not_match_qual_drop_liter == T))

replace_street_liter <- alternative_street_query(gg, which(not_match_qual_drop_liter == T))
gg <- creator_query(gg,nominatim_start,qury_param,liter = FALSE, which(not_match_qual_drop_liter == T), replace_street_liter )

system.time(update_list <- lapply(gg$nomit_query[which(not_match_qual_drop_liter == T)], function(x) nomunatim_geocode(x)))

test_list[which(not_match_qual_drop_liter == T)] <- update_list
requir_json <- lapply(test_list, function(x) unlist(content(x, as = "parsed",type = "application/json", encoding = "UTF-8"), recursive = FALSE))

qual_swap_street_liter <- sapply(requir_json, function(x) unlist(x$place_rank))
# Точность повысилась до 89%
length(qual_swap_street_liter[qual_swap_street_liter == "30"])/length(qual_swap_street_liter)

# Посмотрим адреса которые дали плохой матчинг
not_match_qual_street_liter <- sapply(qual_swap_street_liter, function(x) ifelse(is.null(x), TRUE,x !="30"))

gg$adres_clear[which(not_match_qual_street_liter == T)]

# Иногда мешают корпуса -- возможно попровить
# ПРоблемы с линиями -- возможно попровить
# Буквы Ё Лёни Голикова улицы -- возможно поправить
# отсутствие дефиса между числом и буквой -- возможно поравить
# проблемы с дробью -- только ручное
# нет идентификатора улиц -- возможно попроваить
# чего-то нет на картах осм
# Некоторые корпуса прописаны как дробь

# надо проверить не только по типу матчинга (дом) вдруг это не те дома

# Пробуем убрать корпуса. сразу число кобинаций растет((
gg <- creator_query(gg,nominatim_start,qury_param,liter = TRUE,corpus = FALSE, which(not_match_qual_street_liter == T))

system.time(update_list <- lapply(gg$nomit_query[which(not_match_qual_street_liter == T)], function(x) nomunatim_geocode(x)))

test_list[which(not_match_qual_street_liter == T)] <- update_list
requir_json <- lapply(test_list, function(x) unlist(content(x, as = "parsed",type = "application/json", encoding = "UTF-8"), recursive = FALSE))

qual_corpus <- sapply(requir_json, function(x) unlist(x$place_rank))
# Точность повысилась до 91,6%
length(qual_corpus[qual_corpus == "30"])/length(qual_corpus)

# Посмотрим адреса которые дали плохой матчинг
not_match_corpus <- sapply(qual_corpus, function(x) ifelse(is.null(x), TRUE,x !="30"))

# Без корпуса и литера
gg <- creator_query(gg,nominatim_start,qury_param,liter = FALSE,corpus = FALSE, which(not_match_corpus == T))

system.time(update_list <- lapply(gg$nomit_query[which(not_match_corpus == T)], function(x) nomunatim_geocode(x)))

test_list[which(not_match_corpus == T)] <- update_list
requir_json <- lapply(test_list, function(x) unlist(content(x, as = "parsed",type = "application/json", encoding = "UTF-8"), recursive = FALSE))

qual_corpus_liter <- sapply(requir_json, function(x) unlist(x$place_rank))
# Точность повысилась до 91,9%
length(qual_corpus_liter[qual_corpus_liter == "30"])/length(qual_corpus_liter)

# Посмотрим адреса которые дали плохой матчинг
not_match_corpus_liter <- sapply(qual_corpus_liter, function(x) ifelse(is.null(x), TRUE,x !="30"))

# Без корпуса, с литером, но улицы поменяем
replace_street_corpus <- alternative_street_query(gg, which(not_match_corpus_liter == T))
gg <- creator_query(gg,nominatim_start,qury_param,liter = TRUE,corpus = FALSE, which(not_match_corpus_liter == T), replace_street_corpus )

system.time(update_list <- lapply(gg$nomit_query[which(not_match_corpus_liter == T)], function(x) nomunatim_geocode(x)))

test_list[which(not_match_corpus_liter == T)] <- update_list
requir_json <- lapply(test_list, function(x) unlist(content(x, as = "parsed",type = "application/json", encoding = "UTF-8"), recursive = FALSE))

qual_corpus_street <- sapply(requir_json, function(x) unlist(x$place_rank))
# Точность повысилась не поменялась
length(qual_corpus_street[qual_corpus_street == "30"])/length(qual_corpus_street)

# Посмотрим адреса которые дали плохой матчинг
not_match_corpus_street <- sapply(qual_corpus_street, function(x) ifelse(is.null(x), TRUE,x !="30"))

# Без корупса, лицы поменяли и без литера

replace_street_corpus_liter <- alternative_street_query(gg, which(not_match_corpus_street == T))
gg <- creator_query(gg,nominatim_start,qury_param,liter = FALSE,corpus = FALSE, which(not_match_corpus_street == T), replace_street_corpus_liter )

system.time(update_list <- lapply(gg$nomit_query[which(not_match_corpus_street == T)], function(x) nomunatim_geocode(x)))

test_list[which(not_match_corpus_street == T)] <- update_list
requir_json <- lapply(test_list, function(x) unlist(content(x, as = "parsed",type = "application/json", encoding = "UTF-8"), recursive = FALSE))

qual_corpus_street_liter <- sapply(requir_json, function(x) unlist(x$place_rank))
# Точность повысилась не поменялась почти 92%
length(qual_corpus_street_liter[qual_corpus_street_liter == "30"])/length(qual_corpus_street_liter)

# Посмотрим адреса которые дали плохой матчинг
not_match_corpus_street_liter <- sapply(qual_corpus_street, function(x) ifelse(is.null(x), TRUE,x !="30"))
gg$adres_clear[not_match_corpus_street]

missing_street_name <- gg$street_name[not_match_corpus_street]
test <- as.data.frame(missing_street_name)

# Обновляем уже сами улицы чтоб улучшить матчинг, 
# все эти изменения будут делать на уровне считывая данных
# тут добавил чтоб на запускать матчинг опять

# пропуски в улицах с номерами
for( i in some_miswriting_dictionary){
  line_position <- which(str_detect(gg$street_name, i[1]) == T)
  gg$street_name[line_position] <- str_replace(gg$street_name[line_position],i[1],i[2])
}
# улицы без идентификатора
for( i in street_wh_iden_dictionary){
  line_position <- which(str_detect(gg$street_name, i[1]) == T)
  gg$street_name[line_position] <- i[[2]]
  # временное решение чтобы тестить на матчинг 
  # gg$street_identifer[line_position] <- "улица"
}

# линии всякие 
for( i in line_dictionary){
  line_pattern <- paste0("(",paste0(i[[1]], collapse  = "|"),")")
  line_position <- which(str_detect(gg$street_name, line_pattern) == T)
  gg$street_name[line_position] <- i[[2]]
}

# буква Ё
for(i in e_yo_dictionary){
  prep_osition <- which(str_detect(gg$street_name, i[1]) == T)
  gg$street_name[prep_osition] <- str_replace(gg$street_name[prep_osition], i[1], i[2])
}

# Поменяли сами улицы деламем матчинги
gg <- creator_query(gg,nominatim_start,qury_param, liter = TRUE,corpus = TRUE, which(not_match_corpus_street_liter == T))

system.time(update_list <- lapply(gg$nomit_query[which(not_match_corpus_street_liter == T)], function(x) nomunatim_geocode(x)))

test_list[which(not_match_corpus_street_liter == T)] <- update_list
requir_json <- lapply(test_list, function(x) unlist(content(x, as = "parsed",type = "application/json", encoding = "UTF-8"), recursive = FALSE))

qual_modify_street_name <- sapply(requir_json, function(x) unlist(x$place_rank))
# Точность повысилась до 94%
length(qual_modify_street_name[qual_modify_street_name == "30"])/length(qual_modify_street_name)

# Посмотрим адреса которые дали плохой матчинг
not_match_modify_street_name <- sapply(qual_modify_street_name, function(x) ifelse(is.null(x), TRUE,x !="30"))




manual_work <- gg$adres_clear[not_match_modify_street_name]
manual_df <- data.frame(x = which(not_match_modify_street_name == T), y = manual_work) 
# далее по видимому вручную надо
# деаем лист которые сожержит 
# инфу(позиция, номер дома, литер, корпус,улица,пригород,идентификатор)
namual_work[1]
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

