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
nominatim_start <- "https://nominatim.openstreetmap.org/search?q="
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
  if( liter == FALSE){
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
geocode_list <- readRDS("./Election_analysis/parse_list.rds")
# Специальано сохранили как parse_list_first_run.rds чтоб можно было откатиться
# Парсируем результаты, указываем формат json ведь мы его и просили от nominatim
geocode_result <- lapply(geocode_list, function(x) unlist(content(x, as = "parsed",type = "application/json", encoding = "UTF-8"), recursive = FALSE))


# Создаем отдельно Лист, в который записывает покатель ранк места
# по нему будем проверять качество
#  дом это place_rank (30), NULL будет значит что не удалось найти
qual <- sapply(geocode_result, function(x) unlist(x$place_rank),USE.NAMES = FALSE)

# качество  70 процентов
length(qual[qual == "30"])/length(qual)

# Определям позиции которые не нашлись правильно, для них и только для них потом
# будем переделывать запрос
not_match_qual <- sapply(qual, function(x) ifelse(is.null(x), TRUE,x !="30"))
# не заматчилось около 550 адресов
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

# создаем новый вектор для графы street_name
replace_street <- alternative_street_query(ggt, which(not_match_qual == T))
# Запросы к nominatim изменились (нужно явно указывать аргументы)
ggt <- creator_query(ggt,nominatim_start,qury_param, index_vector = which(not_match_qual == T), street_preposition = replace_street )
# Создаем временный лист, в который записываем запрос
system.time(update_list <- lapply(ggt$nomit_query[which(not_match_qual == T)], function(x) nomunatim_geocode(x)))

# Обновляем наш лист запросов
geocode_list[which(not_match_qual == T)] <- update_list
# Обновляем лист json
geocode_result[which(not_match_qual == T)] <- lapply(update_list, function(x) unlist(content(x, as = "parsed",type = "application/json", encoding = "UTF-8"), recursive = FALSE))
# Новый фактор качества
qual_swap_street <- sapply(geocode_result, function(x) unlist(x$place_rank))
# Качество повысилось до 82 %
length(qual_swap_street[qual_swap_street == "30"])/length(qual_swap_street)
# Смотрим что не заматчило и вектор из FALSE чтоб потом сравнить с начальной 
# и выделить то, что не помогло
not_match_qual_swap_street <- sapply(qual_swap_street, function(x) ifelse(is.null(x), TRUE,x !="30"))
# незаматчилось 327
length(which(not_match_qual_swap_street == T))
# Так как изначально меняли только то что не определилось, то 
# Есть переход от F -> T, значит новые вектор мисматчей сразу же используем
# Для новых модификаций
# Вопрос о порядке их открыт

# Создаем функцию, которая будет убирать литеру в строке адреса
# Просто добавили параметр в функцию построения запроса, чтоб одна строчка менялась

# Делаем новый запрос, улицы будут из street_name, но литера уберутся
ggt <- creator_query(ggt,nominatim_start,qury_param,liter = FALSE, index_vector = which(not_match_qual_swap_street == T) )
system.time(update_list <- lapply(ggt$nomit_query[which(not_match_qual_swap_street == T)], function(x) nomunatim_geocode(x)))
# Обновляем наш лист запросов
geocode_list[which(not_match_qual_swap_street == T)] <- update_list
# Обновляем лист json
geocode_result[which(not_match_qual_swap_street == T)] <- lapply(update_list, function(x) unlist(content(x, as = "parsed",type = "application/json", encoding = "UTF-8"), recursive = FALSE))
# Новый фактор качества
qual_drop_liter <- sapply(geocode_result, function(x) unlist(x$place_rank))
# Качество стало 96 %
length(qual_drop_liter[qual_drop_liter == "30"])/length(qual_drop_liter)

# Совмещаем два подхода
not_match_qual_drop_liter <- sapply(qual_drop_liter, function(x) ifelse(is.null(x), TRUE,x !="30"))
length(which(not_match_qual_drop_liter == T))

# Снова делаем вектор новых имён улиц, который поменялись местами с идентификатором
replace_street_liter <- alternative_street_query(ggt, which(not_match_qual_drop_liter == T))
ggt <- creator_query(ggt,nominatim_start,qury_param,liter = FALSE, index_vector = which(not_match_qual_drop_liter == T), street_preposition = replace_street_liter )

system.time(update_list <- lapply(ggt$nomit_query[which(not_match_qual_drop_liter == T)], function(x) nomunatim_geocode(x)))

geocode_list[which(not_match_qual_drop_liter == T)] <- update_list

geocode_result[which(not_match_qual_drop_liter == T)] <- lapply(update_list, function(x) unlist(content(x, as = "parsed",type = "application/json", encoding = "UTF-8"), recursive = FALSE))

qual_swap_street_liter <- sapply(geocode_result, function(x) unlist(x$place_rank))
# Точность повысилась до 96%
length(qual_swap_street_liter[qual_swap_street_liter == "30"])/length(qual_swap_street_liter)

# Посмотрим адреса которые дали плохой матчинг
not_match_qual_street_liter <- sapply(qual_swap_street_liter, function(x) ifelse(is.null(x), TRUE,x !="30"))

# ggt$adres_clear[which(not_match_qual_street_liter == T)]

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
ggt <- creator_query(ggt,nominatim_start,qury_param,liter = TRUE,corpus = FALSE, index_vector = which(not_match_qual_street_liter == T))

system.time(update_list <- lapply(ggt$nomit_query[which(not_match_qual_street_liter == T)], function(x) nomunatim_geocode(x)))

geocode_list[which(not_match_qual_street_liter == T)] <- update_list
geocode_result[which(not_match_qual_street_liter == T)] <- lapply(update_list, function(x) unlist(content(x, as = "parsed",type = "application/json", encoding = "UTF-8"), recursive = FALSE))

qual_corpus <- sapply(geocode_result, function(x) unlist(x$place_rank))
# Точность повысилась до 99,1%
length(qual_corpus[qual_corpus == "30"])/length(qual_corpus)

# Посмотрим адреса которые дали плохой матчинг
not_match_corpus <- sapply(qual_corpus, function(x) ifelse(is.null(x), TRUE,x !="30"))
ggt$adres_clear[which(not_match_corpus == T)]
# Без корпуса и литера
ggt <- creator_query(ggt,nominatim_start,qury_param,liter = FALSE,corpus = FALSE, index_vector = which(not_match_corpus == T))

system.time(update_list <- lapply(ggt$nomit_query[which(not_match_corpus == T)], function(x) nomunatim_geocode(x)))

geocode_list[which(not_match_corpus == T)] <- update_list
geocode_result[which(not_match_corpus == T)] <- lapply(update_list, function(x) unlist(content(x, as = "parsed",type = "application/json", encoding = "UTF-8"), recursive = FALSE))

qual_corpus_liter <- sapply(geocode_result, function(x) unlist(x$place_rank))
# Точность повысилась до 99,4%
length(qual_corpus_liter[qual_corpus_liter == "30"])/length(qual_corpus_liter)

# Посмотрим адреса которые дали плохой матчинг
not_match_corpus_liter <- sapply(qual_corpus_liter, function(x) ifelse(is.null(x), TRUE,x !="30"))
ggt$adres_clear[which(not_match_corpus_liter == T)]

# Без корпуса, с литером, но улицы поменяем
replace_street_corpus <- alternative_street_query(ggt, which(not_match_corpus_liter == T))
ggt <- creator_query(ggt,nominatim_start,qury_param,liter = TRUE,corpus = FALSE, index_vector = which(not_match_corpus_liter == T),street_preposition = replace_street_corpus )

system.time(update_list <- lapply(ggt$nomit_query[which(not_match_corpus_liter == T)], function(x) nomunatim_geocode(x)))

geocode_list[which(not_match_corpus_liter == T)] <- update_list
geocode_result[which(not_match_corpus_liter == T)] <- lapply(update_list, function(x) unlist(content(x, as = "parsed",type = "application/json", encoding = "UTF-8"), recursive = FALSE))

qual_corpus_street <- sapply(geocode_result, function(x) unlist(x$place_rank))
# Точность  не поменялась
length(qual_corpus_street[qual_corpus_street == "30"])/length(qual_corpus_street)

# Посмотрим адреса которые дали плохой матчинг
not_match_corpus_street <- sapply(qual_corpus_street, function(x) ifelse(is.null(x), TRUE,x !="30"))

# Без корупса, улицы поменяли и без литера

replace_street_corpus_liter <- alternative_street_query(ggt, which(not_match_corpus_street == T))
ggt <- creator_query(ggt,nominatim_start,qury_param,liter = FALSE,corpus = FALSE,index_vector = which(not_match_corpus_street == T), street_preposition = replace_street_corpus_liter )

system.time(update_list <- lapply(ggt$nomit_query[which(not_match_corpus_street == T)], function(x) nomunatim_geocode(x)))

geocode_list[which(not_match_corpus_street == T)] <- update_list
geocode_result[which(not_match_corpus_street == T)] <- lapply(update_list, function(x) unlist(content(x, as = "parsed",type = "application/json", encoding = "UTF-8"), recursive = FALSE))

qual_corpus_street_liter <- sapply(geocode_result, function(x) unlist(x$place_rank))
# Точность не поменялась 99%
length(qual_corpus_street_liter[qual_corpus_street_liter == "30"])/length(qual_corpus_street_liter)

# Посмотрим адреса которые дали плохой матчинг
not_match_corpus_street_liter <- sapply(qual_corpus_street, function(x) ifelse(is.null(x), TRUE,x !="30"))
ggt$adres_clear[not_match_corpus_street]
which(not_match_corpus_street == T)
# Сохраняем матчинг, важно что там 11 плохих
saveRDS(geocode_list, file = "./Election_analysis/geocode_list.rds")

# Не нашлось всего лишь 11 адресов, но с ними трудно ибо на самом ОСМ
# адресов нет. 
# Выход добавить вручную их геокоординаты
uic_geposition <- ggt[,c(1,11)]
# length(geocode_result)
# Отмечаем только NULL чтоб можно было заменить на NA, но
# есть не правильный матчинг, например дороги вместо дома => нужнопользоваться вектором not_match_corpus_street
latitude <-as.numeric(sapply(geocode_result, function(x) ifelse(is.null(x$lat), NA,x$lat)))
longitude <- as.numeric( sapply(geocode_result, function(x) ifelse(is.null(x$lon), NA,x$lon)))
# 
# latitude[1]

# uic_geposition$uic[not_match_corpus_street]
uic_geposition <- cbind(uic_geposition, latitude,longitude)
# Добавляем вручную данные о долготе и широте
manul_coord <- list(list(position = 221, lat = 60.101922, lon = 30.195838),
                     list(position = 638, lat = 59.785065, lon = 30.616211),
                     list(position = 848, lat = 59.669561, lon = 30.073197),
                     list(position = 953, lat = 60.120489, lon = 30.166874),
                     list(position = 965, lat = 60.090818, lon = 29.942852),
                     list(position = 1582, lat = 59.667574, lon = 30.258780),
                     list(position = 1598, lat = 59.761436, lon = 30.361374),
                     list(position = 1599, lat = 59.807431, lon = 30.383879),
                     list(position = 1601, lat = 59.759558, lon = 30.467869),
                     list(position = 1602, lat = 59.759558, lon = 30.467869),
                     list(position = 1818, lat = 59.936434, lon = 30.366614)
                     )
for(i in manul_coord){

  uic_geposition$latitude[i[[1]]] <- i[[2]]
  uic_geposition$longitude[i[[1]]] <- i[[3]]
}

# Попробуем построить карту
# ggplot теперь требует ключа для доступа к API google так что 
# карту быстренько сложнее создать)
# нужен аккаунт, а с 16 июля обязательно надо подключать отдельно платформу
# Google Maps Platform дают 200$ ежемесячно, и нужна карта
# install.packages("ggmap")
# library(ggmap)
# ставим dev версию
if(!requireNamespace("devtools")) install.packages("devtools")
devtools::install_github("dkahle/ggmap", ref = "tidyup", force = TRUE)
library(ggmap)
ggmap(get_googlemap())
install.packages("digest")


# новый api с ограми
register_google(key = "tAIzaSyBcgaHBBfO6JH7cKyzT1zsizNex2TUXYbY")

geocode("waco texas")
geocode("Saint-Petersburg")
# геокодирование только на английском
geocode("Санкт-Петербург")
# Но url запрос делает
# почему же тогда ОСМ, так как рисовать будем не на гугл карте,
# то гугл геокодирование нельзя
url_encode(enc2utf8("Санкт-Петербург"))
geocode("%d0%a1%d0%b0%d0%bd%d0%ba%d1%82-%d0%9f%d0%b5%d1%82%d0%b5%d1%80%d0%b1%d1%83%d1%80%d0%b3")

qmap(location = "Saint-Petersburg") 

spb_center = as.numeric(geocode("Saint-Petersburg"))

spbMap = ggmap(get_googlemap(center=spb_center, scale=4, zoom=10), extent="normal")

spbMap +
  geom_point(data = uic_geposition, aes(x = longitude, y = latitude ))

# Видно, что все точки около питера так как рассматриваемая область задается 
# разбросом точек

write.table(uic_geposition,"./Election_analysis/uic_geposition.csv", sep = ";", fileEncoding = "UTF-8")


str(uic_geposition)

geocode("United States")
get_map("Tokyo", source = "osm")
# нет интерактиности нужно аккуратно маситаб подбирать, делаем через леафлет
install.packages("leaflet")
library(leaflet)
m=leaflet(data = uic_geposition) %>% addTiles() %>% addMarkers(~longitude, ~latitude, popup = ~as.character(uic))
m
suburb_pattern <- paste0("^(", "колпин",")\\W?[^,\\d%(]+" )
suburb_pattern
coplpino <- str_extract(str_to_lower(ggt$adres),suburb_pattern )
any(is.na(coplpino) ==F )
colpino <- na.omit(coplpino)
