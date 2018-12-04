# Загрузка библиотек
library(tabulizer)
library(dplyr)
library(stringr)
library(urltools)
# Считываем таблицу
ggt <- readRDS("./Election_analysis/uic_td.rds")
nominatim_start <- "https://nominatim.openstreetmap.org/search?q="
query <- "q="

house_num_liter <- paste0(ggt$house_number, ifelse(is.na(ggt$house_liter), "", ggt$house_liter))

house_corpus <- ifelse(is.na(ggt$house_corpus),"", paste0("к", ggt$house_corpus))
house_number <- str_remove(paste(house_num_liter,house_corpus), "[ ]$")

street_name <- ifelse(is.na(ggt$street_name), "", ggt$street_name)
suburb_name <- ifelse(is.na(ggt$suburb_name), "", ggt$suburb_name)
city_name <- "Санкт-Петербург"

adress <- str_remove_all(paste(house_number, street_name, suburb_name, city_name, sep = ", "), " ,")

ggt$adres_clear <- adress
house_corpus<- ifelse(is.na(ggt$house_corpus[89]),"", paste0(url_encode(enc2utf8("к")), ggt$house_corpus[89]))
paste
# Всё работает если адрес есть, если его нет, то печаль
# Ограничимся только одним лучшим резульататом
qury_param <- "&format=jsonv2&addressdetails=1&polygon=0&limit=1"
query <- paste0(nominatim_start,"q=", url_encode(enc2utf8(adress)),qury_param)
ggt$nomit_query <- query
ggt$nomit_query[3]

# нужно сдлеать функцию, которая бы создавала запрос
# Можно выбрать какие строчки будут давать запрос
# а так же можно менять написание улиц
# Прорабоать случай когда длины index_vector, street_preposition могут оказаться различными
# или сделать их парными в одном листе
creator_query <- function(input_df,maiun_url, parameters ,index_vector, street_preposition){
  if(missing(index_vector)){
    # Если упущен вектор позиций, значит работает со всеми и запуск впервыи
    input_df$adres_clear <- NA
    input_df$nomit_query <- NA
    index_vector <- 1:nrow(input_df)
  }
  print(length(index_vector))
  if(missing(street_preposition)){
    street_preposition <- input_df$street_name[index_vector]
  }
  print(length(street_preposition))
  
  house_num_liter <- paste0(input_df$house_number[index_vector], ifelse(is.na(input_df$house_liter[index_vector]), "", input_df$house_liter[index_vector]))
  house_corpus <- ifelse(is.na(input_df$house_corpus[index_vector]),"", paste0("к", input_df$house_corpus[index_vector]))
  house_number <- str_remove(paste(house_num_liter,house_corpus), "[ ]$")
  street_name <- ifelse(is.na(street_preposition), "", street_preposition)
  suburb_name <- ifelse(is.na(input_df$suburb_name[index_vector]), "", input_df$suburb_name[index_vector])
  city_name <- "Санкт-Петербург"
  
  adres <- str_remove_all(paste(house_number, street_name, suburb_name, city_name, sep = ", "), " ,")
  print(length(adres))
  input_df$adres_clear[index_vector] <- adres
  # nominatim_start <- "https://nominatim.openstreetmap.org/search?q="
  # qury_param <- "&format=jsonv2&addressdetails=1&polygon=0&limit=1"
  query <- paste0(maiun_url, url_encode(enc2utf8(adres)),parameters)
  input_df$nomit_query[index_vector] <- query
  return(input_df)
}
gg <- ggt[,c(1:10)]
gg <- creator_query(gg,nominatim_start,qury_param)

install.packages("XML")
library(XML)
test <- xmlParse(ggt$nomit_query[3])
test <- xmlTreeParse(ggt$nomit_query[3])
# Нужно сначала отправить запрос
install.packages("httr")
library(httr)
test <- httr::GET(ggt$nomit_query[3])
str(test)
test
headers(test)
# Проверка статуса запроса
http_status(test)
install.packages("xml2")
library(xml2)
# json нормальный формат, он считывается понятнее и парсится
info_test <- content(test, as = "parsed",type = "application/json", encoding = "UTF-8")
str(info_test)
info_test
print(content(test, as = "parsed", type = "text/xml", encoding = "UTF-8"))

sapply(info_test, function(x) x$status_code)
test$content
# Извлика с задержкой чтоб не забанили
# Первую сотню парсим
# Не верно timeout(1) обрывает соединение, придется писать свою функцию
test_list <- lapply(ggt$nomit_query, function(x) httr::GET(x, timeout(1)))
# Вроде как все считалось
sapply(test_list, function(x) x$status_code)

# test_result <- lapply(test_list, function(x) unlist(content(x, as = "parsed",type = "application/json", encoding = "UTF-8"), recursive = F))
# head(sapply(test_result, function(x) x$place_rank))

gg <- unlist(test_result[1], recursive = FALSE)
gg$boundingbox
str(test_result[1])
str(test_result[[1]])
str(gg$place_rank)

# Фунций которая дает задержу, а не то забанят
# Нужен стопер мало ли там все завислл на запросе одном
nomunatim_geocode <- function(request_vecror){
  curr_value <- httr::GET(request_vecror)
  Sys.sleep(1)
  return(curr_value)
}

# Получилось
system.time(test_list <- lapply(ggt$nomit_query, function(x) nomunatim_geocode(x)))
# Сохраняем парсинг
saveRDS(test_list, file = "./Election_analysis/parse_list.rds")

test_result <- lapply(test_list, function(x) unlist(content(x, as = "parsed",type = "application/json", encoding = "UTF-8"), recursive = FALSE))
# Проверяем ранк места, чтоб понять дом это ли это (30) или нечто большее
# NULL гововрит что не удалось найти
qual <- sapply(test_result, function(x) unlist(x$place_rank),USE.NAMES = FALSE)
qual[848]
qual[615]
# качество около 63 процентов
length(qual[qual == "30"])/length(qual)
head(is.null(qual))
# Qual лист чтоб сохранять NULL
null_qual <- sapply(qual, function(x) is.null(x))
# Позиция где NULL для них возможно переделать запрос
which(null_qual == T)


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

replace_street <- alternative_street_query(gg, which(null_qual == T))
# Запросы к nominatim изменились
gg <- creator_query(gg,nominatim_start,qury_param, which(null_qual == T), replace_street )

gg$street_name[c(1,2,3,848,613)]
gg$street_identifer

# но нужно помнить что ещё есть литеры

test <- c("fgh tyu", "tyu fgh")
str_remove_all(test, "fgh")

paste0("(",c("fgh", "fgh"),"|",str_remove_all(test, "fgh"),")(",str_remove_all(test, "fgh"),"|",c("fgh", "fgh"),")")
paste0("(",c("fgh", "fgh"),"|",str_remove_all(test, "fgh"),")(",str_remove_all(test, "fgh"),"|",c("fgh", "fgh"),")")

str_replace(test, c("fgh", "tyu"), "замена")

str_remove_all(str_replace_all(test, "(fgh|[ ]?tyu[ ]?)([ ]?tyu[ ]?|fgh)", "\\2 \\1"), "^[ ]|[ ]$")
install.packages("textclean")
library(textclean)
swap("fgh tyu", "fgh", "tyu")
head(which(null_qual == T))
