library(stringr)
library(httr)
library(rvest)
library(dplyr)
library(xml2)
library(rlang)
library(tidyr)
# необходимо сгрузить данные по выборам с сайта цик

# важно помнить, что некоторые УИК является временными, а значит в uic_table их нет
# Данные только для питера, запросы только для питера по выборам президент и только

# Для удобства начинаем сразу со сводных таблиц на уровне города
# там информация по ТИКА, но что важнее ссылки на сводные таблицы от каждого тик

# Selector gadget показал, что на сайте нет css и все надо через xpath вытаскивать
spb_election_url <- "http://www.st-petersburg.vybory.izbirkom.ru/region/region/st-petersburg?action=show&root=1000075&tvd=100100084849201&vrn=100100084849062&region=78&global=1&sub_region=78&prver=0&pronetvd=null&vibid=100100084849201&type=227"

# Функция по модификации таблиц, так как на сайте результаты в абсолютных значениях
# и процентах находятся в одной ячеке и разедели тегом br, то надо модицифировать
# ибо html_table сливает их вместе
ht_table_modifer <- function(input_table){
  input_table %>%  
    xml2::xml_find_all(".//br") %>% 
    # добавляем новый тег <p> со значением \n
    xml2::xml_add_sibling("p", "\n")
  
  input_table %>%  
    xml2::xml_find_all(".//br") %>% 
    xml2::xml_remove()
  return(input_table)
}

# Основная функция, по ссылке на страницу с таблицей выстаскивает её приводится к
# длинному виду и приводит столбцы к нужному формату
# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# Имеет смысл переписать её чтоб внутри работать с матрицами ибо это будет быстрее
parse_tic_data <- function(link_to_tic_data, percent_value = NULL){
  web_page <- httr::GET(link_to_tic_data)
  table_xpath <- "//table[descendant::table[@align]]"
  extract_content <- rvest::html_nodes(content(web_page), xpath = table_xpath)
  # берем именно второй, так как в первом содержится таблица всей страницы
  pre_table <- rvest::html_nodes(extract_content[2], "table")
  # необходимо модифицировать их, чтобы разделить абсолютные и проценты
  mod_table <- lapply(pre_table, function(x) html_table(ht_table_modifer(x), fill = TRUE))
  
  # собираем один дата фрейм попутно убираем первую колонку
  result_table <- cbind(mod_table[[1]][,-1], mod_table[[2]])
  # делаем нормальные имена
  # необходимо убрать символ № так как с ним все плохо, кодируется как \271
  # нужно унифицировать чтоб годилось как для ТИК так и для УИК
  # или оставить как есть, ибо если расширять её на россию, то там есть и просто название городов
  # заменить только пробелы на нижнее подчеркивание
  res_column_name <- stringr::str_remove(result_table[1,], "№") %>% 
    stringr::str_replace_all(.,"[ ]", "_")
  # знаем, что первый две колонки содержат информацию о бюллетенях
  # колонка sum в абсолютных значениях описвыает сумму голосов за кандидата на этом территориальном избирательном участке
  res_column_name[1:2] <- c("title", "sum")
  names(result_table) <- res_column_name
  result_table <- result_table[-1,] %>% 
    # убираем пустую строчку между блоком кандидатов и бюллетенями
    na.omit(.)
  # имеет смысл разбить таблицу на две 
  # 1) вся инфа о бюллетенях
  # 2) именно кандидаты + можно тут выбрать абсолютные значения или проценты
  # Разделям за счет того, что во всех строчках про бюллетени есть фраза число
  indicate <- stringr::str_detect(result_table$title, "^Число")
  election_result <- split(result_table, f = indicate )
  names(election_result) <- c("candidate", "bulletin_info")
  # приводим к длинному виду
  # цикл по индексам в листе данным
  lapply(1:2, function(x){
    # так как мы знаем какие два столбца мы сами создали, то передаем их имена явно черех -
    election_result[[x]] <<- gather(election_result[[x]], key = IC, value = result, -sum, -title, factor_key = TRUE)
  })
  # приводим к числовому формату, заголовок title оставляем как character
  election_result$bulletin_info <- dplyr::mutate_at(election_result$bulletin_info, funs(as.numeric(.)), .vars = c("result", "sum"))
  # приводим к формату данные и разделяем проценты
  if(is.null(percent_value) == FALSE ){
    maneuver_data <- election_result$candidate
    election_result$candidate <- NULL
    # огромная простыня
    election_result$candidate$absolute <- dplyr::mutate_at(maneuver_data, 
                                                           funs(as.numeric(stringr::str_remove_all(.,"[\\n][[:digit:][:punct:]]*%"))), .vars = c("result", "sum")) %>% 
      mutate(., title = as.factor(title))
    # через группировки и str_replace е получается, по-видимому вложенные группы не распознаются
    election_result$candidate$percentage <- mutate_at(maneuver_data,
                                                      funs(as.numeric(stringr::str_remove_all(stringr::str_extract(.,"[\\n][[:digit:][:punct:]]*%"), "[^[:digit:].,]"))), .vars = c("result", "sum")) %>%
      mutate(., title = as.factor(title))
  }else{
    election_result$candidate <- dplyr::mutate_at(election_result$candidate, 
                                                  funs(as.numeric(stringr::str_remove_all(.,"[\\n][[:digit:][:punct:]]*%"))), .vars = c("result", "sum")) %>% 
      mutate(., title = as.factor(title))
  }
  # возвращаем лист, внутри которого дата-фреймы
  return(election_result)
}

# %%%%%%%%%%%Вытаскиваем с со сводной таблицы по спб ссылки на ТИК %%%%%%%%%%%
# попробуем прописать через Xpath нужные там ссылки чтобы не опираться только на цифры
# есть проблемы с русским языком всё дело в кодировках, пора все же уходить на UNIX
# rvest поддерживает только xpath 1.0 
xpath_query <- "//a[contains(text(),'Территориальная')]"
# Вручную задаём кодировка для запроса.
# перед этим проверили какая кодировка у сайта

# нужно считать сводную таблицу по спб
web <- GET(spb_election_url)
link_to_tic_path <- html_nodes(content(web), xpath = enc2utf8(xpath_query))
# получаем вектор ссылок
# Эти ссылки будут работать только для СПб, так как ищем по названиям ТИК-а
link_to_tic <- html_attr(link_to_tic_path, "href")
# Так же берем имена, чтобы лист был именован
link_name <- html_text(link_to_tic_path)
# Так как работает с питером, то имеет смысл просто называть TIC 
link_name <- stringr::str_remove(link_name, "\271") %>% 
  stringr::str_extract(.,"[[:digit:]]+") %>% 
  paste0("TIC_", .)


spb_election_list <- list()
# Возможно имеет смысл сдеть задержку, мало ли могут банить
spb_election_list <- lapply(1:length(link_name), function(x){
  parse_tic_data(link_to_tic[x])
} )
names(spb_election_list) <- link_name

# Так жe можно скачать данные по Тика
spb_general_election <- parse_tic_data(spb_election_url)


# Удобнее работать с уже сведенными данными, для этого
# объединяем все Тик результаты по кандидатам в один дата фрей
# Будет работать пока нет процентов
temp_table <- lapply(spb_election_list, function(x) x$candidate)
library(plyr)
spb_common_election <- plyr::ldply(temp_table, data.frame) %>% 
  mutate(.id = as.factor(.id)) %>% 
# приходится явно писать пакет ибо есть конфликты с plyr
  dplyr::rename(ter_ic = .id)

View(spb_common_election)
str(spb_common_election)
# Сохраняем для локального использования
saveRDS(spb_common_election, "spb_president_election.RDS")
# сохраняем в csv для выгрузки
write.csv(spb_common_election, "spb_president_election.csv")
# test_common_dt <- unlist(test, use.names = TRUE, recursive = FALSE)
 
# для удобства имеет смысл все данные по УИК объеденить в один дата фрейм,
# где будет графа принадлежнести к определенному тику.
# однако так же будет разграничение на бюлетени и данные с места

# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# О возможности сделать своего рода api для выгрузки данных с сайта цик
# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
library(httr)
library(rvest)
# url_cik <- "http://www.vybory.izbirkom.ru/region/izbirkom"
url_cik <- "http://www.izbirkom.ru/region/izbirkom"
list_qury <- list(start_date = "01.01.2019",
                  urovproved = "all",
                  vidvibref = "all",
                  vibtype = "all",
                  end_date = "30.09.2019",
                  actual_regions_subjcode = "5",
                  old_regions_subjcode = "41^^59^^75^^80^^81^^82^^84^^88",
                  sxemavib = "all",
                  action = "search_by_calendar",
                  region = "0",
                  ok = enc2utf8("Искать"))
# actual_regions_subjcode = "0^^1^^2^^3^^4^^5^^6^^7^^8^^9^^10^^11^^93^^12^^13^^14^^15^^16^^17^^18^^19^^20^^21^^22^^92^^91^^23^^24^^90^^25^^26^^27^^28^^29^^30^^31^^32^^33^^34^^35^^36^^37^^38^^39^^40^^42^^43^^44^^45^^46^^47^^48^^49^^50^^51^^52^^53^^54^^55^^56^^57^^58^^60^^61^^62^^63^^64^^65^^66^^67^^68^^69^^70^^71^^72^^73^^74^^76^^77^^78^^94^^79^^83^^85^^86^^87^^89"
# old_regions_subjcode = "41^^59^^75^^80^^81^^82^^84^^88"
# request <- "start_date=18.03.2018&urovproved=all&vidvibref=all&vibtype=all&end_date=18.03.2018&actual_regions_subjcode=0^^1^^2^^3^^4^^5^^6^^7^^8^^9^^10^^11^^93^^12^^13^^14^^15^^16^^17^^18^^19^^20^^21^^22^^92^^91^^23^^24^^90^^25^^26^^27^^28^^29^^30^^31^^32^^33^^34^^35^^36^^37^^38^^39^^40^^42^^43^^44^^45^^46^^47^^48^^49^^50^^51^^52^^53^^54^^55^^56^^57^^58^^60^^61^^62^^63^^64^^65^^66^^67^^68^^69^^70^^71^^72^^73^^74^^76^^77^^78^^94^^79^^83^^85^^86^^87^^89&old_regions_subjcode=41^^59^^75^^80^^81^^82^^84^^88&sxemavib=all&action=search_by_calendar&region=0&ok=Искать"
# Всё заработало, надо лишь правильно encode выставить
test_post <- httr::POST(url_cik, body = list_qury, encode = "form")
test <- content(test_post, as = "text")

write(test, "cik_post.txt")
str(test)
gg <- content(test_post, "parsed")
test_table <- html_nodes(gg, "table")
test_table[[8]]

