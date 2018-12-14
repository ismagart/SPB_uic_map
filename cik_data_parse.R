library(stringr)
library(httr)
library(rvest)
library(dplyr)
library(xml2)
library(rlang)
# необходимо сгрузить данные по выборам с сайта цик

# важно помнить, что некоторые УИК является временными, а значит в uic_table их нет
# Данные только для питера, запросы только для питера по выборам президент и только

# Для удобства начинаем сразу со сводных таблиц на уровне города
# там информация по ТИКА, но что важнее ссылки на сводные таблицы от каждого тик

# Selector gadget показал, что на сайте нет css и все надо через xpath вытаскивать
spb_election_url <- "http://www.st-petersburg.vybory.izbirkom.ru/region/region/st-petersburg?action=show&root=1000075&tvd=100100084849201&vrn=100100084849062&region=78&global=1&sub_region=78&prver=0&pronetvd=null&vibid=100100084849201&type=227"

web <- GET(spb_election_url)

# table <- html_nodes(content(web), "table")

pre_table <- html_nodes(content(web), "table")[3]

test_table <- html_nodes(pre_table, "table")
# суммированя таблица по всем тикам
summary_pre_table <- test_table[7]
# ВАЖНО, что модификация summary_pre_table влияет на оригинал
# не ясно пока как изолировать их. вся проблема с особенностях xml как такового и семантики R
# XML2 магия, убираем тег br и заменяем его 
# ищем все теги br (на выходе получаем вектор)
summary_pre_table %>%  
  xml_find_all(".//br") %>% 
  # добавляем новый тег <p> со значением \n
  xml_add_sibling("p", "\n")

summary_pre_table %>%  
  xml_find_all(".//br") %>% 
  xml_remove()

# Значения абсолютные и процентные разделены пробелом
summary_info <- html_table(summary_pre_table, fill = TRUE, trim = TRUE)
View(summary_info)

# сами резульаты по тикам
tic_pre_table <- test_table[8]

tic_pre_table %>%  
  xml_find_all(".//br") %>% 
  # добавляем новый тег <p> со значением \n
  xml_add_sibling("p", "\n")

tic_pre_table %>%  
  xml_find_all(".//br") %>% 
  xml_remove()

tic_result <- html_table(tic_pre_table, fill = T)
View(tic_result)
# intermediate_obj <- test_table[7]
# summary_pre_table <- intermediate_obj
tracemem(test_table[7])

# всё равно меняется
summary_pre_table <- rlang::duplicate(test_table[7], shallow = FALSE)
tracemem(summary_pre_table)

# Показывает разные ячеки памяти, но
mody_summary_pre_table <- test_table[7]
tracemem(mody_summary_pre_table)
str(mody_summary_pre_table)
mody_summary_pre_table[[1]]$mod <- "0"
mody_summary_pre_table[[1]]$mod <- NULL
xml_name(mody_summary_pre_table[1])
tracemem(mody_summary_pre_table[1])

xml_name(mody_summary_pre_table)
xml_set_name(mody_summary_pre_table, "new table")


gg <- mody_summary_pre_table[[1]]
tracemem(gg)
xml_set_name(gg, "new table")
gg

xml_name(test_table[7])

# через лист
gg <- pairlist(test_table[7])
gg[[1]]
ggt <- list(test_table[7])
xml_name(gg[[1]])
xml_name(test_table[7])

xml2::xml_name(gg[[1]]) <- "new table"

# Попробуем поменять имя
# всё равно меняются другие файлы 
xml2::xml_name(summary_pre_table) <- "new table"
xml_name(summary_pre_table)

xml_name(test_table[7])
xml_name(test_table[7]) <- "table"
xml_name(test_table[7])
xml_name(summary_pre_table)



# через функцию
new_name_xml <- function(input_xml, new_name){
  xml_set_name(input_xml, new_name)
  return(input_xml)
}

summary_pre_table <- new_name_xml(summary_pre_table, "new table")
xml_name(summary_pre_table)
xml_name(test_table[7])

x <- read_xml("<table><b/></table>")
x
xml_name(x)


xml_name(test_table[7])
xml_name(summary_pre_table)  



xml_name(test_table[7])
xml_name(summary_pre_table)

# возвращаем имя назад
xml_name(summary_pre_table) <- "table"

xml_name(summary_pre_table)
xml_name(test_table[7])
 

# Проверяем изменился ли первоначальный файл
 # и да он меняется
 write_xml(test_table[7], "./Election_analysis/summary_dirty_table_after.xml")
 
# удаляем этот тег br
# хорошо бы посмотреть на tree view теперь

write_xml(summary_pre_table, "./Election_analysis/summary_pre_table.xml")


gg <- xml_find_all(summary_pre_table, ".//br")
gg <- xml_add_sibling(gg, "p", "\n")
gg <- xml_find_all(summary_pre_table, ".//br")
gg <- xml_remove(gg)
ggt <- html_table(gg)

gg[[1]]
# Почему-то действия пайп-лайна распространяются и на другие элементы

# clean_sum_table <- xml_replace(summary_pre_table, ".//br", "\n")

clean_td <- html_table(summary_pre_table, fill = TRUE, trim = TRUE)

View(clean_td)
x <- read_xml("<parent><child>1</child><child>2<child>3</child></child></parent>")
children <- xml_children(x)
t1 <- children[[1]]
t2 <- children[[2]]
t3 <- xml_children(children[[2]])[[1]]
x
# здесь функции именно меняют аргументы, но как они узнают окружение??
xml_add_sibling(t1, t3)
xx
x
# если вызвать ещё
xml_add_sibling(t1, t3)
x
xml_add_sibling(t3, t1, where = "before")
x
# Проблема, на сайте каждая ячейка имеет два числа абсолётное и процент
# при парсинге они слипаются всё из-за тега br`


# внутри есть куча вложенных таблиц
# result_table<-html_table(table[[9]],fill=TRUE)



link_to_tic <- html_nodes(content(web), "a")
link_to_tic[1]
# с 9 значения идут ссылки на сводные таблицы по ТИК
link_to_tic[[9]]
link_to_tic[[38]]

# Все ссылки на вложенные таблицы делаются вручную, как автоматизировать чтоб
# сделать api е ясно. 
# Надежда что порядок "вложенности" сохраняется

x <- read_xml("<a><b/></a>")
y <- read_xml("<a><b/></a>")
z <- duplicate(x, shallow = FALSE)

tracemem(x)
tracemem(z)
str(z)
z$mod <- 0
tracemem(z)
x
z


xml_set_name(x, "new1")
x
y
z

x1 <- c(1, 2, 3)
x2 <- x1
tracemem(x1)
tracemem(x2)

xml_set_name(x, "new3")
# делаем новое окружение
e1 <- new.env()
e1$x <- read_xml("<a><b/></a>")
e1[["x"]]
e2 <- new.env()
e2$z <- duplicate(e1[["x"]])
e2$z

xml_set_name(e1$x, "new1")
