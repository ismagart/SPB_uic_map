install.packages("pdftools")
library(pdftools)

getwd()

text_uic_pdftools <- pdf_text("./Election_analysis/Adres_yic_on_marth_election/spisok_iu_adm_spb.pdf")
saveRDS(text_uic_pdftools, file = "text_uic_pdftools.rds")
print(text)
str(text_uic_pdftools)

text_uic_pdftools[1]

text2 <- strsplit(text, "\n")
str(text2)
text2[[1]][15]

text3 <- strsplit(text, "\r")
text3[[1]][17]

# New packages tabulizer
# понимает что есть шеть столбцов, уже удобнее
# число строк совпадает с числом "линий" в тексте
# но он убирает шапку, смотри пример с pdf_text

install.packages("tabulizer")
library(tabulizer)

out <- extract_tables("./Election_analysis/Adres_yic_on_marth_election_split/1_PDFsam_spisok_iu_adm_spb.pdf",
                      encoding = "UTF-8")
str(out)
str(out[[1]][,1])
out[[1]][,c(1,2,5)]

uic_table <- extract_tables("./Election_analysis/Adres_yic_on_marth_election/spisok_iu_adm_spb.pdf",
                            encoding = "UTF-8")


# после чтения не всегда точно определеятся число колонок
# иноге появляется дополнительная, смотри пример уик 150
str(uic_table)
length(uic_table)
dim(uic_table[[1]])
# создаем вектор, которы определяет 6 или не шеть было столбцов

is_six_column <- sapply(uic_table,function(x) {ifelse(dim(x)[2] !=6, FALSE, TRUE)})

uic_table[[29]]

# убираем не нужные столбцы
dim(uic_table[[1]])
uic_table[[1,2]]
clear_uic <- lapply(uic_table[is_six_column], function(x) x[,c(1,2,5)])
clear_uic <- lapply(uic_table, function(x) {ifelse(dim(x)[2] !=6, x[,c(1,2,6)], x[,c(1,2,5)])})
# ifelse не может обрабатывать так много ?? 
clear_uic_list <- lapply(uic_table, function(x) if(dim(x)[2] == 6) x[,c(1,2,5)] else x[,c(1,2,6)])
clear_uic_matrix <-  do.call(rbind,lapply(uic_table, function(x) if(dim(x)[2] == 6) x[,c(1,2,5)] else x[,c(1,2,6)]))
str(clear_uic)
# В некоторых листах адреса не разбились на строки верно, вместо этого 
# образуется длинная строка в которой все содержится
# в некоторых из-за разрыва страниц возможна комбинация этого
# единственно всегда вокруг них есть пустые строки, точно не проверял
str(clear_uic[1:2])
text <- do.call(rbind, lapply(clear_uic[1:2], function(x) x))
str(text)

clear_uic_list[1]
head(clear_uic_matrix[,c(1,3)], 100)
head(clear_uic, 50)

test <- sapply(clear_uic_matrix[,1], function(x) if(x != "") T else F, USE.NAMES = FALSE)
head(test)
str(head(test))
str(head(clear_uic_matrix[,1]))
length(clear_uic_matrix[1,1])
clear_uic_matrix[1,1]


# Загружаю предварительно сохраненные файлы
clear_uic_matrix <- readRDS(file = "./Election_analysis/clear_uic_matrix.rds")

# Через цикл ищу строки которые относятка к одному иук- у и сцепляю их
# Возможно слипание нескольких строк, так как между ними нет пустых строк
# так при чтении из pdf появилась инсерция "второго" столбца в 
# в третий смотри строки  270:277 clear_uic_matrix[270:277,1:3]
# он он же дублирует их дальше из-за чего возможно соответсвие в других
# столбцах сохранится
k <- 1
out_string <-  ""
 for( i  in 1:length(clear_uic_matrix[1:1000,1])){
  # print(c(i, "I"))
  if(grepl("ТИК", clear_uic_matrix[i,1])){
    g  <-  1
    if(is.na(clear_uic_matrix[i+g,1]) == TRUE){
      break
    }
    current_string <-  clear_uic_matrix[i,1]
    while(clear_uic_matrix[i+g,1] != "" & i+g <= length(clear_uic_matrix[1:1000,1])){
      # print(c(g, "G"))
      current_string <- paste(current_string, clear_uic_matrix[i+g,1], sep = "%%")
      g  <-  g + 1
      }
    out_string[k] <- paste(current_string, i,g, sep = "&&")
    k <- k+1
  }
 }

# Проблемы в текущее матрице значений
# 1.отсутсвую пробелы между строками ТИКов --> решение поиск строк где
# которые начинаеют с ТИК и разделять их так
# 2.Строка с обознаением тика разбивается на множество строчек 
# разделённых пустыми строками
# %%% Выход брать в один уик все от строчки ТИК... до следующего ТИК..
# %%% Объединять в одну строку и там уже разбираться


i <- 1
k <- 1
out_string_test <-  "&&"
G <- FALSE
while(i <= length(clear_uic_matrix[,1])){
  if(grepl("ТИК", clear_uic_matrix[i,1]) & G == FALSE){
    g <-   i
    G <- TRUE
    i <- i+1
    # print(c("G:", g))
  }
  if(grepl("ТИК", clear_uic_matrix[i,1]) & G == TRUE){
    current_string <- paste(c(clear_uic_matrix[g:(i-1),2], paste(c(i,g), collapse = "$$"), paste0(c(clear_uic_matrix[g:(i-1),3]), collapse = "&&")), collapse = "%%")
    out_string_test[length(out_string_test)+1] <- current_string
    # print(c("I:", i))
    g <- i
  }
  # Анализ последней записи
  if(i == length(clear_uic_matrix[,1])){
    current_string <- paste(c(clear_uic_matrix[g:(i-1),2], paste(c(i,g), collapse = "$$")), collapse = "%%")
    out_string_test[length(out_string_test)+1] <- current_string
  }
  i <- i +1
} 
  


i <= length(clear_uic_matrix[,1])

length(out_string_test)
rm(out_string_test)
out_string_test[length(out_string_test)+1] <- 1

out_string_test[1830:1841]

str(out_string_test)
# while( i  <= length(clear_uic_matrix[,1])){
#   # print(c(i, "I"))
#   if(grepl("ТИК", clear_uic_matrix[i,1])){
#     g  <-  1
#     if(i + g > length(clear_uic_matrix[,1])){
#       break
#     }
#     # if(is.na(clear_uic_matrix[i+g,1]) == TRUE){
#     #   break
#     # }
#     # current_string <-  clear_uic_matrix[i,1]
#     for (g in 1:1000){
#       if(i+g > length(clear_uic_matrix[,1])){
#         break()
#       }
#       if(grepl("ТИК", clear_uic_matrix[i+g,1])){
#         break()
#       }
#       
#     }
#     # while(!grepl("ТИК", clear_uic_matrix[i+g,1]) || (i+g < length(clear_uic_matrix[,1]))){
#     #   # print(c(g, "G"))
#     #   # current_string <- paste(current_string, clear_uic_matrix[i+g,1], sep = "%%")
#     #   g  <-  g + 1
#     # }
#     out_string[k] <- paste(c(clear_uic_matrix[i:(i+g-1),2], c(i,g)), collapse = "%%")
#     k <- k+1
#     i <- i + g
#   }else{
#     i <- i + 1
#   }
# }


g

# Тестируем по числу иуков, должно ббыть 2268
out_string[1839]

out_string[c(164, 171, 184, 233, 249, 297)]
out_string[1]
View(out_string)
test <- out_string
test1 <- sub('%%.*','',test)

all(grepl("[^0-9]", test1))
!grepl("[^0-9]", test1[1:10])

test1[grepl("[^0-9]", test1)] <- "358"

test2 <- as.integer(test1)
any(is.na(test2))
test2[is.na(test2)]

which(is.na(test2))
test[which(is.na(test2))]
test3 <- na.omit(test2)


test4 <- clear_uic_matrix[!grepl("[^0-9]", clear_uic_matrix[,2]),2]

test4 <- test4[test4 !=""]

test5 <- test4 <- as.integer(test4)

sum(test5)
sum(test3)

any(is.na(test))
# Число уиков нам известно, значит можно считать их сумму как контроль
# Не сходится
sum(1:2268)
sum(test)

paste(c(clear_uic_matrix[582:597,1],c(1,2)), collapse = "%%")

paste(c(clear_uic_matrix[622:(622+15),1], c(622,15)), sep = "&&", collapse = "")

str(clear_uic_matrix[582:597,1])

head(out_string,50)
test
g <- clear_uic_matrix[1:100,1]
g[51]
length(g[51])

g[51] == ""
length(g[51])
length(g[50])
g[51] == ""
g[49] == ""
dim(g[49])
clear_uic_matrix[1:50,1]
test <- clear_uic_matrix[92,1]
"ТИК" %in% test
grep("ТИК", "ТИКТИК", value =  T)
grepl("ТИК", "ТАКТАК")
test
g <- ''
g[2] <- "d"
clear_uic_matrix[270:277,1:3]

# Возможно структура считанной таблице более запутана

uic_table <- readRDS(file = "./Election_analysis/uic_table.rds")

# Всего два вида "таблиц" есть по 6 и по 7
dim_test <- sapply(uic_table, function(x) dim(x)[2] )
unique(dim_test)

# проанализирууем отдельно кадый из видов 
# Всё так же и есть

uic_six_col_matrix <- do.call(rbind,lapply(uic_table, function(x) if(dim(x)[2] == 6) x))
uic_seven_col_matrix <- do.call(rbind,lapply(uic_table, function(x) if(dim(x)[2] == 7) x))


# 

uic_table[10]

library(stringi)
remove("&&",list = out_string_test)
test_num <- as.numeric(stri_extract_first_regex(out_string_test[-(1:1)], "[0-9]+"))

all(grepl("[^0-9]", test_num))

any(grepl("[A-z]", test_num))

sum(as.numeric(test_num))

# Есть повторы
length(unique(as.numeric(test_num)))
print(as.numeric(test_num),width = getOption("max.print"))




head(out_string_test[-(1:1)])
# Номера уиков идут не подряд смотри пример про ТИК 16 -> ТИК 30
# В таблице разрыв, на сайте тоже.
# Верификация тогда на сайт, по числу элементов?? (их меньше около 1877)

# ТУт в итоге видны границы медлу уиками, но их должно быть меньше (29)
# Номера даже внутри одного ТИК не попордяку
which((c(as.numeric(test_num[-(1:1)]), as.numeric(length(test_num))) - as.numeric(test_num)) !=1)


# Считываю то что, с сайта ( важно!, что эти данные о новых уиках до 23 года)
uic_site_table <- read.delim("./Election_analysis/копия с сайта уиков.txt",
                             encoding = "UTF-8",
                             header = FALSE,
                             stringsAsFactors = FALSE)[[1]]
str(uic_site_table)
head(uic_site_table)


test <- as.numeric(str_extract(uic_site_table, "[0-9]+"))

test[test = 358]


# То чего нет в списке сайта
unique(test_num[! test_num %in% test])


# То чего нет в списке таблицы
unique(test[! test %in% test_num])


# Списки разные,  а значит по ним сверяться нельзя
grep("[0-9]+", x = uic_site_table[1], value = TRUE)
test_num[953:959]

# Поэтому остается только верить и разобраться с повторами
test_num[duplicated(test_num)] # сами дубли
test_num[duplicated(test_num, fromLast = TRUE)]

# Чтобы получить все позиции дублей нужно пройти с двух строн
# ибо функция всегда идет с одной
which(duplicated(test_num) | duplicated(test_num, fromLast = TRUE))

which(test_num[duplicated(test_num)] == test_num)
which(test_num == test_num[duplicated(test_num)])



str_extract(uic_site_table[1800], "[0-9]+")
(c(as.numeric(test_num[-(1:1)]), as.numeric(length(test_num))) - as.numeric(test_num)) !=1
# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# Делаем готовый код и пусть что будет, чистим от дублей.

uic_table <- readRDS(file = "./Election_analysis/uic_table.rds")

# Очищаем от ненужных столбцов, сцепляет два варианта в единый лист или матрицу
clear_uic_list <- lapply(uic_table, function(x) if(dim(x)[2] == 6) x[,c(1,2,5)] else x[,c(1,2,6)])
clear_uic_matrix <-  do.call(rbind,lapply(uic_table, function(x) if(dim(x)[2] == 6) x[,c(1,2,5)] else x[,c(1,2,6)]))


clear_uic_matrix[270:277,1:3]

# Поиск по тикам, считаем сколько сток отнсится  к нему и сжимаем в строчку

i <- 1
k <- 1
out_string_test <-  "&&"
G <- FALSE
while(i <= length(clear_uic_matrix[,1])){
  if(grepl("ТИК", clear_uic_matrix[i,1]) & G == FALSE){
    g <-   i
    G <- TRUE
    i <- i+1
    # print(c("G:", g))
  }
  if(grepl("ТИК", clear_uic_matrix[i,1]) & G == TRUE){
    current_string <- paste(c(clear_uic_matrix[g:(i-1),2], paste(c(i,g), collapse = "$$"), paste(c(clear_uic_matrix[g:(i-1),3][nzchar(clear_uic_matrix[g:(i-1),3])], "@"), collapse = "&&")), collapse = "%%")
    out_string_test[length(out_string_test)+1] <- current_string
    # print(c("I:", i))
    # nzchar(clear_uic_matrix[523:528,3])
    g <- i
  }
  # Анализ последней записи
  if(i == length(clear_uic_matrix[,1])){
    # current_string <- paste(c(clear_uic_matrix[g:(i-1),2], paste(c(i,g), collapse = "$$")), collapse = "%%")
    current_string <- paste(c(clear_uic_matrix[g:(i-1),2], paste(c(i,g), collapse = "$$"), paste(c(clear_uic_matrix[g:(i-1),3], ""), collapse = "&&")), collapse = "%%")
    out_string_test[length(out_string_test)+1] <- current_string
  }
  i <- i +1
}

# Делим удалем. оставляем инфу об номере уик (как char), строках, первом адресе
single_row <-  out_string_test[2]
library(stringr)

pater <- "(^[:digit:]).*(%%[:digit:]+$+[:digit:]+).*(%[^(%|&)]+&)"
str_subset(single_row, pater)
str_extract(single_row, "(^\\d+).*(%%\\d+\\$+\\d+)")
str_extract_all(single_row, "(^\\d+).*(%%\\d+\\$+\\d+).*(%[^(%|&)]+&)")


str_extract(single_row, "(^\\d)")
str_extract(single_row, "(%[^(%|&)]+&)")
str_match(single_row, "(%[^(%|&)]+&)")
str_match(single_row, "(^\\d+).*(%%\\d+\\$+\\d+).*(%[^(%|&)]+&)")

extract_row <- str_match(single_row, "(^\\d+).*(%%\\d+\\$+\\d+).*(%[^(%|&)]+&)")

typeof(str_match(single_row, "(^\\d+).*(%%\\d+\\$+\\d+).*(%[^(%|&)]+&)"))

text <- sapply(out_string_test[1:10], function(x) str_match(x,"(^\\d+).*(%%\\d+\\$+\\d+).*(%[^(%|&)]+&)"))
# Проблема со строками где есть указатели \r
str_match(out_string_test[4], "(^\\d+).*(%%\\d+\\$+\\d+).*(%[^(%|&)]+&)")
out_string_test[4]
str_match_all(out_string_test[4], "(^\\d+)\\s*(%%\\d+\\$+\\d+).*(%[^(%|&)]+&)")

value_exstractor <- function(string_vector, pattern_vecor){
  
  return(sapply(pattern_vecor, function(x) str_extract(string_vector,x), USE.NAMES = F))
}
# str_replace_all(string_vector, "[:cntrl:]","")

value_exstractor(out_string_test[4], c("(^\\d+)", "(\\d+\\$+\\d+)","(%[^(%|&)]+&)"))



test <- lapply(out_string_test[1:10], function(x) value_exstractor(x, c("(^\\d+)", "(\\d+\\$+\\d+)","(%[^(%|&)]+&)")))

uic_adres_clearly <- lapply(out_string_test, function(x) value_exstractor(x, c("(^\\d+)", "(\\d+\\$+\\d+)","(%[^%|&]+&)")))
# При вытаскивании адреса 
# Встаки \r исправлено более точным рег выражением "(%[^%|&]+&)"
out_string_test[22]
out_string_test[23]
out_string_test[100]
# Отсутсвие &&
out_string_test[49]

# Вставка && сразу после %%
# из-за пустой строки перед информацией полезной
# Добавил в слепател, что только не пустные строки объединились
out_string_test[100]

lofi <- sapply(uic_adres_clearly, function(x) if(is.na(x[3])){T}else{F})
length(lofi)
lofi[1] <- F
# Позиции всех пропусков (по кодровкe out_string_test)
logi <- which(lofi == T)
# их наконец нет
out_string_test[317]

out_string_test[49]

# Наличие \r, но проблема лишь в рег выражении
str_match_all(out_string_test[22], "(%[^(%|&)]+&)")

str_match_all(out_string_test[22], "[:cntrl:]")
str_match_all(out_string_test[22], ".*")

str_extract(out_string_test[22], ".*")

str_replace_all(out_string_test[22],"[:cntrl:]", "")
out_string_test[2]
str_replace_all(out_string_test[2],"[:cntrl:]", "")

identical(out_string_test[2], str_replace_all(out_string_test[2],"[:cntrl:]", "") )

str_match_all(str_replace_all(out_string_test[22],"[:cntrl:]", ""), "(%[^%|&]+&)")


# Проблема с отсутвие && (исправлять в на уровне экстактора)
# Возникает когда не нужно сшивать строчки из матрицы, адрес уже в ней
# добавил сшивание строки адрессов с пустой строкой, из-за этого
# происходит объединение даже одной строки и с добавление &&
out_string_test[49]
clear_uic_matrix[270:271,3]
paste(c(clear_uic_matrix[270:270,3], ""), collapse = "&&")

# Вставка && сразу после %%
out_string_test[100]
clear_uic_matrix[523:528,3]

str_remove_all(clear_uic_matrix[523:528,3], "")
grepl("",  clear_uic_matrix[523:528,3])


str_match_all(clear_uic_matrix[523:528,3], "")
str(clear_uic_matrix[523:528,3])
out_string_test[1841]
# Определяем не пустая ли строка
nzchar(clear_uic_matrix[523:528,3])

# Теперь делаем один дата фрейм

data_uic <- data.frame(do.call("rbind",uic_adres_clearly), 
                       stringsAsFactors = F)
# Адреса разбиты (из-за рег выражения) лучше сразу по ходу записыватьэ
# дата фрейм



# Заранее длинну нашего дата фрейма не знаем, так как мы режим всё.
# "угадаем" пусть по числу уик то есть 2500
 length(clear_uic_matrix[,1]) #это максимальная длинна которая может быть

gg <- data.frame(uic = character(2500), 
                 start_position = integer(2500),
                 end_position = integer(2500),
                 adres = character(2500),
                 street_name = character(2500),
                 stringsAsFactors = F)
i <- 1
k <- 1
G <- FALSE
while(i <= length(clear_uic_matrix[,1])){
  if(grepl("ТИК", clear_uic_matrix[i,1]) & G == FALSE){
    g <-   i
    G <- TRUE
    i <- i+1
    # print(c("G:", g))
  }
  if(grepl("ТИК", clear_uic_matrix[i,1]) & G == TRUE){
    # gg[k,1] <- paste(clear_uic_matrix[g:(i-1),2], collapse = "%%")
    gg[k,1] <- clear_uic_matrix[g,2]
    # gg[k,2] <- paste(as.character(c(i,g)),collapse = "%%")
    gg[k,2] <- g
    gg[k,3] <- i-1
    adres_string <- clear_uic_matrix[g:(i-1),3]
    adres_string <- str_replace_all(adres_string, stop_word_patern, "")
    full_adres_string <- paste(adres_string[nzchar(adres_string)], collapse = "%%")
    gg[k,4] <- full_adres_string
    # опрределям название улицы
    gg[k,5] <- str_extract(str_to_lower(full_adres_string),street_name_pattern)
    
    
    # home_adres_work <- paste(adres_string[str_detect(adres_string, good_pater)], collapse = "%%")
    # gg[k,5] <- home_adres_work
    # gg[k,4] <- paste(clear_uic_matrix[g:(i-1),3][nzchar(clear_uic_matrix[g:(i-1),3])], collapse = "%%")
    # current_string <- paste(c(clear_uic_matrix[g:(i-1),2], paste(c(i,g), collapse = "$$"), paste(c(clear_uic_matrix[g:(i-1),3][nzchar(clear_uic_matrix[g:(i-1),3])], "@"), collapse = "&&")), collapse = "%%")
    # out_string_test[length(out_string_test)+1] <- current_string
    # print(c("I:", i))
    # nzchar(clear_uic_matrix[523:528,3])
    g <- i
    k <- k+1
  }
  # Анализ последней записи
  if(i == length(clear_uic_matrix[,1])){
    gg[k,1] <- clear_uic_matrix[g,2]
    gg[k,2] <- g
    gg[k,3] <- i-1
    adres_string <- clear_uic_matrix[g:(i-1),3]
    adres_string <- str_replace_all(adres_string, stop_word_patern, "")
    full_adres_string <- paste(adres_string[nzchar(adres_string)], collapse = "%%")
    gg[k,4] <- full_adres_string
    # опрределям название улицы
    gg[k,5] <- str_extract(str_to_lower(full_adres_string),street_name_pattern)
    # gg[k,4] <- paste(clear_uic_matrix[g:(i-1),3][nzchar(clear_uic_matrix[g:(i-1),3])], collapse = "%%")
    # current_string <- paste(c(clear_uic_matrix[g:(i-1),2], paste(c(i,g), collapse = "$$")), collapse = "%%")
    # current_string <- paste(c(clear_uic_matrix[g:(i-1),2], paste(c(i,g), collapse = "$$"), paste(c(clear_uic_matrix[g:(i-1),3], ""), collapse = "&&")), collapse = "%%")
    # out_string_test[length(out_string_test)+1] <- current_string
  }
  i <- i +1
}
# Очищаем дата фрейм от пустот в конце из-за большего размера дата фрейма
# Есть пустоты внутри, не много, и возможно они связаны только с тем
# некоторые УИК-и в таблице pdf разбивались на две страницы
# см УИК 230, 228 
# отдельно не проверял

# gg[1841,1]
# head(!nzchar(gg[,1]))
test <- gg[which(nzchar(gg[,1])),]

saveRDS(test, file = "./Election_analysis/uic_adres_data_frame.rds")
# test[!nzchar(gg[,1])] <- NULL
# head(gg[T])
# head(which(nzchar(gg[,1])))
gg <- gg[which(nzchar(gg[,1])),]
# Извлекаем нормально адрес


street_identifer <- c("ул\\.", "улица", "бул\\.", "бульвар", "шос\\.", "шоссе",
                      "шосс\\.", "наб\\.","ш\\.", "набережная", "квартал", "линия",
                      "пер\\.", "переулок", "пл\\.", "площадь", "пр\\.", "проспект",
                      "аллея", "канал", "остров", "проезд", "пр-т" ,"шос", "б- р",
                      "ул", " по ", "участок")


str_test <- str_detect(str_to_lower(test$adres), paste(street_identifer, collapse = "|"))

# street_name_pattern <- paste0("[^,\\.\"]*(",paste(street_identifer, collapse = "|"),
#                               ")[^,\\.:]*", collapse = "")
# Новый патерн без точек
street_name_pattern <- paste0("[^,\"]*(",paste(street_identifer, collapse = "|"),
                              ")[^,:(]*", collapse = "")


any(str_test == F)
test$adres[!str_test]
"ул." %in% gg[1:10, 4]
pmatch("ул.", gg[1:10, 4], dup = F)

head(str_to_lower(test$adres))
test$street_prefix <- str_extract(str_to_lower(test$adres), paste(street_identifer, collapse = "|"))
str_detect(test$adres[1:10], "ул\\.")

grep("ул\\.", test$adres[1])
grepl("ул\\.", test$adres[1])

dd <- regexpr("ул\\.", test$adres[1])
dd[1]
gregexpr("ул\\.", test$adres[1])
regexec("ул\\.", test$adres[1])

# Определяем позиции префикса от него дальше работаем
str_locate(test$adres[1], "ул\\.")
# более менее работет ибо структура простая

dd <- sapply(test$adres, function(x) str_extract(str_to_lower(x),paste0("[^,\\.\"]*(",paste(street_identifer, collapse = "|"),")[^,%]*", collapse = "")))
head(dd)
test$street_name <- dd

paste0("[^,\\.\"]*(",paste(street_identifer, collapse = "|"),")[^,]*", collapse = "")
street_name_pattern <- paste0("[^,\\.\"]*(",paste(street_identifer, collapse = "|"),
                              ")[^,:]*", collapse = "")

# Хотим матчить дома, очень сильно мешают всякие вставки в результат 
# адрес дома сильно разнесен по строке, 
# пытаемся убрать попавшие заголовки таблицы
single_test <- test$adres[139]

# Делаем поттерн с убираемыми словами
stop_word <- c("Адрес помещения для", "Адрес помещения для",
               "работы участковой", "избирательной комиссии",
               "\\(наименование объекта\\),", "телефон" )
stop_word_patern <- paste0(stop_word, collapse = "|")

str_replace_all(single_test, stop_word_patern, "")

str_split(single_test, "%%")
unlist(str_split(single_test, "%%"))
str_split(single_test, "%%", simplify = T)
unlist(str_split(single_test, "%%", simplify = T))
str_replace_all(unlist(str_split(single_test, "%%")), stop_word_patern, "")

str_detect(single_test, "\\(наименование объекта\\),")

# Вытаскиваем номер дома
test_home <- test$adres[142]
str_split(test_home, "%%")
# Какие слова нам в принципе интересны? а ещё и числа
good_words <- c("к\\.", "корпус", "д\\.", "дом", "лит\\.",
                "литер", "литера", "No", "\\W(А)\\W", "\\W(Б)\\W", "\\W(В)\\W", "\\W(Г)\\W", "\\d")
good_pater <- paste0(good_words, collapse = "|")
str_detect(unlist(str_split(test_home, "%%")), "\\W(А)\\W")
str_detect(unlist(str_split(test_home, "%%")), good_pater)

sapply(test$adres, function(x) str_detect(unlist(str_detect(x, "%%")), good_pater))

# Паттерн чтоб искать просто номер дома и его дробь
# (д\.|дом)\s?(No)?\s?\d{0,3}(\/\d{0,3})? считаем что норме дома
# с дробью слиты
# адреса без указаня дома не ищутся

dd <- sapply(test$adres, function(x) str_extract(x,"(д\\.|дом)\\W*(No)?\\W*\\d{0,3}(\\/\\d{0,3})?"))
head(dd)
tail(dd)
test$home_number <- dd
test$adres[74]
str_extract(str_to_lower(test$adres[2]),"(д\\.|дом)\\W*(No)?\\W*\\d{0,3}(\\/\\d{0,3})?")

# Определяем корпус есть он есть 
dd <- sapply(test$adres, function(x) str_extract(x, "(к\\.|корпус)\\W*\\d{0,3}"))
test$corpus <- dd
# Определяем литер домов
dd <- sapply(test$adres, function(x) str_extract(x, "(лит\\.|литера|литер)(\\W*)(А|Б|В|Г)"))
str_extract("ул. Зои Космодемьянской, д.4 литер А (Государственно","(лит\\.|литера|литер)(\\W*)(А|Б|В|Г)")
test$liter <- dd

# Хорошо бы это сделать ещё на стадии считывания матрицы и в виде функции
uic_reader <- function(matrix_uic, stop_word_patern, street_name_pattern, subburn_pattern){
gg <- data.frame(uic = character(2500), 
                 start_position = integer(2500),
                 end_position = integer(2500),
                 adres = character(2500),
                 street_name = character(2500),
                 house_number = character(2500),
                 house_corpus = character(2500),
                 house_liter = character(2500),
                 subburn_name = character(2500),
                 stringsAsFactors = F)
i <- 1
k <- 1
G <- FALSE
while(i <= length(matrix_uic[,1])){
  if(grepl("ТИК", matrix_uic[i,1]) & G == FALSE){
    g <-   i
    G <- TRUE
    i <- i+1
    # print(c("G:", g))
  }
  if(grepl("ТИК", matrix_uic[i,1]) & G == TRUE){
    # gg[k,1] <- paste(clear_uic_matrix[g:(i-1),2], collapse = "%%")
    gg[k,1] <- matrix_uic[g,2]
    # gg[k,2] <- paste(as.character(c(i,g)),collapse = "%%")
    gg[k,2] <- g
    gg[k,3] <- i-1
    adres_string <- matrix_uic[g:(i-1),3]
    adres_string <- str_replace_all(str_replace_all(adres_string, stop_word_patern, ""), "\\r", " ")
    full_adres_string <- paste(adres_string[nzchar(adres_string)], collapse = "%%")
    gg[k,4] <- full_adres_string
    # опрределям название улицы
    gg[k,5] <- str_extract(str_to_lower(full_adres_string),street_name_pattern)
    gg[k,6] <- str_extract(full_adres_string,"(д\\.|дом|(д No))\\W*(No)?\\W*\\d{0,3}(\\/\\d{0,3})?")
    gg[k,7] <- str_extract(full_adres_string, "(к\\.|корпус)\\W*\\d{0,3}")
    gg[k,8] <- str_extract(full_adres_string,
                           "((лит\\.|литера|литер)(\\W*)(А|Б|В|Г))|(\\d(а|б|в|г|А|Б|В|Г))|(\\d[ ](а|б|в|г)\\W)")
    gg[k,9] <- str_extract(full_adres_string, subburn_pattern)
    # gg[k,8] <- str_extract(full_adres_string, "(лит\\.|литера|литер)(\\W*)(А|Б|В|Г)")
    # home_adres_work <- paste(adres_string[str_detect(adres_string, good_pater)], collapse = "%%")
    # gg[k,5] <- home_adres_work
    # gg[k,4] <- paste(clear_uic_matrix[g:(i-1),3][nzchar(clear_uic_matrix[g:(i-1),3])], collapse = "%%")
    # current_string <- paste(c(clear_uic_matrix[g:(i-1),2], paste(c(i,g), collapse = "$$"), paste(c(clear_uic_matrix[g:(i-1),3][nzchar(clear_uic_matrix[g:(i-1),3])], "@"), collapse = "&&")), collapse = "%%")
    # out_string_test[length(out_string_test)+1] <- current_string
    # print(c("I:", i))
    # nzchar(clear_uic_matrix[523:528,3])
    g <- i
    k <- k+1
  }
  # Анализ последней записи
  if(i == length(matrix_uic[,1])){
    gg[k,1] <- matrix_uic[g,2]
    gg[k,2] <- g
    gg[k,3] <- i-1
    adres_string <- matrix_uic[g:(i-1),3]
    adres_string <- str_replace_all(adres_string, stop_word_patern, "")
    full_adres_string <- paste(adres_string[nzchar(adres_string)], collapse = "%%")
    gg[k,4] <- full_adres_string
    # опрределям название улицы
    gg[k,5] <- str_extract(str_to_lower(full_adres_string),street_name_pattern)
    gg[k,6] <- str_extract(full_adres_string,"(д\\.|дом)\\W*(No)?\\W*\\d{0,3}(\\/\\d{0,3})?")
    gg[k,7] <- str_extract(full_adres_string, "(к\\.|корпус)\\W*\\d{0,3}")
    gg[k,8] <- str_extract(full_adres_string,
                           "((лит\\.|литера|литер)(\\W*)(А|Б|В|Г))|(\\d(а|б|в|г|А|Б|В|Г))|(\\d[ ](а|б|в|г)\\W)")
    gg[k,9] <- str_extract(full_adres_string, subburn_pattern)
    # gg[k,4] <- paste(clear_uic_matrix[g:(i-1),3][nzchar(clear_uic_matrix[g:(i-1),3])], collapse = "%%")
    # current_string <- paste(c(clear_uic_matrix[g:(i-1),2], paste(c(i,g), collapse = "$$")), collapse = "%%")
    # current_string <- paste(c(clear_uic_matrix[g:(i-1),2], paste(c(i,g), collapse = "$$"), paste(c(clear_uic_matrix[g:(i-1),3], ""), collapse = "&&")), collapse = "%%")
    # out_string_test[length(out_string_test)+1] <- current_string
  }
  i <- i +1
}
return(gg)
}

gg <- uic_reader(clear_uic_matrix, stop_word_patern, street_name_pattern)
install.packages("microbenchmark")
library(microbenchmark)
microbenchmark(gg <- uic_reader(clear_uic_matrix, stop_word_patern, street_name_pattern))
system.time(gg <- uic_reader(clear_uic_matrix, stop_word_patern, street_name_pattern, subburn_pattern))

# Разбираемся с островами и другим точечными проблема

gg$street_name[which(gg$uic == "139" | gg$uic == "140" )] <- "ул.остоумова"
gg$street_name[which(gg$uic == "1254")] <- "ул. восточная"
gg$street_name[which(gg$uic == "1905")] <- "пр. авиаконструкторов"
gg$street_name[which(gg$uic == "158")] <- "ул. наличная"
# Решение проблемы большой монетной 29
gg$street_name[which(gg$uic == "1634")] <- "большая монетная"
gg$street_name[which(gg$uic == "1701")] <- "ул. ульяновская"
gg$street_name[which(gg$uic == "1725" | gg$uic == "1724")] <- "советская ул."

# Теперь надо подчистить всё
# 1) убрать нижнюю ненужную часть
# 2) знаки %% в адресах
# 3) обозначение цифры в графе литеры
# 4) лишнии обозначения домов в строке улицы
# как правило это ^ связано с тем, что нет разделителя в виде запятой
# поэтому валивается и указание дом и т.д.
# 5) номера домов без указания что это дом, просто дан номер
# 6) проблемы с островами (ручное решение)
# 7) создать поселки/деревни/территории (чистка от улиц и т.д)
# 8) улицы без идентификатора что это улицы или проспекты
# 9) Проблема "Больших" улиц. Б.Пароховской, Б.Морской, Б. Сампсониевский (решено)
# Дело в точке перед ними, для тех случав, когда название идет перед улицей
# 10) Чистка посёлков от названия улиц

# !!! пролема с литерой дома, они могут быть просто буквой после цифры
# считаем что после номера и пропуска большой буквы нет *(1 А такого нет)
# 1А есть 1а есть 1 а тоже есть 
# Более страшная регулрярка вроде как разрулила.
library(dplyr)
cleaning_uic_table <- function(uic_dataframe){
  # 1) убрать нижнюю ненужную часть
  clean_dt <- gg[which(nzchar(uic_dataframe[,1])),]

  # 5) номера домов без указания что это дом, просто дан номер
  bad_position_house <- which(is.na(clean_dt$house_number) == T & is.na(clean_dt$street_name) != T)
  for (i in bad_position_house){
    finding_pattern <- paste0(paste0("(", clean_dt$street_name[i], collapse = ""),
                              ")\\W*\\d{0,3}(\\/\\d{0,3})?")
    clean_dt[i,6] <- str_remove_all(str_extract(str_to_lower(clean_dt$adres[i]), finding_pattern),
                                    clean_dt$street_name[i])
  }
  # 8) улицы без идентификатора что это улицы или проспекты
  empty_street_position <- which(is.na(clean_dt$street_name) == T & is.na(clean_dt$subburn_name) == T & is.na(clean_dt$house_number) != T)
  for (i in empty_street_position){
    street_empty_pattern <- paste0("[^,\\.\\d]*\\W*(",
                                   paste0(str_replace(clean_dt$house_number[i], "\\.", "\\\\." ),")", collapse = ""))
    clean_dt[i,5] <- str_remove_all(str_extract(str_to_lower(clean_dt$adres[i]), street_empty_pattern),
                              str_replace(gg$house_number[i], "\\.", "\\\\." ))
  }
  
  # 2) знаки %% в адресах
  # 3) обозначение цифры в графе литеры
  # 4) лишнии обозначения домов в строке улицы
  clean_dt <- mutate(clean_dt,house_number = str_remove_all(house_number,"%|No"),
                     house_corpus = str_remove_all(house_corpus, "%"),
                     house_liter = str_remove_all(house_liter, "((%)|(\\d))"),
                     street_name = str_remove_all(street_name, "(д\\.|дом|дом no)\\W*\\d.*|(т\\.)?\\d+-\\d+.*"))
  
  clean_dt <- mutate(clean_dt, house_number = str_remove_all(house_number, ","),
                     street_name = str_replace_all(street_name, "%%", " "))
  
# 10) Чистка посёлков от названия улиц
  bad_subburn_position <- which(apply(clean_dt[,c('street_name','subburn_name')], 1,function(x) str_detect(str_to_lower(x[2]), x[1])))
  for( i in bad_subburn_position){
    subburn_double_pattern <- paste0(".*(",
                                   paste0(str_replace(clean_dt$street_name[i], "\\.", "\\\\." ),")", collapse = ""))
    clean_dt[i,9] <-  str_remove_all(str_extract(str_to_lower(clean_dt$subburn_name[i]), subburn_double_pattern),
                                    str_replace(clean_dt$street_name[i], "\\.", "\\\\." ))
  }
  return(clean_dt)
}
"(д\\.|дом|дом no)\\W*\\d.*"
"((д\\.|дом|дом no)\\W*\\d.*|(т\\.)?\\d+-\\d+.*)"
system.time(gg <- cleaning_uic_table(gg))
gg %>% 
mutate(., house_number_new = str_remove(house_number,"%+")) -> ggt

# test <- gg$street_name[which(is.na(gg$house_number) == T)][11]
# str_remove_all(test, "\r")

# пункт 5, удобно делать через позицию в строке.
# зная улицу, и например корпус или литер, предполагаем, что нормер дома
# между ними
# если литера и корпуса нет, то до конца
# выбираем толкьо те строки где пропущен адрес
# возможно проблема если идем по литеру , а он только был буквой, см УИК1974
#  строчка 1574
# Тогда удобнее использовать только адрес как префикс и от него на лево
# должны быть цифры это и есть номер дома
# Позиции где не нет номера дома, а улица есть
bad_position_house <- which(is.na(gg$house_number) == T & is.na(gg$street_name) != T)
for (i in bad_position_house){
  finding_pattern <- paste0(paste0("(", gg$street_name[i], collapse = ""),
                            ")\\W*\\d{0,3}(\\/\\d{0,3})?")
  gg[i,6] <- str_remove_all(str_extract(str_to_lower(gg$adres[i]), finding_pattern),
                            gg$street_name[i])
}

# есть проблема со знаками \r
gg$street_name[which(is.na(gg$house_number) == T)]

test <- which(is.na(gg$house_number) == T & is.na(gg$street_name) != T)

head(gg[784,])

street_name_pattern <- paste0("[^,\\.\"]*(",paste(street_identifer, collapse = "|"),
                              ")[^,\\.:]*", collapse = "")

# Проблема острова всего лишь 3 записи проще вручнную
# это уик 139, 140,1254
# это можно сдлеать до наведеняи порядка с домама, чтобы 
# у уик 1254 правильер выставился номер дома
# УИК 1678 нет адреса так как у него череж д No 10
gg$street_name[97:98] <- "ул.остоумова"
gg$street_name[942] <- "ул. восточная"

test <- which(gg$uic == "139" | gg$uic == "140" )
which(test == T)
which(c("139", "140") %in% gg$uic)

gg$street_name[which(gg$uic == "139" | gg$uic == "140" )] <- "ул.остоумова"
gg$street_name[which(gg$uic == "1254")] <- "ул. восточная"



# Поселки города, деревни
# Работаем в режими как есть, литерра важна
# Надемся на разделитель запятой и цифры и отсутсвия двойных названий
subburn_name <-c("пос\\.", "посёлок", "поселок", "п\\.", "Пос\\.", "г\\.")
subburn_pattern <- paste0("^(", paste0(subburn_name, collapse = "|"),")\\W?[^,\\d%(]+" )
                          
street_name_pattern <- paste0("[^,\\.\"]*(",paste(street_identifer, collapse = "|"),
                                                        ")[^,\\.:]*", collapse = "")
test <- gg[809,4]
str_extract_all(test, "\\r")
str_remove_all(test, "\\r")
str_replace_all(test, "\\r", " ")

# 8) улицы без идентификатора что это улицы или проспекты
# три типа проблем: -нет указателя что это, но есть номер дома
# -есть только указание на номер школы
# - это какоя-то странный поселок с территориями
# Сфокусируемся на тех что с номерами дома (и это не поселки, улица есть)
empty_street_position <- which(is.na(gg$street_name) == T & is.na(gg$subburn_name) == T & is.na(gg$house_number) != T)
# Расчитываем, на сознательность разметки
gg$adres[empty_street_position]


for (i in empty_street_position){
  street_empty_pattern <- paste0("[^,\\.\\d]*\\W*(",
    paste0(str_replace(gg$house_number[i], "\\.", "\\\\." ),")", collapse = ""))
   gg[i,5] <- str_remove_all(str_extract(str_to_lower(gg$adres[i]), street_empty_pattern),
                             str_replace(gg$house_number[i], "\\.", "\\\\." ))
}
"[^,.]*\W*(д\.3\/5)"
str_replace("д. 62", "\\.", "\\\\." )
str_detect("ГБОУ No35 Кадетская, д.3/5%%    %%т. 573-97-29", "(д\\.3/5)" )

# Недобитки УИК 1267, 1266 можно испраить вписав в идентификаторы улиц,
# их особые признаки
test <- str_detect(gg$adres, " ПО ")

# Проблемы с большими улицама 
# ул. Б.Монетная, 2/4, ГБОУ школа No 84 тел.: 232-13-93
# ул.б а номера нома нет
# наб. р. Фонтанки, д. 134 а – ГБОУ ДППО центр повыше
# наб. р
# ул.Ак.Константинова, д.10, к.2 (помещение гимнази
# ул.ак
# ул. М.Разночинная, 2/4, ГБОУ школа No 50 тел.: 417-54
# ул м.
# ГБОУ No4, Большой пр. В.О., д.88 т.322-21-47
# большой пр. в

# Особый случай Пулковское отд. СПК%%«Шушары», 30, под. 10,%%тел
# номер дома просто д.

# пос. Большая Каменка, д. 10 (паспортный стол) 8-931
# название поселка имеет пробел
# Так же как и пос. Лисий нос
# Разборки с большими улицами
# убрал точку как стоп символ для паттернов
street_test_name_pattern <- paste0("[^,\"]*(",paste(street_identifer, collapse = "|"),
                                   ")[^,:(]*", collapse = "")
gg$test_street <- str_extract(str_to_lower(gg$adres), street_test_name_pattern)
# Вылезло Общежитие Горного%%университета. ул. Наличная,%
# ул. Орбели дом No 24, лит. А,%%596-19-74, 670-90-68% -- как ул. орбели дом no 24
# Это работает только для тех у кого идентификатор ул. стоит после её названия

# Лечим ул. Б. Пороховскую -- ул. б.
street_identifer <- c("ул\\.", "улица", "бул\\.", "бульвар", "шос\\.", "шоссе",
                      "шосс\\.", "наб\\.","ш\\.", "набережная", "квартал", "линия",
                      "пер\\.", "переулок", "пл\\.", "площадь", "пр\\.", "проспект",
                      "аллея", "канал", "остров", "проезд", "пр-т" ,"шос", "б- р",
                      "ул", " по ", "участок")
street_identifer_pattern <- paste0("(", paste0(street_identifer, collapse = "|"), ")")

gg$test_street <-  str_remove(gg$street_name, street_identifer_pattern)

# Улицы с цифрами слишком коротки, не ясно как с профессором
# ул. М.Разночинная, 2/4, ГБОУ школа No 50 тел.: 417-54 -- не определяется номер дома
# ул. проф. Попова, 37-Б, СПбГЭТУ тел.: 234-15-73 -- ни дома ни литера

# пусть длина меньше или равно 4, если переопределим что уже есть не страшно
# Надо убрать все пропуски, например профессор не определился
test <- nchar(str_remove_all(gg$test_street, "[^[:alnum:]]")) < 5
which(nchar(gg$test_street) < 5)
gg$street_name[which(nchar(gg$test_street) < 5)]
nchar("проф")
# Чистка от поселков пос. Ольгино Советская ул., д. 2 (Муниципальный сове -- как ольгино советская ул.
gg$test_test <- str_remove_all(gg$test_street, "[^[:alnum:]]")

# Не продуктивный подход, так как многие такие улицы в добавок не имеют идентификатора дома
# в результате их номера тоже нет.
# Проще не стопить на знаке точки, а потом просто почистить по номеру дома


# п. Песочный, ул. Ленинградская, 53, 573 98 40 Государств -- неправильно определ дом
# 3-я линия 1-й половины, д. 10а (лицей No 5 -- непривально определ литер
# пос. Большая Каменка, д. 10 (паспортный стол) 8-931-3 -- поселок не весь определился
# 2-я Советская ул., д.27/2, филиал No9 библиотеки им. -- не правильно литер -9б, а его не должно быть
# "((лит\\.|литера|литер)(\\W*)(А|Б|В|Г))|(\\d(а|б|в|г|А|Б|В|Г))|(\\d[ ](а|б|в|г)\W)")
# ^ паттерн, чтоб после буквы был любой не буквенный символ один


# Общежитие Горного%%университета. ул. Наличная,%% -- проблема осталась
# Костюшко, д. 62 (школа No544, 1 этаж, учительская) 37
# После извлечения адреса по номеру дома, нодо номер дома то убрать
# пр., Авиаконструкторов, д.%%21, к. 2 (школа No 579) 3 --
# не опредилалась улица, так как запятая все портит, а по номеру не идет, вель идентификатор есть

# Точечно это все исправляем делаем это перед чисткой
# общежитие -- УИК 158
# Проспект авиа -- УИК 1905

# Необходимо чистить строку поселка, от попадания туда улицы
# А строку Улицы от попадания туда домов или поселков

# особый случай г. Петергоф ул. Ульяновская, дом No 3 Санкт- Петерб
# пос. Ольгино Советская ул., д. 2 (Муниципальный с
# тут и в улицу и в поселлок попало одно и то же, нужно в ручную кого-то записать

# Большая Монетная, 29 (класс ГО --  не определилась, нет идентификатора нужно забить в ручную
# ^ решено
test <- sapply(gg, function(x) typeof(x))

test <- apply(gg[,c('street_name','subburn_name')], 1,function(x) str_detect(str_to_lower(x[2]), x[1]))
which(test == T)


str_detect(gg$subburn_name, )
gg$street_name
str_detect(c("aaaab", "asdasd"), "b")
