lapply(c('dplyr',
         'ggplot2',
         'stringr',
         'data.table',
         'reshape2',
         'XLConnect',
         'reshape',
         'gridExtra',
         'biglm'
), require, character.only =T)


read.xls <- function(filename, sheetnumber=1, sheetname=NULL, forceConversion=TRUE, startCol=0,  stringsAsFactors=TRUE) {
  wb <- loadWorkbook(filename)
  if (is.null(sheetname)) sheetname = getSheets(wb)[sheetnumber]
  df <- readWorksheet(wb, sheet=sheetname, forceConversion=forceConversion, startCol=startCol)
  if (stringsAsFactors) {
    ischar <- sapply(df, class) == "character"
    for (i in 1:length(df)) {
      if (ischar[i]) df[,i] <- factor(df[,i])
    }
  }
  df
}


#중복되어 나타나는 값을 다른 변수와 연관지어서 보는 함수
dupl <- function(data, index, index2){
  data2 <- data %>% select(colnames(data)[index]) %>% distinct() 
  a <- which(colnames(data2) == colnames(data)[index2])
  dd <- data2 %>% select(a) %>% duplicated()
  dd2 <- data2 %>% filter(dd) %>% select(a) %>% unlist() %>% as.character()
  res <- data2  %>% filter(unlist(select(data2,a)) %in% dd2)
  return(res)
}



d <- list.files('C:\\Users\\baoro\\Desktop\\공모전\\상경대 빅데이터 경진대회\\모나미') %>%
  as.data.frame() %>% 
  filter(grepl('20190103',.) & grepl('csv',.)) %>%
  unlist() %>%
  as.character()




