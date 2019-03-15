

GsIntLap <- function(int, n){
  # crawling a number of scholars' information in google scholar for specific
  # field
  # 1st argument 'int' refers to the specific field that is interested
  # 2nd 'n' is for how many people you want to search
  
  # load package
  lapply(c('xml2', 'rvest', 'dplyr', 'ggplot2', 'stringr', 'data.table'), require, character.only=T)
  
  dat <- c()
  if(n %% 10 != 0){
    iteration <- n %/% 10 + 1
  }else{
    iteration <- n/10
  }
  
  page <- read_html(paste0('https://scholar.google.co.kr/citations?hl=ko&view_op=search_authors&mauthors=label%3A',
                           int,
                           '&btnG='),
                    encoding = 'gb2312')
  
  dat <- lapply(1:iteration, function(i){
    authors <- page %>% 
      html_nodes('#gsc_sa_ccl') %>%
      html_nodes('.gs_ai_name') %>%
      html_text()
    
    aff <- page %>% 
      html_nodes('#gsc_sa_ccl') %>%
      html_nodes('.gs_ai_aff') %>%
      html_text()
    
    cited <- page %>%
      html_nodes('#gsc_sa_ccl') %>%
      html_nodes('.gs_ai_cby') %>%
      html_text() %>%
      gsub('\\D', '', .)
    
    dat <- cbind(authors, aff, cited) %>% as.data.frame()
    
    next_botton <- page %>%
      html_nodes('#gsc_authors_bottom_pag') %>%
      html_nodes('.gsc_pgn') %>%
      html_nodes('button') %>%
      html_attr('onclick')
    next_botton_author <- strsplit(next_botton[2], 'x3d')[[1]][6] %>% str_sub(., 1, 12)
    next_botton_num <- strsplit(next_botton[2], 'x3d')[[1]][7] %>% str_sub(., 1, 2)
    
    page <- read_html(paste0('https://scholar.google.co.kr/citations?view_op=search_authors&hl=ko&mauthors=label:',
                             int,
                             '&after_author=',
                             next_botton_author,
                             '&astart=',
                             next_botton_num),
                      encoding = 'gb2312')
    return(dat)
  }) %>% rbindlist()
}