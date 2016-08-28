
sweep_anlaysis <- function(sweep) {
  
  result <- sweep %>% dplyr::group_by(index, Country, Year, Season, Fieldno, visit, DVS) %>%
    dplyr::summarise(m.GLH = mean(GLH.sweep)) %>%
    dplyr::group_by(index, Country, Year, Season, Fieldno) %>% 
    dplyr::summarise(GLH.audpc = audpc(m.GLH, DVS))
  
}
