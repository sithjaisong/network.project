
sweep_anlaysis <- function(sweep) {
  
  result <- sweep %>% group_by(index, Country, Year, Season, Fieldno, visit, DVS) %>%
    summarise(m.GLH = mean(GLH.sweep)) %>%
    group_by(index, Country, Year, Season, Fieldno) %>% 
    summarise(GLH.audpc = audpc(m.GLH, DVS))
  
}
