co_oc.set.zero <- list()

for (i in 1:length(season_country_dataset)) {
  
  temp <- season_country_dataset[[i]] %>% select(-c(country, season))
  
  table <- cooc_table(temp)
  
  # select only the pairs have p.adjusted < 0.05
  table$rho[which(table$p.value > 0.05)] <- 0
  co_oc.set.zero[[i]] <- table
}
names(co_oc.set.zero) <- c("ind_ds", "ind_ws", "idn_ds", "idn_ws", "phl_ds", "phl_ws", "tha_ds", "tha_ws", "vnm_ds", "vnm_ws")

cooc_setzero.df <- do.call(rbind, co_oc.set.zero)

cooc_setzero.df$country_season <- gsub("\\..*", "", row.names(cooc_setzero.df))

row.names(cooc_setzero.df) <- NULL

cooc_setzero.df$country_season <- as.factor(cooc_setzero.df$country_season)

rho.value <- cooc_setzero.df %>% select(country_season, rho)


rho.value$correlation <- ifelse(rho.value[2] < 0, "negative", 
                                ifelse(rho.value[2] > 0, "positive",
                                       "no correlation"))
rho.value$rho <- abs(rho.value$rho)
names(rho.value) <- c("country_season", "rho", "correlation")
rho.value$rho <- as.numeric(rho.value$rho)
rho.value$correlation <- as.factor(rho.value$correlation)


percent_correaltion <-  rho.value %>% 
  select(country_season, correlation) %>% 
  group_by(country_season, correlation) %>% 
  summarise(n_cor_type = n()) %>%
  ungroup() %>%
  group_by(country_season) %>%
  mutate(percent_n = n_cor_type/ sum(n_cor_type) *100)

percent_correaltion <- percent_correaltion %>% separate(country_season, c("country", "season"), "_")
ggplot(percent_correaltion, aes(x = country, y = percent_n, fill = correlation)) +
  geom_bar(stat="identity") + facet_grid(~season)






