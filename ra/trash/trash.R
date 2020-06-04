#trash
# Treatment
Treat <- read.csv("09 - tbl_fupbio.csv", sep = ";")
Treat <- as_tibble(Treat)

Treat2 <- Treat %>% 
  filter(fupno == 1) %>%
  select(LocalRandID, drugid) %>%
  distinct(LocalRandID, .keep_all = TRUE)

base3 <- base2 %>% left_join(Treat2, by = "LocalRandID") %>%
  filter(!is.na(drugid)) %>%
  filter(drugid %in% c(0, 5, 1394)) %>% 
  mutate(treat = recode(drugid, '0' = "No Trt", '5' = "RTX", '1394' = "TCZ"),
         treat1 = recode(drugid, '0' = '1', '5' = '2', '1394' = '3'))
