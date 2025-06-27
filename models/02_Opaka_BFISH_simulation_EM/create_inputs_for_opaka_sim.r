library(r4ss)
library(tidyverse)

rep <- SS_output(file.path(getwd(), "Model", "22_Opaka_BFISH_simulation_EM"))
SS_plots(rep)

recdev_df <- rep$parameters %>% filter(str_detect(Label, "Rec")) %>% 
separate(Label, into = c("type", "recdev", "year"), sep = "_") %>%
select(c(type, year, Value))
write.csv(recdev_df, "C:/Users/Megumi.Oshima/Documents/Opaka_simulation/Inputs/recdev_df.csv")

F_df <- rep$exploitation %>% select(c(Yr, FRS, Non_comm))
write.csv(F_df, "C:/Users/Megumi.Oshima/Documents/Opaka_simulation/Inputs/F_df.csv")

effN <- rep$sizedbase %>% select(Yr, effN) %>% distinct()
write.csv(effN, "C:/Users/Megumi.Oshima/Documents/Opaka_simulation/Inputs/effN.csv")
