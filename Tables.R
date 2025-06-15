getwd()
library(tidyverse)
library(gtsummary)
library(gt)
library(readr)
library(easystats)
library(readxl)
library(MASS)
library(broom)
#read an excel file
AMR_KAP <- read_excel("Data/AMR_KAP_Coded.xlsx", sheet = 2)
view(AMR_KAP)
colnames(AMR_KAP)

#select the demographic characteristics
AMR_KAP_Demographics <- AMR_KAP %>%
  select(1:11) 
colnames(AMR_KAP_Demographics)
view(AMR_KAP_Demographics)

#create a summary table for the AMR_KAP_Demographics data
# Table 1. Demographics characteristics of study participants 
AMR_KAP_Demographics |> 
  tbl_summary() |> 
  as_gt() |> 
  gtsave("tables/Table1.docx")






#rename the column
AMR_KAP <- AMR_KAP %>%
  rename( source_info = `Source(s) of information about antibiotics`)
view(AMR_KAP)


#Split multiple sources into long format
source_long <- AMR_KAP %>%
  select(`source_info`) %>%
  separate_rows(`source_info`, sep = " {2,}") %>%
  mutate(`source_info` = str_trim(`source_info`))

view(source_long)






#Count occurrences and calculate percentages
total_n <- nrow(AMR_KAP)
source_counts <- source_long %>%
  count(`source_info`, name = "Frequency") %>%
  arrange(desc(Frequency)) %>%
  mutate(Percent = round(Frequency / total_n * 100, 1))
view(source_counts)




# Format source_counts as a GT summary table
source_counts %>%
  mutate(`N = 704` = paste0(Frequency, " (", Percent, "%)")) %>%
  select(Characteristic = source_info, `N = 704`) %>%
  gt() %>%
  tab_header(
    title = md("**Table 2. Major sources of information about antibiotic parents (N = 704).**")
  ) %>%
  cols_label(
    Characteristic = md("**Characteristic**"),
    `N = 704` = md("**N = 704**")
  ) %>%  gtsave("Tables/Table2.docx")





# Table 3.  Level of knowledge, attitudes, and practices towards antibiotic resistance among parents with school
#going children (N = 704).
AMP_KAP <- AMR_KAP %>%
  select(69:71)

view(AMP_KAP)

AMP_KAP |> 
  tbl_summary() |> 
  as_gt() |> 
  gtsave("Tables/Table3.docx")




glimpse(AMR_KAP)




table(AMR_KAP$Knowledge_Level)





#Create a new factor variable for Knowledge_Level
AMR_KAP$Knowledge_Level <- factor(AMR_KAP$Knowledge_Level, ordered=TRUE )




#perform ordinal logistic regression using polr
AMR_KAP %>% 
  dplyr::select(1:9, Knowledge_Level) %>% 
  tbl_uvregression(
   method = polr,
   method.args = list(Hess = TRUE),
   y = Knowledge_Level,
   exponentiate = TRUE,
    )

#perform ordinal logistic regression using clm
AMR_KAP %>%
  dplyr::select(1:9, Knowledge_Level) %>%
  tbl_uvregression(
    method = clm,
    y = Knowledge_Level,
    exponentiate = TRUE
  ) %>%
  bold_p(t = 0.05) %>%
  as_gt() %>%
  gt::gtsave(filename = "Tables/Table4.docx")



#Create a new factor variable for Knowledge_Level
AMR_KAP$Attitude_Level <- factor(AMR_KAP$Attitude_Level, ordered=TRUE )
table(AMR_KAP$Attitude_Level)


#perform ordinal logistic regression using clm
AMR_KAP %>% 
  dplyr::select(1:9, Attitude_Level) %>% 
  tbl_uvregression(
    method = clm,
    y = Attitude_Level,
    exponentiate = TRUE,
  ) %>% 
  bold_p(t = 0.05) %>% 
  as_gt() %>% 
  gt::gtsave(filename = "Tables/Table5.docx")







#Create a new factor variable for Practice_level
AMR_KAP$Practice_Level <- factor(AMR_KAP$Practice_Level , ordered=TRUE )


#perform ordinal logistic regression using clm
AMR_KAP %>% 
  dplyr::select(c(1:9,69:70), Practice_Level) %>% 
  tbl_uvregression(
    method = clm,
    y = Practice_Level,
    exponentiate = TRUE,
  ) %>% 
  bold_p(t = 0.05) %>% 
  as_gt() %>% 
  gt::gtsave(filename = "Tables/Table6.docx")















