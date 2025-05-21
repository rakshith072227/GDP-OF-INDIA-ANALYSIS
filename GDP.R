library(janitor)
library(stringi)
library(tidyverse)
#read_csv("GDP Data/NAD-Andhra_Pradesh-GSVA_cur_2016-17.csv")

dir()
dir(path  = "GDP Data")

"GDP Data/NAD-Andhra_Pradesh-GSVA_cur_2016-17.csv" %>% 
  read_csv() -> ap_df1

View(ap_df1)
ap_df1 %>% pull(Item) %>% unique()

"NAD-Andhra_Pradesh-GSVA_cur_2016-17.csv" %>% 
  str_split("-") %>% 
  unlist() -> state_name_vector
view(state_name_vector)
  
st_name <- "Andhra Pradesh"
  ap_df %>% 
    slice(-c(7, 11, 27:33)) %>% 
    pivot_longer(-c(1, 2), names_to = "year", values_to = "gsdp") %>% 
    clean_names() %>% 
    select(-1) %>% 
    mutate(state = st_name)
  
dir(path = "GDP Data",
    pattern ="NAD") -> state_files
  
  for (i in state_files) {
    print(paste0("File name: ",i))
  }

# Step 2 == extract state names form file name
#print(paste0("File Name: ", st_name))

for (i in state_files) {
  i %>% 
    str_split("-") %>% 
    unlist() -> state_name_vector
  
  state_name_vector[2] -> st_name
  
  print(paste0("State Name: ", st_name))
}

#Step3 read all csv fies
tempdf <- tibble()
for (i in state_files) {
  i %>% 
    str_split("-") %>% 
    unlist() -> state_name_vector
  
  state_name_vector[2] -> st_name
  
  print(paste0("State Name: ", st_name))

paste0("GDP Data/",i) %>% 
  read_csv() -> st_df1

st_df1 %>% 
  slice(-c(7, 11, 27:33)) %>% 
  pivot_longer(-c(1, 2), names_to = "year", values_to = "gsdp") %>% 
  clean_names() %>% 
  select(-1) %>% 
  mutate(state = st_name) -> state_df
print(state_df)
bind_rows(tempdf,state_df) -> tempdf
}

tempdf -> final_statewise_gsdp
view(final_statewise_gsdp)

final_statewise_gsdp %>% 
  write_csv("final_statewise_gsdp")
write_csv("final_statewise_gsdp")

