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

tempdf -> final_statewise_gsdp1
view(final_statewise_gsdp1)

final_statewise_gsdp %>% 
  write_csv("final_statewise_gsdp1.csv")

#1. For every financial year, which sector has performed well
#2. For every financial year, which sector has performed least
#3. For every financial year, which state has performed well
#4. For every financial year, which state has performed least
#5. Top 5 performing states in Manufacturing
#6. Top 5 performing states in Construction
#7. For financial year 2016-17, for every state get top performing sector
#8. For financial year 2016-17, for every state get top 5 performing sectors
#9. How many states are performing well in Manufacturing, (if Manufacturing is in top 3)
#10. What is the GROSS GSDP of Karnataka for all financial years

# Load necessary libraries
library(dplyr)
library(readr)



read_csv("final_statewise_gsdp1.csv") -> statewise_gsdp

statewise_gsdp %>%
  group_by(year,item) %>% 
  summarize(total_gsdp = sum(gsdp,na.rm=T)) %>% 
  ungroup() %>% 
  group_by(year) %>% 
  arrange(desc(total_gsdp)) %>% view()



#1. For every financial year, which sector has performed well
statewise_gsdp %>%
  group_by(year,item) %>% 
  summarize(total_gsdp = sum(gsdp,na.rm=T), .groups = "drop") -> va1

va1 %>% 
  group_by(year) %>% 
  arrange(desc(total_gsdp)) %>% 
  slice(1)

#2. For every financial year, which sector has performed least
statewise_gsdp %>%
  group_by(year,item) %>% 
  summarize(total_gsdp = sum(gsdp,na.rm=T), .groups = "drop") -> va1


va1 %>% 
  group_by(year) %>% 
  arrange((total_gsdp)) %>% 
  slice(1)

#3. For every financial year, which state has performed well
statewise_gsdp %>%
  group_by(year,state) %>% 
  summarize(total_gsdp = sum(gsdp,na.rm=T), .groups = "drop") -> va1


va1 %>% 
  group_by(year) %>% 
  arrange(desc(total_gsdp)) %>% 
  slice(1)

#4. For every financial year, which state has performed least
statewise_gsdp %>%
  group_by(year,state) %>% 
  summarize(total_gsdp = sum(gsdp,na.rm=T), .groups = "drop") -> va1


va1 %>% 
  group_by(year) %>% 
  arrange((total_gsdp)) %>% 
  slice(1)

#5. Top 5 performing states in Manufacturing
library(dplyr)


va1 <- statewise_gsdp %>%
  filter(item == "Manufacturing") %>%
  group_by(state) %>%
  summarize(total_gsdp = sum(gsdp, na.rm = TRUE), .groups = "drop") -> top5_states


top5_states <- va1 %>%
  arrange(desc(total_gsdp)) %>%
  slice(1:5)

print(top5_states)

#6. Top 5 performing states in Construction
 statewise_gsdp %>%
  filter(item == "Construction") %>%
  group_by(state) %>%
  summarize(total_gsdp = sum(gsdp, na.rm = TRUE), .groups = "drop") -> va1


  va1 %>%
  arrange(desc(total_gsdp)) %>%
  slice(1:5) 
  print(top5_states)

#7. For financial year 2016-17, for every state get top performing sector
  
 statewise_gsdp %>%
    filter(year == "2016-17") %>%      
    group_by(state) %>%
    slice_max(order_by = gsdp, n = 1, with_ties = FALSE) %>% 
    ungroup() -> va2
  print(va2)
  
  statewise_gsdp %>%
    filter(year == "2016-17") %>%      
    group_by(state) %>%
    filter(gsdp == max(gsdp, na.rm = TRUE)) %>%   
    ungroup() -> va2
  print(va2)

  #OR
  statewise_gsdp %>% 
    filter(year == "2016-17") %>% #pull(state) %>% unique()
    group_by(state, sector) %>% 
    summarise(total_gsdp = sum(gsdp, na.rm = T)) -> va1
  
  df %>% 
    group_by(state) %>% 
    arrange(desc(total_gsdp)) %>% 
    slice(1)
  
  #for 2015-16
  statewise_gsdp %>% 
    filter(year == "2015-16") %>% #pull(state) %>% unique()
    group_by(state, sector) %>% 
    summarise(total_gsdp = sum(gsdp, na.rm = T)) -> va1
  
  va1 %>% 
    group_by(state) %>% 
    arrange(desc(total_gsdp)) %>% 
    slice(1)
  
  
#8. For financial year 2016-17, for every state get top 5 performing sectors
  statewise_gsdp %>% 
    filter(year == "2016-17") %>% 
    group_by(state, item) %>% 
    summarise(total_gsdp = sum(gsdp, na.rm = T)) -> va2
  
  va2 %>% 
    group_by(state) %>% 
    arrange(desc(total_gsdp)) %>% 
    slice(1:5)
  #9. How many states are performing well in Manufacturing, (if Manufacturing is in top 3)
  statewise_gsdp %>% 
    group_by(state, item) %>% 
    summarise(total_gsdp = sum(gsdp, na.rm = T)) -> va2
  
  va2 %>% 
    group_by(state) %>% 
    arrange(desc(total_gsdp)) %>% 
    slice(1:3) %>% 
    filter(item == "Manufacturing") -> no_of_states_in_top3_manufacturing
  
  nrow(no_of_states_in_top3_manufacturing)

  #10. What is the GROSS GSDP of Karnataka for all financial years
  statewise_gsdp %>% 
    filter(state == "Karnataka") %>% 
    group_by(year) %>% 
    summarise(total_gsdp = sum(gsdp, na.rm = T))
    
#11. What is the Total GSDP of Karnataka for 2015-16 financial year by all sector
#12 percentage of contributio of each sector  
  
  
#11. What is the Total GSDP of Karnataka for 2015-16 financial year by all sector
  
final_statewise_gsdp %>%
    filter(state == "Karnataka", year == "2015-16") %>%
    group_by(item) %>%
    summarise(sector_gsdp = sum(gsdp, na.rm = TRUE), .groups = "drop") %>%
    mutate(
      total_gsdp = sum(sector_gsdp),
      percentage_contribution = round((sector_gsdp / total_gsdp) * 100, 2)
    ) %>%
    select(item, percentage_contribution)
  
  