library(pdftools)
library(tidyverse)
library(janitor)
library(eeptools)

# setwd("/Volumes/GoogleDrive/My Drive/QJL/ACLU COVID Investigation")

arapahoe_file <- "Data Files/Completed/Arapahoe.pdf"

arapahoe_pdf_structure <- pdf_data(arapahoe_file)

 arapahoe_pages <- 
   lapply( 1:length(arapahoe_pdf_structure), function (x) {
 
      arapahoe_pdf_structure[[x]] %>% 
      arrange(x, y) %>%
      pivot_wider(id_cols = y, names_from = x, values_from = text) %>%
      arrange(y) %>% 
      separate(`18`, c("Indicator", NA), 1)  %>%
      filter(Indicator == "2")  %>%
      rename(`Last Name` = `147`) %>%
      rename(DOB = `326`) %>%
      unite(col = "Gender", `426`:`427`, sep = "", remove = TRUE, na.rm = TRUE) %>%
       unite(col = "Race", `378`:`398`, sep = "", remove = TRUE, na.rm = TRUE) %>%  
       unite(col = "Ethnicity", `450`:`507`, sep = "", remove = TRUE, na.rm = TRUE) %>%  
       select(`Last Name`, DOB, Race, Ethnicity ) 
  }
)
 

arapahoe <- do.call(rbind, arapahoe_pages) %>%
                   filter(!is.na(DOB)) %>%
  mutate(DOB = as.Date(DOB, "%m/%d/%Y"),
         Age = floor(
                    age_calc(DOB,
                        as.Date("2020-04-20"), 
                        units = "years")
                       )
                    ) 

arapahoe %>% View()

######################
larimer_file <-  "Data Files/Completed/Larimer.pdf"

larimer_pdf_structure <- pdf_data(larimer_file)


larimer_pages <-    
  
  lapply( 1:length(larimer_pdf_structure), function (x) {

larimer_pdf_structure[[x]] %>% 
  arrange(x, y) %>% 
  pivot_wider(id_cols = y, names_from = x, values_from = text) %>%  
  arrange(y) %>% 
  rename(Name = `12`) %>%
  filter(!is.na(Name) | !is.na(lag(Name, 1)) | !is.na(lag(Name, 2))
         ) %>% 
  select(y, Name, DOB = `180`, Race = `273`, Ethnicity = `327`)  %>% 
  mutate(indicator = ceiling((1:n())/3  )) %>%
  select(-y) %>%
  gather( colnames ,values, -indicator) %>%
  arrange(indicator) %>% 
  filter(!is.na(values)) %>%
  spread(colnames, values) %>%
  mutate(DOB = as.Date(DOB, "%m/%d/%Y"),
         Age = floor(
           age_calc(DOB,
                    as.Date("2020-04-20"), 
                    units = "years")
              )
         ) %>%
  separate(Name, c("Last Name", NA), sep = ",") %>%
  select(`Last Name`, DOB, Race, Ethnicity, Age )
    
})
 

larimer_pages

larimer <- do.call(rbind, larimer_pages)

larimer %>% View()
  

######################

jefferson_file <-  "Data Files/Completed/Jefferson.pdf"

jefferson_pdf_structure <- pdf_data(jefferson_file)

jefferson_pages <- lapply( 1:20, function (x) {
  
    jefferson_pdf_structure[[x]] %>%
      arrange(x, y) %>% 
      pivot_wider(id_cols = y, names_from = x, values_from = text) %>%  
      arrange(y) %>% 
      rename(Name = `18`) %>%
      filter(!is.na(Name)) %>% 
      unite(col = "Gender", `198`:`199`, sep = "", remove = TRUE, na.rm = TRUE) %>% 
      add_column( d = "", .before = "254") %>%
      unite(col = "Race", `221`:d, sep = "", remove = TRUE, na.rm = TRUE) %>%
      unite(col = "DOB", `254`:`271`, sep = "", remove = TRUE, na.rm = TRUE) %>%
      select(Name, DOB, Gender, Race) %>%
      slice(2:n()) %>%
      filter(!Name %in% c("Booking_Name")) 

 }
)

jefferson <- do.call(rbind, jefferson_pages) %>%
  bind_rows(


jefferson_pdf_structure[[21]] %>%
  arrange(x, y) %>% 
  pivot_wider(id_cols = y, names_from = x, values_from = text) %>%  
  arrange(y) %>% 
  rename(Name = `18`) %>%
  filter(!is.na(Name)) %>% 
  unite(col = "Gender", `198`:`199`, sep = "", remove = TRUE, na.rm = TRUE) %>% 
 add_column( d = "", .before = "255") %>%
  unite(col = "Race", `221`:d, sep = "", remove = TRUE, na.rm = TRUE) %>% 
  unite(col = "DOB", `255`:`271`, sep = "", remove = TRUE, na.rm = TRUE) %>%
  select(Name, DOB, Gender, Race) %>%
  slice(2:n()) %>%
  filter(!Name %in% c("Booking_Name"))
) %>%
  bind_rows(

  jefferson_pdf_structure[[22]] %>%
  arrange(x, y) %>% 
  pivot_wider(id_cols = y, names_from = x, values_from = text) %>%  
  arrange(y) %>% 
  rename(Name = `18`) %>%
  filter(!is.na(Name)) %>%  
  unite(col = "Gender", `198`:`199`, sep = "", remove = TRUE, na.rm = TRUE)  %>%
  add_column( d = "", .before = "254") %>%
  unite(col = "Race", `221`:d, sep = "", remove = TRUE, na.rm = TRUE) %>% 
  unite(col = "DOB", `254`:`271`, sep = "", remove = TRUE, na.rm = TRUE) %>%
  select(Name, DOB, Gender, Race) %>%
  slice(2:n()) 
  
  )  %>%

filter(!Name %in% c("Booking_Name", "Total"))   

jefferson <- 
jefferson  %>% 
  mutate(DOB = as.Date(DOB, "%m/%d/%Y"),
         Age = floor(
           age_calc(DOB,
                    as.Date("2020-04-20"), 
                    units = "years")
         )
  )  %>%
separate(Name, c("Last Name", NA), sep = ",") %>%
  mutate(Ethnicity = NA) %>%
  select(`Last Name`, DOB, Race, Ethnicity, Age)


jefferson %>% View()

######################

douglas_file <-  "Data Files/Completed/Douglas.pdf"

douglas_pdf_structure <- pdf_data(douglas_file)

douglas_pages <- lapply( 1:length(douglas_pdf_structure), function (x) {
  
  douglas_pdf_structure[[x]] %>%
     arrange(x, y) %>% 
     pivot_wider(id_cols = y, names_from = x, values_from = text) %>%  
     arrange(y)  %>%  
    add_column(d= "") %>%
     unite(col = "Data", `29`:d, sep = " ", remove = TRUE, na.rm = TRUE) %>%
    select(Data)
    }
  )

douglas_pages

douglas <- do.call(rbind, douglas_pages) %>%
  mutate(Data = ifelse(Data == "", "UNKNOWN", Data)) %>%
  filter(
    # Data %in% race | 
    # Data %in% ethnicity | 
      grepl("00:00", Data) == TRUE |
      grepl("00:00", lag(Data,1 ) ) == TRUE |
      grepl("00:00", lag(Data,2 ) ) == TRUE 
    
  ) %>%

   mutate(index = ceiling((1:n())/3),
       labels = rep(c("DOB", "Race", "Ethnicity"), max(index) ) 
       
) %>%
  spread(labels, Data) 

douglas <- 
douglas %>% 
  separate(DOB, c("DOB", NA), sep = " ") %>%
  
  mutate(DOB = as.Date(DOB),
         Age = floor(
           age_calc(DOB,
                    as.Date("2020-04-20"), 
                    units = "years")
         )
                )   %>% 

  mutate(`Last Name` = NA) %>%
  select(`Last Name`, DOB, Race, Ethnicity, Age)


douglas %>% View()
