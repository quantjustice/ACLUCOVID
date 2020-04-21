library(pdftools)
library(tidyverse)
library(janitor)
library(eeptools)

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
       select(`Last Name`, DOB, Gender, Race, Ethnicity ) 
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



