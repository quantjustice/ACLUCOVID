library(pdftools)
library(janitor)
library(eeptools)
library(openxlsx)
library(tesseract)
library(magick)

library(tidyverse)



# setwd("/Volumes/GoogleDrive/My Drive/QJL/ACLU COVID Investigation")

####################
# Arapahoe 

arapahoe_file <- "Data Files/Completed/Arapahoe.pdf" # The File

arapahoe_pdf_structure <- pdf_data(arapahoe_file)    # Get coordinates of data in pdf

 arapahoe_pages <- 
   lapply( 1:length(arapahoe_pdf_structure), function (x) {
 
      arapahoe_pdf_structure[[x]] %>% 
      arrange(x, y) %>%
      pivot_wider(id_cols = y, names_from = x, values_from = text) %>%
      arrange(y) %>% 
      separate(`18`, c("Indicator", NA), 1)  %>%      # Seperate out the first letter in the first column
      filter(Indicator == "2")  %>%                   # Only rows with IDs of ppl should have 2's at the front
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
                    ) %>%
  mutate(Location = "Arapahoe")


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
  filter(!is.na(Name) | !is.na(lag(Name, 1)) | !is.na(lag(Name, 2))   # If its a name or the two rows below it, keep it. 
         ) %>% 
  select(y, Name, DOB = `180`, Race = `273`, Ethnicity = `327`)  %>% 
  mutate(indicator = ceiling((1:n())/3  )) %>%                        # Make an indicator for three row groups
  select(-y) %>%
  gather( colnames ,values, -indicator) %>%                           # gather so we can filter out the nas
  arrange(indicator) %>% 
  filter(!is.na(values)) %>% 
  spread(colnames, values) %>%                                        # put it all back together
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
 


larimer <- do.call(rbind, larimer_pages) %>%
  mutate(Location = "Larimer")


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
  select(`Last Name`, DOB, Race, Ethnicity, Age) %>%
  mutate(Location = "Jefferson")



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
  select(`Last Name`, DOB, Race, Ethnicity, Age) %>%
  mutate(Race = str_sub(Race,  1,-2),
         Ethnicity = str_sub(Ethnicity, 1,  -2)) %>%
  mutate(Location = "Douglas")



# douglas %>% View()

######################
# Adams 1

Adams1 <- read_csv("Data Files/Completed/Adams 1.csv")

Adams1 <- 
Adams1  %>%

  mutate(DOB = as.Date(Age, "%m/%d/%Y") ,

         Age = floor(
           age_calc(DOB,
                    as.Date("2020-04-20"), 
                    units = "years")
         )
  ) %>%
  separate(`Last First Middle Name`, c("Last Name", NA), sep = " ") %>%
  select(`Last Name`, DOB, Race, Ethnicity, Age) %>%
  mutate(Location = "Adams")

  # Adams1 %>% View()

#######################
# El Paso

elpaso <- read_csv("Data Files/Completed/El Paso.csv")

elpaso <- 
elpaso  %>%
  group_by(BookingNo) %>%
  slice(1) %>%
  ungroup(BookingNo) %>%
   separate(DOB, c("D", "M", "Y"), sep = "/") %>%
   mutate( Y = ifelse(Y > 20, 
                      paste0("19", Y),
                      paste0("20", Y))
   ) %>%
  unite(col = "DOB", `D`:`Y`, sep = "/", remove = TRUE, na.rm = TRUE) %>%  
     mutate(DOB = as.Date(DOB, "%m/%d/%Y") ,
         
            Age = floor(
                  age_calc(DOB,
                       as.Date("2020-04-20"), 
                       units = "years")
                       )
          ) %>%
    separate(N, c("Last Name", NA), sep = ",") %>%
    select(`Last Name`, DOB, Race, Ethnicity, Age) %>%
    mutate(Location = "El Paso")

# elpaso

#######################
# Weld 

weld_file <- read_csv("Data Files/Completed/Weld.csv")


weld <- 
  weld_file  %>%
  separate(DOB, c("M", "D", "Y"), sep = "/") %>%
  mutate( Y = ifelse(Y > 20, 
                     paste0("19", Y),
                     paste0("20", Y))
  ) %>%
  unite(col = "DOB", `M`:`Y`, sep = "/", remove = TRUE, na.rm = TRUE) %>%  
  mutate(DOB = as.Date(DOB, "%m/%d/%Y") ,
         
         Age = floor(
           age_calc(DOB,
                    as.Date("2020-04-20"), 
                    units = "years")
         )
  ) %>%
  select(`Last Name`, DOB, Race, Ethnicity, Age) %>%
  mutate(Location = "Weld")

# weld

##################
# Boulder


boulder_file <- read_csv("Data Files/Completed/Boulder.csv")

boulder <- 
  boulder_file  %>%
  group_by(Name, Booked) %>%
  slice(1) %>%
  ungroup(Name, Booked) %>%
  separate(Name, c("Last Name", NA), sep = ",") %>%
  mutate(DOB = NA, Ethnicity = NA) %>%
  select(`Last Name`, DOB, Race, Ethnicity, Age) %>%
  mutate(Location = "Boulder")

# boulder

#######################
# Mesa

mesa_file <- read_csv("Data Files/Completed/Mesa.csv")

mesa <- 
  mesa_file  %>%
  select(Name = `Booking Inmate Global Subject`, 
         DOB = `Booking Inmate Global Subject Date Of Birth`, 
         Ethnicity = `Booking Inmate Global Subject Ethnicity`,
         Race = `Booking Inmate Global Subject Race`) %>%
  group_by(Name, DOB) %>%
  slice(1) %>%
  ungroup() %>%

  mutate(DOB = as.Date(DOB, "%m/%d/%Y") ,
         
         Age = floor(
           age_calc(DOB,
                    as.Date("2020-04-20"), 
                    units = "years")
         )
  ) %>%
  
  separate(Name, c("First Name", "Middle Name", "Last Name"), sep = " " , fill = "left", extra = "merge") %>%
  select(`Last Name`, DOB, Race, Ethnicity, Age) %>%
  mutate(Location = "Mesa")

# mesa 


#######################
# Fremont

fremont_file<- read_csv("Data Files/Completed/Fremont.csv")

fremont <- 
  fremont_file  %>%
  group_by(InmateName, DOB) %>%
  slice(1) %>%
  ungroup() %>%
  
  separate(DOB, c("M", "D", "Y"), sep = "/") %>%
  mutate( Y = ifelse(Y > 20, 
                     paste0("19", Y),
                     paste0("20", Y))
  ) %>%
  unite(col = "DOB", `M`:`Y`, sep = "/", remove = TRUE, na.rm = TRUE) %>%  
  
  mutate(
        DOB = as.Date(DOB, "%m/%d/%Y") 
          ) %>%
  
  separate(InmateName, c("Last Name", NA), sep = "," , extra = "drop") %>%
  mutate(Ethnicity = NA) %>%
  select(`Last Name`, DOB, Race, Ethnicity, Age) %>%
  mutate(Location = "Fremont")

# fremont 



#######################
# Denver

denver_file <- read_csv("Data Files/Completed/Denver.csv")

numbermonths <- data.frame(Month = c( "Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec"),
                           M = 1:12)


denver <- 
  denver_file  %>%
  group_by(`First Name`, `Last Name`, `Birth Date`) %>%
  slice(1) %>%
  ungroup() %>%
  rename(DOB = `Birth Date`) %>%

  separate(DOB, c("D", "Month", "Y"), sep = "-") %>%
  inner_join(numbermonths) %>%
  mutate( Y = ifelse(Y > 20, 
                     paste0("19", Y),
                     paste0("20", Y)),
       
  )   %>%
  unite(col = "DOB", c("M", "D", "Y"), sep = "/", remove = TRUE, na.rm = TRUE)  %>%  
  
  mutate(
      DOB = as.Date(DOB, "%m/%d/%Y"),
    Age = floor(
      age_calc(DOB,
               as.Date("2020-04-20"), 
               units = "years")
    )
    
  ) %>%
  
  select(`Last Name`, DOB, Race, Ethnicity, Age) %>%
  mutate(Location = "Denver")

# denver 

#####################
# La Plata

laplata <- read_csv("Data Files/Completed/La Plata.csv")

######################
# Logan

# pdf_subset('Data Files/Problem Children/Logan.pdf',
#           pages = 1, output = "Logan1.pdf")

# pdf_subset('Data Files/Problem Children/Logan.pdf',
#           pages = 2, output = "Logan2.pdf")

logan_file <- read_csv("Data Files/Completed/Logan.csv")

logan <-
  logan_file %>%
      separate(Name, c("First Name", "Middle Name", "Last Name"), sep = " " , fill = "left", extra = "merge") %>%
      separate(DOB, c("DOB", NA), sep = " ") %>% 
      separate(DOB, c("M", "D", "Y"), sep = "/") %>%
      mutate( Y = ifelse(Y > 20, 
                     paste0("19", Y),
                     paste0("20", Y))
              ) %>%
      unite(col = "DOB", `M`:`Y`, sep = "/", remove = TRUE, na.rm = TRUE) %>% 
      mutate(DOB = as.Date(DOB, "%m/%d/%Y") ,
         
            Age = floor(
                 age_calc(DOB,
                    as.Date("2020-04-20"), 
                    units = "years")
            )
            )  %>%
      select(`Last Name`, DOB, Race, Ethnicity = ETHNICITY, Age) %>%
      mutate(Location = "Logan")






######################
# Washington

# pdf_subset('Data Files/Problem Children/Washington.pdf',
#           pages = 1, output = "Washington1.pdf")

# pdf_subset('Data Files/Problem Children/Washington.pdf',
#           pages = 2, output = "Washington2.pdf")

# pdf_subset('Data Files/Problem Children/Washington.pdf',
#           pages = 3, output = "Washington3.pdf")

# pdf_subset('Data Files/Problem Children/Washington.pdf',
#           pages = 4, output = "Washington4.pdf")


washington_file <- read_csv("Data Files/Completed/Washington.csv")

washington <- 
washington_file %>%
  separate(DOB, c("M", "D", "Y"), sep = "/") %>%
  mutate( Y = ifelse(Y > 20, 
                     paste0("19", Y),
                     paste0("20", Y))
        ) %>%
  unite(col = "DOB", `M`:`Y`, sep = "/", remove = TRUE, na.rm = TRUE) %>% 
  mutate(DOB = as.Date(DOB, "%m/%d/%Y") ,
         
         Age = floor(
           age_calc(DOB,
                    as.Date("2020-04-20"), 
                    units = "years")
         )
  ) %>%
  mutate(`Last Name` = NA) %>%
  select(`Last Name`, DOB, Race, Ethnicity = `Ethnic Origin`, Age) %>%
  mutate(Location = "Washington")

##########################
person_data <- 
bind_rows(arapahoe, larimer, jefferson, douglas, Adams1, elpaso, weld, boulder, mesa, fremont, denver, logan, washington)


write.csv(person_data, file = "Data Files/jail_pop_cleaned.csv")



