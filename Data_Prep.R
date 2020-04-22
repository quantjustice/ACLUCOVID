library(openxlsx)
library(tidyverse)


setwd("/Volumes/GoogleDrive/My Drive/QJL/ACLU COVID Investigation")

current_pop <- read_csv("Data Files/jail_pop_cleaned.csv") %>% select(-X1)

laplata_pueblo <- read_csv("Data Files/Completed/La Plata.csv") %>%
  mutate(
         Percent = round(Count/Total*100,1) )

jan_pop <- read_csv("Data Files/HB19-1297Data .csv")


#####
# Who is  Over 65? 




###################
# Calculate Demographic Percentages for Each Jail

levels(as.factor(current_pop$Ethnicity))

current_pop %>%
  filter(Ethnicity %in% c("AFRICAN", "ASIAN", "FUGITIVE FROM JUSTICE", "WHITE"))

current_pop %>% 
  filter(Race %in% c("C", "P", "S", "H")) #%>% View()


levels(as.factor(current_pop$Race))

Denver <-
  cleaned_current_pop %>%
  filter(Location == "Denver")
Denver$Race


cleaned_current_pop <- 
current_pop %>% 
  mutate(
    
    Over60 = ifelse(Age >= 60, "60+", "<60"),

    Ethnicity = case_when(
      is.na(Ethnicity) == TRUE ~ "UNKNOWN",
      Ethnicity == "UNKNOWN"  ~ "UNKNOWN",
      Ethnicity == "U"  ~ "UNKNOWN",
      Ethnicity == "FUGITIVE FROM JUSTICE" ~ "UNKNOWN",
      Ethnicity == "UNKNOW N" ~ "UNKNOWN",
      Ethnicity == "Unknown" ~ "UNKNOWN",
      Ethnicity == "NULL" ~ "UNKNOWN",
      Ethnicity == "NULL" ~ "UNKNOWN",
      Ethnicity == "WHITE" ~ "UNKNOWN",
      Ethnicity == "P" ~ "UNKNOWN",
      Ethnicity == "S" ~ "UNKNOWN",

      Ethnicity == "AFRICAN" ~ "NON-HISPANIC",
      Ethnicity == "ASIAN" ~ "NON-HISPANIC",
      Ethnicity == "Non-Hispanic" ~ "NON-HISPANIC",
      Ethnicity == "OINVUSIH-NON" ~ "NON-HISPANIC",
      Ethnicity == "N" ~ "NON-HISPANIC",
      Ethnicity == "NON-HISPANIC" ~ "NON-HISPANIC",

      Ethnicity == "HISPANIC" ~ "HISPANIC",
      Ethnicity == "DINVASIH" ~ "HISPANIC",
      Ethnicity == "H" ~ "HISPANIC",
      Ethnicity == "HISPANIC/LATINO" ~ "HISPANIC",
      Ethnicity == "HISPANIĆ" ~ "HISPANIC",
      Ethnicity == "Hispanic" ~ "HISPANIC"
    ),
    
    Race = case_when(
      
      is.na(Race) == TRUE ~ "UNKNOWN",
      Race == "UNKNOWN" ~ "UNKNOWN",
      Race == "NULL" ~ "UNKNOWN",
      Race == "U" ~ "UNKNOWN",
      Race == "Unknown" ~ "UNKNOWN",
      
      Race == "NATIVE AMERICAN" ~ "NATIVE AMERICAN",
      Race == "AMER INDIAN/ALASKAN" ~ "NATIVE AMERICAN",
      Race == "American Indian, Alaskan Native" ~ "NATIVE AMERICAN",
      Race == "AMERICAN INDIAN/ALASKAN NATIVE" ~ "NATIVE AMERICAN",
      Race == "I" ~ "NATIVE AMERICAN",
      Race == "INDIAN" ~ "NATIVE AMERICAN",
      Race == "INDIAN/ALASKA NATIVE" ~ "NATIVE AMERICAN",
      
      Race == "OTHER" ~ "OTHER",
      Race == "A" ~ "OTHER",
      Race == "ASIAN" ~ "OTHER",
      Race == "Asian / Pacific Islander" ~ "OTHER",
      Race == "ASIAN OR PACIFIC ISLANDER" ~ "OTHER",
      Race == "ASIAN PACIFIC ISLANDER" ~ "OTHER",
      
      Race == "WHITE" ~ "WHITE",
      Race == "C" ~ "WHITE",
      Race == "H" ~ "WHITE",
      Race == "W" ~ "WHITE",
      Race == "W   " ~ "WHITE",
      Race == "W HITE  " ~ "WHITE",
      Race == "White" ~ "WHITE",
      Race == "White  " ~ "WHITE",
      Race == "WHITE\n-" ~ "WHITE",
      Race == "WHITE." ~ "WHITE",
      
      Race == "BLACK" ~ "BLACK",
      Race == "B" ~ "BLACK",
      Race == "Black" ~ "BLACK",
      Race == "BLACK/AFRICAN AMER" ~ "BLACK",
      Race == "BLACK/AFRICAN AMERICAN" ~ "BLACK"
     )
 )
  

current_stats <- 
cleaned_current_pop %>%
  select(-c(DOB, `Last Name`, Age))  %>%
  gather(Demographic, Level, -Location) %>%
  group_by(Location, Demographic) %>%
  add_count() %>%
  group_by(Location, Demographic,  Level) %>%
  summarize(Total = mean(n), Count = n()) %>%
  mutate(Percent = round(Count/Total*100,1)) %>%
  bind_rows(
    laplata_pueblo
  ) 



##########
counties <- levels(as.factor(current_stats$Location))

jan_pop_stats <-
jan_pop %>%
  filter(Measure %in% c("Average Daily Population", "Number of inmates") ) %>%
  select(County, 
         Measure,
         Race_BLACK =  Black, 
         `Race_NATIVE AMERICAN` = `Native American`, 
         Race_OTHER = `Other Race`,
         Race_WHITE = White,
         Race_UNKNOWN = `Unknown Race`,
         `Ethnicity_NON-HISPANIC` = `Non-Hispanic`,
         Ethnicity_HISPANIC = Hispanic,
         Ethnicity_UNKNOWN = `Unknown Ethnicity`
         ) %>%
  filter(County %in% counties) %>%
  gather(Demographic, Count, -c(County, Measure) ) %>%
  separate(Demographic, c("Demographic", "Level"), sep = "_") %>%
  arrange(County, Demographic, Measure) %>% 
  group_by(County, Demographic, Measure) %>%
  mutate(Total = sum(Count)) %>%
  filter(Total > 0) %>%
  ungroup() %>%
  select(-Total) %>%
  spread(Measure, Count) %>%
  rename(Count = `Average Daily Population`) %>%
  mutate(Count = ifelse(is.na(Count), `Number of inmates`, Count)) %>%
  select(Location = County, Demographic, Level, Count) %>%
  group_by(Location, Demographic) %>%
  mutate(Total = sum(Count)) %>%
  ungroup() %>%
  mutate(Percent = round(Count/Total*100,1)) %>%
  mutate(Time = "January")
  
jan_pop_stats

jan_pop_stats %>% filter(Location == "Pueblo")
##########
race_ethnicity_current <- 
  current_stats %>%
  filter(!Demographic %in% c("Over60")) %>%
  mutate(Time = "April")


final_stats <- 
  race_ethnicity_current  %>%
  bind_rows(jan_pop_stats) %>%
  mutate(order = ifelse(Time == "April", 2, 1),
         order2 = 
           case_when(
             Level == "HISPANIC" ~ 1,
             Level == "NON-HISPANIC" ~ 2,
             Demographic == "Ethnicity" & Level == "UNKNOWN" ~ 3,
             
             Level == "WHITE" ~ 1,
             Level == "BLACK" ~ 2,
             Level == "NATIVE AMERICAN" ~ 3,
             Level == "OTHER" ~ 4,
             Demographic == "Race" & Level == "UNKNOWN" ~ 5
           ))

final_stats_printout <-
   final_stats %>%
  ungroup() %>%
   mutate(Display = paste0(Percent, "% (", Count, ")")) %>%
  select(Location, Demographic, Level, Display, Time) %>% 
  spread(Time, Display) %>%
  select(Location, Demographic, Level, January, April)

############
over60_numbers <-  
  current_stats %>%
  filter(Demographic %in% c("Over60")) 


above60 <- 
  cleaned_current_pop %>% 
  filter(Age >= 60)  %>%
  select(`Last Name`, DOB, Race, Ethnicity, Age, Location)


write.csv(above60 , file = "Results/popabove60.csv")

#############

final_stats


  



  
wb <- createWorkbook()

for (location in levels(as.factor(final_stats$Location ))[c(13)] ) {
  
  sheetname <- location 
  addWorksheet(wb, sheetname, gridLines = FALSE)
  
  over60output <-
    over60_numbers %>%
    filter(Location == location, Level == "60+")
  
  above60location <- above60 %>%
    filter(Location == location) %>%
    select(-Location)
  
  num60 <- paste0("There are ", over60output$Count, " people in ", location, " jail aged 60 and over" )
  percent60 <- paste0("This comprises ", over60output$Percent, "% of the jail population")
  
  writeData(wb, sheetname, sheetname, startCol = 2, startRow = 1)
  writeData(wb, sheetname, num60, startCol = 2, startRow = 3)
  writeData(wb, sheetname, percent60, startCol = 2, startRow = 4)
  writeData(wb, sheetname, "Those people are listed here:", startCol = 2, startRow = 6)
  writeData(wb, sheetname, above60location, startCol = 2, startRow = 8)

#################### 
  graph_data_race  <- 
     final_stats %>% 
     filter(Location == location, Demographic == "Race")  
  
p1<-  
  ggplot( graph_data_race , aes(x = reorder(Level, order2), y = Percent, fill = reorder(Time, order)) ) + 
    geom_bar(stat = "identity", position = "dodge", alpha = .8, color = "black") +
    ggtitle(paste0("Differences in Race Between January and April in ", location) )+
    theme(
      plot.title = element_text( face="bold", hjust = 0.5 ),
      legend.title = element_text(),
      axis.title.x = element_text(vjust=-.5),
      axis.title.y = element_text(vjust= 2),
      axis.text.x=element_text(vjust=-2),
      axis.ticks.x = element_blank()
      
    )+ 
    scale_y_continuous(labels = function(x) paste0(round(x), "%"), expand = c(0, 0 ), limits = c(-1, max(graph_data_race$Percent) + 5)) +
    geom_text(aes(label= paste0(round(Percent), "% (", round(Count), ")"), y = Percent + max(graph_data_race$Percent)/50), position = position_dodge(width = .9), size = 3  ) +
 #   geom_text(aes(label= paste0( "(n=", Total,")"), y = 0), position = position_dodge(width = .9), vjust = 1.5 ) +
    labs(x= "Race", y="Proportion of Jail Population, %", fill = "")  +
    coord_cartesian(clip = 'off')
  
     
###########  


graph_data_ethnicity  <- 
  final_stats %>% 
  filter(Location == location, Demographic == "Ethnicity")  

p2 <- 
ggplot( graph_data_ethnicity, aes(x = reorder(Level, order2), y = Percent, fill = reorder(Time, order)) ) + 
  geom_bar(stat = "identity", position = "dodge", alpha = .8, color = "black") +
  ggtitle(paste0("Differences in Ethnicity Between January and April in ", location) )+
  theme(
    plot.title = element_text( face="bold", hjust = 0.5 ),
    legend.title = element_text(),
    axis.title.x = element_text(vjust=-.5),
    axis.title.y = element_text(vjust= 2),
    axis.text.x=element_text(vjust=-2),
    axis.ticks.x = element_blank()
    
  )+ 
  scale_y_continuous(labels = function(x) paste0(round(x), "%"), expand = c(0, 0 ), limits = c(-1, max(graph_data_ethnicity$Percent) + 5)) +
  geom_text(aes(label= paste0(round(Percent), "% (", round(Count), ")"), y = Percent + max(graph_data_ethnicity$Percent)/50), position = position_dodge(width = .9), size = 3  ) +
#  geom_text(aes(label= paste0( "(n=", Total,")"), y = 0), position = position_dodge(width = .9), vjust = 1.5 ) +
  labs(x= "Ethnicity", y="Proportion of Jail Population, %", fill = "")  +
  coord_cartesian(clip = 'off')



########
jail_pops<-
graph_data_ethnicity[,c(2,4,7)] %>% unique() %>%
  bind_rows(
    graph_data_race[,c(2,4,7)] %>% unique()
  )  %>%
  spread(Time, Total) 
  

jail_pop <- paste0("In January, there were about ", 
                   max(jail_pops$January), " in ", 
                   location, " jail, while in April, there were about ",  
                   max(jail_pops$April))

writeData(wb, sheetname, jail_pop, startCol = 9, startRow = 2)

print(p1)
insertPlot(wb, sheetname, xy = c("T", 4), width = 10, height = 5,  fileType = "png", units = "in")

print(p2)
insertPlot(wb, sheetname, xy = c("H", 4), width = 7.5, height = 5,  fileType = "png", units = "in")
  
}



saveWorkbook(wb, "Results/COVIDTracking.xlsx", overwrite = TRUE)




