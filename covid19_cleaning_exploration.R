# Load the library
library(tidyverse) # This is all I need for cleaning and exploration
library(egg)


#read in data
df<-read_csv("CovidData.csv")


# The dataset is from a lab...
spec(df)

# Data type conversion 
df<-df |> 
  mutate(gender = as.factor(gender),
         date_collected = as.POSIXct(date_collected, format = "%d/%m/%Y"),
         HGB = as.numeric(HGB),
         PCR_outcome = as.factor(PCR_outcome))

str(df)

# Check for missing observations per column
sapply(df, function(x) {
  sum(is.na(x))
})  

# Just a missing observation in age. We can omit this.
df<- na.omit(df)

# Explore-------####
#-------Line/Area Chart

df |> 
  select(date_collected, PCR_outcome) |> 
  mutate(date_collected = as.Date(date_collected),
         date_collected = strftime(date_collected, format = "%Y-%m")) |>
  group_by(date_collected, PCR_outcome) |> 
  count()  |> 
  mutate(date_collected = as.Date(paste0(date_collected,"-01"))) |> 
  
  # Plot the chart
  ggplot(aes(x= date_collected, y= n, fill = PCR_outcome))+
  geom_area(alpha = .8)+
  geom_line(position = 'stack', size = 1, alpha = .5, color = 'white')+
  scale_x_date(date_labels = "%b-%Y")+
  scale_fill_manual(values = c("#FFBA08","#F48606"))+
  theme_bw()+
  labs(x = "Date Collected", y = "Number of Patients",
       title = "Number of patients/month")+
  theme(legend.position = "top",
        legend.title = element_text(size=10))


#------ Doughnut1 (Gender)
dd<- df |>  
  select(gender) |> 
  count(gender, sort = TRUE) 


dd$frac=dd$n/sum(dd$n)
dd$ymax = cumsum(dd$frac)
dd$ymin = c(0,head(dd$ymax, n = -1))

# Gender Distribution
p1<- dd %>% 
  ggplot(aes(ymax = ymax, ymin = ymin, xmax = 4, xmin = 3, fill = gender))+
  geom_rect()+
  ggtitle("Gender Distribution") +
  coord_polar(theta = "y")+
  xlim(c(1.8,4))+ theme_void()+
  theme(legend.position = "left",
        legend.key.size = unit(.8,"cm"),
        legend.text = element_text(size=8),
        legend.title = element_text(size=10))+
  labs(fill = "Gender")+
  scale_fill_manual(values = c("#FFBA08","#F48606"))


#------ Doughnut2 (Status)
dn<- df |>  
  select(PCR_outcome) |> 
  count(PCR_outcome, sort = TRUE) 


dn$frac=dn$n/sum(dn$n)
dn$ymax = cumsum(dn$frac)
dn$ymin = c(0,head(dn$ymax, n = -1))

# Gender Distribution
p2<- dn %>% 
  ggplot(aes(ymax = ymax, ymin = ymin, xmax = 4, xmin = 3, fill = PCR_outcome))+
  geom_rect()+
  ggtitle("Covid Detected?") +
  coord_polar(theta = "y")+
  xlim(c(1.8,4))+ theme_void()+
  theme(legend.position = "right",
        legend.key.size = unit(.8,"cm"),
        legend.text = element_text(size=8),
        legend.title = element_text(size=10))+
  labs(fill = "Status")+
  scale_fill_manual(values = c("#9D0208","#F48C06"))


az<- ggarrange(p1,p2, ncol = 2, padding = unit(2, "cm"))

#----- Bar chart 1
df |> 
  ggplot(aes(x= PCR_outcome, fill = gender))+
  geom_bar(stat = "count", position = "dodge")+
  scale_fill_manual(values = c("#FFBA08","#F48606"))+
  theme_bw()+
  theme(legend.position = "top")+
  labs(x = "Status", y = "Number of Patients",
       title = "Distribution of patients and covid status")


#----- Age Groups

df |> 
  select(age) |> 
  mutate(ageGroup = case_when(age<=12~ "child",
                              age>12 & age<=19 ~ "teenager",
                              age>19 & age <=30 ~ "young adults",
                              age>30 & age <= 45 ~ "middle aged adults",
                              age>45 & age <= 60 ~ "elderly",
                              TRUE ~ "old")) |> 
  mutate(ageGroup = factor(ageGroup, levels = c("child","teenager","young adults",
                                                "middle aged adults", "elderly",
                                                "old"))) |> 
  count(ageGroup) |> 
  
  # Make the plot
  ggplot(aes(x = reorder(ageGroup, n), y = n, fill = n))+
  geom_bar(stat = "identity")+
  scale_fill_gradient(low = "#FFBA08", high = "#370617")+
  labs(x = NULL, y = "count", title = "Age group distribution")+
  coord_flip()+
  theme_bw()+
  theme(legend.position = "none")

#------ Basophils
df |> 
  ggplot(aes(x=PCR_outcome, y = Basophils, fill = PCR_outcome))+
  geom_bar(stat = "summary", fun = "mean", width = .5)+
  scale_fill_manual(values = c("#FFBA08","#F48606"))+
  scale_x_discrete(expand = c(1.5,0.5))+
  theme_bw()+
  theme(legend.position = "none")+
  labs(x = "Covid status", 
       title = "Average basophils level for Covid status")
  
  
#------ total white blood cell count
df |> 
  ggplot(aes(x=PCR_outcome, y = Total_WBC_Count, fill = PCR_outcome))+
  geom_bar(stat = "summary", fun = "mean", width = .5)+
  scale_fill_manual(values = c("#9D0208","#DC2F02"))+
  scale_x_discrete(expand = c(1.5,0.5))+
  theme_bw()+
  theme(legend.position = "none")+
  labs(x = "Covid status", y = "Total WBC count",
       title = "The average total WBC count for covid status")


#---- Total white blood cell count by age group
df |> 
  select(age, Total_WBC_Count) |> 
  mutate(ageGroup = case_when(age<=12~ "child",
                              age>12 & age<=19 ~ "teenager",
                              age>19 & age <=30 ~ "young adults",
                              age>30 & age <= 45 ~ "middle aged adults",
                              age>45 & age <= 60 ~ "elderly",
                              TRUE ~ "old")) |> 
  
  ggplot(aes(x=reorder(ageGroup, Total_WBC_Count), y = Total_WBC_Count, fill = ageGroup))+
  geom_bar(stat = "summary", fun = "mean", width = .6)+
  scale_fill_manual(values = c("#03071E","#370617","#6A040F",
                               "#9D0208","#F48C06","#FFBA08"))+
  theme_bw()+
  theme(legend.position = "none")+
  labs(x = "Age group", y = "Total WBC count",
       title = "The average total WBC count for covid status")+
  coord_flip()
  

