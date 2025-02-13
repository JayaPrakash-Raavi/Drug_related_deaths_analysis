library(tidyverse)


#----------------------------------
# data cleaning
#----------------------------------

dfv1 <- read.csv(file="Accidental_Drug_Related_Deaths_2012-2022 - v1.csv") |> as_tibble()
dim(dfv1) #10654x48
glimpse(dfv1)
dfv1 <- dfv1 |> mutate(Age=as.character(Age))

# check missing in key columns
dfv1 <- dfv1 |> mutate(across(everything(), ~na_if(.x , "")))
dfv1 |> filter(is.na(Date) | is.na(Date.Type) | is.na(Age) | is.na(Sex) | is.na(Race)) #38*48
dfv1 <- dfv1 |> filter(!is.na(Date) & !is.na(Date.Type) & !is.na(Age) & !is.na(Sex) & !is.na(Race)) #10616*48


dfv1 <- dfv1 |> mutate(Date=as_date(Date, format= "%m/%d/%Y"))
dfv1 <- dfv1 |> mutate(Age=as.integer(Age))


# write.csv(dfv1, "dfv1.csv")



#label opioids users

dfv2<- dfv1 |> mutate(type_opioids=as.character(NA))
for (i in 1:nrow(dfv2)) {
  if (!is.na(dfv2$Heroin[i]) | 
      !is.na(dfv2$Heroin.death.certificate..DC.[i]) | 
      !is.na(dfv2$Fentanyl[i]) | 
      !is.na(dfv2$Fentanyl.Analogue[i])| 
      !is.na(dfv2$Oxycodone)[i]| 
      !is.na(dfv2$Oxymorphone)[i]| 
      !is.na(dfv2$Hydrocodone)[i]| 
      !is.na(dfv2$Methadone)[i]| 
      !is.na(dfv2$Tramad)[i]| 
      !is.na(dfv2$Hydromorphone)[i]| 
      !is.na(dfv2$Morphine..Not.Heroin.)[i]| 
      !is.na(dfv2$Opiate.NOS)[i]| 
      !is.na(dfv2$Heroin.Morph.Codeine)[i]| 
      !is.na(dfv2$Other.Opioid)[i]) {
    dfv2$type_opioids[i]="Y"
      }
}

#label psycostimulants users

dfv2<- dfv2 |> mutate(type_stimulants=as.character(NA))
for (i in 1:nrow(dfv2)) {
  if (!is.na(dfv2$Cocaine[i]) | 
      !is.na(dfv2$Meth.Amphetamine[i]) | 
      !is.na(dfv2$Amphet[i])) {
    dfv2$type_stimulants[i]="Y"
  }
}



#label sedatives users

dfv2<- dfv2 |> mutate(type_sedatives=as.character(NA))
for (i in 1:nrow(dfv2)) {
  if (!is.na(dfv2$Ethanol[i]) | 
      !is.na(dfv2$Benzodiazepine[i]) | 
      !is.na(dfv2$Xylazine[i]) |
      !is.na(dfv2$Gabapentin[i])) {
    dfv2$type_sedatives[i]="Y"
  }
}

glimpse(dfv2)

dfv2 |> select (type_opioids) |> filter(type_opioids=="Y")  # 9659 cases
dfv2 |> select (type_stimulants) |> filter(type_stimulants=="Y") #4152 cases
dfv2 |> select (type_sedatives) |> filter(type_sedatives=="Y") #5343 cases

# write.csv(dfv2, "dfv2.csv")


#----------------------------------
# basic plots
#----------------------------------


temp_sequence <- dfv2 |> group_by(year(dfv2$Date),month(dfv2$Date)) |> summarise(count=n())
plot(temp_sequence)
table_yearmonth <- temp_sequence |> mutate(yearmonth=make_date(`year(dfv2$Date)`,`month(dfv2$Date)`))

# plot overall by gender

dfv2|> summarize(
  male= sum(Sex=="Male"),
  female= sum(Sex=="Female"),
  total= n()
) 

plot_overall_gender <- dfv2|> ggplot()+
  geom_bar(aes(x=reorder(Sex, -table(Sex)[Sex]), alpha=0.5), show.legend = FALSE)+
  labs(x="", y="overal death counts", title ="Overall drug related death trend by gender from 2012 to 2022")+
  theme_minimal()
plot_overall_gender





# plot overall by Race
plot_overall_race <- dfv2|> arrange(desc(Race)) |> 
  ggplot()+
  geom_bar(aes(x=reorder(Race, -table(Race)[Race]), fill=Sex), show.legend = T)+
   labs(x="", y="overal death counts", title ="Overall drug related death trend by race from 2012 to 2022")+
  theme_minimal()+
  theme(axis.text.x = element_text(angle = 90, hjust = 1))
plot_overall_race




# plot death trend
plot_trend_overall <- table_yearmonth|> ggplot(aes(x=yearmonth, y=count))+
  geom_line(alpha=0.5)+
  geom_smooth()+
  labs(x="", y="monthly deth counts", title = "Overall Trend of Monthly Drug Induced Death from 2012-2022")+
  theme_minimal()
plot_trend_overall






# plot trend by gender
Sex_df <- dfv2 |> mutate(yearmonth=make_date(year(Date),month(Date)), .before=1) |> 
  group_by(year(Date),month(Date)) |> 
  reframe( yearmonth,
 male=sum(Sex == "Male"), 
 female=sum(Sex == "Female")
) |> distinct()

plot_trend_gender <- Sex_df |> pivot_longer(
  cols = ends_with("male"),
  names_to = "gender",
  values_to = "counts"
) |> 
  ggplot()+
  geom_line(aes(x=yearmonth, y=counts, color=gender))+
  geom_smooth(aes(x=yearmonth, y=counts, color=gender))+
  labs(x="", y="monthly deth counts", title = "Overall Trend of Monthly drug induced Death for Gender")+
  theme_minimal()
plot_trend_gender






# plot overall trend by agegroup

age_df <- dfv2 |> mutate(
  age_group=cut(Age, 
            breaks = c(0, 18, 30, 40, 50, 60, 70, 80, Inf),
            labels = c("<18", "19-30", "31-40", "41-50", "51-60", "61-70", "71-80", ">80"),
            right = FALSE), yearmonth=floor_date(Date, "month"), .before=1
) 


plot_overall_age <- age_df |> 
  ggplot()+
  geom_bar(aes(x=age_group, fill=Sex), show.legend = T)+
  labs(x="age groups", y="overal death counts", title ="Overall drug related death trend by age from 2012 to 2022")+
  theme_minimal()+
  theme(axis.text.x = element_text(angle = 0, hjust = 1))
plot_overall_age


table_agegroup <- age_df |>   group_by(age_group) |> 
  summarise(
    counts=n()
  )



# plot trend by age;

plot_trend_age <- age_df |> group_by(yearmonth) |> 
  reframe(
  ag18=sum(age_group=="<18"),
  ag30=sum(age_group=="19-30"),
  ag40=sum(age_group=="31-40"),
  ag50=sum(age_group=="41-50"),
  ag60=sum(age_group=="51-60"),
  ag70=sum(age_group=="61-70"),
  ag80=sum(age_group=="71-80"),
  ag81up=sum(age_group==">80"),
) |> pivot_longer(cols = starts_with("ag"), 
                  names_to = "age_group",
                  values_to = "counts") |> 
  
  ggplot()+
  geom_line(aes(x=yearmonth, y=counts, color=age_group))+
  geom_smooth(aes(x=yearmonth, y=counts, color=age_group))+
  labs(x="", y="monthly death counts", title = "Trend of Monthly drug induced Death by ages")+
  theme_minimal()
plot_trend_age

# plot_trend_drugtype
glimpse(age_df)

table_yearmonth_drugtype <- age_df |>
  group_by(yearmonth) |> 
  reframe(
    type_opioids=sum(!is.na(type_opioids)),
    type_stimulants=sum(!is.na(type_stimulants)),
    type_sedatives=sum(!is.na(type_sedatives))
  )

table_yearmonth_drugtype |> 
  pivot_longer(cols = starts_with("type_"), 
               names_to = "drug_type",
               values_to = "counts") |> 
  ggplot()+
  geom_line(aes(x=yearmonth, y=counts, color=drug_type))+
  geom_smooth(aes(x=yearmonth, y=counts, color=drug_type))+
  labs(x="", y="monthly death counts", title = "Trend of Monthly drug induced Death by drug types")+
  theme_minimal()

#--------------------------------------------
#                        across(starts_with("type_"), n()),

age_df |>
