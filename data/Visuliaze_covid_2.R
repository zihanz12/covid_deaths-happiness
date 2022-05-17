# Read Files
covid = read.csv("covid_country_daily_excess_deaths.csv")
covid[covid$country=='Taiwan',]$region = "Asia"
covid[covid$country=='Taiwan',]$subregion = "Eastern Asia"

happiness = read.csv("World Happiness Report (2021).csv")

# Match the countries
country = unique(happiness$Entity)
covid1 = covid[covid$country %in% country,]

# 18 sub regions
unique(covid1$subregion)
covid1$date = as.Date(covid1$date)
covid1$country = as.factor(covid1$country)

## Southern Asia
ggplot(covid1[covid1$subregion=="Eastern Asia",],aes(x=date,y=daily_covid_cases_per_100k,color=country))+
  geom_point(size = 0.8)+
  labs(title = 'Daily Covid Cases in Eastern Asia')+
  theme_bw()+
  scale_x_date(date_breaks = '1 week')+
  theme(axis.text.x = element_text(size = 5,angle = 90, hjust = 0.3, vjust = 0.3),legend.position="bottom")

ggplot(covid1[covid1$subregion=="Eastern Asia",],aes(x=date,y=daily_covid_deaths_per_100k,color=country))+
  geom_point(size = 0.8)+
  labs(title = 'Daily Covid Deaths in Eastern Asia')+
  theme_bw()+
  scale_x_date(date_breaks = '1 week')+
  theme(axis.text.x = element_text(size = 5,angle = 90, hjust = 0.3, vjust = 0.3),legend.position="bottom")

# Calculate the total number of cases and deaths in 2020 and 2021
## split date into year,month,day
covid1 = separate(data = covid, col = date, into = c("Year", "Month","day"), sep = "-")
covid_2020 = covid1[covid1$Year==2020,]
covid_2021 = covid1[covid1$Year==2021,]

covid_2020 = data.table(covid_2020)
covid_2021 = data.table(covid_2021)

covid_2020_total = covid_2020[,.(total_cases = sum(daily_covid_cases,na.rm = TRUE),total_deaths = sum(daily_covid_deaths,na.rm = TRUE),
                   total_cases_per_100k = sum(daily_covid_cases_per_100k,na.rm = TRUE),total_deaths_per_100k = sum(daily_covid_deaths_per_100k,na.rm = TRUE)),
                   by = country][order(total_deaths,total_cases,decreasing = TRUE)]


covid_2021_total = covid_2021[,.(total_cases = sum(daily_covid_cases,na.rm = TRUE),total_deaths = sum(daily_covid_deaths,na.rm = TRUE),
                   total_cases_per_100k = sum(daily_covid_cases_per_100k,na.rm = TRUE),total_deaths_per_100k = sum(daily_covid_deaths_per_100k,na.rm = TRUE)),
                   by = country][order(total_deaths,total_cases,decreasing = TRUE)]

write.csv(x = covid_2020_total,file = "covid_2020_total.csv")
write.csv(x = covid_2021_total,file = "covid_2021_total.csv")

