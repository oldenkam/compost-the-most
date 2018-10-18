library(dplyr)
library(data.table)
library(stringr)
library(ggplot2)
library(lubridate)

orders <-
  fread('/Users/adamoldenkamp/Desktop/Monthly Reporting/Amazon/Amazon FBA Returns - Master.csv') %>%
  tbl_df

orders$date <- as.Date(orders$`date/time`, '%b %d, %Y')
orders <- orders %>% mutate(month = format(date, "%m"), year = format(date, "%Y"))


orders$date_group <- format(as.Date(orders$date), "%b-%y")
orders <- select(orders, -`date/time`)

orders <- group_by(orders, year, month)
orders <- summarise(orders, units_ordered = sum(`Medicine Units Ordered`, `Vitamin Units Ordered`), units_returned = sum(`Medicine Units Returned`, `Vitamin Units Returned`), returns_perc = units_returned/units_ordered * 100)

orders$date <- as.Date(with(orders, paste(year, month, '1', sep="-")), "%Y-%m-%d")

#orders$date_group <- as.Date(orders$date_group, "%m-%Y")



orders$two_month_trailing_return_rate <- 'NA'


i <- 1
while(i <= nrow(orders)){
  if(i >= 3){
    mon_0now_ordered <- orders[i, which(colnames(orders) == "units_ordered")]
    mon_0now_returned <- orders[i, which(colnames(orders) == "units_returned")]
    mon_1ago_ordered <- orders[i - 1, which(colnames(orders) == "units_ordered")]
    mon_1ago_returned <- orders[i - 1, which(colnames(orders) == "units_returned")]
    mon_2ago_ordered <- orders[i - 2, which(colnames(orders) == "units_ordered")]
    mon_2ago_returned <- orders[i - 2, which(colnames(orders) == "units_returned")]
  
    two_month_trailing <- (sum(mon_0now_returned, mon_1ago_returned, mon_2ago_returned)/3)/(sum(mon_0now_ordered, mon_1ago_ordered, mon_2ago_ordered)/3)
    #print(two_month_trailing)
    orders[i, which(colnames(orders) == "two_month_trailing_return_rate")] <- two_month_trailing * 100
  }
  i <- i + 1
}

orders$two_month_trailing_return_rate <- as.double(orders$two_month_trailing_return_rate)



lbls <- paste0(month.abb[month(orders$date)], " ", lubridate::year(orders$date))
brks <- orders$date

fit <- glm(orders$two_month_trailing_return_rate~orders$date)
co <- coef(fit)
abline(fit, col="blue", lwd=2)

orders$two_month_trailing_return_rate <- round(orders$two_month_trailing_return_rate, 2) 


# plot
ggplot(orders, aes(x=date)) + 
  geom_line(aes(y=two_month_trailing_return_rate), na.rm = TRUE, col="blue") + 
  geom_point(aes(y=two_month_trailing_return_rate), col="blue") + 
  geom_text(aes(y=two_month_trailing_return_rate, label=paste0(two_month_trailing_return_rate, "%")), hjust=1, vjust=-1) +
  #geom_text(aes(label=two_month_trailing_return_rate)) +
  abline(fit, col="blue", lwd=2) +
  labs(title="Monthly Time Series", 
       subtitle="Returns Percentage for Amazon FBA", 
       caption="Source: Amazon FBA", 
       y="Returns %") +  # title and caption
  scale_y_continuous(limit = c(0, 30)) + 

  scale_x_date(labels = lbls, 
               breaks = brks) +  # change to monthly ticks and labels
  theme(axis.text.x = element_text(angle = 90, vjust=0.5),  # rotate x axis text
        panel.grid.minor = element_blank())  # turn off minor grid
