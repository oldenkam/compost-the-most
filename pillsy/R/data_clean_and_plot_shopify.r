library(dplyr)
library(data.table)
library(stringr)

orders <-
  fread('/Users/adamoldenkamp/Desktop/Monthly Reporting/Shopify/orders_export.csv') %>%
  tbl_df %>%
  select('Created at', Subtotal, 'Lineitem quantity', 'Lineitem name')

orders <- filter(orders, !str_detect(tolower(orders$`Lineitem name`), 'battery'))
orders <- filter(orders, !str_detect(tolower(orders$`Lineitem name`), 'countertop display'))
orders <- filter(orders, !str_detect(tolower(orders$`Lineitem name`), 'bands'))
orders <- filter(orders, !str_detect(tolower(orders$`Lineitem name`), 'return shipping labels'))
orders <- filter(orders, !str_detect(tolower(orders$`Lineitem name`), 'scanner'))
orders <- filter(orders, !str_detect(tolower(orders$`Lineitem name`), 'compatible bottles'))

orders$pack <- 1
orders <- mutate(orders, pack = ifelse(str_detect(tolower(orders$`Lineitem name`), '2-pack'), 2, pack))
orders <- mutate(orders, pack = ifelse(str_detect(tolower(orders$`Lineitem name`), '3-pack'), 3, pack))
orders <- mutate(orders, pack = ifelse(str_detect(tolower(orders$`Lineitem name`), '4-pack'), 4, pack))
orders <- mutate(orders, pack = ifelse(str_detect(tolower(orders$`Lineitem name`), '8-pack'), 8, pack))
orders <- mutate(orders, pack = ifelse(str_detect(orders$`Lineitem name`, 'Retail Case Pack'), 8, pack))


orders[which(orders$Subtotal == 10000), which(colnames(orders) == 'Lineitem quantity')] <- 110


orders <-
  mutate(
    orders,
    sample_order = str_detect(orders$'Lineitem name', 'Sample'),
    sample_volume = sample_order * `Lineitem quantity` * pack
  )
orders <-
  mutate(
    orders,
    replacement_order = str_detect(orders$'Lineitem name', 'Pillsy Replacement'),
    replacement_volume = replacement_order * `Lineitem quantity` * pack
  )

orders <-
  mutate(
    orders,
    product_order = !(replacement_order | sample_order),
    product_volume = product_order * `Lineitem quantity` * pack
  )

orders$Month_Yr <- format(as.Date(orders$'Created at'), "%Y-%m")



orders <- group_by(orders, by = Month_Yr)
orders <- summarize(orders, samples_sent_via_shopify = sum(sample_volume), replacements_sent_via_shopify = sum(replacement_volume), products_sent_to_pilots_and_fulfillment_via_shopify = sum(product_volume))

write.csv(orders, '/Users/adamoldenkamp/Desktop/Monthly Reporting/Shopify/shopify_sent.csv')






