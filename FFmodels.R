library(tidyverse)

setwd("D://PhD Heriot-Watt//Publiplication//Does ESG provide any value proposition for ETF and Mutual Fund Manager")

FF5 <- read_csv("F-F_Research_Data_5_Factors_2x3_daily.csv")

y <- substr(FF5$Date, 1, 4)
m <- substr(FF5$Date, 5, 6)
d <- substr(FF5$Date, 7, 8)
FF5$Date <- sapply(seq_along(FF5$Date), function(i) paste0(y[i],"-",m[i],"-",d[i]))
FF5$Date <- as.Date(FF5$Date)


dataETF <- read_csv("ETFdata.csv")
ETF.close <- dataETF %>% select(c("ticker","ref_date", "price_close")) %>% 
  dcast(ref_date~ticker) %>% arrange(desc(ref_date)) %>% column_to_rownames('ref_date')


FF <- FF5 %>% filter(Date >= "2010-01-01")
FF$Date <- as.Date(FF$Date)
ICF <- ETF.close %>% select(c("ICF")) %>% rownames_to_column('Date')
ICF$Date <- as.Date(ICF$Date)
ICF <- data.frame(Date = ICF$Date[-1], ICF = diff(ICF$ICF))

bframe <- merge(FF, ICF, by ='Date', all = T) 

model <- lm(ICF ~ ., data = bframe)
summary(model)


ESG <- read_csv("ESGUScompanies.csv")

ggplot(data = ESG, aes(y = governanceScore.raw, group = Size, color = Size)) +
  geom_boxplot()


data <- yfR::yf_get(ticker = "MFV",
            first_date = Sys.Date() - 500,
            last_date = Sys.Date())
plot(data$price_close)


mega <- ESG %>% filter(Size == 'midcap') %>% arrange(totalEsg.raw)
Best <- head(mega, 10)
Worst <- tail(mega,10)

Best.Port <-  yfR::yf_get(ticker = Best$Symbol,
                          first_date = Sys.Date() - 500,
                          last_date = Sys.Date())
Worst.Port <- yfR::yf_get(ticker = Worst$Symbol,
                          first_date = Sys.Date() - 500,
                          last_date = Sys.Date())

Best.Port.Return <- Best.Port %>% select(c("ticker", "ref_date", "ret_adjusted_prices")) %>% dcast(ref_date~ticker) %>% drop_na()

Worst.Port.Return <- Worst.Port %>% select(c("ticker", "ref_date", "ret_adjusted_prices")) %>% dcast(ref_date~ticker) %>% drop_na()

Eq.Best <- rowSums(Best.Port.Return[,-1]/(ncol(Best.Port.Return)-1) )
Eq.Worst <- rowSums(Worst.Port.Return[,-1]/(ncol(Worst.Port.Return)-1) )

BMW <- Eq.Best - Eq.Worst 

BMW.F <- data.frame(Date = Best.Port.Return$ref_date, BMW)
BMW.F$Date <- as.Date(BMW.F$Date)

All <- merge(bframe, BMW.F, by = 'Date')
All$BMW <- All$BMW*100

ggplot(data = melt(All,id.vars = 'Date'), aes(x = Date, y = value, group = variable, color = variable)) +
geom_line()




levelplot(t(as.matrix(cor(All[,-1] %>% drop_na()))),
          col.regions = coul
          #panel = function(...){
          #panel.levelplot(...)
          #panel.abline(h = 2.5)
          #panel.abline(v = 2.5)
          #}
)


model <- lm(ICF ~ ., data = All[,-1])
summary(model)
