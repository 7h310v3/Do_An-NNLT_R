setwd("C:\\Users\\DUC-PC\\Downloads\\COVID-19-master\\COVID-19-master\\csse_covid_19_data\\csse_covid_19_daily_reports_us")
#mở file csv bằng notepad để xem phân cách giữa các số liệu là gì?
#install va update thu vien 
update.packages("tools")
install.packages("ggplot2", lib="C:/Users/DUC-PC/Documents/R/win-library/3.3")
update.packages("ggplot2")
update.packages("data.table")
library(data.table)  

#
files <- list.files(pattern = ".csv")
temp <- lapply(files, fread, sep=",")
data <- rbindlist( temp, fill=TRUE)
names(data)
View(data)

#Tao cac list va dataframe ve so luong nguoi nhiem covid, nguoi chet
#do covid, nguoi da hoi phuc, ti le tu vong
#ung voi tung ngay o bang Alabama o US
days <- c()
deaths <- c()
confirmed <- c()
recovered <- c()
cfr <- c()
n <- 0
data$Last_Update[1]
for (i in 1:length(data$Province_State)){
  if (data$Province_State[i] == "Alabama"){
    deaths[n] <- data$Deaths[i]
    days[n] <- data$Last_Update[i]
    confirmed[n] <- data$Confirmed[i]
    recovered[n] <- data$Recovered[i]
    cfr[n] <- data$Case_Fatality_Ratio[i]
    n <- n+1
  }
} 

#tao dataframe
dt <- data.frame(days, confirmed, deaths, recovered, cfr)

#doc file du lieu ve covid cua cac bang/thanh pho
#tai My ngay 5/12/2020
df <- read.table("05-12-2020.csv", 
                 header = TRUE,
                 sep = ",")
names(df)
#dung thu vien ggplot2 de ve do thi
library("ggplot2", lib.loc="~/R/win-library/4.0")

#Do thi the hien so luong nguoi chet boi covid theo tung ngay o bang Alabama
#layers trong ggplot2 goi la 'geoms'
#su dung goem_point 
#do thi 1
ggplot(dt, aes(x=days, y=deaths, color=deaths)) + geom_point() + labs(title="Số lượng người chết bởi covid ở Alabama 
                                                                      theo từng ngày", x="Date", y="Deaths")
#do thi 2
#tuong tu nhu tren nhung co tinh tham my ben trong goems, them goem_smooth
ggplot(dt, aes(x=days, y=deaths)) + geom_point(aes(color=deaths)) + geom_smooth() + labs(title="Số lượng người chết bởi covid ở Alabama 
                                                                                         theo từng ngày", x="Date", y="Deaths")

#do thi 3
#Su dung goem_line
ggplot(dt[1:11,], aes(x=days, y=deaths)) + geom_line(aes(x=days, y=deaths), colour = "purple") +
  labs(title="Số lượng người chết bởi covid ở Alabama 
       theo từng ngày", x="Date", y="Deaths")
N <- as.numeric(data$Last_Update[1])

#do thi 4
ggplot(dt) + geom_line(aes(x=days, y=cfr, color=cfr)) + 
  labs(title="Tỉ lệ tử vong bởi covid ở Alabama 
             theo từng ngày", x="Date", y="Deaths")

#do thi 5
#Su dung ggplot ve pie chart
ggplot(df[5:11,], aes(x='', y=Deaths, fill=Province_State)) +
  geom_bar(stat="identity", width=1) +
  coord_polar("y", start=0)+
  theme_void() + labs(title="Số lượng người chết bởi covid 19 một số bang/thành phố
                        trong ngày 2021-01-01")  +
  geom_text(aes(label = paste0(Deaths)), position = position_stack(vjust=0.5)) 

#do thi 6
#Ve heatmap
ggplot(df[1:11], aes(x=Deaths, y=Province_State)) +
  geom_tile(aes(fill = Deaths), colour = "red") + 
  labs(title="Số lượng người chết bởi covid 19 ở các bang/thành phố
              trong ngày 2021-01-01",x = "Deaths", y="Province")

# do thi 6
ggplot(df, aes(x=Deaths, y=Province_State)) + 
  geom_point(aes(color=Deaths), colour = "orange") + 
  labs(title="Số lượng người chết bởi covid 19 ở các bang/thành phố
       trong ngày 2021-01-01",x = "Deaths", y="Province")


#do thi 7
ggplot(dt1, aes(x=deaths_ps, color=province)) +
  geom_histogram(fill="white", bins = 30) + 
  labs(title="Số lượng người chết bởi covid 19 ở các bang/thành phố
       trong ngày 2021-01-01")

#do thi 8
ggplot(dt1, aes(x=confirmed_ps, y=province, fill= confirmed_ps)) + 
  geom_point(aes(color=confirmed_ps)) + 
  labs(title="Số lượng người được xác nhận nhiễm covid 19 
       ở các bang/thành phố trong ngày 2021-01-01",x = "Confirmed", y="Province")

#do thi 9
ggplot(dt1, aes(x=recovered_ps, y=province, fill= recovered_ps)) + 
  geom_point(aes(color=recovered_ps)) + 
  labs(title="Số lượng người được xác nhận đã hồi phục sau khi nhiễm covid 19 
       ở các bang/thành phố trong ngày 2021-01-01",x = "Confirmed", y="Province")

#do thi 10
ggplot(dt1, aes(x='', y=confirmed_ps, fill=province)) +
  geom_bar(stat="identity", width=1) +
  theme_void() +
  coord_polar("y", start=0)+ labs(title="Số lượng người được xác nhận nhiễm covid 19 ở các bang/thành phố
                                  trong ngày 2021-01-01")

#do thi 11 
ggplot(dt1, aes(x=cfr_ps, y=province, color=cfr_ps)) + geom_point() + labs(title="Tỉ lệ tử vong bởi covid 19
                                                                           ở các bang/T=thành phố ngày 2021-01-01", x="Case Fatality Ratio", y="Province")

#do thi 12
ggplot(dt) + geom_line(aes(x=days, y=cfr, color=cfr)) + 
  labs(title="Tỉ lệ tử vong bởi covid ở Alabama 
       theo từng ngày", x="Date", y="Deaths")

#do thi 13
ggplot(dt1[1:11,], aes(x='', y=cfr_ps, fill=province)) +
  geom_bar(stat="identity", width=1) +
  theme_void() +
  coord_polar("y", start=0) + 
  labs(title="Tỉ lẹ tử vong bởi covid 19 ở một số bang/thành phố
                          ở Mỹ trong ngày 2021-01-01")

#do thi 14
ggplot(dt1[25:36,], aes(x='', y=recovered_ps, fill=province)) +
  geom_bar(stat="identity", width=1) +
  theme_void() +
  coord_polar("y", start=0) + 
  labs(title="Số lượng người hồi phục sau khi nhiễm covid 19 ở một số bang/thành phố 
            tại Mỹ trong ngày 2021-01-01")