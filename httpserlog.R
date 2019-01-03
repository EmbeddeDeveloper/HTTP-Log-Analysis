# install.packages("tidyverse")
# install.packages("readr")
# devtools::install_github("tidyverse/readr")

setwd("C:/Users/Vasudeo/Documents/WE_assignments/Trim- 4/R/Lecture_8/filesforrfinalproject")

library(dplyr)
library(stringr)
library(ggplot2)
library(wordcloud)
library(tidyverse)

df_OG <- read_log("./data/apache_dataset.log",skip =0,col_names = FALSE)
#df <- subset( df_OG, select = -c(X2, X3 ) )
df <- rename(df_OG, RemoteHost=X1,Id=X2,HTTPUser=X3,DateTime=X4,RequestLine=X5,StatusCode=X6,Size=X7)
#write.csv(df,file = "Mydata.csv")
Hours <- format(strptime(df$DateTime,"%d/%B/%Y:%H:%M:%S") ,format = "%H:%M:%S")
Dates <- format(strptime(df$DateTime,"%d/%B/%Y:%H:%M:%S") ,format = "%d/%b/%Y")
df$Dates <- Dates
df$Hours <- Hours
df$Dates=as.Date(df$Dates,"%d/%b/%Y")
df <- subset( df, select = -DateTime)
ggplot(data=df, aes(x=format(StatusCode))) + geom_bar() + xlab("Status") + ylab("Count") + ggtitle("Status")

reqs = as.data.frame(table(df$Dates))
ggplot(data = reqs, aes(x=as.Date(Var1), y=Freq)) + geom_point() + geom_line(colour = "blue") + xlab("Month") + ylab("Requests") + labs(title="Traffic to Site")


cat("\014")
df_conn <- filter(df, StatusCode == "200")

df_conn <- data.frame(df_conn,
                    Year = as.numeric(format(df_conn$Dates, format = "%Y")),
                    Month = as.numeric(format(df_conn$Dates, format = "%m")),
                    Day = as.numeric(format(df_conn$Dates, format = "%d")))

thpd = as.data.frame(table(df_conn$Day,df_conn$Month))
thpd <- rename(thpd, Days=Var1,Months=Var2)
ggplot(data = thpd, aes(x=Days, y=Freq,group=Months,colour=Months)) + geom_point() + geom_line() + xlab("Day") + ylab("Freq") + labs(title="Total Hits Per Day")


####Q1####
cat("\014")
#View(data.frame(table(dfcts$Month)))
dfcts <- data.frame(table(df_conn$RemoteHost,df_conn$Month))
dfcts.Sort <- arrange(dfcts,desc(Freq))
dfcts.Sort <- rename(dfcts.Sort,Host=Var1,Month=Var2)
View(dfcts.Sort)
ggplot(data = dfcts.Sort, aes(x=Month, y=Freq)) + geom_bar(stat = "identity") + xlab("Months") + ylab("Freq") + labs(title="Connection to Server Per Month")

#####

####Q2####
cat("\014")
dfreq <- data.frame(table(df_conn$RequestLine,df_conn$Month))
dfreq.Sort <- arrange(dfreq,desc(Freq)) 
dfreq.Sort <- rename(dfreq.Sort,Page=Var1,Month=Var2)
View(dfreq.Sort)
######

####Q3###
cat("\014")
dfSize <- aggregate(Size ~ RemoteHost + Month, data = df_conn, sum)
dfSize <- filter(dfSize, !grepl("0",dfSize$Size))
dfSize.Sort <- arrange(dfSize,desc(Size))
View(dfSize.Sort)
ggplot(data=dfSize.Sort, aes(x=format(Month))) + geom_bar() + xlab("Months") + ylab("Download Size") + ggtitle("Total Download Size Per Month")

dfSizepd <- aggregate(Size ~ Day + Month, data = df_conn, sum)
dfSizepd <- filter(dfSizepd, !grepl("0",dfSizepd$Size))
ggplot(data = dfSizepd, aes(x=Day, y=Size,group=Month,colour=Month)) + geom_point() + geom_line() + xlab("Day") + ylab("Download Size") + labs(title="Total Download Size Per Day")
####

####Q4###
cat("\014")
dfPageSize <- aggregate(Size ~ RequestLine + Month, data = df_conn, sum)
dfPageSize <- filter(dfPageSize, !grepl("0",dfPageSize$Size))
dfPageSize.Sort <- arrange(dfPageSize,desc(Size))
View(dfPageSize.Sort)
ggplot(data=dfPageSize.Sort, aes(x=format(Month))) + geom_bar() + xlab("Months") + ylab("Data Sent Out") + ggtitle("Total Data Sent Out Per Month")
####

#Which host has connected the maximum number of times to our server? Give the host name & count of connections from that host.
RemoteHost.Sort <- data.frame(table(df_conn$RemoteHost))
r <- arrange(RemoteHost.Sort,desc(Freq))
View(r)
max(r$Freq)


#Which page that has been requested the maximum number of times from our server? Give the page name & count of the times the page was requested.
RequestLine.Sort <-data.frame(table(df_conn$RequestLine))
r <- arrange(RequestLine.Sort,desc(Freq))
r <- filter(r, !grepl("GET / HTTP/1.0",r$Var1))
View(r)
max(r$Freq)

#How many unique hosts have connected to our server? Give counts.
length(unique(df_conn$RemoteHost))  

#How many unique pages have been requested from our server? Give counts.
length(unique(df_conn$RequestLine)) 

#Which page has caused maximum data transfer from our server? Give page name & the data transfer for the page.
r <- summarise(group_by(dfPageSize, RequestLine), sum(Size))
r <- arrange(r,desc(`sum(Size)`))
r$RequestLine[1]
max(r$`sum(Size)`)

#Which host has caused maximum data transfer from our server? Give host name & the data transfer for the host.
r <- summarise(group_by(df_conn,RemoteHost ), sum(Size))
#r <- filter(r, !grepl(0,r$`sum(Size)`))
r <- arrange(r,desc(`sum(Size)`))
r$RemoteHost[1]

#Which page has maximum download size from our server? Give page name & the size for the page.
s <- dfPageSize.Sort$RequestLine[1]
s
dfPageSize.Sort$Size[1] 

#What is the download count of the page that has maximum download size from our server? Give page name & download count
cnt_s <- filter(dfPageSize.Sort, grepl(s,dfPageSize.Sort$RequestLine))
s
nrow(cnt_s)

#Which page has minimum download size from our server? Give page name & the size for the page.
dfAscSize.Sort <- arrange(dfPageSize,Size)
m <- dfAscSize.Sort$RequestLine[1]
m
dfAscSize.Sort$Size[1]

#What is the download count of the page that minimum download size from our server? Give page name & the size for the page.
cnt_m <- filter(dfAscSize.Sort, grepl(m,dfAscSize.Sort$RequestLine))
m
nrow(cnt_m)




