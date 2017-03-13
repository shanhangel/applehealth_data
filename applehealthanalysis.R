setwd("/Users/casper/百度云同步盘/Data Analysis/applehealth_data")

#载入包
library(XML)
library(dplyr)
library(ggplot2)
library(ggthemes)
library(lubridate)

#读取数据
xml <- xmlParse("/Users/Casper/百度云同步盘/Data Analysis/apple_health_export/导出.xml")
df <- XML:::xmlAttrsToDataFrame(xml["//Record"])
write.csv(df, "/Users/Casper/百度云同步盘/Data Analysis/apple_health_export/data.csv")
df <- read.csv("/Users/Casper/百度云同步盘/Data Analysis/apple_health_export/data.csv")

#看看用数据的结构和类型
str(df)
#一共包含了九类共一百多万条健康数据!第一列是数据类型，第二和第三列是数据来源，第四列是数据单位，第五列到第七列是记录数据
#的时间，最后一列是设备，包含了硬件设备和健康软件。

#显示下健康数据类型
unique(df$type)
#选择体重、卡路里消耗以及心率三个指标做分析
#HKQuantityTypeIdentifierBodyMass, HKQuantityTypeIdentifierHeartRate,

#数据处理
df$value <- as.numeric(as.character(df$value))
df$endDate <- ymd_hms(df$endDate, tz="Asia/Shanghai") #tz参数一定要加上，不加上会以UTC时间计算
df$month <- format(df$endDate,"%m")
df$year <- format(df$endDate,"%Y")
df$date <- format(df$endDate,"%Y-%m-%d")
df$ym <- format(df$endDate,"%Y-%m")
df$dayofweek <- wday(df$endDate, label=TRUE, abbr=FALSE)
df$hour <- format(df$endDate,"%H")
#判断是否周末
df$weekend <- lubridate::wday(df$date) %in% c(1, 7)
#判断休息时间

#体重数据分析
BodyMass <- subset(df, type == "HKQuantityTypeIdentifierBodyMass")
ggplot(BodyMass, aes(ym, value)) + 
geom_boxplot() +
theme_economist() + 
labs(x = "Year - Month", y="BodyMass(kg)")

#心率数据分析
#选择类型为心率的数据，在操作过程中发现，有几天在短时间之类出现了多次超过170的数据，分析应该是异常数据，可能来自于软件
#硬件的bug。考虑到运动时的最高心率也就在140左右，决定将这些异常数据基于170进行排除
HeartRate <- subset(df, type == "HKQuantityTypeIdentifierHeartRate" & value < 170)

table(HeartRate$hour)
#Apple watch晚上处于充电状态，所以记录的次数应该非常少，查看小时区段的数据计数，上午9点到晚上11点的记录数是明显高于0点到
#上午8点的，于是选择9点-23点区间进行分析
HeartRate <- subset(HeartRate, hour %in% c("09", "10","11", "12", "13", "14", "15", "16", "17", "18", "19", "20",
                                           "21", "22", "23"))

HeartRate %>%
    group_by(weekend, hour) %>%
    summarize(HeartRate=mean(value)) %>%
    #print table steps by month by year
    print (n=100) %>%
    #graph data by month by year
    ggplot(aes(x=hour, y=HeartRate, fill=weekend)) + 
    geom_bar(position='dodge', stat='identity') +
    theme_economist() + 
    coord_cartesian(ylim = c(70, 95))
#尝试了一下心率数据根月份、小时和星期数显示，发现了两个有意思的现象。按照小时显示，有两个比较明显并且平滑的峰，最高值在
#分别在13点和20点，最低值分别在11点和18点。而13点和20点一般是我吃饭的时候，难道饮食还让人心率加快？带着这个疑问，上网查
#了一下，有这样的解释，饭后血液从全身向消化系统输送，这就给心脏增加了工作量，导致心率的增加。第二个现象是，工作日每个时
#段的心率都要高于周末，可能是工作带来的压力和用脑导致了焦虑和心率增加。


#卡路里消耗
EnergyBurned <- subset(df, type=="HKQuantityTypeIdentifierBasalEnergyBurned"|
                           type=="HKQuantityTypeIdentifierActiveEnergyBurned")

temp <- data.frame(tapply(EnergyBurned$value, EnergyBurned$date, sum))
EnergyBurned <- data.frame(Date = rownames(temp), value = temp[,])

EnergyBurned$Date <- ymd(EnergyBurned$Date)
EnergyBurned$dayofweek <- wday(EnergyBurned$Date, label = TRUE, abbr = TRUE)
rownames(EnergyBurned) <- as.vector(1:nrow(EnergyBurned))
EnergyBurned$weekend <- lubridate::wday(EnergyBurned$Date) %in% c(1, 7)
EnergyBurned$month <- format(EnergyBurned$Date,"%m")
EnergyBurned$year <- format(EnergyBurned$Date,"%Y")

y <- EnergyBurned %>%
    group_by(dayofweek, month, year) %>%
    summarize(energy=mean(value)) %>%
    print(n=133)

Energy_day_year <- data.frame(Weekday = unlist(y[1]), Month = unlist(y[2]), EnergyBurned = unlist(y[4]))

ggplot(Energy_day_year, aes(x = Weekday, y = EnergyBurned)) + 
    geom_bar(position = 'dodge', stat = 'identity') +
    scale_y_continuous(labels = scales::comma) +
    facet_wrap(~Month)

#从月份上来看，6、7、8月的每日能量消耗是最高的，而11、12和1月的每日能量消耗最低。



    