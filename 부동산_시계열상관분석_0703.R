# 데이터 로드
setwd("C:/rdata")
data <- read.csv("data2.csv" ,stringsAsFactors = F , header = T,encoding = 'cp949')
data
data$평당거래금액<- as.numeric(data$평당거래금액)

library(plyr)
library(zoo)
library(readr)

# 데이터 필터
library(sqldf)
asset <- sqldf("select 지역,계약년월,avg(평당거래금액) as x 
                from data group by 지역,계약년월")
asset$x <- asset$x*10000
asset$x <- round(asset$x)
state_df <- sqldf("select 지역 from asset group by 지역")

#################################################################################################################

# 시게열 데이터: 시간의 흐름에 따라 관찰된 데이터

# 정상성: 대부분의 시계열 자료는 다루기 어려운 비정상성 시계열 자료이기 떄문에 분석하기 쉬운 정상성 시계열 자료로 변환해야 한다. 

# 1) 평균이 일정: 모든 시점에 대해 일정한 평균을 가진다. 
#    평균이 일정하지 않은 시계열은 차분(diffrence)을 통해 정상화
#    차분은 현시점 자료에서 이점 시점 자료를 빼는 것. 

# 2) 분산도 시점에 의존하지 않음: 분산이 일정하지 않은 시계열은 변환(transformation)을 통해 정상화 

# 3) 공분산도 시차에만 의존할 뿐, 특정 시점에는 의존하지 않음


#################################################################################################################

# 시계열기초분석
#for( i in (1:length(state_df[,1])) ) { 
# i<- 2
for( i in (1:length(state_df[,1])) ) { 
state <- as.character( state_df[ i , c("지역") ] )
house <- asset[asset$지역==state,c("계약년월","x")]
house <- within( house, {
  년월일 <- paste( 계약년월, "01", sep="")
})  

house$ymd <- as.Date(house$년월일, "%Y%m%d")  
house$계약년월 <- house$년월일 <- NULL

timeseries <- ts(house$x, c(2015,4), frequency = 12)


tsdecomp  <- decompose(timeseries)

ts_data <- data.frame( house$ymd
                       ,tsdecomp$x  
                       ,tsdecomp$trend,tsdecomp$seasonal,tsdecomp$random )
ts_data$광역시도 <- state
colnames( ts_data ) <- c("년월일","평당거래금액","추이"
                         ,"계절성(364일)","오차","광역시도")


if( i==1 ) {
  write.table(ts_data, file="ts_house.csv", sep=",", append=FALSE,
            row.names=FALSE, col.names=TRUE, fileEncoding = "CP949")
} else {
  write.table(ts_data, file="ts_house.csv", sep=",", append=TRUE,
              row.names=FALSE, col.names=FALSE, fileEncoding = "CP949")
}

if( state=="서울특별시" ) seoul <- house

}
# End Of For


################################################################################################################

# 시계열상관분석(CCF)
# 기준지역 : 서울



for( i in (1:length(state_df[,1])) ) { 
  i <- 1
  state <- as.character( state_df[ i , c("지역") ] )
  house <- asset[asset$지역==state,c("계약년월","x")]
  house <- within( house, {
    년월일 <- paste( 계약년월, "01", sep="")
  })  
  
  house$ymd <- as.Date(house$년월일, "%Y%m%d")  
  house$계약년월 <- house$년월일 <- NULL
  
route1 <- zoo(seoul$x, seoul$ymd )
route2 <- zoo(house$x, house$ymd )

ccf_title  <- paste("서울","-",state,sep="")  

ccf_cor <- ccf(route1 , window(route2, start = "2015-04-01" , end = "2020-04-01") , 
               lag.max = 6, plot=TRUE, main = ccf_title)

ccf_acf1 <- ccf_cor$acf[1:15,1,1]
ccf_acf2 <- ccf_cor$acf[15:1,1,1]
ccf_acf1 <- round(ccf_acf1,3)
ccf_acf2 <- round(ccf_acf2,3)
ccf_df <- data.frame( tof=tof_str,
                      airport1 = c(now_airport1,now_airport1),
                      airport2 = c(now_airport2,now_airport2),
                      name = c("->","<-"),
                      d7 = c(ccf_acf1[1],ccf_acf2[1]),
                      d6 = c(ccf_acf1[2],ccf_acf2[2]),
                      d5 = c(ccf_acf1[3],ccf_acf2[3]),
                      d4 = c(ccf_acf1[4],ccf_acf2[4]),
                      d3 = c(ccf_acf1[5],ccf_acf2[5]),
                      d2 = c(ccf_acf1[6],ccf_acf2[6]),
                      d1 = c(ccf_acf1[7],ccf_acf2[7]),
                      d0 = c(ccf_acf1[8],ccf_acf2[8]),
                      n1 = c(ccf_acf1[9],ccf_acf2[9]),
                      n2 = c(ccf_acf1[10],ccf_acf2[10]),
                      n3 = c(ccf_acf1[11],ccf_acf2[11]),
                      n4 = c(ccf_acf1[12],ccf_acf2[12]),
                      n5 = c(ccf_acf1[13],ccf_acf2[13]),
                      n6 = c(ccf_acf1[14],ccf_acf2[14]),
                      n7 = c(ccf_acf1[15],ccf_acf2[15])
)
 



