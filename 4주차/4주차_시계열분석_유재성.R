library("TTR")
library("forecast")

# 1)소스 데이터를 시계열 데이터로 변환
Air_ts=ts(AirPassengers, frequency = 12, start = c(1949,1))

# 2)시계열 데이터를 x,trend,seasonal,random 값으로 분해
Air_comp=decompose(Air_ts)
plot(Air_comp)

# 3)시계열 데이터를 이동평균한 값 생성
Air_sma3=SMA(Air_ts, n=3)
Air_sma8=SMA(Air_ts, n=8)

# 4)시계열 데이터를 차분
Air_diff1=diff(Air_ts,differences = 1)

# 5)ACF값과 그래프를 통해 래그 절단값을 확인
par(mfrow=c(1,1))
acf(Air_diff1,lag.max =13)

# 6) PACF값과 그래프를 통해 래그 절단값 확인
pacf(Air_diff1,lag.max =13)

# 7) 데이터를 활용하여 최적의 ARIMA모형을 선택
auto.arima(Air_ts)

# 8) 선정된 ARIMA모형으로 데이터를 보정(fitting)
Air_arima=arima(Air_ts, order = c(2,1,1), seasonal = list(order=c(0,1,0),period=12))
Air_arima

# 9) ARIMA모형에 의해 보정된 데이터를 통해 미래값을 예측
Air_fcast=forecast(Air_arima,h=24)
Air_fcast
# 10) 시계열 데이터를 그래프로 표현
plot.ts(Air_ts)

# 11) 예측된 시계열 데이터를 그래프로 표현
plot(Air_fcast)

