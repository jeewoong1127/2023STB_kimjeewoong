#이항분포 확률함수
dbinom(x, size, prob)
#x : 성공의횟수(확률변수x), size : 시행횟수, prob : 성공확률
dbinom(3,4,0.5)
#이항분포 누적함수
pbinom(x, size, prob) 
#x : 성공의횟수(확률변수x), size : 시행횟수, prob : 성공의확률
pbinom(1,1000,0.01) 
pbinom(9,100,0.2)=1-pbinom(90,100,0.8)
#포아송분포 확률함수
dpois(x, lambda) 
#x : 사건의발생횟수, lambda : 평균발생횟수
dpois(2,1.2) 
#포아송분포 누적함수
ppois(x, lambda) 
#x : 사건의발생횟수, lambda : 평균발생횟수
1-ppois(40, 45) 
