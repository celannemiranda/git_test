#ASSIGNMENT 02
#Celine Miranda
#November 14, 2018

#1. Computes the factorial given an integer argument

f <- function(x){
  y <- 1
  for(i in 1:x){
    y <-y*((1:x)[i])
  }
    print(y)
}
f(10)

#2. Sorts vector in decreasing order. Should accept numeric or character vectors

arr = c(1,4,6,8,9,100, 1000, -1, 3,4) 
# numeric
arr = c('ab','bc','cx','ey', 'fz') 
# characters


self_sort <- function(arr){
  for (j in 2:length(arr)) {
    init = arr[j] 
    
    i = j - 1 
    while (i > 0 && arr[i] < init) {
      arr[(i + 1)] = arr[i]
      i = i - 1 
    }
    arr[(i + 1)] = init
  }
  return (arr)
} 

self_sort(arr)

#3. Function that returns the nth highest number

nth_highest<-function(arr,nth) {
  return(self_sort(arr)[nth])
}
nth_highest(arr,1)

#4. Check if the integer is prime or not (TRUE or FALSE)

tpn<-function(prime.num) {return(sum(prime.num/1:prime.num==prime.num %/% 1:prime.num) == 2
)}
tpn(10)
tpn(11)

#OR to print "prime"

tpn <- function(pn){
  
  if(sum(pn/1:pn==pn%/%1:pn)==2)
    print("prime")
  
}
tpn(12)

#5. Compound Interest of an Investment

CI <- function(principal, interest_rate=0.01, compound_period=1){
  return(principal * ((1+interest_rate)**compound_period-1))
}
CI(1000)

#OR VECTORIZED APPROACH
#example values:
P=1000
n=60
rate=1

P * sum((1+rate/100)^(0:(n-1))*rate/100)
