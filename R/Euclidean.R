#' 1.1.1 euclidean function
#'
#' The function is simple, we just needed to define a method to assert before
#' calculations Always pay attention to main conditions defined by function that can
#' stops your code. This function automatically swap inputs if the second parameter is lower than the first
#' @param a is the first input of function. Any integer is accepted.
#' @param b is the second input of function. Any integer is accepted.
#' @return The greatest common divisor of two integers using Euclidean division
#' @examples
#' temp1=euclidean(100L,1000L)
#' temp2=euclidean(123612,13892347912)
#' temp3=euclidean(100,1000)
#' print(c(temp1,temp2,temp3))
#' @export
#'
#'@references \url{https://en.wikipedia.org/wiki/Euclidean_algorithm}
#'
euclidean=function(a,b)

{
  a= abs(a)
  b= abs(b)
  if (any(is.numeric(c(a,b))==FALSE) | (a%%1!=0) | (b%%1!=0)){
  stop()
} else {
if(b>a){
swap=a
a=b
b=swap
#print(c(a,b))
}
while(b!=0){
euc=b
b=a%%b
a=euc
}
return(a)
}}

