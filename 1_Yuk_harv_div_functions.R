
#------------------------------------------------------------------------------#
# AYK-SSI #1701 Yukon Chinook
# 
# functions file
#------------------------------------------------------------------------------#



# Posterior summary function
post.summ = function(post.samp, var) {
  post.samp = as.matrix(post.samp)
  
  # if parameter is indexed
  if(substr(var, nchar(var), nchar(var)) == "[") {
    post = post.samp[,substr(colnames(post.samp), 1, nchar(var)) == var]
    summ = apply(post, 2, function(x) c(mean = mean(x), sd = sd(x), quantile(x, c(0.5, 0.025, 0.975))))
    return(summ)
  }
  
  # if parameter is not indexed
  if(substr(var, nchar(var), nchar(var)) != "[") {
    post = post.samp[,substr(colnames(post.samp), 1, nchar(var)) == var]
    summ = c(mean = mean(post), sd = sd(post), quantile(post, c(0.5, 0.025, 0.975)))
    return(summ)
  }
}





# Function for estimating overfished and extinct
SC.eq <- function(U,a,b){
  S.eq <- max(0,(a-(-log(1-U)))/b)
  C.eq <- max(0,((a-(-log(1-U)))/b)*exp(a-b*((a-(-log(1-U)))/b))-((a-(-log(1-U)))/b))
  OF <- ifelse(U>0.5*a-0.07*a^2,1,0)
  EX <- ifelse(S.eq==0,1,0)
  return(c(S.eq,C.eq,OF,EX))
}


# not_in function from purrr
`%not_in%` <- purrr::negate(`%in%`)




