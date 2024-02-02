SimpLinR = function(X,Y){
  try(if(length(X) != length(Y)) stop('Nonequal vectors'))
  SimpLinCpp(X,Y)
}

