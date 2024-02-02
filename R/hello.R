SimpLinR = function(X,Y){
  try(if(length(X) != length(Y)) stop('Nonequal vectors'))
  if(length(X) == length(Y)){SimpLinCpp(X,Y)}
}


