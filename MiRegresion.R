MiRegresion <- function(X,Y) {
  numvar <- length(X);
  suma <- 0;
  for(i in 1:numvar){suma <- suma +X[i]}
  xbarra <- suma/numvar;
  suma <- 0;
  for(i in 1:numvar){suma <- suma +Y[i]}
  ybarra <- suma/numvar;
  suma <- 0;
  for(i in 1:numvar){suma <- suma +(X[i]-xbarra)^2}
  Sxx <- suma/numvar;
  suma <- 0;
  for(i in 1:numvar){suma <- suma+((X[i]-xbarra)*(Y[i]-ybarra))}
  Sxy <- suma/numvar;
  Beta1  <- Sxy/Sxx;
  Beta0  <- ybarra-Beta1*xbarra;
  Ygorro <- Beta0+Beta1*X; 
  resultado <- list(beta0 = Beta0,
                    beta1 = Beta1,
                    Yest  = Ygorro)
  return(resultado)
}

