# 181122 function file nonlinear estimator

prod.function.sr1.internal1 <- 
  function(data,W,beta,gamma){
    K <- exp(data$pc2000)
    WK<-lag.listw(W,K)
    H <- exp(data$hc2000)
    WH<-lag.listw(W,H)
    
    physical.c <- log(K + beta*WK)
    human.c <- log(H + gamma*WH)
    
    if (sum(is.nan(physical.c))==0 & sum(is.nan(human.c))==0) {
      model1 <- lm( pib2000 ~ -1 + physical.c + human.c, data = data )
      e <- resid(model1)
      output <- sum((e-mean(e))^2)
      output
    } 
    else {
      output <- NA
      output
    }
    
  }


prod.function.sr1<- 
  function(data,W){
    accuracy<-.01
    seq.holder1 <- seq(-1,1,by = accuracy)
    length <- length(seq.holder1)
    matrix.holder1 <- matrix(rep(0,3*length^2),ncol=3,nrow=length^2)
    position.SQR <- 1 #initial position of SQR data
    position.beta <- 1
    position.gamma <- 1
    for (beta in seq.holder1) {
      for (gamma in seq.holder1) {
        
        SQR <- prod.function.sr1.internal1(data,W,beta,gamma)
        
        matrix.holder1[position.beta,1] <- beta  
        matrix.holder1[position.gamma,2] <- gamma
        matrix.holder1[position.SQR,3] <- SQR
        
        position.SQR <- position.SQR + 1
        position.gamma <- position.gamma + 1
        position.beta <- position.beta + 1
      }}
    df.holder1 <- as.data.frame(matrix.holder1)
    nm.holder1 <- as.matrix(df.holder1[order(df.holder1$V3),]) 
    vec.holder1 <- nm.holder1[1,]
    
    #increasing degree of accuracy
    accuracy<-.00001
    seq.holder1 <- seq(-0.001,0.001,by = accuracy)
    length <- length(seq.holder1)
    matrix.holder1 <- matrix(rep(0,3*length^2),ncol=3,nrow=length^2)
    position.SQR <- 1 #initial position of SQR data
    position.beta <- 1
    position.gamma <- 1
    for (beta in seq(vec.holder1[1]-.001,vec.holder1[1]+.001,by=accuracy)) {
      for (gamma in seq(vec.holder1[2]-.001,vec.holder1[2]+.001,by=accuracy)) {
        
        SQR <- prod.function.sr1.internal1(data,W,beta,gamma)
        
        matrix.holder1[position.beta,1] <- beta  
        matrix.holder1[position.gamma,2] <- gamma
        matrix.holder1[position.SQR,3] <- SQR
        
        position.SQR <- position.SQR + 1
        position.gamma <- position.gamma + 1
        position.beta <- position.beta + 1
      }}
    df.holder1 <- as.data.frame(matrix.holder1)
    df.holder2 <- df.holder1[order(df.holder1$V3),]
    df.holder3 <- df.holder2[1,]
    names(df.holder3) <- c('beta','gamma','min SSR')
    df.holder3
}








