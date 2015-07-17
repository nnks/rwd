#Compute expected/theoretical item information function
f.iif.exp <- function(theta, abc){
 D <- 1.7
 out <- c()
 if (is.matrix(abc) == TRUE){     #if input abc as a matrix
     aa <- abc[,2]; bb <- abc[,3]; cc <- abc[,4]; j.id <- abc[,1]
     ni  <- length(theta)
     nj  <- length(aa)
     iif <- matrix(0, nrow = ni, ncol = nj)
     for (i in 1:ni){
      for (j in 1:nj){ 
       iif[i,j] <- (D*D*aa[j]*aa[j]*(1-cc[j]))/
                   ((cc[j]+exp( D*aa[j]*(theta[i]-bb[j]) ))*
                    (    1+exp(-D*aa[j]*(theta[i]-bb[j]) ))^2 )
       out <- rbind(out, cbind(theta[i], j.id[j], aa[j], bb[j], cc[j], iif[i,j]))
      }#for j
     }#for i
 }#if
 if (is.matrix(abc) == FALSE){    #if input abc as a vector
     aa <- abc[1];  bb <- abc[2];  cc <- abc[3]; j.id <- 1
     iif <- (D*D*aa*aa*(1-cc))/
                 ((cc+exp( D*aa*(theta-bb) ))*
                 (  1+exp(-D*aa*(theta-bb) ))^2 )
       out <- rbind(out, cbind(theta, j.id, aa, bb, cc, iif))
 }
 colnames(out) <- c("theta", "j.id", "a", "b", "c", "iif")
 return(out)
}#f.iif.exp


#IRT logistic models
irf.3pl <- function(theta, abc){
 D <- 1.7; a <- abc[1]; b <- abc[2]; c <- abc[3]
 irf <- c +(1-c)*(exp(D*a*(theta-b)))/(1+exp(D*a*(theta-b)))
 names(irf) <- c("Prob")
 return(irf)
}#irf.3pl


#Check response pattern
resp.chk <- function(resp){
 if (all(c(0,1) %in% resp)){ resp.pat <- 10
 } else if (1 %in% resp)   { resp.pat <- 1
   } else                  { resp.pat <- 0
     }
 return(resp.pat)
}#resp.chk

#Update theta.est and Select the next item
update.theta <- function(resp, abc, theta.old){
 if (resp.chk(resp) == 10){ 			
     x <- unlist(resp)
     nj <- length(abc[,1])
     params <- list("3pl" = list(a = abc[,1], b = abc[,2], c = abc[,3]), "gpcm" = NULL)
     theta.tem <- irt.ability(x, params, ind.dichot = 1:nj, method = "EAP", std.err = TRUE, control = list(D = 1.7,start_val = theta.old))
     sem.tem   <- attributes(theta.tem)$"std.err"

     if((is.nan(theta.tem)==FALSE) && (abs(theta.tem) <= 3.5)) {
           theta.new <- theta.tem
           sem.obs   <- sem.tem
        } else if ((is.nan(theta.tem)==FALSE) && (theta.tem < 0)) {
                   theta.new <- -3.5
                   sem.obs   <- 3
                } else if ((is.nan(theta.tem)==FALSE) && (abs(theta.tem) > 3.5)) {
                           theta.new <- 3.5
                           sem.obs   <- 3
                       } else if ((is.nan(theta.tem)==TRUE) && (theta.old < 0)) {
                                  theta.new <- -3.5
                                  sem.obs   <- 3
                              } else if ((is.nan(theta.tem)==TRUE) && (theta.old > 0)) {
                                         theta.new <- 3.5
                                         sem.obs   <- 3
                                     }
 } else if (resp.chk(resp) == 0){
     theta.new <- max(theta.old - .5, -3.5)
     sem.obs   <- 3
 } else if  (resp.chk(resp) == 1){	
   
       theta.new <- min(theta.old + .5, 3.5)
       sem.obs   <- 3     
     }
 results <- cbind(theta.new, sem.obs)
 return(results)
}#update.theta2



#Compute observed SEM for the ML-based theta.est
get.sem.obs <- function(resp, abc, theta){
 D <- 1.7; aa <- abc[,1]; bb <- abc[,2]; cc <- abc[,3]
 prob.1  <- cc +(1-cc)/(1+exp(-D*aa*(theta-bb)))
 prob.0  <- 1 - prob.1
 d2like  <- sum(D^2*aa^2*((prob.1-cc)/(1-cc)^2)*((prob.0/prob.1))*
                ((resp*cc-prob.1^2)/(prob.1)))
 sem.obs <- 1/sqrt(-1*d2like)
 return(sem.obs)
}