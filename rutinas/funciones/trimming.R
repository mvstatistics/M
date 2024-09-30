trimming <- function(x, p=3.5){
    
    # Upper bound
    U <- p*median(x)
    
    # factores trim
    i <- x>U
    sum(i)
    
    xt <- ifelse(x>U, U, x)
    
    s <- sum(x-xt)
    j <- 0
    
    # lazo while
    while (s>0){
        x <- xt + (s/(length(x)-sum(i)))*(1-i)
        
        i <- x>U
        sum(i)
        
        xt <- ifelse(x>U, U, x)
        
        s <- sum(x-xt)
        
        j <- j+1
        #print(s); print(j)
    }
    
    #plot(x, xt)
    #sum(x); sum(xt)
    
    return(xt)
    
}