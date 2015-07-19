model4 <- function(){
    max <- vector(length = 6) 
    pos <- vector(length = 6)
    count <- 0
    for(a1 in seq(from = 0, to = 1, by = 0.01)){
        for(a2 in seq(from = 0, to = (1-a1), by = 0.01)){
            for(a3 in seq(from = 0, to = (1-a1-a2), by = 0.01)){
                for(a4 in seq(from = 0, to = (1-a1-a2-a3), by = 0.01)){
                    for(a5 in seq(from = 0, to = (1-a1-a2-a3-a4), by = 0.01)){
                        ##for(i in 1:6){
                         ##   if(max[i]<sum(a1*s1[i]+a2*s2[i]+a3*s3[i]+a4*s4[i]+a5*s5[i]+(1-a1-a2-a3-a4-a5)*s6[i])){
                         ##       max[i] <- sum(a1*s1[i]+a2*s2[i]+a3*s3[i]+a4*s4[i]+a5*s5[i]+(1-a1-a2-a3-a4-a5)*s6[i])
                         ##       pos[i] <- paste0(a1,"*s1+",a2,"*s2+",a3,"*s3+",a4,"*s4+",a5,"*s5+",(1-a1-a2-a3-a4-a5),"*s6")
                         ##   } 
                        ##}
                    count<-count+1    
                    }
                }
            }
        }
        print(a1)
    }
    print(max)
    print(pos)
    max
    count
}