model3 <- function(){
    max <- 0
    pos <- ""
    for(a1 in seq(from = 0, to = 1, by = 0.001)){
        for(a4 in seq(from = 0, to = (1-a1), by =  0.001)){
            if(max < sum(a1*s1+a4*s4+(1-a1-a4)*s6) ){
                max <- sum(a1*s1+a4*s4+(1-a1-a4)*s6)
                pos <- paste0(a1,"*s1+",a4,"*s4+",(1-a1-a4),"*s6")
            }
            print(max)
            print(pos)
            Sys.sleep(0.01)
        }
        
    }
}