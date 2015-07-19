model1 <- function(){
    for(i in seq(from = 0, to = 1, by = 0.001)){
        for(j in seq(from = 0, to = (1-i), by = 0.001)){
            if(all((s2-(i*s1+j*s4+(1-i-j)*s6))<0)){
                print(i*s1+j*s4+(1-i-j)*s6)
                print(s2)
                print(paste0(i, "*s1+", j, "*s4+", (1-i-j), "*s6"))
            }
        }
    }
}