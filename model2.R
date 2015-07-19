model2 <- function(){
    for(i in seq(from = 0, to = 1, by = 0.001)){
        for(j in seq(from = 0, to = (1-i), by = 0.001)){
            if(all((s3-(i*s1+j*s4+(1-i-j)*s6))<0)){
                print(i*s1+j*s4+(1-i-j)*s6)
                print(s3)
                print(paste0(i, "*s1+", j, "*s4+", (1-i-j), "*s6"))
            }
        }
    }
}