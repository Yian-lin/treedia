## code to prepare `Monty_hall` dataset goes here

Monty_hall <- data.frame(matrix(nrow=3*3*3, ncol = 3*2))
colnames(Monty_hall) <- c("Level_1_Class", "Level_1_Prob",
                          "Level_2_Class", "Level_2_Prob",
                          "Level_3_Class", "Level_3_Prob")

Monty_hall$Level_1_Class[1:9] <- "C1"
Monty_hall$Level_1_Class[(1:9)+9] <- "C2"
Monty_hall$Level_1_Class[(1:9)+9*2] <- "C3"
Monty_hall$Level_1_Prob <- 1/3

Monty_hall$Level_2_Class <- rep(rep(c("P1", "P2", "P3"),each=3),3)
Monty_hall$Level_2_Prob <- 1/3

Monty_hall$Level_3_Class <- rep(c("M1","M2","M3"), 9)
Monty_hall$Level_3_Prob <- c(c(0,0.5,0.5),c(0,0,1), c(0,1,0),
                             c(0,0,1), c(0.5,0,0.5), c(1,0,0),
                             c(0,1,0), c(1,0,0), c(0.5,0.5,0))

usethis::use_data(Monty_hall, overwrite = TRUE)
