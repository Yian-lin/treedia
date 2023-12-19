prepare_df <- function(X_dist, X_parameters, X_low_q=0.01, X_high_q=0.99,
                       Y_given_X_dist=NULL, Y_parameters, Y_low_q=0.1, Y_high_q=0.9){
  ## Returns a data.frame that can be used as the input df to the plot_tree_diagram()
  # X_parameters: a vector containing the parameters for X_dist.
  # X_low_q: the left quantile of X. Used to determine the min of possible values
  #          of X that will be included in the tree diagram.
  # X_high_q: the right quantile of X. Used to determine the max of possible values
  #          of X that will be included in the tree diagram.
  # Y_parameters: a vector containing the parameters for Y_given_X_dist.
  #               Can use a string to express the relationship between a parameter and X(the level 1 variable)
  #               For example, if Y|X ~ Bern(1/X), then Y_given_X_dist = "Bin", Y_parameters = c(1,"1/X");

  #### create a df that contains each possible combination of x and y, and P(y|x)

  ### level_1_df: create a df that contains each possible value x and P(X=x) ###
  if(X_dist %in% c("Pois","Geo", "Bin", "NegBin")){

    if(X_dist=="Pois") {
      #valid values of X
      X <- stats::qpois(X_low_q,X_parameters[1]):stats::qpois(X_high_q,X_parameters[1])
      # P(X=x)
      Prob <- sapply(X, FUN = function(x){stats::dpois(x,lambda=X_parameters[1])})
    } else if (X_dist=="Geo") {
      X <- stats::qgeom(X_low_q,X_parameters[1]):stats::qgeom(X_high_q,X_parameters[1])
      Prob <- sapply(X, FUN = function(x){stats::dgeom(x,X_parameters[1])})
    } else if (X_dist=="Bin") {
      #Check if X_parameters is a vector of length == 2
      if(length(X_parameters)!=2){
        stop("X_parameters must be a vector of length 2 for the distribution that you specify for X.")}
      X <- stats::qbinom(X_low_q,X_parameters[1],X_parameters[2]):stats::qbinom(X_high_q,X_parameters[1],X_parameters[2])
      Prob <- sapply(X, FUN = function(x){stats::dbinom(x,X_parameters[1],X_parameters[2])})
    } else { #X_dist=="NegBin"
      if(length(X_parameters)!=2){
        stop("X_parameters must be a vector of length 2 for the distribution that you specify for X.")}
      X <- stats::qnbinom(X_low_q,X_parameters[1],X_parameters[2]):stats::qnbinom(X_high_q,X_parameters[1],X_parameters[2])
      Prob <- sapply(X, FUN = function(x){stats::dnbinom(x,X_parameters[1],X_parameters[2])})
    }

    level_1_df <- data.frame("X"=X, "Prob"=Prob)

  } else {
    stop("Your input for X_dist cannot be recognized. X_dist can only be
           \"Pois\",\"Geo\", \"Bin\", or \"NegBin\". If you want X_dist to be
           Bernoulli distribution, please specify X_dist to be \"Bin\" because
           Bernoulli is just a special case of Binomial distribution.")
  }

  ### level_2_df ###
  if(!is.null(Y_given_X_dist)){

    if (Y_given_X_dist %in% c("Geo", "Pois")){
      #Prepare parameters for Y|X based on the value of X
      X_p <- data.frame(matrix(nrow = length(X), ncol = 2)) # a matrix to store X, p
      colnames(X_p) <- c('X', 'Para_1') # Para_1 is p for Geo
      X_p$X <- X
      X_p$Para_1 <- eval(parse(text=Y_parameters[1])) # p for Geom(p)
      #remove rows with any para == Inf
      X_p <- X_p[!(is.infinite(X_p$Para_1)),]
      #print(X_p)

      #construct level_2_df (include every combination of X and Y)
      level_2_df <- data.frame(matrix(0, nrow = 0, ncol = ncol(X_p)+1))
      colnames(level_2_df) <- c('X', 'Para_1', 'Y')

      for (i in 1:nrow(X_p)) {
        #Find valid values of Y|X based on the values of Para_1 and Para_2 in ith row of X_r_p
        if (Y_given_X_dist=="Geo") {
          Y_given_X <- stats::qgeom(Y_low_q,prob=X_p$Para_1[i]):stats::qgeom(Y_high_q,prob=X_p$Para_1[i])
        } else { #Y_given_X_dist=="Pois"
          Y_given_X <- stats::qpois(Y_low_q,lambda=X_p$Para_1[i]):stats::qpois(Y_high_q,lambda=X_p$Para_1[i])
        }

        n_Y_given_X <- length(Y_given_X) #number of valid values of Y|X, based on this specific p
        component_df <- data.frame(matrix(rep(as.numeric(X_p[i,]),n_Y_given_X), nrow = n_Y_given_X, ncol = 2, byrow = T))
        colnames(component_df) <- colnames(X_p)
        component_df$Y <- Y_given_X
        #str(component_df)
        #print(component_df)
        level_2_df <- rbind(level_2_df, component_df)
      }
      # reorder columns in level_2_df
      level_2_df <- level_2_df[, c('X', 'Y', 'Para_1')]
      #print(level_2_df)
      #str(level_2_df)
      #P(Y|X)
      if (Y_given_X_dist=="Geo") {
        level_2_df$Prob <- apply(level_2_df, MARGIN = 1,
                                 FUN = function(row) {stats::dgeom(x=row[2],prob=row[3])})
      } else { #Y_given_X_dist=="Pois"
        level_2_df$Prob <- apply(level_2_df, MARGIN = 1,
                                 FUN = function(row) {stats::dpois(x=row[2],lambda=row[3])})
      }
    }

    else if (Y_given_X_dist %in% c("Bin","NegBin")) { #Note, Bern(p) = Bin(1,p)

      #Check if Y_parameters is a vector of length == 2
      if(length(Y_parameters)!=2){
        stop("Y_parameters must be a vector of length 2 for the distribution that you specify for Y|X.")}

      #Prepare parameters for Y|X based on the value of X
      X_r_p <- data.frame(matrix(nrow = length(X), ncol = 3)) # a matrix to store X, n or r, p
      colnames(X_r_p) <- c('X', 'Para_1', 'Para_2')
      X_r_p$X <- X
      X_r_p$Para_1 <- eval(parse(text=Y_parameters[1])) # n for Bin(n,p); r for NegBin(r,p)
      X_r_p$Para_2 <- eval(parse(text=Y_parameters[2])) # p
      #remove rows with any para == Inf
      X_r_p <- X_r_p[!(is.infinite(X_r_p$Para_1) | is.infinite(X_r_p$Para_2)),]

      #construct level_2_df (include every combination of X and Y)
      level_2_df <- data.frame(matrix(0, nrow = 0, ncol = ncol(X_r_p)+1))
      colnames(level_2_df) <- c('X', 'Para_1', 'Para_2', 'Y')

      for (i in 1:nrow(X_r_p)) {
        #Find valid values of Y|X based on the values of Para_1 and Para_2 in ith row of X_r_p
        if (Y_given_X_dist == "Bin") {
          Y_given_X <- stats::qbinom(Y_low_q,size=X_r_p$Para_1[i],prob =X_r_p$Para_2[i]):stats::qbinom(Y_high_q,size=X_r_p$Para_1[i],prob =X_r_p$Para_2[i])
        } else { #Y_given_X_dist == "NegBin"
          Y_given_X <- stats::qnbinom(Y_low_q,size=X_r_p$Para_1[i],prob =X_r_p$Para_2[i]):stats::qnbinom(Y_high_q,size=X_r_p$Para_1[i],prob =X_r_p$Para_2[i])
        }
        n_Y_given_X <- length(Y_given_X) #number of valid values of Y|X, based on this specific r and p
        component_df <- data.frame(matrix(rep(as.numeric(X_r_p[i,]),n_Y_given_X), nrow = n_Y_given_X, ncol = 3, byrow = T))
        colnames(component_df) <- colnames(X_r_p)
        component_df$Y <- Y_given_X
        level_2_df <- rbind(level_2_df, component_df)
      }
      # reorder columns in level_2_df
      level_2_df <- level_2_df[, c('X', 'Y', 'Para_1', 'Para_2')]

      #P(Y|X)
      if (Y_given_X_dist == "Bin") {
        level_2_df$Prob <- apply(level_2_df, MARGIN = 1,
                                 FUN = function(row) {stats::dbinom(x=row[2],size=row[3],prob= row[4])})
      } else { #Y_given_X_dist == "NegBin"
        level_2_df$Prob <- apply(level_2_df, MARGIN = 1,
                                 FUN = function(row) {stats::dnbinom(x=row[2],size=row[3],prob= row[4])})
      }
    }

    else {
      stop("Your input for Y_given_X_dist cannot be recognized. Y_given_X_dist can only be
           \"Pois\",\"Geo\", \"Bin\", or \"NegBin\". If you want Y_given_X_dist to be a
           Bernoulli distribution, please specify Y_given_X_dist to \"Bin\" because
           Bernoulli is just a special case of Binomial distribution.")
    }
  }

  ### construct the final_df ###
  if (is.null(Y_given_X_dist)){
    final_df <- level_1_df
  } else {
    #merge level_1_df and level_2_df
    final_df <- merge(level_1_df, level_2_df[,c("X","Y","Prob")], by="X", all = T)
    final_df <- final_df[stats::complete.cases(final_df), ] #remove rows with NA
  }

  return(final_df)
}
