
pathString_Prob_non_leaf <- function(df, root_name){
  ## Returns a dataframe that contains two columns: pathString and Prob
  ## Prob will be the label on edges, so they are conditional prob.
  # df: the input df should be a dataframe with columns
  #     Level_1_Class, Level_2_Class, ..., Level_n_Class, Level_n_Prob
  # root_name: name of the root node
  sub_df <- df[!duplicated(df[,1:(ncol(df)-1)]), ] #remove duplications
  colnames(sub_df)[ncol(sub_df)] <- "Prob"
  if (ncol(sub_df)==2) {
    sub_df$pathString <- paste(root_name, sub_df[,1], sep = "/")
  } else {
    sub_df$pathString <- apply(sub_df[,1:(ncol(sub_df)-1)], MARGIN = 1,
                               FUN = function(x) paste(root_name,paste(x, collapse = "/"),sep = "/"))
  }
  output <- sub_df[,c("pathString","Prob")]
  return(output)
}

plot_tree_diagram <- function(df, root_name, tree_direction="LR",
                              remove_0 = TRUE,
                              edge_p_digits=3, joint_p_digits=5, joint_p_scientific=TRUE,
                              bgcolor = "lightgrey", joint_p_edge_style = ""){
  ## Plot a tree diagram
  # df: the input data frame, each row should represent a leaf in the data.tree structure.
  #     Also, the columns should be in order of level_1_var, Prob_level_1, level_2_var, Prob_level_2, ...
  #     The 1st column should contain values of the level 1 variable;
  #     the 2nd column should contain the probability of level 1 variable equal to the corresponding value in the same row of the 1st column;
  #     the 3rd column should contain values of the level 2 variable;
  #     the 4nd column should contain the conditional probability of level 2 variable given the level 1 variable
  # root_name: name of the root node
  # tree_direction: specify the direction of the tree. The default is "LR", meaning
  #                 from left to right. It can also be "TB" (top to bottom), "BT" (bottom to top), "RT"(right to left).
  # remove_0: set to TRUE if we don't want to show leaves/paths with prob=0.
  # edge_p_digits: positive integer indicating the number of decimal places to be
  #                used when rounding probabilities on edges of the tree. The default value is 3.
  # joint_p_digits: positive integer indicating the number of decimal places to be
  #                used when rounding joint probabilities. The default value is 5.
  # joint_p_scientific: Logical. Specify whether the joint probabilities should be
  #                     displayed in scientific format. The default value is TRUE.
  # joint_p_edge_style: can set to "invis" so that there is no edge connecting the leaves and the joint prob
  n_col <- ncol(df) # number of cols in the input df
  n_stage <- n_col/2 # number of stages

  if (n_stage==1) {
    df$pathString <- paste(root_name, df[,1], sep="/")
    final_pathString_df <- df
  } else
  {
    prob_cols_numb <- seq(2, n_col, by=2) # prob cols
    # compute the joint prob for each leaf/path
    df$Prob <- apply(df[,prob_cols_numb], MARGIN=1, FUN = prod)

    # Add a pathString col to df
    df$pathString <- pathString_Prob_non_leaf(df[,c(prob_cols_numb-1,n_col+1 ,n_col+1)],root_name)$pathString

    ## Construct a dataframe that contains the path and Prob or all edges
    final_pathString_df <- df[,c("pathString", "Prob")]
    col_candidates <- seq(1,n_col,by=2)
    for (i in 1:n_stage) {
      component_pathString_df <- pathString_Prob_non_leaf(df[,c(col_candidates[1:i],col_candidates[i]+1)],root_name)
      final_pathString_df <- rbind(component_pathString_df, final_pathString_df)
    }
  }

  #remove any row with Prob == 0
  if (remove_0 == TRUE) {
    final_pathString_df <- final_pathString_df[final_pathString_df$Prob!=0,]
  }

  ### conversion to Node
  tree <- data.tree::as.Node(final_pathString_df)

  ### set styles before plotting ###
  data.tree::SetGraphStyle(tree, rankdir = tree_direction, bgcolor=bgcolor)

  if (n_stage==1) {
    ## add edge label
    tree$Do(
      function(node) {
        data.tree::SetEdgeStyle(node, label = round(node$Prob,edge_p_digits))
      },
      filterFun = function(node) {!node$isRoot}
    )
  } else {
    ### For joint prob
    # Set node style: shape = "none", round joint prob
    data.tree::Do(tree$leaves, function(node) data.tree::SetNodeStyle(node, shape = "none",
                                                label=format(round(node$Prob, joint_p_digits),
                                                             scientific = joint_p_scientific)))
    #Set edge style
    data.tree::Do(tree$leaves, function(node) data.tree::SetEdgeStyle(node, style = joint_p_edge_style, arrowhead = "none", color="grey42"))

    ### For edge prob
    # add edge label
    tree$Do(
      function(node) {
        data.tree::SetEdgeStyle(node, label = round(node$Prob,edge_p_digits))},
      filterFun = function(node) {!node$isRoot && !node$isLeaf}
    )
  }

  ### Plot ###
  plot(tree)
}
