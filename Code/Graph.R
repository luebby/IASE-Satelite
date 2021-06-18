library(ggdag)


co <- data.frame(x=c(0,0,1), y=c(0,1,0), name=c("X", "Z", "Y")) 
DAG_GPG1 <- dagify(Z ~ X,
                   Y ~ X,
                   Y ~ Z, coords = co) %>% 
  ggdag(node_size = 10, text_size = 6, text = TRUE, text_col = "lightgray") + theme_dag_blank() +
  geom_dag_edges(arrow_directed = grid::arrow(length = grid::unit(10, "pt"), type = "closed"))  + 
  geom_text(label = "X - Exposure\nZ - Management\nY - Salary", 
            hjust = 1, vjust = 1,
            x = 1, y = 1, size = 5, color = "darkgrey") +
  labs(title = "Model A")

DAG_GPG2 <- dagify(X ~ Z,
                   Y ~ X,
                   Y ~ Z, coords = co) %>% 
  ggdag(node_size = 10, text_size = 6, text = TRUE, text_col = "lightgray") + theme_dag_blank() +
  geom_dag_edges(arrow_directed = grid::arrow(length = grid::unit(10, "pt"), type = "closed"))  + 
  geom_text(label = "X - Exposure\nZ - Management\nY - Salary", 
            hjust = 1, vjust = 1,
            x = 1, y = 1, size = 5, color = "darkgrey") +
  labs(title = "Model B")

GPG <- gridExtra::grid.arrange(DAG_GPG1, DAG_GPG2, nrow=1)
ggsave("DAG.png", GPG, width = 8, height = 4)