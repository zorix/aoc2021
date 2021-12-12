print("==============")
data = read.table("./input.txt", header=F, sep="-")
data = t(data)

part_a <- function(current_node, nodes_names) {
  #cat("tracing:", current_node)
  #print('')
  nodes1 <- data[,data[1,]==current_node]
  if (is.matrix(nodes1))
    nodes1 <- nodes1[2,]

  nodes2 <- data[,data[2,]==current_node]
  if (is.matrix(nodes2))
    nodes2 <- nodes2[1,]

  nodes <- c(nodes1, nodes2)

  #cat("nodes", nodes)
  #print('')
  numder_of_ends <- 0
  for (node in nodes) {
    if (node == current_node) {
    }
    else if (node == "end") {
      numder_of_ends <- numder_of_ends + 1
      #print("Reached END!")
    }
    else if (!(node %in% nodes_names)) {
      new_names <- nodes_names
      if (tolower(node) == node) {
        new_names = append(nodes_names, node)
      }
      #cat(current_node, "->", node)
      #print('')
      numder_of_ends <- numder_of_ends + part_a(node, new_names)
    }
  }
  return(numder_of_ends)
}

print(part_a("start", c("start")))

part_b <- function(current_node, nodes_names, visited_small_twice) {
  nodes1 <- data[,data[1,]==current_node]
  if (is.matrix(nodes1))
    nodes1 <- nodes1[2,]

  nodes2 <- data[,data[2,]==current_node]
  if (is.matrix(nodes2))
    nodes2 <- nodes2[1,]

  nodes <- c(nodes1, nodes2)

  numder_of_ends <- 0
  for (node in nodes) {
    if (node == current_node) {
    }
    else if (node == "start") {
    }
    else if (node == "end") {
      numder_of_ends <- numder_of_ends + 1
    }
    else if (!visited_small_twice) {
      new_names <- nodes_names
      if (tolower(node) == node) {
        new_names = append(nodes_names, node)
      }
      #cat(current_node, "->", node)
      #print('')

      numder_of_ends <- numder_of_ends + part_b(node, new_names, node %in% nodes_names)
    }
    else if (!(node %in% nodes_names)) {
      new_names <- nodes_names
      if (tolower(node) == node) {
        new_names = append(nodes_names, node)
      }
      #cat(current_node, "->", node)
      #print('')
      numder_of_ends <- numder_of_ends + part_b(node, new_names, visited_small_twice)
    }
  }
  return(numder_of_ends)
}

print(part_b("start", c("start"), F))
