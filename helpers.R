# General helper functions-----------------------------------------------------

#' Adjust nb of agents to allow for equally sized technology groups
#' 
#' Adjusts the number of agents in the network one cannot form equally sized 
#' groups preferring each of the technologies. 
#' If there is a remainder in the integer division of the number of agents by
#' the number of technologies, the new number of agents is the old minus the 
#' remainder plus the number of technologies.
#' 
#' @param n_agents The original number of agents
#' @param n_techs The number of technologies
#' @return The adjusted number of agents
adj_n <- function(n_agents, n_techs){
  remainder <- n_agents%%n_techs
  if (remainder == 0){
    n_new <- n_agents
  } else {
    n_new <- n_agents - remainder + n_techs
  }
  stopifnot(n_new%%n_techs==0)
  return(n_new)
}

# Simulation-specific helpers--------------------------------------------------

#' Create a network with agents
#' 
#' @param n_agents The number of agents; get adjusted if 
#'  \code{n_agents}/\code{n_techs} yields a remainder
#' @param n_techs The number of technologies
#' @param topology The topology of the network
#' @return An igraph graph
create_network <- function(n_agents, n_techs, topology){
  n_agents <- adj_n(n_agents, n_techs)
  if (topology=="Komplettes Netzwerk"){
    network_used <- make_full_graph(
      n_agents, directed = FALSE, loops = FALSE)
    plot_layout <- layout.kamada.kawai(network_used)
  } else if (topology=="Ring"){
    network_used <- make_ring(
      n_agents, directed = FALSE, mutual = FALSE, circular = TRUE)
    plot_layout <- layout_in_circle(network_used)
    
  } else {
    stop("No correct network topology given!")
  }
  
  network_used <- set_vertex_attr(network_used, "technology", 
                                  index = V(network_used), NA)
  
  agents_per_group <- n_agents / n_techs
  groups <- Hmisc::partition.vector(V(network_used), rep(agents_per_group, n_techs))
  for (i in 1:length(names(groups))){
    network_used <- set_vertex_attr(network_used, "preferred_tech", 
                                    index = groups[[names(groups)[i]]], 
                                    value = all_techs[i])
  }
  return(network_used)
}

#' Lets an agents choose a technology
#' 
#' @param graph_used
#' @param vertex_used
#' @param all_techs
#' @param intrinsic_preference
#' @param intrinsic_utilities
#' @param choice_mode
#' @param print_decisions If TRUE, print decisions of each agent
#' @return The chosen technology
choose_tech <- function(graph_used, vertex_used, all_techs, 
                        intrinsic_preference, intrinsic_utilities, 
                        choice_mode="share", print_decisions=FALSE){
  neighborhood <- neighbors(graph_used, vertex_used)
  techs_neighborhood <- factor(
    neighborhood$technology[!is.na(neighborhood$technology)], 
    levels = c(levels(all_techs)))
  neighborhood_len <- length(techs_neighborhood)
  if (neighborhood_len == 0){
    tech_chosen <- V(graph_used)[vertex_used]$preferred_tech
  } else{
    abs_freqs <- table(techs_neighborhood)
    rel_freqs <- prop.table(abs_freqs)
    if (choice_mode=="share"){
      utilities <- unname(rel_freqs)
      if (length(utilities)!=length(intrinsic_utilities)){
        browser()
      }
      stopifnot(length(utilities)==length(intrinsic_utilities))
      utilities <- utilities + intrinsic_utilities
      utilities[V(graph_used)[vertex_used]$preferred_tech] <- utilities[V(graph_used)[vertex_used]$preferred_tech] + intrinsic_preference
    }
    index_max <- which(utilities==max(utilities))
    if (length(index_max)>1){
      index_max <- sample(index_max, 1)
    }
    tech_chosen <- all_techs[index_max]
  }
  if (print_decisions){
    print(paste0("V", vertex_used, " chose tech ", tech_chosen, 
                 " (oriented on ", neighborhood_len, " neighbors)"))
  }
  return(tech_chosen)
}

#' Runs a single simulation instance
#' 
#' @param interaction_id An identifier of the simulation; will be put in 
#'  column 'interaction_id' in the resulting data.frame
#' @param n_agents Number of agents; will potentially adjusted via \code{adj_n}
#' @param n_techs Number of technologies
#' @param network_topology The topology of the network
#' @param choose_mode Specifies whether agents determine network utility via
#'  usage shares of absolute users
#' @param intrinsic_preference The intensity of the personal intrinsic 
#'  preference of the agents
#' @param intrinsic_utilities Vector of length \code{n_techs}; gives intrinsic
#'  values of technologies that are taken into account by all agents, not only 
#'  those that prefer the technology over the others.
#' @return A data.frame with the simulation results
run_simulation <- function(interaction_id, n_agents, n_techs, 
                           network_topology, choose_mode, 
                           intrinsic_preference, intrinsic_utilities){
  # Fix parameters and history lists
  n_agents <- adj_n(n_agents, n_techs)
  all_techs <- factor(1:n_techs)
  shares_list <- list()
  for (t in all_techs){
    shares_list[[t]] <- rep(NA, n_agents)
  }
  
  # Create network
  network_used <- create_network(n_agents = n_agents, n_techs = n_techs, 
                                 topology = network_topology)
  
  # Agents choose technologies
  agents_sequence <- sample(V(network_used))
  
  for (v in 1:length(agents_sequence)){
    chosen_tech <- choose_tech(network_used, agents_sequence[v], all_techs, 
                               intrinsic_preference=intrinsic_preference, 
                               intrinsic_utilities = intrinsic_utilities,
                               choice_mode=choose_mode)
    network_used <- set_vertex_attr(
      network_used, "technology", index = v, value = chosen_tech)
    current_rel_freqs <- prop.table(table(factor(V(network_used)$technology, 
                                                 levels = levels(all_techs))))
    for (t in all_techs){
      shares_list[[t]][v] <- current_rel_freqs[t]
    }
  }
  
  # Create result frame
  result_frame <- data.frame(shares_list, 
                             row.names = NULL, 
                             stringsAsFactors = FALSE) %>%
    dplyr::mutate(interaction_id=interaction_id,
                  time = 1:length(agents_sequence)) %>%
    pivot_longer(cols = -one_of("interaction_id", "time"), 
                 names_to = "tech", values_to = "share")
  
  return(result_frame)
}

#' Conducts a number of simulation runs
#' 
#' Calls \code{run_simulation} for \code{n_iterations} times and 
#' concatenates the respective data.frames to a single data.frame.
#' 
#' @param n_iterations The number of iterations
#' @param n_agents Number of agents; will potentially adjusted via \code{adj_n}
#' @param n_techs Number of technologies
#' @param network_topology The topology of the network
#' @param choose_mode Specifies whether agents determine network utility via
#'  usage shares of absolute users
#' @param intrinsic_preference The intensity of the personal intrinsic 
#'  preference of the agents
#' @param intrinsic_utilities Vector of length \code{n_techs}; gives intrinsic
#'  values of technologies that are taken into account by all agents, not only 
#'  those that prefer the technology over the others.
#' @return A data.frame with the simulation results
run_n_simulations <- function(n_iterations, n_agents, n_techs, 
                              network_topology, choose_mode, 
                              intrinsic_preference, intrinsic_utilities){
  result_frames <- list()
  for (i in 1:n_iterations){
    print(paste0("Run simulation ", i, "/", n_iterations))
    result_frames[[as.character(i)]] <- run_simulation(
      interaction_id=i, 
      n_agents=n_agents, 
      n_techs=n_techs, 
      network_topology=network_topology, 
      choose_mode=choose_mode, 
      intrinsic_preference=intrinsic_preference, 
      intrinsic_utilities=intrinsic_utilities)
  }
  return(dplyr::bind_rows(result_frames))
}

# Visualization functions------------------------------------------------------

#' Visualizes the dynamics of shares in a single simulation run
#' 
#' @param simul_id The id of the simulation run
#' @param result_frame The result frame as produced by \code{run_n_simulations}
#'  or \code{run_simulation}
#' @return A ggplot object
make_single_simul_dynamics <- function(simul_id, result_frame){
  n_tech <- length(unique(result_frame$tech))
  result_frame %>%
    dplyr::filter(interaction_id==simul_id) %>%
    ggplot(data = ., mapping = aes(x=time, y=share, color=tech, 
                                   linetype=tech, shape=tech)) +
    geom_line() + geom_point(alpha=.5) +
    xlab("Zeit") +
    scale_y_continuous(labels = scales::percent_format(), limits = c(0, 1),
                       name="Nutzer*innenanteil") +
    scale_color_manual(values=wes_palette(
      name = "Darjeeling1", n = n_tech, type = "continuous"), 
      labels=c(1:n_tech), name="Technologie") +
    guides(linetype="none", shape="none") +
    ggtitle(paste0("Nutzer*innenanteile (Simulation ", simul_id, ")")) +
    theme_bw() +
    theme(panel.border = element_blank(), 
          axis.line = element_line(), 
          legend.position = "bottom")
}

#' Visualizes the dynamics of the technologies with highest final usage shares
#' 
#' @param simul_data The simulation results as produced by 
#'  \code{run_n_simulations}
#' @return A ggplot2 object
plot_dynamics_dom_tech <- function(simul_data){
  max_techs <- simul_data %>% 
    dplyr::filter(time==max(simul_data$time)) %>%
    group_by(interaction_id) %>%
    dplyr::filter(share==max(share))
  
  runs <- max_techs$interaction_id
  run_max_tech <- max_techs$tech
  rel_techs <- list()
  for (r in 1:length(runs)){
    rel_techs[[r]] <- simul_data %>%
      dplyr::filter(interaction_id==runs[r],
                    tech==run_max_tech[r])
  }
  
  dplyr::bind_rows(rel_techs) %>%
    ggplot(data = ., mapping = aes(x=time, y=share, 
                                   color=as.factor(interaction_id))) +
    geom_line() + geom_point(alpha=.5) +
    xlab("Zeit") + ylab("Nutzer*innenanteil") +
    scale_y_continuous(labels = scales::percent_format(), limits = c(0, 1)) +
    scale_color_manual(values=wes_palette(
      name = "Darjeeling1", n = length(runs), type = "continuous")
    ) +
    ggtitle(paste0("Nutzer*innenanteile der weitverbreitesten Technologien")) +
    theme_bw() +
    theme(panel.border = element_blank(), 
          axis.line = element_line(), 
          legend.position = "none")
}

#' Visualizes the final shares in the simulation
#' 
#' @param simul_results The simulation results as produced by 
#'  \code{run_n_simulations}
#' @param kind Either 'normal' or 'ranked'. In the former case, results for 
#'  the different technologies as such as are presented. Otherwise, techs
#'  are ranked and grouped by rank, i.e. shares for the techs with highest 
#'  share etc. are shows
#' @return A ggplot2 object
make_final_shares <- function(simul_results, kind="normal"){# "ranked
  if (kind=="normal"){
    title_used <- "Nutzer*innen je Technologie"
    y_name_used <- "Technologie"
    plot_results <- simul_results %>%
      dplyr::filter(time==max(simul_results$time))
  } else if (kind=="ranked"){
    title_used <- "Nutzer*innen nach gerankten Technologien"
    y_name_used <- "Technologie (Rang)"
    
    
    plot_results <- simul_results %>% 
      dplyr::filter(time==max(simul_results$time)) %>%
      group_by(interaction_id) %>%
      dplyr::mutate(tech=rank(-share, ties.method = "first"))
  } else {
    stop(paste("Wrong kind used; allowed: 'normal' or 'ranked', not", kind))
  }
  ggplot(plot_results, aes(x=as.factor(tech), y=as.double(share),
                           fill=as.factor(tech))) +
    geom_boxplot(alpha=0.75) +
    stat_summary(fun=mean, geom="point", 
                 shape=20, size=10, alpha=0.5, 
                 color="black", fill="black") +
    ggtitle(title_used) +
    scale_y_continuous(labels = scales::percent_format(), limits = c(0, 1),
                       name=TeX("Populationsanteil nach $t_{max}$")) +
    scale_x_discrete(
      labels=as.character(1:length(unique(simul_results$tech))), 
      name=y_name_used) +
    scale_fill_brewer(palette="Set1") + 
    theme_bw() +
    theme(panel.border = element_blank(), 
          axis.line = element_line(), 
          legend.position = "none")
} 
