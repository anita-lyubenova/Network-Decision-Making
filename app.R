library(shiny)
library(shinyMatrix)
library(visNetwork)
library(dplyr)
library(shinyjs)
library(DT)
library(highcharter)
library(igraph)
about_text <- "
<div style='max-width: 800px; margin: 0 auto;'>
  <h2>About the app</h2>

  <p>This app serves as an interactive simulation of decision-making in the context of social networks. It provides a visual and dynamic representation of how agents interact and make decisions within a network.</p>

  <h3>Background</h3>
  <p>Agent-based modeling (ABM) and network simulations are powerful tools used in understanding complex systems, social phenomena, and decision-making processes. By simulating individual agents' behaviors and interactions, we can explore emergent behaviors at the system level.</p>

  <h3>Model Overview</h3>
  <p>In this app, agents represent decision-makers, and each agent has a set of attributes that influence their decisions. Decision options are also defined, each having its own attributes. Agents compute the utility of each option by taking into account the products of their attributes and the option attributes. Subsequently, the agent selects the option with the highest utility for a given iteration.</p>

  <p>After each iteration, there is 'communication' between connected agents in the network. Agents update their attributes by shifting towards the average attitudes of the agents they are connected to. The degree of this shift is determined by the 'suggestibility' parameter.</p>

  <h3>Interactivity</h3>
  <p>The app allows you to experiment with different parameters, such as the number of agents, options, and attributes, as well as the level of 'suggestibility' and 'stochasticity.' Modify the network's structure by editing the adjacency matrix to observe how changes impact decision outcomes.</p>

  <h3>Data Visualization</h3>
  <p>The app offers  visualizations to  gain insights into the decision-making process within the network. You can explore the network graph, centrality measures of nodes, and decision shifts across iterations.</p>

  <h3>Disclaimer</h3>
  <p>Please note that this app is intended for illustrative purposes only. It serves as an initial exploration of phenomena reported in the social networks literature and does not necessarily represent real-world decision-making scenarios. Also, the app has not been extensively tested, and it is possible that bugs exist</p>

  <h3>Author</h3>
  <p>The app was created by Anita Lyubenova . The complete code for the app is available at [link].</p>
</div>
"

#Define classes --------------------------------

# Define the option class
Option <- setClass(
  "Option",
  slots = list(id = "numeric", attributes = "numeric"),
  prototype = list(attributes = numeric())
)
# Functions -----------------------------------

# Create the create_network function
create_network <- function(num_agents, num_groups, prob_within_group, prob_between_groups, symmetric = TRUE) {
  group_assignments <- rep(1:num_groups, each = num_agents / num_groups)
  adj_matrix <- matrix(0, nrow = num_agents, ncol = num_agents)
  
  for (i in 1:(num_agents - 1)) {
    for (j in (i + 1):num_agents) {
      if (group_assignments[i] == group_assignments[j]) {
        if (runif(1) < prob_within_group) {
          adj_matrix[i, j] <- 1
          if (symmetric) {
            adj_matrix[j, i] <- 1
          }
        }
      } else {
        if (runif(1) < prob_between_groups) {
          adj_matrix[i, j] <- 1
          if (symmetric) {
            adj_matrix[j, i] <- 1
          }
        }
      }
    }
  }
  dimnames(adj_matrix)<-list(1:num_agents, 1:num_agents)
  return(list(adj_matrix = adj_matrix, group_assignments = group_assignments))
}

#function to simulate the options
o<-function(num_options, num_attributes,seed){
  set.seed(seed)
  
  options <- vector("list", num_options)
  for (i in 1:num_options) {
    options[[i]] <- new("Option", id = i, attributes = runif(num_attributes,min=-0.5,max=0.5))
    #options[[i]] <- new("Option", id = i, attributes = rnorm(num_attributes,mean=0.5,sd=1))
  }
  
  names(options)<-paste0("opt", 1:num_options)
  
  # option_attr<-sapply(options, USE.NAMES = TRUE, function(x){
  #   x@attributes%>% 
  #     setNames(paste0("attr",1:num_attributes))
  # })
  # 
  # #the total score of each option according to sum of their attributes
  # option_score<-colSums(option_attr) %>% as.data.frame()
  
  return(options)
  #paste0(paste0(names(option_score) ,": ", round(option_score,2)), collapse =  " | ")
}#end of options function


# Function to compute node centralities using igraph
compute_centralities <- function(adj_matrix) {
  g <- igraph::graph_from_adjacency_matrix(adj_matrix, mode = "undirected")
  centralities <- data.frame(node = 1:vcount(g),
                             degree = degree(g),
                             betweenness = betweenness(g),
                             closeness = closeness(g) ,
                             eigenvector = eigen_centrality(g)$vector,
                             pagerank=page_rank(g)$vector
  )
  return(centralities)
}

m<-function(#simulation controls
  num_agents=20,
  num_options=4,
  num_attributes=7,
  num_iterations=30,
  suggestibility=0.5, #the degree to which the attitudes of connecting agents influence the updated attitudes
  stochasticity=0.5, #random component when updating the attitudes of agents (sd of a normal dist with mu=0)
  #external data
  options, #initialized (decision) options
  adj_matrix,  #adjacency matrix - determines connected/influencing agents
  group_assignments,#group information for each agent
  # adj_matrix info - not needed for computations
  prob_within_group, #probability of a link between any two nodes
  prob_between_groups,
  symmetric
){
  
  # # Initialize agents
  # agents <- vector("list", num_agents)
  # for (i in 1:num_agents) {
  #   agents[[i]] <- new("Agent", id = i, attributes = rnorm(num_attributes,0,1), decision = NA_integer_)
  # }
  #a list to hold information about each agent
  agents <- vector("list", num_agents)
  for (i in 1:num_agents) {
    agents[[i]] <- list(
      id = i,
      attributes = rnorm(num_attributes, 0, 1),
      # decision = vector("integer", num_iterations),
      # attributes_history = matrix(NA, nrow = num_iterations, ncol = num_attributes,
      #                             dimnames = list(paste0("iter", 1:num_iterations),
      #                                             paste0("attr", 1:num_attributes)
      #                                             )),
      group = group_assignments[i]
    )
  }
  
  # R<-array(NA,dim = c(num_agents,
  #                     num_options,
  #                     num_iterations),
  #          dimnames=list(agent=1:num_agents,
  #                        option=1:num_options,
  #                        iter=1:num_iterations)
  # )
  
  
  
  R<-matrix(NA, ncol = 4+num_attributes, nrow = num_agents*num_iterations) %>% as.data.frame()
  colnames(R)<-c("agent",  "iter","group", "decision",  paste0("attr", 1:num_attributes))
  R$agent<-rep(c(1:num_agents),times=num_iterations)
  R$iter<-rep(c(1:num_iterations),each=num_agents)
  
  
  #Iterations loop  
  for (iteration in 1:num_iterations) {
    # Decision-making loop
    for (i in 1:num_agents) {
      agent <- agents[[i]]
      
      # Calculate utility for each option
      utilities <- numeric(num_options)
      for (j in 1:num_options) {
        option <- options[[j]]
        utilities[j] <- sum(agent$attributes * option@attributes)
      }
      
      
      # Make a decision based on the max utility and
      decision<-which.max(utilities)
      
      # store it iin the agent's list
      # agents[[i]]$decision[iteration] <- decision
      # #save the decision in an array
      # R[agent$id,which.max(utilities),iteration]<-1
      
      R[R$agent==i & R$iter==iteration,3:ncol(R)]<-c(agent$group, decision, agent$attributes)
      
      
    } # decisions are made
    
    ### Update attributes
    # At the end of each iteration update agent attributes based on the influence of connected agents
    for (i in 1:num_agents) {
      agent <- agents[[i]]
      
      # Identify the connected agents based on the adjacency matrix
      connected_agents <- which(adj_matrix[i, ] == 1)
      
      # Update agent attributes based on the current attributes and the attributes of connected agents
      if (length(connected_agents) > 0) {
        connected_attributes <- sapply(agents[connected_agents], function(agent) agent$attributes)
        updated_attributes <- agent$attributes + suggestibility * rowMeans(connected_attributes) + rnorm(num_attributes, sd = stochasticity)
        # beta=rowMeans(connected_attributes)/stochasticity^2
        # alpha=beta*rowMeans(connected_attributes)
        # updated_attributes <- rgamma(num_options, shape=alpha, rate=beta)#rnorm(num_options, mean=0, sd = stochasticity)
        
      }else{
        updated_attributes <-agent$attributes + rnorm(num_attributes, sd = stochasticity)
      }
      
      #agents[[i]]@attributes <- pmax(0, pmin(1, updated_attributes))  # Keep the updated attributes within [0, 1] range
      agents[[i]]$attributes <- updated_attributes
      
      #agents[[i]]$attributes_history[iteration,] <- updated_attributes
    }
    
    # # Update option attributes (example: randomly change attributes)
    # for (i in 1:num_options) {
    #   options[[i]]@attributes <- runif(num_options)
    # }
  } # end of iterations loop
  
  
  return(list(R=R,
              agents=agents,
              num_agents=num_agents,
              num_options=num_options,
              num_iterations=num_iterations,
              num_attributes=num_attributes,
              suggestibility=suggestibility,
              stochasticity=stochasticity, 
              #external data
              options=options,
              adj_matrix=adj_matrix,
              group_assignments=group_assignments,
              # adj_matrix info - not needed for computations
              prob_within_group=prob_within_group, #probability of a link between any two nodes
              prob_between_groups=prob_between_groups,
              symmetric=symmetric
  )
  )
  
}#end of function m()



plot_decisions <- function(X, centralities) {
  # Extract necessary variables from the list
  for (i in 1:length(X)) {
    assign(names(X)[i], X[[i]])
  }
  
  # Compute options score
  option_attr <- sapply(options, USE.NAMES = TRUE, function(x) {
    x@attributes %>% setNames(paste0("attr", 1:num_attributes))
  })
  
  # The total score of each option according to the sum of their attributes
  option_score <- colSums(option_attr)
  
  lineplot.subtitle <- paste0(paste0(names(option_score), ": ", round(option_score, 2)), collapse = " | ")
  
  lineplot.caption <- paste0("suggestibility: ", suggestibility,
                             "| stochasticity: ", stochasticity,
                             "| agents: ", num_agents,
                             "| attributes: ", num_attributes)
  
  
  all_colors<-viridis::viridis(length(unique(group_assignments)))
  
  xseries <-  R %>%
    mutate(jitter=runif(nrow(R),-0.2,0.2),
           decision_jitter=decision+jitter,
           color=all_colors[group]) %>%
    # use `name` to name  series according the value of `cat` avariable
    select(agent,iter,decision_jitter) %>%
    rename(x=iter, y=decision_jitter) %>%
    group_by(name = agent) %>%
    do(data = list_parse2(.[, c("x","y")])) %>%
    # add type of series
    mutate(type = "line",
           group=group_assignments[name],
           color=all_colors[group]) 
  
  
  highchart2() %>% 
    hc_add_series_list(xseries)%>%#
    hc_plotOptions(line=list(marker=list(enabled=FALSE))) %>% 
    hc_xAxis(categories = as.character(1:num_iterations), title = list(text = "Iterations")) %>%
    hc_yAxis(categories = as.character(1:num_options), reversed = TRUE, title = list(text = "Option")) %>% 
    hc_plotOptions(
      line = list(
        lineWidth = 1.5,
        marker = list(enabled = FALSE),
        animation = TRUE
      )
    ) %>%
    hc_legend(enabled = FALSE)%>%
    hc_tooltip(enabled=FALSE)
  
  
  
}


#UI ----------------------------------------------------------------------------
ui <- navbarPage(
  title = "Decision-Making in Networks: A Basic ABM",
  useShinyjs(),
  tabPanel("Network Simulation",
           h2("Network Simulation"),
           p("In this tab, you can generate a random network with agents (nodes) and connections (links) between them."),
           p("The network can be modified by adjusting the number of agents, the number of groups, and the probabilities of links within and between groups."),
           sidebarLayout(
             sidebarPanel(
               numericInput("num_agents", "Number of Agents:", value = 10, min = 1),
               numericInput("num_groups", "Number of Groups:", value = 1, min = 1),
               sliderInput("prob_within_group", "Link Probability Within Group:",
                           value = 0.3, min = 0, max = 1, step = 0.01),
               sliderInput("prob_between_groups", "Link Probability Between Groups:",
                           value = 0.1, min = 0, max = 1, step = 0.01),
               checkboxInput("symmetric", "Symmetric", value = TRUE),
               actionButton("generate_matrix", "Generate Matrix")
             ),
             mainPanel(
               h4("Network Visualization"),
               visNetworkOutput("graph_output"),
               DTOutput("centrality_output"),
               verbatimTextOutput("test_output"),
               checkboxInput("show_matrix", "Show/Edit Adjacency Matrix", value = FALSE),
               # Hide/show the matrix and the "Update Matrix" button div
               shinyjs::hidden(
                 tags$div(
                   id = "matrix_with_button",
                   style = "display:none;",
                   matrixInput("adj_matrix", "Adjacency Matrix:",
                               value = matrix(0, nrow = 10, ncol = 10, dimnames = list(1:10,
                                                                                       1:10)),
                               rows = list(names=TRUE,extend = FALSE, delete = FALSE),
                               cols = list(names=TRUE,extend = FALSE, delete = FALSE),
                               class = "numeric"),
                   actionButton("update_matrix", "Update Matrix"),
                   tags$script("Shiny.addCustomMessageHandler('resetValue', function(variableName) {
                                    Shiny.onInputChange(variableName, null);
                                  });
                                ")
                 )
               )
             )
           )
  ),
  tabPanel("Agent-based Model",
           h2("Agent-based Model"),
           p("In this tab, you can run a basic Agent-based Model (ABM) for decision-making in networks."),
           p("The main characteristics of the ABM are:"),
           tags$ul(
             tags$li("The attributes of decision options and the corresponding agent attributes/attitudes are randomly sampled."),
             tags$li("The utility of each option is computed as the sum of products of agent attributes and option attributes."),
             tags$li("The agents choose the option with the highest utility for the current iteration."),
             tags$li("After each iteration, there is 'communication' between the agents, in which each agent shifts their attitudes toward the average attitudes of the connected agents."),
             tags$li("The degree of this shift depends on the 'suggestibility' parameter. In each change, there is also a 'stochasticity' parameter that can be varied and determines the degree of randomness in the attitude shift.")
           ),
           sidebarLayout(
             sidebarPanel(
               h5("Define decision options to choose from"),
               numericInput("num_options", "Number of Options:", value = 4, min = 1),
               numericInput("num_attributes", "Number of Attributes:", value = 7, min = 1),
               numericInput("seed", "Seed:", value = 123),
               actionButton("generate_options", "Generate Options"),
               hr(),
               
               numericInput("num_iterations", "Number of Iterations:", value = 30, min = 1),
               sliderInput("suggestibility", "Suggestibility:",
                           value = 0.5, min = 0, max = 1, step = 0.01),
               sliderInput("stochasticity", "Stochasticity:",
                           value = 0.5, min = 0, max = 1, step = 0.01),
               actionButton("run_abm", "Run Agent-based Model")
             ),
             mainPanel(
               h4("Options"),
               # Add any visualization or result output related to the agent-based model here
               tableOutput("options_table"),
               h4("Decisions of each agent across iterations"),
               highchartOutput("decisions_plot")
             )
           )
  ),
  tabPanel(
    "About",
    tags$div(
      id = "about_tab_content",
      style = "padding: 20px;",
      HTML(about_text)
    )
  )
)

server <- function(input, output, session) {
  
  session$onSessionEnded(function() {
    stopApp()
  })
  
  # Tab Network ---------------------------------
  # Reactive values to track the changes
  adj_matrix_reactive <- reactiveVal(matrix(0, nrow = 10, ncol = 10))
  group_assignments_reactive <- reactiveVal(rep(1, 10)) # Set default value for group_assignments
  centralities_react<-reactiveVal(NULL)
  
  observeEvent(input$generate_matrix, {
    result <- isolate({
      create_network(input$num_agents, input$num_groups,
                     input$prob_within_group, input$prob_between_groups,
                     input$symmetric)
    })
    adj_matrix <- result$adj_matrix
    group_assignments <- result$group_assignments
    
    adj_matrix_reactive(adj_matrix)
    group_assignments_reactive(group_assignments)
    
    updateMatrixInput(session, "adj_matrix", value = adj_matrix)
    
    # Compute node centralities
    centralities<-compute_centralities(adj_matrix_reactive())
    centralities_react(centralities)
  })
  
  observeEvent(input$update_matrix, {
    adj_matrix_reactive(input$adj_matrix)
    
    # Compute node centralities
    centralities<-compute_centralities(adj_matrix_reactive())
    centralities_react(centralities)
  })
  
  
  output$centrality_output<-renderDT({
    if(!is.null(centralities_react())){
      datatable(round(centralities_react(),digits = 2),
                selection = "single")
    }
    
  })
  
  net_react<-reactiveVal(NULL)
  
  output$graph_output <- renderVisNetwork({
    adj_matrix <- adj_matrix_reactive()
    group_assignments <- group_assignments_reactive()
    all_colors<-viridis::viridis(length(unique(group_assignments)))
    
    # Visualization of the graph using visNetwork
    edges_df <- which(adj_matrix != 0, arr.ind = TRUE) %>%
      as.data.frame() %>%
      rename(from = row, to = col) %>%
      mutate(weight = adj_matrix_reactive()[which(adj_matrix_reactive() != 0, arr.ind = TRUE)],
             arrows="to"
      ) %>%
      arrange(from) 
    
    nodes_df <- isolate({
      seq_len(input$num_agents) %>%
        as_tibble() %>%
        rename(id = value) %>%
        mutate(group = factor(group_assignments),
               label = as.character(id),
               color=all_colors[group])
    })
    
    net <- visNetwork(nodes_df, edges_df) %>%
      visNodes(size = 15, color = list(background = "white", border = "black")) %>%
      visEdges(smooth = TRUE) %>%
      visPhysics(solver = "forceAtlas2Based",
                 forceAtlas2Based = list(gravitationalConstant = -20,
                                         springConstant = 0.02))%>% 
      visOptions(highlightNearest = TRUE,
                 nodesIdSelection=TRUE) %>% 
      visEvents(select = "function(nodes) {
                Shiny.onInputChange('graph_output_select', nodes);
                ;}"
      )
    
    net_react(net)
    return(net)
  })
  
  ##Interactions Network - Table ----------
  observeEvent(input$graph_output_select$nodes,{
    if(length(input$graph_output_select$nodes) != 0){
      # Highlight the corresponding row in the centrality_output table
      DT::dataTableProxy('centrality_output') %>%
        selectRows(selected = unlist(input$graph_output_select$nodes))
    }else{
      DT::dataTableProxy('centrality_output') %>%
        selectRows(selected =NULL)
    }
    
    
  })
  
  
  observeEvent(input$centrality_output_rows_selected, {
    if(!is.null(input$centrality_output_rows_selected)){
      # Highlight the corresponding node in the graph_output
      visNetworkProxy("graph_output") %>%
        visSelectNodes(id=input$centrality_output_rows_selected)
    }
  })
  
  
  
  # Hide/show adjacency matrix based on checkbox input
  observe({
    if (input$show_matrix) {
      shinyjs::show("matrix_with_button")
    } else {
      shinyjs::hide("matrix_with_button")
    }
  })
  
  
  # Tab ABM ---------------------------------
  ##o()-----------------------------------------
  # Reactive value to store the generated options
  options_react <- reactiveVal(matrix("Generate options to see their attributes here",1,1))
  
  observeEvent(input$generate_options, {
    # Call the 'o' function to generate options
    options<- o(input$num_options, input$num_attributes, input$seed)
    
    # #the total score of each option according to sum of their attributes
    # option_score<-colSums(option_attr) %>% as.data.frame()
    
    options_react(options)
    
    
  })
  
  
  # Display the generated options in a table
  output$options_table <- renderTable({
    options<- options_react()
    
    if (length(options)>1) {
      
      option_attr<-sapply(options, USE.NAMES = TRUE, function(x){
        x@attributes%>%
          setNames(paste0("attr",1:input$num_attributes))
      })
      
      options_df <- rbind(option_attr,colSums(option_attr))
      
      rownames(options_df)<-c(rownames(option_attr), "sum")
      options_df
    }else{
      
      options
    }
  },rownames =TRUE)
  
  ## m() ------------------------------------------
  abm_react<-reactiveVal("Please generate options before running the Agent-based Model.")
  decisions_plot_react <- reactiveVal(NULL)
  
  
  observeEvent(input$run_abm, {
    
    # Check if the options have been generated
    options<- options_react()
    
    if (!is.null(options)) {
      # Call the 'm' function with the necessary inputs
      abm_result <-
        m(
          num_agents = input$num_agents,
          num_options = input$num_options,
          num_attributes = input$num_attributes,
          num_iterations = input$num_iterations,
          suggestibility = input$suggestibility,
          stochasticity = input$stochasticity,
          options = options, # Use the generated options as input
          adj_matrix = adj_matrix_reactive(), # Use the adjacency matrix from the first tab
          group_assignments = group_assignments_reactive(),
          prob_within_group = input$prob_within_group,
          prob_between_groups = input$prob_between_groups,
          symmetric = input$symmetric
        )
      
      abm_react(abm_result)
      
      decisions_plot <- plot_decisions(abm_result, centralities = centralities_react())
      decisions_plot_react(decisions_plot)
      
      
    } 
  })
  
  # output$test<-renderDT({
  #   abm_react()$R%>%
  #     mutate(jitter=runif(nrow(abm_react()$R),-0.8,0.8))
  # })
  
  
  output$decisions_plot <- renderHighchart({
    decisions_plot_react()
    
  })
  
  
  
}




shinyApp(ui, server)
