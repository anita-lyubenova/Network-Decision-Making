# Network-Decision-Making

## About the Project

This project explores decision-making in networks using a simple Agent-based Model (ABM). The goal is to simulate how agents in a network make decisions based on their attributes and the attributes of decision options available to them. The project includes an interactive Shiny app that visualizes the network, computes centrality measures, and demonstrates the decision-making process.

## About the Model

The Agent-based Model (ABM) used in this project follows a simple decision-making process:

1. Agents: Agents represent individuals in the network and have attributes that influence their decision-making.

2. Decision Options: Decision options are choices available to the agents, and they also have attributes.

3. Utility Calculation: Agents calculate the utility of each decision option based on the sum of products of their attributes and the attributes of the decision options.

4. Decision: Agents choose the decision option with the highest utility for the current iteration.

5. Communication: After each iteration, agents communicate with their connected peers, and their attitudes shift toward the average attitudes of connected agents. The degree of attitude shift depends on the "suggestibility" parameter. There is also a "stochasticity" parameter that introduces randomness to the attitude shift.

## About the App

The interactive Shiny app provides a user-friendly interface to explore the decision-making process in networks:

1. **Tab 1: Network Visualization and Centrality Measures**
   - Users can generate a random network by specifying the number of agents, number of groups, and link probabilities.
   - The app computes centrality measures (degree, betweenness, closeness, eigenvector, and pagerank) for each agent and displays them in a table.
   - Users can modify the network by editing the adjacency matrix.

2. **Tab 2: Agent-based Model**
   - Users can generate decision options with random attributes.
   - The ABM simulates the decision-making process over multiple iterations, and the app plots the decisions made by agents over time.
   - The "suggestibility" and "stochasticity" parameters can be adjusted to observe their impact on decision-making.

## How to Run the App
The app is available at https://anita-lyubenova.shinyapps.io/Network-Decision-Making/ .

To run the app locally, you will need R and the required R packages, including `shiny`, `shinyMatrix`, `visNetwork`, `dplyr`, `shinyjs`, `DT`, `highcharter`, and `igraph`. 

1. Clone or download this repository to your local machine.
2. Open RStudio or any other R environment.
3. Set the working directory to the folder where you cloned/downloaded the project.
4. Install the required R packages using `install.packages(c("shiny", "shinyMatrix", "visNetwork", "dplyr", "shinyjs", "DT", "highcharter", "igraph"))` if you haven't installed them already.
5. Run the Shiny app by executing `shiny::runApp()` in the R console.

