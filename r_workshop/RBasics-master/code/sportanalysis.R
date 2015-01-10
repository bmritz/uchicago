## Reading player data into R
require(dplyr)
require(ggplot2)

ap <- read.csv("./data/allplayerdata.csv", stringsAsFactors=F)

ap <- tbl_df(ap)

########### Team Analysis

team_details <- ap %>%
    group_by(team_name) %>%
    summarise(
        totcost = sum(now_cost), 
        numplayers=n_distinct(id),
        mediancost = median(now_cost), 
        avgcost = round(mean(now_cost),2),
        maxcost = max(now_cost),
        mincost = min(now_cost)
    )

team_details <- team_details %>%
    mutate(costvar = (totcost - mean(totcost)),
           sign = ifelse(costvar > 0, "pos", "neg"))

## Teams arranged by total cost of players
ggplot(team_details,aes(x=reorder(team_name,totcost),y=totcost,fill=totcost)) + 
    geom_bar(stat="identity",color="black") + 
    coord_flip() + 
    xlab("Team Name")
    ylab("Total Cost $MM")


## Cost variation spread over x axis positive and negative
ggplot(team_details, 
       aes(x = reorder(team_name, costvar), y = costvar, fill = sign)) + 
    geom_bar(stat = "identity", position = "identity", 
             color="black", size=0.4) + 
    coord_flip() + 
    xlab("Teams") + 
    ylab("Cost Variation ($MM)") + 
    scale_fill_manual(values = c("pos" = "red", "neg" = "black")) +
    guides(fill=FALSE)

## Colors for positive / negative display

ggplot(team_details,aes(x=team_name,y=costvar,fill=sign)) + 
    geom_bar(stat = "identity", position = "identity",
             color = "black", size = 0.5) + 
    scale_fill_manual(values = c("pos" = "red", "neg" = "black")) + 
    guides(fill = FALSE) + 
    xlab("Teams") + 
    ylab("Cost Variation ($MM)")

####################### Player Type Analysis

player_details <- ap %>%
    group_by(type_name) %>%
    summarise(
        typecount = n(),
        totcost = sum(now_cost), 
        numplayers=n_distinct(id),
        mediancost = median(now_cost),
        avgcost = round(mean(now_cost),2),
        maxcost = max(now_cost),
        mincost = min(now_cost)
    )

player_details <- player_details %>%
    mutate(costvar = (totcost - mean(totcost)),
           sign = ifelse(costvar > 0, "pos", "neg"))

ggplot(player_details,aes(x = reorder(type_name, costvar), 
                          y = costvar, fill = sign)) + 
    geom_bar(stat = "identity", position = "identity", size = 0.4) + 
    geom_text(aes(label = paste0("$",costvar)), vjust = 2,
              position=position_dodge(.9), size = 5) + 
    scale_fill_manual(values = c("pos" = "yellow", "neg" = "darkgray")) +
    guides(fill=FALSE) + 
    xlab("Player Type") + 
    ylab("Cost Variation ($MM)") + 
    coord_flip()