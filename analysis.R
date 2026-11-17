# =============================================================================
# Cracking the NCAA Team Rankings Code: analysis script (R)
# Kyla Rounsoville, Maddie Wilson and Alana Lee, Fall 2024
#
# Scrapes team statistics from saved ESPN pages (data_raw/*.html) and the
# Associated Press Top 25 from ESPN's rankings pages, then builds the charts
# comparing each year's #1 team with every other team, 2014-2023:
#   - 2023 factors: touchdowns, rushing yards/game, yards/pass, offensive yards/game
#   - touchdowns, defensive yards/game, offensive yards/game and yards/pass over time
#
# Note: the saved ESPN HTML files in data_raw/ are not included in this repository.
# Full write-up: https://eda.seas.gwu.edu/showcase/2024-Fall/ncaa-rankings.html
# =============================================================================

library(rvest)
library(dplyr)

data_list23 <- read_html(here::here('data_raw', '2023_passing_offense.html')) %>% 
    html_table()

data23<- cbind(data_list23[[1]], data_list23[[2]])
#View(data23)


top25 <- c(
    "Ohio State Buckeyes", 
    "Notre Dame Fighting Irish", 
    "Alabama Crimson Tide", 
    "Clemson Tigers", 
    "Iowa Hawkeyes",
    "LSU Tigers",
    "Michigan Wolverines",
    "Oklahoma State Cowboys",
    "Texas Longhorns",
    "Georgia Bulldogs",
    "Washington Huskies",
    "Cincinnati Bearcats",
    "Florida Gators",
    "Oklahoma Sooners",
    "Oregon Ducks",
    "USC Trojans",
    "Utah Utes",
    "Auburn Tigers",
    "Boise State Broncos",
    "Florida State Seminoles",
    "Miami Hurricanes",
    "Northwestern Wildcats",
    "Oklahoma Sooners",
    "Tennessee Volunteers",
    "Penn State Nittany Lions",
    "Texas A&M Aggies")

filtered_data23 <- data23 %>%
    mutate(InTop25 = Team %in% top25) %>% 
    filter(InTop25 == TRUE)
#View(filtered_data23)

#View(filtered_data23)
library(ggplot2)

team_colors <- c(
    "Michigan Wolverines" = "blue",
    "Alabama Crimson Tide" = "red",
    "Texas Longhorns" = "darkorange",
    "Washington Huskies" = "purple")

ggplot(filtered_data23, aes(x = Team, y = YDS, fill = Team)) +
    geom_bar(stat = "identity") +
    scale_fill_manual(values = team_colors) +
    theme_minimal() +
    labs(title = "YDS by Team", x = "Team", y = "Score")

# Load necessary packages
library(ggplot2)
library(dplyr)


filtered_data23$YDS <- gsub("[^0-9.-]", "", filtered_data23$YDS) 

filtered_data23$YDS <- as.numeric(filtered_data23$YDS)
#View(filtered_data23)


yds_by_team <- ggplot(filtered_data23, aes(x = reorder(Team, YDS), y = YDS, fill = Team)) +  # Reorder Team by yards on x-axis
    geom_bar(stat = "identity") +
    scale_fill_manual(values = team_colors) +
    labs(x = "Team", y = "Total Offensive Yards", title = "Total Offensive Yards by Team in 2023 Season") +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))+
    theme(legend.position = "none")# Rotate x-axis labels for readability
yds_by_team


yds_by_game <- ggplot(filtered_data23, aes(x = reorder(Team, `YDS/G`), y = `YDS/G`, fill = Team)) +  # Reorder Team by YDS/G
    geom_bar(stat = "identity") +
    scale_fill_manual(values = team_colors) +
    labs(x = "Offensive Yards Per Game", y = "Team", title = "Offensive Yards Per Game in 2023 Season") +  # Adjust labels for flipped axes
    theme(axis.text.y = element_text(angle = 0, hjust = 1),  # Adjust for readability on flipped axis
          legend.position = "none") +  # Remove legend
    coord_flip()  # Flip x and y axes

yds_by_game


TotalTD <- ggplot(filtered_data23, aes(x = reorder(Team, TD), y = TD, fill = Team)) +  # Reorder Team by TD
    geom_bar(stat = "identity") +
    scale_fill_manual(values = team_colors) +
    labs(x = "# of Touchdowns", y = "Team", title = "Number of Total Touchdowns") +  # Adjust labels for flipped axes
    theme(axis.text.y = element_text(angle = 0, hjust = 1),  # Adjust for readability on flipped axis
          legend.position = "none") +  # Remove legend
    coord_flip()  # Flip x and y axes

TotalTD


filtered_data23 %>% arrange(SACK)

completion_percentage <- ggplot(filtered_data23, aes(x = reorder(Team, `CMP%`), y = `CMP%`, fill = Team)) +  # Reorder Team by CMP% on x-axis
    geom_bar(stat = "identity") +
    scale_fill_manual(values = team_colors) +
    labs(x = "Team", y = "Completion Percentage", title = "Completion Percentage by Team") +
    theme(axis.text.x = element_text(angle = 45, hjust = 1),  # Rotate x-axis labels for readability
          legend.position = "none")  # Remove legend

completion_percentage


AVG_Yards_per_play <- ggplot(filtered_data23, aes(x = reorder(Team, AVG), y = AVG, fill = Team)) +  # Reorder Team by AVG
    geom_bar(stat = "identity") +
    scale_fill_manual(values = team_colors) +
    labs(x = "Average Yards per Gain", y = "Team", title = "Average Yards Per Gain") +  # Adjust labels after flipping
    theme(axis.text.y = element_text(angle = 0, hjust = 1),  # Adjust for vertical axis
          legend.position = "none") +  # Remove legend
    coord_flip()  # Flip x and y axes

AVG_Yards_per_play




library(rvest)
library(ggplot2)
library(tidyr)
library(dplyr)
library(stringdist)
library(janitor)
library(stringr)
#install.packages("stringdist")

football_defense_list <- read_html(here::here('data_raw', 'defense_23.html')) %>% 
    html_table()
defense2023 <- cbind(football_defense_list[[1]], football_defense_list[[2]])

new_headers <- defense2023[1, ]  # Extract the first row as new headers
defense2023 <- defense2023[-1, ]  # Remove the first row from the dataset
names(defense2023) <- new_headers

rownames(defense2023) <- NULL



colnames(defense2023)[1:10] <- c(
    'Team',
    'Games Played', 
    'Total YDS', 
    'Total YDS/Game', 
    'Passing YDS', 
    'Passing YDS/Game', 
    'Rushing YDS', 
    'Rushing YDS/Game', 
    'Points', 
    'Points/Game'
)

long_defense_data <- defense2023 %>%
    pivot_longer(cols = c('Games Played':'Points/Game'),  # Specify the range using column names
                 names_to = "Metric", 
                 values_to = "Value") 

create_bar_chart <- function(metric_name) {
    ggplot(filter(long_defense_data, Metric == metric_name), aes(x = Team, y = as.numeric(Value), fill = Team)) +
        geom_bar(stat = "identity") +
        theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
        labs(title = paste("Comparison of", metric_name, "across Teams"),
             x = "Teams",
             y = metric_name) +
        guides(fill = "none")  # Use "none" instead of FALSE to remove the fill legend
}

# List of metrics for which we want to create bar charts
metrics <- c('Games Played', 'Total YDS', 'Total YDS/Game', 'Passing YDS', 
             'Passing YDS/Game', 'Rushing YDS', 'Rushing YDS/Game', 'Points', 'Points/Game')

# Generate the bar charts for each metric
#for (metric in metrics) {
    #print(create_bar_chart(metric))
#}

# Identify rows that can't be converted to numeric
problematic_rows <- long_defense_data %>%
    filter(is.na(as.numeric(Value))) %>%
    distinct()

# Display the problematic rows
#print(problematic_rows)

football_defense_list <- read_html(here::here('data_raw', 'defense_23.html')) %>% 
    html_table()
defense2023 <- cbind(football_defense_list[[1]], football_defense_list[[2]])

new_headers <- defense2023[1, ]  # Extract the first row as new headers
defense2023 <- defense2023[-1, ]  # Remove the first row from the dataset
names(defense2023) <- new_headers

rownames(defense2023) <- NULL



colnames(defense2023)[1:10] <- c(
    'Team',
    'Games Played', 
    'Total YDS', 
    'Total YDS/Game', 
    'Passing YDS', 
    'Passing YDS/Game', 
    'Rushing YDS', 
    'Rushing YDS/Game', 
    'Points', 
    'Points/Game'
)

long_defense_data <- defense2023 %>%
    pivot_longer(cols = c('Games Played':'Points/Game'),  # Specify the range using column names
                 names_to = "Metric", 
                 values_to = "Value") 

# Identify rows that can't be converted to numeric
problematic_rows <- long_defense_data %>%
    filter(is.na(suppressWarnings(as.numeric(Value)))) %>%  # This will filter rows that contain non-numeric values
    distinct()

# View the problematic rows
#print(problematic_rows)

# Display the problematic rows
#print(problematic_rows)
# Remove commas from the Value column and convert to numeric
long_defense_data <- long_defense_data %>%
    mutate(Value = as.numeric(gsub(",", "", Value)))

# Check for any remaining NA values
problematic_rows <- long_defense_data %>%
    filter(is.na(Value)) %>%
    distinct()

## View the problematic rows
#print(problematic_rows)


years <- 2016:2023

all_data <- data.frame()

for (year in years) {
    
    url <- paste0("https://www.espn.com/college-football/rankings/_/poll/1/week/1/year/", year, "/seasontype/3")
    
    page <- tryCatch({
        read_html(url)
    }, error = function(e) {
        message(paste("Error reading page for year:", year))
        return(NULL)
    })
    
    
    if (!is.null(page)) {
        
        table <- page %>% html_node("table") 
        
        
        if (!is.null(table)) {
            data <- table %>%
                html_table(fill = TRUE)
            
            data$Year <- year
            all_data <- bind_rows(all_data, data)
        } else {
            message(paste("No table found for year:", year))
        }
    }
}



all_data <- all_data %>% 
    select("RK",'Team', "REC","PTS","Year") 

top_teams <- all_data %>%
    group_by(Team) %>%
    summarize(count = n()) %>% 
    arrange(desc(count)) %>% 
    slice_head(n = 25) 

write.csv(top_teams, "top_teams.csv", row.names = FALSE)



top25 <- c(
    "Ohio State Buckeyes",
    "Notre Dame Fighting Irish",
    "Alabama Crimson Tide",
    "Clemson Tigers",
    "Iowa Hawkeyes",
    "LSU Tigers",
    "Michigan Wolverines",
    "Oklahoma State Cowboys",
    "Texas Longhorns",
    "Georgia Bulldogs",
    "Washington Huskies",
    "Cincinnati Bearcats",
    "Florida Gators",
    "Oklahoma Sooners",
    "Oregon Ducks",
    "USC Trojans",
    "Utah Utes",
    "Auburn Tigers",
    "Boise State Broncos",
    "Florida State Seminoles",
    "Miami Hurricanes",
    "Northwestern Wildcats",
    "Oklahoma Sooners",
    "Tennessee Volunteers",
    "Penn State Nittany Lions", 
    "Texas A&M Aggies"
    )

long_defense_data <- long_defense_data %>%
    mutate(InTop25 = Team %in% top25) %>%
    filter(InTop25 == TRUE)

# Bar Charts

long_defense_data$Color <- ifelse(long_defense_data$Team %in% 
                                      c("Michigan Wolverines", "Washington Huskies", "Texas Longhorns", "Alabama Crimson Tide"), 
                                  "lightblue", "grey")




rushing_yds <- ggplot(filter(long_defense_data, Metric == 'Rushing YDS/Game'), 
                      aes(x = reorder(Team, -Value), y = Value, fill = Team)) +  # Reorder Team by Value
    geom_bar(stat = "identity") +
    scale_fill_manual(values = team_colors) +  # Use custom team colors
    labs(
        x = "Rushing YDS/Game",
        y = "Team",
        title = "Comparison of Rushing YDS/Game across Teams"
    ) +
    theme(
        axis.text.y = element_text(angle = 0, hjust = 1),  # Adjust for flipped axes
        legend.position = "none"  # Remove the legend
    ) +
    coord_flip()  # Flip x and y axes

rushing_yds
AVG_Yards_per_play
TotalTD
yds_by_game



library(cowplot)

# Combine the four plots into a grid
combined_plot <- plot_grid(
    rushing_yds, AVG_Yards_per_play, TotalTD, yds_by_game,  # The plots to combine
    ncol = 2,  # Arrange plots in 2 columns
    align = "v"  # Align vertically within columns
)


# Display the combined plot
combined_plot


library(cowplot)

# Reduce text size for each plot


rushing_yds <- rushing_yds + theme(plot.title = element_text(size = 10), axis.text = element_text(size = 5))
AVG_Yards_per_play <- AVG_Yards_per_play + theme(plot.title = element_text(size = 10), axis.text = element_text(size = 5))
TotalTD <- TotalTD + theme(plot.title = element_text(size = 10), axis.text = element_text(size = 5))
yds_by_game <- yds_by_game + theme(plot.title = element_text(size = 10), axis.text = element_text(size = 5))

# Combine all plots in a single row
combined_plot <- plot_grid(
    rushing_yds, 
    AVG_Yards_per_play, 
    TotalTD, 
    yds_by_game,
    ncol = 4  # Arrange plots in a single row
)

# Display the combined plot
combined_plot


# Define a consistent order for teams (e.g., alphabetical)
library(dplyr)

# Arrange teams by most TD and create consistent order
consistent_team_order <- filtered_data23 %>%
    arrange(desc(TD)) %>%  # Sort teams by TD in descending order
    pull(Team) %>%         # Extract the sorted team names
    unique()               # Ensure unique team names

# Apply this order to the 'Team' column
filtered_data23$Team <- factor(filtered_data23$Team, levels = consistent_team_order)

# If using another dataset, apply the same factor levels
long_defense_data$Team <- factor(long_defense_data$Team, levels = consistent_team_order)


# Apply this order to all datasets
filtered_data23$Team <- factor(filtered_data23$Team, levels = consistent_team_order)
long_defense_data$Team <- factor(long_defense_data$Team, levels = consistent_team_order)



avg_rushing <- mean(filter(long_defense_data, Metric == 'Rushing YDS/Game')$Value, na.rm = TRUE)

rushing_yds <- ggplot(filter(long_defense_data, Metric == 'Rushing YDS/Game'), 
                      aes(x = Value, y = Team, fill = Team)) +  # Teams aligned on y-axis
    geom_bar(stat = "identity") +
    geom_vline(xintercept = avg_rushing, linetype = "dashed", color = "black", size = 0.75) +  # Add average line
    scale_fill_manual(values = team_colors) +  # Use custom team colors
    labs(
        x = "Rushing YDS/Game",
        y = "Team",
        title = "Comparison of Rushing YDS/Game across Teams"
    ) +
    theme(
        axis.text.y = element_text(size = 8),
        legend.position = "none"  # Remove the legend
    )
avg_yards <- mean(filtered_data23$AVG, na.rm = TRUE)

AVG_Yards_per_play <- ggplot(filtered_data23, 
                             aes(x = AVG, y = Team, fill = Team)) +  # Teams aligned on y-axis
    geom_bar(stat = "identity") +
    geom_vline(xintercept = avg_yards, linetype = "dashed", color = "black", size = 0.75) +  # Add average line
    scale_fill_manual(values = team_colors) +
    labs(
        x = "Average Yards per Play",
        y = "Team",
        title = "Average Yards Per Play by Team"
    ) +
    theme(
        axis.text.y = element_text(size = 8),
        legend.position = "none"
    )
avg_td <- mean(filtered_data23$TD, na.rm = TRUE)

TotalTD <- ggplot(filtered_data23, 
                  aes(x = TD, y = Team, fill = Team)) +  # Teams aligned on y-axis
    geom_bar(stat = "identity") +
    geom_vline(xintercept = avg_td, linetype = "dashed", color = "black", size = 0.75) +  # Add average line
    scale_fill_manual(values = team_colors) +
    labs(
        x = "Total Touchdowns",
        y = "Team",
        title = "Total Touchdowns by Team"
    ) +
    theme(
        axis.text.y = element_text(size = 8),
        legend.position = "none"
    )

avg_yds_game <- mean(filtered_data23$`YDS/G`, na.rm = TRUE)

yds_by_game <- ggplot(filtered_data23, 
                      aes(x = `YDS/G`, y = Team, fill = Team)) +  # Teams aligned on y-axis
    geom_bar(stat = "identity") +
    geom_vline(xintercept = avg_yds_game, linetype = "dashed", color = "black", size = 0.75) +  # Add average line
    scale_fill_manual(values = team_colors) +
    labs(
        x = "Offensive Yards per Game",
        y = "Team",
        title = "Offensive Yards per Game by Team"
    ) +
    theme(
        axis.text.y = element_text(size = 8),
        legend.position = "none"
    )

combined_plot <- plot_grid(
    TotalTD, 
    rushing_yds, 
    AVG_Yards_per_play, 
    yds_by_game,
    ncol = 4  # Arrange plots in a single row
)

# Display the combined plot
combined_plot


rushing_yds <- ggplot(filter(long_defense_data, Metric == 'Rushing YDS/Game'), 
                      aes(x = Value, y = Team, fill = Team)) +
    geom_bar(stat = "identity") +
    geom_vline(xintercept = avg_rushing, linetype = "dashed", color = "black", size = 0.75) +
    scale_fill_manual(values = team_colors) +
    labs(
        x = "Rushing YDS/Game",
        y = NULL,  # Remove y-axis title
        title = "Rushing YDS/Game Per Teams"
    ) +
    theme(
        axis.text.y = element_blank(),  # Remove team names
        legend.position = "none"
    )

AVG_Yards_per_play <- ggplot(filtered_data23, 
                             aes(x = AVG, y = Team, fill = Team)) +
    geom_bar(stat = "identity") +
    geom_vline(xintercept = avg_yards, linetype = "dashed", color = "black", size = 0.75) +
    scale_fill_manual(values = team_colors) +
    labs(
        x = "Average Yards per Play",
        y = NULL,  # Remove y-axis title
        title = "Average Yards/Pass by Team"
    ) +
    theme(
        axis.text.y = element_blank(),  # Remove team names
        legend.position = "none"
    )

TotalTD <- ggplot(filtered_data23, 
                  aes(x = TD, y = Team, fill = Team)) +
    geom_bar(stat = "identity") +
    geom_vline(xintercept = avg_td, linetype = "dashed", color = "black", size = 0.75) +
    scale_fill_manual(values = team_colors) +
    labs(
        x = "Total Touchdowns",
        y = "Team",  # Retain y-axis title and team names
        title = "Total Touchdowns by Team"
    ) +
    theme(
        axis.text.y = element_text(size = 10),  # Retain team names
        legend.position = "none"
    )

yds_by_game <- ggplot(filtered_data23, 
                      aes(x = `YDS/G`, y = Team, fill = Team)) +
    geom_bar(stat = "identity") +
    geom_vline(xintercept = avg_yds_game, linetype = "dashed", color = "black", size = 0.75) +
    scale_fill_manual(values = team_colors) +
    labs(
        x = "Offensive Yards per Game",
        y = NULL,  # Remove y-axis title
        title = "Offensive Yards/Game by Team"
    ) +
    theme(
        axis.text.y = element_blank(),  # Remove team names
        legend.position = "none"
    )

combined_plot <- plot_grid(
    TotalTD, 
    rushing_yds, 
    AVG_Yards_per_play, 
    yds_by_game,
    ncol = 4  # Arrange plots in a single row
)

# Display the combined plot
combined_plot


combined_plot <- plot_grid(
    TotalTD, 
    rushing_yds, 
    AVG_Yards_per_play, 
    yds_by_game,
    ncol = 4,           # Arrange plots in a single row
    align = "h",       # Align horizontally and vertically
    axis = "tb",       # Align axes
    rel_widths = c(5, 2, 2, 2)  # Equal widths for all plots
)

# Display the combined plot
combined_plot

library(ggplot2)
library(tidyr)
library(dplyr)
library(readr)
library(tidyverse)
library(rvest)
library(here)
library(cowplot)

years <- c(14,15,16,17,18,19,20,21,22,23)

scrape_clean <- function(start_year, end_year) {
    # Initialize an empty list to store yearly data
    all_data <- list()
    
    # Loop over the range of years
    for (year in start_year:end_year) {
        # Generate the file path
        file_path <- here("data_raw", paste0("defense_yards_", year, ".html"))
        
        # Read the HTML file and extract tables
        defense_yards <- read_html(file_path) %>%
            html_table(fill = TRUE)
        
        # Combine the first two tables (assuming they are in the desired format)
        combined_table <- cbind(defense_yards[[1]], defense_yards[[2]])
        
        # Add a column for the year
        combined_table$Year <- year
        
        # Append to the list
        all_data[[as.character(year)]] <- combined_table
    }
    
    # Combine all the yearly tables into one data frame
    combined_data_def <- bind_rows(all_data)
    
    return(combined_data_def)
}

# Example usage
start_year <- 14
end_year <- 23

combined_defense_data <- scrape_clean(start_year, end_year)

# View the combined data

new_headers <- combined_defense_data[1, ] 
combined_offense_data <- combined_defense_data[-1, ]
names(combined_defense_data) <- new_headers

cleaned_defense <- combined_defense_data[, c(1,8,11)] %>%
    rename(Team = 1, Yards_Game = 2, Year = 3) %>%
    mutate(Yards_Game = as.numeric(gsub(",", "", Yards_Game))) 
#or (1 in years) {

cleaned_defense$Year <- cleaned_defense$Year + 2000
glimpse(cleaned_defense)  

cleaned_defense 
#unique(all_data$Team)


all_data <- data.frame()

years <- 2014:2023
# Read in data for each year
for (year in years) {
    url <- paste0("https://www.espn.com/college-football/rankings/_/poll/1/week/1/year/", year, "/seasontype/3")
    page <- tryCatch({
        read_html(url)
    }, error = function(e) {
        message(paste("Error reading page for year:", year))
        return(NULL)
    })
    if (!is.null(page)) {
        table <- page %>% html_node("table") 
        if (!is.null(table)) {
            data <- table %>%
                html_table(fill = TRUE)
            data$Year <- year
            all_data <- bind_rows(all_data, data)
        } else {
            message(paste("No table found for year:", year))
        }
    }
} 
View(all_data)
# all data is a smaller data frame consisted of 
# rankings of top 25 teams every year

# edit offense data frame to also include RK information form all_data. 


# Create a mapping of old names to new names
name_mapping <- c(
    "OSU Ohio State" = "OSU",
    "OSU Ohio State " = "OSU",
    "UCF UCF " = "UCF",
    "ORE Oregon" = "ORE",
    "TCU TCU" = "TCU",
    "ALA Alabama" = "ALA",
    "ALA Alabama " = "ALA",
    "FSU Florida State" = "FSU",
    "MSU Michigan State" = "MSU",
    "BAY Baylor" = "BAY",
    "GT Georgia Tech" = "GT",
    "UGA Georgia" = "UGA",
    "UGA Georgia " = "UGA",
    "UCLA UCLA" = "UCLA",
    "MSST Mississippi State" = "MSST",
    "ASU Arizona State" = "ASU",
    "WIS Wisconsin" = "WIS",
    "MIZ Missouri" = "MIZ",
    "CLEM Clemson" = "CLEM",
    "CLEM Clemson " = "CLEM",
    "BSU Boise State" = "BSU",
    "MISS Ole Miss" = "MISS",
    "KSU Kansas State" = "KSU",
    "ARIZ Arizona" = "ARIZ",
    "USC USC" = "USC",
    "UTAH Utah" = "UTAH",
    "AUB Auburn" = "AUB",
    "MRSH Marshall" = "MRSH",
    "LOU Louisville" = "LOU",
    "MEM Memphis" = "MEM",
    "STAN Stanford" = "STAN",
    "OKLA Oklahoma" = "OKLA",
    "HOU Houston" = "HOU",
    "IOWA Iowa" = "IOWA",
    "ND Notre Dame" = "ND",
    "MICH Michigan" = "MICH",
    "MICH Michigan " = "MICH",
    "UNC North Carolina" = "UNC",
    "LSU LSU" = "LSU",
    "LSU LSU " = "LSU",
    "NAVY Navy" = "NAVY",
    "OKST Oklahoma State" = "OKST",
    "TENN Tennessee" = "TENN",
    "NW Northwestern" = "NW",
    "WKU Western Kentucky" = "WKU",
    "FLA Florida" = "FLA",
    "WASH Washington" = "WASH",
    "PSU Penn State" = "PSU",
    "VT Virginia Tech" = "VT",
    "COLO Colorado" = "COLO",
    "WVU West Virginia" = "WVU",
    "USF South Florida" = "USF",
    "MIAMI Miami" = "MIAMI",
    "SDSU San Diego State" = "SDSU",
    "NCST NC State" = "NCST",
    "TEX Texas" = "TEX",
    "WSU Washington State" = "WSU",
    "UK Kentucky" = "UK",
    "SYR Syracuse" = "SYR",
    "TA&M Texas A&M" = "TA&M",
    "FRES Fresno State" = "FRES",
    "ARMY Army" = "ARMY",
    "USU Utah State" = "USU",
    "CIN Cincinnati" = "CIN",
    "MINN Minnesota" = "MINN",
    "APP Appalachian State" = "APP",
    "AFA Air Force" = "AFA",
    "OU Oklahoma" = "OU",
    "ISU Iowa State" = "ISU",
    "BYU BYU" = "BYU",
    "IU Indiana" = "IU",
    "CCU Coastal Carolina" = "CCU",
    "UL Louisiana" = "UL",
    "LIB Liberty" = "LIB",
    "BALL Ball State" = "BALL",
    "SJSU San José State" = "SJSU",
    "BUFF Buffalo" = "BUFF",
    "PITT Pittsburgh" = "PITT",
    "WAKE Wake Forest" = "WAKE",
    "ARK Arkansas" = "ARK",
    "TULN Tulane" = "TULN",
    "ORST Oregon State" = "ORST",
    "TROY Troy" = "TROY",
    "SC South Carolina" = "SC",
    "SMU SMU" = "SMU",
    "KU Kansas" = "KU"
)

# Update the 'Team' column using the mapping

# Remove numbers and parentheses

all_data <- all_data %>%
    mutate(
        Team = gsub("[0-9()]", "", Team) 
    )

all_data <- all_data %>%
    mutate(Team = if_else(Team %in% names(name_mapping), 
                          name_mapping[Team], Team))


combined_data_def <- all_data %>%
    inner_join( cleaned_defense , by = c("Team", "Year"))
# final combined datat frame for the past 9 yeard
glimpse(combined_data_def)


combined_data_def <- combined_data_def[,c(1,2,8,9)]


combined_data_def <- combined_data_def %>%
    group_by(Year) %>%  # Group by Year so that ranking is done per year
    mutate(yards_rank = rank(Yards_Game, ties.method = "min")) %>%  # Rank within each year
    ungroup()

head(combined_data_def)

combined_data_def<- combined_data_def %>%
    filter(Year %in% c(2014, 2021, 2023))
 
defensive_dot_plot <- ggplot(combined_data_def %>% 
                               filter(RK == 1),  # Filter for top-ranked teams
                             aes(x = Year, y = yards_rank)) +  # Mapping Year to x and yards_rank to y
  geom_point(color = "purple") +  # Uniform color for points
  geom_line(color = "black", size = 0.25) +  # Uniform color for lines
  scale_y_reverse() +  # Reverse y-axis
  labs(
    title = "Do #1 Teams Excel in Defensive Yards/Game?",
    x = "Year",
    y = "Defensive Yards Rank"
  ) +
  theme_minimal() +
  theme(
    legend.position = "none"  # Remove the legend
  )

# Display the plot
defensive_dot_plot
defensive_dot_plot

## plot two
combined_data_def_2 <- cleaned_defense %>%
    left_join(all_data, by = c("Team", "Year"))

glimpse(combined_data_def_2)


#View(combined_data)

combined_data_def_2 <- combined_data_def_2[,c(1,2,3,4)]
glimpse(combined_data_def_2)

combined_data_def_2<- combined_data_def_2 %>%
    filter(Year %in% c(2014, 2021, 2023))

defensive <- ggplot(data = combined_data_def_2, 
       aes(x = Team, y = Yards_Game, 
           fill = case_when(
               RK == 1 ~ "Top Rank",  # Only color "Top Rank" for RK == 1
               TRUE ~ NA_character_  # Don't assign fill for other ranks
           ))) + 
    # Plot all points (background points with light grey color)
    geom_point(size = 1, shape = 21, color = "lightgrey", fill = "lightgrey") +  
    
    # Plot the blue points for RK == 1 (on the front layer, larger size)
    geom_point(data = combined_data_def_2 %>% filter(RK == 1), 
               aes(fill = "Top Rank"), size = 2, shape = 21) +  # Blue dots, larger size
    
    facet_wrap(~ Year, scales = "free_x") + 
    
    geom_hline(data = combined_data_def_2 %>% 
                   group_by(Year) %>%
                   summarise(avg_yards = mean(Yards_Game, na.rm = TRUE)), 
               aes(yintercept = avg_yards), 
               linetype = "dashed", color = "black",size = 0.75) +  # Add dotted line for the average Yards_Game
    
    labs(
        title = "Defensive Yards Per Game by Year",
        x = NULL,  # Remove x-axis label
        y = "Yards Per Game",
        fill = "Legend"  # Legend title
    ) + 
    scale_fill_manual(
        values = c("Top Rank" = "purple"),  # Only show blue for "Top Rank"
        na.value = "transparent"  # Make the other fill values invisible in the legend
    ) + 
    theme_minimal() + 
    theme(
        panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank(),
        axis.text.x = element_blank(),
        legend.position = "bottom",  # Place legend at the bottom
        strip.text = element_text(size = 10),
        aspect.ratio = .7
    )
defensive

library(rvest)
library(dplyr)
####### 2023 ###################################################################
data_list23 <- read_html(here::here('data_raw', '2023_passing_offense.html')) %>% 
    html_table()

data23<- cbind(data_list23[[1]], data_list23[[2]])
View(data23)


#iew(data23)
top25 <- c(
    "Ohio State Buckeyes", 
    "Notre Dame Fighting Irish", 
    "Alabama Crimson Tide", 
    "Clemson Tigers", 
    "Iowa Hawkeyes",
    "LSU Tigers",
    "Michigan Wolverines",
    "Oklahoma State Cowboys",
    "Texas Longhorns",
    "Georgia Bulldogs",
    "Washington Huskies",
    "Cincinnati Bearcats",
    "Florida Gators",
    "Oklahoma Sooners",
    "Oregon Ducks",
    "USC Trojans",
    "Utah Utes",
    "Auburn Tigers",
    "Boise State Broncos",
    "Florida State Seminoles",
    "Miami Hurricanes",
    "Northwestern Wildcats",
    "Oklahoma Sooners",
    "Tennessee Volunteers",
    "Penn State Nittany Lions",
    "Texas A&M Aggies")

filtered_data23 <- data23 %>%
    mutate(InTop25 = Team %in% top25) %>% 
    filter(InTop25 == TRUE)
#View(filtered_data23)

#View(filtered_data23)
library(ggplot2)

team_colors <- c(
    "Michigan Wolverines" = "blue",
    "Alabama Crimson Tide" = "red",
    "Texas Longhorns" = "orange",
    "Washington Huskies" = "purple")


library(dplyr)

# Rank teams in df23 based on the number of TDs
data23 <- data23 %>%
    arrange(desc(TD)) %>%  # Arrange teams in descending order of TDs
    mutate(Rank = dense_rank(desc(TD))) %>% 
    mutate(Year = 2023)


average_TD_23 <- data23 %>%
    group_by(Year) %>%
    summarize(Average_Touchdowns = mean(TD, na.rm = TRUE))

#View(data23)
####### 2022 ###################################################################
data_list22 <- read_html(here::here('data_raw', '2022_passing_offense.html')) %>% 
    html_table()

data22<- cbind(data_list22[[1]], data_list22[[2]])
#View(data22)

top25 <- c(
    "OHIO", 
    "ND", 
    "ALA", 
    "CLEM", 
    "IOWA",
    "LSU",
    "MICH",
    "OKST",
    "TEX",
    "UGA",
    "WASH",
    "CIN",
    "FLA",
    "OU",
    "ORE",
    "USC",
    "UTAH",
    "AUB",
    "BOIS",
    "FSU",
    "MIA",
    "NU",
    "TENN",
    "PSU",
    "TA&M")


filtered_data22 <- data22 %>%
    mutate(InTop25 = Team %in% top25) %>% 
    filter(InTop25 == TRUE)

#View(filtered_data22)

team_lookup <- c(
    "OHIO" = "Ohio State Buckeyes",
    "ND" = "Notre Dame Fighting Irish",
    "ALA" = "Alabama Crimson Tide",
    "CLEM" = "Clemson Tigers",
    "IOWA" = "Iowa Hawkeyes",
    "LSU" = "LSU Tigers",
    "MICH" = "Michigan Wolverines",
    "OKST" = "Oklahoma State Cowboys",
    "TEX" = "Texas Longhorns",
    "UGA" = "Georgia Bulldogs",
    "WASH" = "Washington Huskies",
    "CIN" = "Cincinnati Bearcats",
    "FLA" = "Florida Gators",
    "OU" = "Oklahoma Sooners",
    "ORE" = "Oregon Ducks",
    "USC" = "USC Trojans",
    "UTAH" = "Utah Utes",
    "AUB" = "Auburn Tigers",
    "BOIS" = "Boise State Broncos",
    "FSU" = "Florida State Seminoles",
    "MIA" = "Miami Hurricanes",
    "NU" = "Northwestern Wildcats",
    "TENN" = "Tennessee Volunteers",
    "PSU" = "Penn State Nittany Lions",
    "TA&M" = "Texas A&M Aggies"
)

# Rename teams in the 'Team' column of the dataset
filtered_data22 <- filtered_data22 %>%
    mutate(Team = recode(Team, !!!team_lookup))

#View(filtered_data22)

data22 <- data22 %>%
    arrange(desc(TD)) %>%  # Arrange teams in descending order of TDs
    mutate(Rank = dense_rank(desc(TD))) %>% 
    mutate(Year = 2022)
#View(df22)
####### 2021 ###################################################################
data_list21 <- read_html(here::here('data_raw', '2021_passing_offense.html')) %>% 
    html_table()

data21<- cbind(data_list21[[1]], data_list21[[2]])
#View(data21)

top25 <- c(
    "OHIO", 
    "ND", 
    "ALA", 
    "CLEM", 
    "IOWA",
    "LSU",
    "MICH",
    "OKST",
    "TEX",
    "UGA",
    "WASH",
    "CIN",
    "FLA",
    "OU",
    "ORE",
    "USC",
    "UTAH",
    "AUB",
    "BOIS",
    "FSU",
    "MIA",
    "NU",
    "TENN",
    "PSU",
    "TA&M")


filtered_data21 <- data21 %>%
    mutate(InTop25 = Team %in% top25) %>% 
    filter(InTop25 == TRUE)

#View(filtered_data21)

team_lookup <- c(
    "OHIO" = "Ohio State Buckeyes",
    "ND" = "Notre Dame Fighting Irish",
    "ALA" = "Alabama Crimson Tide",
    "CLEM" = "Clemson Tigers",
    "IOWA" = "Iowa Hawkeyes",
    "LSU" = "LSU Tigers",
    "MICH" = "Michigan Wolverines",
    "OKST" = "Oklahoma State Cowboys",
    "TEX" = "Texas Longhorns",
    "UGA" = "Georgia Bulldogs",
    "WASH" = "Washington Huskies",
    "CIN" = "Cincinnati Bearcats",
    "FLA" = "Florida Gators",
    "OU" = "Oklahoma Sooners",
    "ORE" = "Oregon Ducks",
    "USC" = "USC Trojans",
    "UTAH" = "Utah Utes",
    "AUB" = "Auburn Tigers",
    "BOIS" = "Boise State Broncos",
    "FSU" = "Florida State Seminoles",
    "MIA" = "Miami Hurricanes",
    "NU" = "Northwestern Wildcats",
    "TENN" = "Tennessee Volunteers",
    "PSU" = "Penn State Nittany Lions",
    "TA&M" = "Texas A&M Aggies"
)

# Rename teams in the 'Team' column of the dataset
filtered_data21 <- filtered_data21 %>%
    mutate(Team = recode(Team, !!!team_lookup))

#View(filtered_data21)

data21 <- data21 %>%
    arrange(desc(TD)) %>%  # Arrange teams in descending order of TDs
    mutate(Rank = dense_rank(desc(TD))) %>% 
    mutate(Year = 2021)
####### 2020 ###################################################################
data_list20 <- read_html(here::here('data_raw', '2020_passing_offense.html')) %>% 
    html_table()

data20<- cbind(data_list20[[1]], data_list20[[2]])
#View(data20)

data20 <- data20 %>%
    arrange(desc(TD)) %>%  # Arrange teams in descending order of TDs
    mutate(Rank = dense_rank(desc(TD)))%>% 
    mutate(Year = 2020)

####### 2019 ###################################################################

data_list19 <- read_html(here::here('data_raw', '2019_passing_offense.html')) %>% 
    html_table()

data19<- cbind(data_list19[[1]], data_list19[[2]])
#View(data19)

data19 <- data19 %>%
    arrange(desc(TD)) %>%  # Arrange teams in descending order of TDs
    mutate(Rank = dense_rank(desc(TD))) %>% 
    mutate(Year = 2019)

####### 2018 ###################################################################

data_list18 <- read_html(here::here('data_raw', '2018_passing_offense.html')) %>% 
    html_table()

data18<- cbind(data_list18[[1]], data_list18[[2]])
#View(data18)

data18 <- data18 %>%
    arrange(desc(TD)) %>%  # Arrange teams in descending order of TDs
    mutate(Rank = dense_rank(desc(TD)))%>% 
    mutate(Year = 2018)

####### 2017 ###################################################################

data_list17 <- read_html(here::here('data_raw', '2017_passing_offense.html')) %>% 
    html_table()

data17<- cbind(data_list17[[1]], data_list17[[2]])
#View(data17)

data17 <- data17 %>%
    arrange(desc(TD)) %>%  # Arrange teams in descending order of TDs
    mutate(Rank = dense_rank(desc(TD))) %>% 
    mutate(Year = 2017)

#View(data17)

####### 2016 ###################################################################

data_list16 <- read_html(here::here('data_raw', '2016_passing_offense.html')) %>% 
    html_table()

data16<- cbind(data_list16[[1]], data_list16[[2]])
#View(data16)

data16 <- data16 %>%
    arrange(desc(TD)) %>%  # Arrange teams in descending order of TDs
    mutate(Rank = dense_rank(desc(TD))) %>% 
    mutate(Year = 2016)

####### 2015 ###################################################################

data_list15 <- read_html(here::here('data_raw', '2015_passing_offense.html')) %>% 
    html_table()

data15<- cbind(data_list15[[1]], data_list15[[2]])
#View(data15)

data15 <- data15 %>%
    arrange(desc(TD)) %>%  # Arrange teams in descending order of TDs
    mutate(Rank = dense_rank(desc(TD))) %>% 
    mutate(Year = 2015)

####### 2014 ###################################################################

data_list14 <- read_html(here::here('data_raw', '2014_passing_offense.html')) %>% 
    html_table()

data14<- cbind(data_list14[[1]], data_list14[[2]])
#View(data14)

data14 <- data14 %>%
    arrange(desc(TD)) %>%  # Arrange teams in descending order of TDs
    mutate(Rank = dense_rank(desc(TD))) %>% 
    mutate(Year = 2014)

####### BUILDING THE DATA SET ###################################################################

df23 <- data23 %>%
    filter(Team == "Michigan Wolverines") 


df22 <- data22 %>%
    filter(Team == "UGA")

df21 <- data21 %>%
    filter(Team == "UGA")

df20 <- data20 %>%
    filter(Team == "ALA") 

df19 <- data19 %>%
    filter(Team == "LSU") 

df18 <- data18 %>%
    filter(Team == "CLEM") 

df17 <- data17 %>%
    filter(Team == "ALA") 

df16 <- data16 %>%
    filter(Team == "CLEM") 

df15 <- data15 %>%
    filter(Team == "ALA") 

df14 <- data14 %>%
    filter(Team == "ALA") 

# Combine all data sets into one
combined_data <- bind_rows(df23, df22, df21, df20, df19, df18, df17, df16, df15, df14)

#View(combined_data)
library(ggplot2)


td_rank <- ggplot(combined_data, aes(x = Year, y = Rank)) + 
  # Black line connecting all points
  geom_line(color = "black", linewidth = 0.25) +  
  # Uniform points for each year
  geom_point(color = "red", size = 2) +  
  # Reverse y-axis to show Rank 1 at the top
  scale_y_reverse() +
  labs(
    title = "Do #1 Teams Excel in Total Number of Touchdowns?",
    x = "Year",
    y = "Rank"
  ) + 
  theme_minimal() + 
  theme(
    legend.position = "none"  # Remove the legend
  )

# Display the plot
td_rank


touchdowns <- ggplot(combined_data, aes(x = Year, y = TD)) + 
    geom_line(color = "black", linewidth = 0.25) +  # Black line connecting all points
    geom_point(aes(color = Team), size = 3) +    # Points for each year and team with color
                          # Reverse y-axis to show Rank 1 at the top
    labs(
        title = "#1 Team Number of Touchdowns Over the Years",
        x = "Year",
        y = "Touchdowns",
        color = "Team"
    ) + 
    theme_minimal() + 
    theme(
        legend.position = "bottom"
    )
td_rank
touchdowns


combined_whole_data <- bind_rows(data23, data22, data21, data20, data19, data18, data17, data16, data15, data14)
#View(combined_whole_data)


library(dplyr)

combined_whole_data <- combined_whole_data %>%
    mutate(row_key = do.call(paste, c(., sep = "_")))
#View(combined_whole_data)
combined_data <- combined_data %>%
    mutate(row_key = do.call(paste, c(., sep = "_")))

# Check presence and assign color code
combined_whole_data <- combined_whole_data %>%
    mutate(
        present_in_combined_data = row_key %in% combined_data$row_key,
        color_code = ifelse(present_in_combined_data, "red", "lightgrey")
        )


# Reorder the data so red points are plotted last
combined_whole_data <- combined_whole_data %>%
    arrange(color_code == "red")
#View(combined_whole_data)
# Calculate the overall average touchdowns
average_TD <- mean(combined_whole_data$TD, na.rm = TRUE)

# Create the plot with an average line and label
everyone <- ggplot(combined_whole_data, aes(x = Team, y = TD, color = color_code)) +
    geom_point(size = 1.5, position = position_dodge(width = 0.5)) +
    geom_hline(yintercept = average_TD, linetype = "dashed", color = "black", size = 0.5) +  # Add average line
    annotate(
        "text", x = "ALA", y = 15 + 0.5, 
        label = "Average TDs", color = "black", size = 2.5, hjust = 0
    ) +  # Add label for the average line
    scale_color_manual(values = c("red" = "red", "lightgrey" = "lightgrey")) +
    theme_minimal() +
    labs(
        title = "Total Touchdowns by Team (Split by Year)",
        x = "Team",
        y = "Total Touchdowns",
        color = "Presence in Combined Data"
    ) +
    theme(
        axis.text.x = element_blank(),  # Remove x-axis text
        axis.ticks.x = element_blank(),  # Remove x-axis ticks
        legend.position = "bottom"  # Remove the legend
    ) +
    facet_wrap(~ Year)  # Use fixed y-axis for all facets
#
everyone
touchdowns
td_rank

everyone <- ggplot(combined_whole_data, aes(x = Team, y = TD, color = color_code)) +
  # Plot points with slight dodge for clarity
  geom_point(size = 1.5, position = position_dodge(width = 0.5)) +
  
  # Add dashed line for the average
  geom_hline(yintercept = average_TD, linetype = "dashed", color = "black", size = 0.5) +
  
  # Add label for the average line
  annotate(
    "text", x = "ALA", y = 15 + 0.5, 
    label = "Average TDs", color = "black", size = 2.5, hjust = 0
  ) +
  
  # Customize color scale to only include red in the legend
  scale_color_manual(
    values = c("red" = "red"),
    labels = c("red" = "Top Teams"),  # Only keep red in the legend
    guide = guide_legend(override.aes = list(size = 3))  # Adjust legend point size
  ) +
  
  # Apply minimal theme
  theme_minimal() +
  
  # Add labels and adjust layout
  labs(
    title = "Total Touchdowns by Team (Split by Year)",
    x = NULL,  # Remove x-axis label
    y = "Total Touchdowns",
    color = NULL  # Remove legend title
  ) +
  
  # Adjust theme
  theme(
    axis.text.x = element_blank(),  # Remove x-axis text
    axis.ticks.x = element_blank(),  # Remove x-axis ticks
    legend.position = "bottom"  # Place legend at the bottom
  ) +
  
  # Add facets
  facet_wrap(~ Year)  # Use fixed y-axis for all facets

# Display the plot
everyone

everyone <- ggplot(combined_whole_data, aes(x = Team, y = TD)) +
  # Plot all points as light grey
  geom_point(color = "lightgrey", size = 1.5, position = position_dodge(width = 0.5)) +
  
  # Overlay red points for "Top Teams"
  geom_point(
    data = combined_whole_data %>% filter(color_code == "red"),
    aes(color = "Top Teams"), size = 1.5, position = position_dodge(width = 0.5)
  ) +
  
  # Add dashed line for the average
  geom_hline(yintercept = average_TD, linetype = "dashed", color = "black", size = 0.5) +
  
  # Add label for the average line
  annotate(
    "text", x = "LSU", y = 15 + 0.5, 
    label = "Average", color = "black", size = 2.5, hjust = 0
  ) +
  
  # Customize color scale to show only "Top Teams" in the legend
  scale_color_manual(
    values = c("Top Teams" = "red"),  # Red for "Top Teams"
    labels = c("Top Teams"),  # Only "Top Teams" in legend
    guide = guide_legend(override.aes = list(size = 3))  # Adjust legend point size
  ) +
  
  # Apply minimal theme
  theme_minimal() +
  
  # Add labels and adjust layout
  labs(
    title = "Total Touchdowns by Year",
    x = NULL,  # Remove x-axis label
    y = "Total Touchdowns",
    color = NULL  # Remove legend title
  ) +
  
  # Adjust theme
  theme(
    axis.text.x = element_blank(),  # Remove x-axis text
    axis.ticks.x = element_blank(),  # Remove x-axis ticks
    legend.position = "bottom"  # Place legend at the bottom
  ) +
  
  # Add facets
  facet_wrap(~ Year)  # Use fixed y-axis for all facets

# Display the plot
everyone


library(ggplot2)
library(tidyr)
library(dplyr)
library(readr)
library(tidyverse)
library(rvest)

library(here)
library(cowplot)

years <- c(14,15,16,17,18,19,20,21,22,23)

scrape_clean <- function(start_year, end_year) {
  # Initialize an empty list to store yearly data
  all_data <- list()
  
  # Loop over the range of years
  for (year in start_year:end_year) {
    # Generate the file path
    file_path <- here("data_raw", paste0("offense_yards_", year, ".html"))
    
    # Read the HTML file and extract tables
    offense_yards <- read_html(file_path) %>%
      html_table(fill = TRUE)
    
    # Combine the first two tables (assuming they are in the desired format)
    combined_table <- cbind(offense_yards[[1]], offense_yards[[2]])
    
 
       # Add a column for the year
    combined_table$Year <- year
    
    # Append to the list
    all_data[[as.character(year)]] <- combined_table
  }
  
  # Combine all the yearly tables into one data frame
  combined_data_def <- bind_rows(all_data)
  
  return(combined_data_def)
}

# Example usage
start_year <- 14
end_year <- 23

combined_offense_data <- scrape_clean(start_year, end_year)

# View the combined data

new_headers <- combined_offense_data[1, ] 
combined_offense_data <- combined_offense_data[-1, ]
names(combined_offense_data) <- new_headers

cleaned_offense <- combined_offense_data[, c(1,8,11)] %>%
  rename(Team = 1, Yards_Game = 2, Year = 3) %>%
  mutate(Yards_Game = as.numeric(gsub(",", "", Yards_Game))) 
#or (1 in years) {

cleaned_offense$Year <- cleaned_offense$Year + 2000
glimpse(cleaned_offense)  





all_data <- data.frame()

years <- 2014:2023
# Read in data for each year
for (year in years) {
  url <- paste0("https://www.espn.com/college-football/rankings/_/poll/1/week/1/year/", year, "/seasontype/3")
  page <- tryCatch({
    read_html(url)
  }, error = function(e) {
    message(paste("Error reading page for year:", year))
    return(NULL)
  })
  if (!is.null(page)) {
    table <- page %>% html_node("table") 
    if (!is.null(table)) {
      data <- table %>%
        html_table(fill = TRUE)
      data$Year <- year
      all_data <- bind_rows(all_data, data)
    } else {
      message(paste("No table found for year:", year))
    }
  }
} 

# all data is a smaller data frame consisted of 
# rankings of top 25 teams every year

# edit offense data frame to also include RK information form all_data. 


# Create a mapping of old names to new names
name_mapping <- c(
  "OSU Ohio State" = "OSU",
  "OSU Ohio State " = "OSU",
  "UCF UCF " = "UCF",
  "ORE Oregon" = "ORE",
  "TCU TCU" = "TCU",
  "ALA Alabama" = "ALA",
  "ALA Alabama " = "ALA",
  "FSU Florida State" = "FSU",
  "MSU Michigan State" = "MSU",
  "BAY Baylor" = "BAY",
  "GT Georgia Tech" = "GT",
  "UGA Georgia" = "UGA",
  "UGA Georgia " = "UGA",
  "UCLA UCLA" = "UCLA",
  "MSST Mississippi State" = "MSST",
  "ASU Arizona State" = "ASU",
  "WIS Wisconsin" = "WIS",
  "MIZ Missouri" = "MIZ",
  "CLEM Clemson" = "CLEM",
  "CLEM Clemson " = "CLEM",
  "BSU Boise State" = "BSU",
  "MISS Ole Miss" = "MISS",
  "KSU Kansas State" = "KSU",
  "ARIZ Arizona" = "ARIZ",
  "USC USC" = "USC",
  "UTAH Utah" = "UTAH",
  "AUB Auburn" = "AUB",
  "MRSH Marshall" = "MRSH",
  "LOU Louisville" = "LOU",
  "MEM Memphis" = "MEM",
  "STAN Stanford" = "STAN",
  "OKLA Oklahoma" = "OKLA",
  "HOU Houston" = "HOU",
  "IOWA Iowa" = "IOWA",
  "ND Notre Dame" = "ND",
  "MICH Michigan" = "MICH",
  "MICH Michigan " = "MICH",
  "UNC North Carolina" = "UNC",
  "LSU LSU" = "LSU",
  "LSU LSU " = "LSU",
  "NAVY Navy" = "NAVY",
  "OKST Oklahoma State" = "OKST",
  "TENN Tennessee" = "TENN",
  "NW Northwestern" = "NW",
  "WKU Western Kentucky" = "WKU",
  "FLA Florida" = "FLA",
  "WASH Washington" = "WASH",
  "PSU Penn State" = "PSU",
  "VT Virginia Tech" = "VT",
  "COLO Colorado" = "COLO",
  "WVU West Virginia" = "WVU",
  "USF South Florida" = "USF",
  "MIAMI Miami" = "MIAMI",
  "SDSU San Diego State" = "SDSU",
  "NCST NC State" = "NCST",
  "TEX Texas" = "TEX",
  "WSU Washington State" = "WSU",
  "UK Kentucky" = "UK",
  "SYR Syracuse" = "SYR",
  "TA&M Texas A&M" = "TA&M",
  "FRES Fresno State" = "FRES",
  "ARMY Army" = "ARMY",
  "USU Utah State" = "USU",
  "CIN Cincinnati" = "CIN",
  "MINN Minnesota" = "MINN",
  "APP Appalachian State" = "APP",
  "AFA Air Force" = "AFA",
  "OU Oklahoma" = "OU",
  "ISU Iowa State" = "ISU",
  "BYU BYU" = "BYU",
  "IU Indiana" = "IU",
  "CCU Coastal Carolina" = "CCU",
  "UL Louisiana" = "UL",
  "LIB Liberty" = "LIB",
  "BALL Ball State" = "BALL",
  "SJSU San José State" = "SJSU",
  "BUFF Buffalo" = "BUFF",
  "PITT Pittsburgh" = "PITT",
  "WAKE Wake Forest" = "WAKE",
  "ARK Arkansas" = "ARK",
  "TULN Tulane" = "TULN",
  "ORST Oregon State" = "ORST",
  "TROY Troy" = "TROY",
  "SC South Carolina" = "SC",
  "SMU SMU" = "SMU",
  "KU Kansas" = "KU"
)

# Update the 'Team' column using the mapping

# Remove numbers and parentheses

all_data <- all_data %>%
  mutate(
    Team = gsub("[0-9()]", "", Team) 
  )

all_data <- all_data %>%
  mutate(Team = if_else(Team %in% names(name_mapping), 
                        name_mapping[Team], Team))


combined_offense_data <- all_data %>%
  inner_join( cleaned_offense , by = c("Team", "Year"))
# final combined datat frame for the past 9 yeard
glimpse(combined_offense_data)


combined_offense_data <- combined_offense_data[,c(1,2,8,9)]


combined_offense_data_2 <- combined_offense_data %>%
  group_by(Year) %>%  # Group by Year so that ranking is done per year
  mutate(yards_rank = rank(Yards_Game, ties.method = "min")) %>%  # Rank within each year
  ungroup()




## plot 1
graph1 <- ggplot(combined_offense_data_2 %>% 
                   filter(RK == 1),  # Filter for top-ranked teams
                 aes(x = Year, y = yards_rank)) +  # Mapping Year to x and yards_rank to y
  geom_point(color = "blue", size = 2) +  # Fixed color for points
  geom_line(color = "black", size = 0.25) +  # Fixed color for lines
  scale_y_reverse() +  # Reverse y-axis
  labs(
    title = "Do #1 Teams Excel in Offensive Yards per Game?",
    x = "Year",
    y = "Offensive Yards Rank"
  ) +
  theme_minimal() +
  theme(
    legend.position = "none"  # Remove the legend
  )

# Display the plot
graph1

## plot two
combined_offense_data_2 <- cleaned_offense %>%
  left_join(all_data, by = c("Team", "Year"))

combined_offense_data_2 <- combined_offense_data_2[,c(1,2,3,4)]
## graph 2
offensive <- ggplot(data = combined_offense_data_2, 
       aes(x = Team, y = Yards_Game, 
           fill = case_when(
             RK == 1 ~ "Top Rank",  # Only color "Top Rank" for RK == 1
             TRUE ~ NA_character_  # Don't assign fill for other ranks
           ))) + 
  # Plot all points (background points with light grey color)
  geom_point(size = 1, shape = 21, color = "lightgrey", fill = "lightgrey") +  
  
  # Plot the blue points for RK == 1 (on the front layer, larger size)
  geom_point(data = combined_offense_data_2 %>% filter(RK == 1), 
             aes(fill = "Top Rank"), size = 2, shape = 21) +  # Blue dots, larger size
  
  facet_wrap(~ Year, scales = "free_x") + 
  
  geom_hline(data = combined_offense_data_2 %>% 
               group_by(Year) %>%
               summarise(avg_yards = mean(Yards_Game, na.rm = TRUE)), 
             aes(yintercept = avg_yards), 
             linetype = "dashed", color = "black") +  # Add dotted line for the average Yards_Game
  
  labs(
    title = "Offensive Yards Per Game by Year",
    x = NULL,  # Remove x-axis label
    y = "Yards Per Game",
    fill = "Legend"  # Legend title
  ) + 
  scale_fill_manual(
    values = c("Top Rank" = "blue"),  # Only show blue for "Top Rank"
    na.value = "transparent"  # Make the other fill values invisible in the legend
  ) + 
  theme_minimal() + 
  theme(
    panel.grid.major.x = element_blank(),
    panel.grid.minor.x = element_blank(),
    axis.text.x = element_blank(),
    legend.position = "bottom",  # Place legend at the bottom
    strip.text = element_text(size = 10),
    aspect.ratio = .7
  )

library(ggplot2)
library(dplyr)

# Compute yearly averages
yearly_avg <- combined_offense_data_2 %>%
  group_by(Year) %>%
  summarise(avg_yards = mean(Yards_Game, na.rm = TRUE))

# Create the plot
offensive <- ggplot(data = combined_offense_data_2, 
                    aes(x = Team, y = Yards_Game, 
                        fill = case_when(
                          RK == 1 ~ "Top Rank",  # Only color "Top Rank" for RK == 1
                          TRUE ~ NA_character_  # Don't assign fill for other ranks
                        ))) + 
  # Plot all points (background points with light grey color)
  geom_point(size = 1, shape = 21, color = "lightgrey", fill = "lightgrey") +  
  
  # Plot the blue points for RK == 1 (on the front layer, larger size)
  geom_point(data = combined_offense_data_2 %>% filter(RK == 1), 
             aes(fill = "Top Rank"), size = 2, shape = 21) +  # Blue dots, larger size
  
  facet_wrap(~ Year, scales = "free_x") + 
  
  # Add dotted line for the average Yards_Game
  geom_hline(data = yearly_avg, 
             aes(yintercept = avg_yards), 
             linetype = "dashed", color = "black") +
  
  # Add "Average" text label
  geom_text(data = yearly_avg, 
            aes(x = Inf, y = 150, label = "Average"), 
            inherit.aes = FALSE, 
            hjust = 1.1,  # Adjust horizontal alignment
            color = "black", 
            size = 3) +
  
  labs(
    title = "Offensive Yards Per Game by Team, Faceted by Year",
    x = NULL,  # Remove x-axis label
    y = "Yards Per Game",
    fill = "Legend"  # Legend title
  ) + 
  scale_fill_manual(
    values = c("Top Rank" = "blue"),  # Only show blue for "Top Rank"
    na.value = "transparent"  # Make the other fill values invisible in the legend
  ) + 
  theme_minimal() + 
  theme(
    panel.grid.major.x = element_blank(),
    panel.grid.minor.x = element_blank(),
    axis.text.x = element_blank(),
    legend.position = "bottom",  # Place legend at the bottom
    strip.text = element_text(size = 10),
    aspect.ratio = .7
  )

# Display the plot
offensive

offensive <- ggplot(combined_offense_data_2, aes(x = Team, y = Yards_Game, color = case_when(
  RK == 1 ~ "Top Rank",  # Highlight "Top Rank" teams
  TRUE ~ "Other Teams"  # All other teams
))) +
  # Plot all points
  geom_point(size = 1.5, position = position_dodge(width = 0.5)) +
  
  # Add dashed line for average Yards/Game
  geom_hline(data = yearly_avg, aes(yintercept = avg_yards), 
             linetype = "dashed", color = "black", size = 0.5) +
  
  # Add text annotation for the average line
  geom_text(data = yearly_avg, aes(x = -Inf, y = avg_yards, label = "Average"), 
            inherit.aes = FALSE, hjust = -0.1, vjust = -0.5, 
            color = "black", size = 2.5) +
  
  # Manual color scale
  scale_color_manual(
    values = c("Top Rank" = "blue", "Other Teams" = "lightgrey")
  ) +
  
  # Facet by Year
  facet_wrap(~ Year, scales = "free_x") +
  
  # Labels and theme adjustments
  labs(
    title = "Offensive Yards Per Game by Team (Split by Year)",
    x = "Team",
    y = "Yards Per Game",
    color = "Rank"
  ) +
  theme_minimal() +
  theme(
    axis.text.x = element_blank(),  # Remove x-axis text
    axis.ticks.x = element_blank(),  # Remove x-axis ticks
    panel.grid.major.x = element_blank(),  # Remove major gridlines
    panel.grid.minor.x = element_blank(),  # Remove minor gridlines
    legend.position = "none",  # Remove legend
    strip.text = element_text(size = 10)  # Adjust facet label text size
  )

# Display the plot
offensive

offensive <- ggplot() +
  # Plot all points (background points with light grey color)
  geom_point(data = combined_offense_data_2, 
             aes(x = Team, y = Yards_Game), 
             color = "lightgrey", size = 1.5, position = position_dodge(width = 0.5)) +
  
  # Plot blue points for "Top Rank" on top
  geom_point(data = combined_offense_data_2 %>% filter(RK == 1), 
             aes(x = Team, y = Yards_Game, color = "Top Rank"), 
             size = 1.5, position = position_dodge(width = 0.5)) +
  
  # Add dashed line for average Yards/Game
  geom_hline(data = yearly_avg, aes(yintercept = avg_yards), 
             linetype = "dashed", color = "black", size = 0.5) +
  
  # Add text annotation for the average line
  geom_text(data = yearly_avg, aes(x = 35, y = avg_yards, label = "Average"), 
            inherit.aes = FALSE, hjust = -0.1, vjust = -0.5, 
            color = "black", size = 2.5) +
  
  # Manual color scale to include only "Top Rank" in the legend
  scale_color_manual(
    values = c("Top Rank" = "blue"),  # Blue for "Top Rank"
    labels = c("Top Rank"),  # Only keep "Top Rank" in legend
    guide = guide_legend(override.aes = list(size = 3))  # Adjust legend point size
  ) +
  
  # Facet by Year
  facet_wrap(~ Year, scales = "free_x") +
  
  # Labels and theme adjustments
  labs(
    title = "Offensive Yards Per Game by Team",
    x = NULL,  # Remove x-axis label
    y = "Yards Per Game",
    color = "Rank"
  ) +
  theme_minimal() +
  theme(
    axis.text.x = element_blank(),  # Remove x-axis text
    axis.ticks.x = element_blank(),  # Remove x-axis ticks
    panel.grid.major.x = element_blank(),  # Remove major gridlines
    panel.grid.minor.x = element_blank(),  # Remove minor gridlines
    legend.position = "bottom",  # Place legend at the bottom
    strip.text = element_text(size = 10)  # Adjust facet label text size
  )

# Display the plot
offensive

library(rvest)
library(tidyr)
library(dplyr)
library(stringdist)
library(janitor)
library(stringr)
library(ggplot2)
library(gganimate)
library(magick)
library(here)

###### GATHERING THE DATA #######

data_list23 <- read_html(here::here('data_raw', '2023_passing_offense.html')) %>% 
    html_table()
data23 <- cbind(data_list23[[1]], data_list23[[2]])

top25 <- c(
    "Ohio State Buckeyes", 
    "Notre Dame Fighting Irish", 
    "Alabama Crimson Tide", 
    "Clemson Tigers", 
    "Iowa Hawkeyes",
    "LSU Tigers",
    "Michigan Wolverines",
    "Oklahoma State Cowboys",
    "Texas Longhorns",
    "Georgia Bulldogs",
    "Washington Huskies",
    "Cincinnati Bearcats",
    "Florida Gators",
    "Oklahoma Sooners",
    "Oregon Ducks",
    "USC Trojans",
    "Utah Utes",
    "Auburn Tigers",
    "Boise State Broncos",
    "Florida State Seminoles",
    "Miami Hurricanes",
    "Northwestern Wildcats",
    "Oklahoma Sooners",
    "Tennessee Volunteers",
    "Penn State Nittany Lions",
    "Texas A&M Aggies")

filtered_data23 <- data23 %>%
    mutate(InTop25 = Team %in% top25) %>% 
    filter(InTop25 == TRUE)

df2 <- filtered_data23 %>%
    arrange(YDS)

years <- 2014:2023
all_data <- data.frame()

# Read in data for each year
for (year in years) {
    url <- paste0("https://www.espn.com/college-football/rankings/_/poll/1/week/1/year/", year, "/seasontype/3")
    page <- tryCatch({
        read_html(url)
    }, error = function(e) {
        message(paste("Error reading page for year:", year))
        return(NULL)
    })
    if (!is.null(page)) {
        table <- page %>% html_node("table") 
        if (!is.null(table)) {
            data <- table %>%
                html_table(fill = TRUE)
            data$Year <- year
            all_data <- bind_rows(all_data, data)
        } else {
            message(paste("No table found for year:", year))
        }
    }
} 

# Select only the columns we need
all_data <- all_data %>% 
    select("RK",'Team', "REC","PTS","Year") 
ranked_1 <- all_data %>% filter(RK == 1)

###### DOT AND LINE GRAPH #######

extract_avg_ypp <- function(year) {
    # Construct the file name for the HTML file
    file_name <- paste0(year, "_avg_yds_per_play.html")
    
    # Load the data
    avg_ypp <- read_html(here::here('data_raw', file_name)) %>%
        html_table()
    
    avg_ypp <- as.data.frame(avg_ypp) %>%
        select("Team", "AVG") %>%
        arrange(desc(AVG))
    
    # Save the data frame globally
    assign("avg_ypp", avg_ypp, envir = .GlobalEnv)
    
    # Return the data frame as well
    return(avg_ypp)
}

# Loop through years 2014 to 2023
for (year in 2014:2023) {
    assign(paste0("avg_ypp_", year), extract_avg_ypp(year))
}

# Assuming avg_ypp_2014, avg_ypp_2015, ..., avg_ypp_2023 are defined
avg_ypp_list <- list(
    "2014" = avg_ypp_2014,
    "2015" = avg_ypp_2015,
    "2016" = avg_ypp_2016,
    "2017" = avg_ypp_2017,
    "2018" = avg_ypp_2018,
    "2019" = avg_ypp_2019,
    "2020" = avg_ypp_2020,
    "2021" = avg_ypp_2021,
    "2022" = avg_ypp_2022,
    "2023" = avg_ypp_2023
)

combined_data <- all_data %>%
    filter(RK <= 1) %>%  # Filter for top 10 teams
    mutate(Year = as.numeric(Year))  # Ensure Year is numeric

combined_data <- combined_data %>%
    rowwise() %>%
    mutate(
        # Extract the abbreviated team name (e.g., "OSU" from "OSU Ohio State (59)")
        AbbrevTeam = str_extract(Team, "^[A-Z]+"),
        
        # Dynamically look up the corresponding AVG value
        AVG = avg_ypp_list[[as.character(Year)]] %>%
            filter(Team == AbbrevTeam) %>%
            pull(AVG) %>%
            first()  # Return the first match, if any
    ) %>%
    ungroup()

# Plot
avg_ypp_line_and_dot <- ggplot(combined_data, aes(x = Year, y = AVG)) +
  # Black line connecting all points
  geom_line(color = "black", linewidth = 0.25, aes(group = 1)) +
  # Uniform points for each year
  geom_point(color = "orange", size = 2) +
  labs(
    title = "Do #1 Teams Excel in Average Offensive Yards/Pass?",
    x = "Year",
    y = "Average Yards/Pass Rank"
  ) +
  theme_minimal() +
  theme(
    legend.position = "none"  # Remove the legend
  )

# Display the plot
avg_ypp_line_and_dot



###### FACETED DOT PLOT #########
# Function to prepare data for plotting
prepare_avg_ypp_data <- function(year, rank_index) {
    # Construct the file name for the HTML file
    file_name <- paste0(year, "_avg_yds_per_play.html")
    
    # Load the data
    avg_ypp <- read_html(here::here('data_raw', file_name)) %>%
        html_table()
    
    avg_ypp <- as.data.frame(avg_ypp)
    avg_ypp <- avg_ypp %>%
        select('Team', "AVG") %>%
        arrange(-AVG)
    
    # Dynamically extract the abbreviation from the 'ranked_1' data based on rank_index
    top_team_abbreviation <- str_extract(ranked_1$Team[rank_index], "^[A-Za-z]+")
    
    # Create a new column to indicate the highlight color
    top_50_teams <- avg_ypp[1:50, ]
    top_50_teams <- top_50_teams %>%
        mutate(
            is_highlight = Team == top_team_abbreviation,
            fill_color = ifelse(is_highlight, "red", "lightgrey"),  # Use regular red for highlight
            display_name = ifelse(is_highlight, Team, ""),
            Year = year  # Add the year column for faceting
        )
    
    return(top_50_teams)
}

# Combine data for all years
all_years_data <- bind_rows(lapply(2014:2023, function(year) {
    rank_index <- year - 2013  # Map year to rank index
    prepare_avg_ypp_data(year, rank_index)
}))
# Calculate the average AVG for each year
yearly_averages <- all_years_data %>%
    group_by(Year) %>%
    summarize(avg_yds = mean(AVG))
# Merge averages back into the data for labeling
all_years_data <- all_years_data %>%
    left_join(yearly_averages, by = "Year")
# Create the faceted dot plot with average line
ggplot(all_years_data) +
    # Plot all grey dots first
    geom_point(data = filter(all_years_data, !is_highlight),
               aes(x = reorder(Team, AVG), y = AVG),
               color = "lightgrey", size = 1.5) +
    # Plot the red highlighted dot on top with a legend
    geom_point(data = filter(all_years_data, is_highlight),
               aes(x = reorder(Team, AVG), y = AVG, color = "Top Rank"),
               size = 1.5) +
    # Add an average line for each year
    geom_hline(data = yearly_averages, aes(yintercept = avg_yds), linetype = "dashed", color = "black") +
    # Add a label for the average line
    geom_text(data = yearly_averages,
              aes(x = 25, y = avg_yds, label = "Average Yds/Pass"),  # Move text slightly left
              color = "black", hjust = 0, vjust = -0.5, size = 2.5) + # Black text, smaller size
    labs(
        x = "Team",
        y = "Average Yards per Pass",
        title = "Top 50 Teams - Average Yards Per Pass by Year",
        color = ""  # Legend title for red dot
    ) +
    facet_wrap(~Year, ncol = 2) +  # Facet by year
    theme_minimal() +
    theme(
        axis.text.x = element_blank(),           # Remove x-axis text labels
        axis.ticks.x = element_blank(),          # Remove x-axis ticks
        legend.position = "bottom"              # Add legend at the bottom
    ) +
    scale_color_manual(values = c("Top Rank" = "red"))  # Legend for red dot


# Create the faceted dot plot with average line styled like "everyone" graph
average_YPP <- ggplot(all_years_data, aes(x = Team, y = AVG)) +
  # Plot all points as light grey
  geom_point(color = "lightgrey", size = 1.5, position = position_dodge(width = 0.5)) +
  
  # Overlay orange points for "Top Rank"
  geom_point(
    data = filter(all_years_data, is_highlight),
    aes(color = "Top Rank"), size = 1.5, position = position_dodge(width = 0.5)
  ) +
  
  # Add dashed line for the average
  geom_hline(data = yearly_averages, aes(yintercept = avg_yds), 
             linetype = "dashed", color = "black", size = 0.5) +
  
  # Add label for the average line
  annotate(
    "text", x = 45, y = 7.25 + 0.5, 
    label = "Average", color = "black", size = 2.5, hjust = 0
  ) +
  
  # Customize color scale to show only "Top Rank" in the legend
  scale_color_manual(
    values = c("Top Rank" = "orange"),  # Orange for "Top Rank"
    labels = c("Top Rank"),  # Only "Top Rank" in legend
    guide = guide_legend(override.aes = list(size = 3))  # Adjust legend point size
  ) +
  
  # Apply minimal theme
  theme_minimal() +
  
  # Add labels and adjust layout
  labs(
    title = "Offensive Yards Per Pass by Year",
    x = NULL,  # Remove x-axis label
    y = "Average Yards per Pass",
    color = NULL  # Remove legend title
  ) +
  
  # Adjust theme
  theme(
    axis.text.x = element_blank(),  # Remove x-axis text
    axis.ticks.x = element_blank(),  # Remove x-axis ticks
    legend.position = "bottom"  # Place legend at the bottom
  ) +
  
  # Add facets
  facet_wrap(~ Year)  # Use fixed y-axis for all facets

# Display the plot
average_YPP




rushing_yds <- ggplot(filter(long_defense_data, Metric == 'Rushing YDS/Game'), 
                      aes(x = Value, y = Team, fill = Team)) +
    geom_bar(stat = "identity") +
    geom_vline(xintercept = avg_rushing, linetype = "dashed", color = "black", size = 0.75) +
    scale_fill_manual(values = team_colors) +
    labs(
        x = "Rushing YDS/Game",
        y = NULL,  # Remove y-axis title
        title = "Rushing YDS/Game Per Teams"
    ) +
    theme(
        axis.text.y = element_blank(),  # Remove team names
        legend.position = "none"
    )

AVG_Yards_per_play <- ggplot(filtered_data23, 
                             aes(x = AVG, y = Team, fill = Team)) +
    geom_bar(stat = "identity") +
    geom_vline(xintercept = avg_yards, linetype = "dashed", color = "black", size = 0.75) +
    scale_fill_manual(values = team_colors) +
    labs(
        x = "Average Yards per Play",
        y = NULL,  # Remove y-axis title
        title = "Average Yards/Pass by Team"
    ) +
    theme(
        axis.text.y = element_blank(),  # Remove team names
        legend.position = "none"
    )

TotalTD <- ggplot(filtered_data23, 
                  aes(x = TD, y = Team, fill = Team)) +
    geom_bar(stat = "identity") +
    geom_vline(xintercept = avg_td, linetype = "dashed", color = "black", size = 0.75) +
    scale_fill_manual(values = team_colors) +
    labs(
        x = "Total Touchdowns",
        y = "Team",  # Retain y-axis title and team names
        title = "Total Touchdowns by Team"
    ) +
    theme(
        axis.text.y = element_text(size = 7),  # Retain team names
        legend.position = "none"
    )

yds_by_game <- ggplot(filtered_data23, 
                      aes(x = `YDS/G`, y = Team, fill = Team)) +
    geom_bar(stat = "identity") +
    geom_vline(xintercept = avg_yds_game, linetype = "dashed", color = "black", size = 0.75) +
    scale_fill_manual(values = team_colors) +
    labs(
        x = "Offensive Yards per Game",
        y = NULL,  # Remove y-axis title
        title = "Offensive Yards/Game by Team"
    ) +
    theme(
        axis.text.y = element_blank(),  # Remove team names
        legend.position = "none"
    )

combined_plot <- plot_grid(
    TotalTD, 
    rushing_yds, 
    AVG_Yards_per_play, 
    yds_by_game,
    ncol = 4,           # Arrange plots in a single row
    align = "h",       # Align horizontally and vertically
    axis = "tb",       # Align axes
    rel_widths = c(3.5, 2, 2, 2)  # Equal widths for all plots
)
combined_plot
td_rank
everyone
defensive_dot_plot
defensive
graph1
offensive
avg_ypp_line_and_dot
average_YPP
