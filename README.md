# Cracking the NCAA Team Rankings Code

What makes a college football team one of the top four picked for the College Football Playoff? This exploratory data analysis looks at which team statistics the College Football Playoff Selection Committee seems to reward, first in 2023 and then across the decade from 2014 to 2023.

**📊 Read the full report:** [Cracking the NCAA Team Rankings Code: The Attributes of the Best NCAA Football Playoff Teams Over a Decade](https://eda.seas.gwu.edu/showcase/2024-Fall/ncaa-rankings.html)

## Question

Which attributes matter most to the Selection Committee when choosing the top four teams? Did the factors that mattered in 2023 also matter over the previous ten years?

## Data

- **ESPN:** the main source, with rankings and team statistics.
- **Associated Press Top 25 poll**, **NCAA** rankings and statistics, and the **College Football Playoff** website.

## Approach

- Scraped team statistics and rankings from ESPN pages with R's `rvest` package, and standardized team names across sources with a mapping table.
- Found the statistics that best separated the top four teams from the rest of the top 25 in 2023: total touchdowns, rushing yards per game, yards per pass and offensive yards per game.
- Ranked teams within each year from 2014 to 2023 and compared each year's top teams with all other teams.

## Findings

- **Touchdowns:** top teams usually scored well above the average number of touchdowns, though they weren't always the top scorer.
- **Defensive yards allowed per game:** allowing fewer yards was associated with a higher chance of being a top team.
- **Offensive yards per game:** every top-ranked team was at or above average, and the top team gained more yards than about 85% of other teams.
- **Yards per pass:** no consistent relationship across years.
- These statistics matter but don't guarantee selection. Strength of schedule, conference performance and other hard-to-measure factors also play a role, and the committee's choices sometimes look inconsistent.

## Code

[`analysis.R`](analysis.R) contains the full analysis: scraping, cleaning, ranking and every chart in the report.

To run it, you'd need the saved ESPN statistics pages in a `data_raw/` folder (for example, `2023_passing_offense.html` and `defense_yards_14.html`). These aren't included in this repository. The Associated Press rankings are scraped directly from ESPN's rankings pages.

## Tools

R: `rvest`, `dplyr`, `tidyr`, `ggplot2`, `cowplot`, `gganimate`, `stringdist`, `janitor`, `stringr`, `here`, `magick`.

## Team

Alana Lee, Kyla Rounsoville and Maddie Wilson, Exploratory Data Analysis, The George Washington University, Fall 2024.
