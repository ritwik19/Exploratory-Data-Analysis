---
title: "MiniProject 2"
author: "Srimanth Agastyaraju, Yash Shah, Ritwik Budhiraja"
date: "03/31/2022"
output: pdf
---


library(tidyverse)
library(gapminder)
library(rio)
library(broom)
library(gridExtra)
library(cluster)

#-------------------------------------------------------------------------------
#--------------------------------- PART 1 --------------------------------------
#-------------------------------------------------------------------------------

standardize_categorical_cols = function (categorical_data){
  ifelse(categorical_data %in% c('Yea', 'Guilty'),
         1,
         ifelse(categorical_data %in% c('Nay', 'Not Guilty'), 
                -1,0
         )
  )
}


func_load_data = function(year) {
  df_members = import(paste0(c(file_directory, year, members_data), collapse='/'))
  df_votes = import(paste0(c(file_directory, year, votes_data), collapse='/'))
  df_votes[is.na(df_votes)] = 'Not Voting'
  df_votes = df_votes %>% 
    left_join(df_members, by=c("id" = "id")) %>%
    mutate(year = year)
  return (df_votes)
}

func_gen_scatterplot = function(df_input, mds_reduced_data, year, title, subtitle, xlabel, ylabel) {
  scatter_plot = mds_reduced_data %>%
    data.frame() %>%
    ggplot() +
    geom_point(aes(x = X1, y = X2, color = df_input$party), alpha = 0.6, size = 2) +
    ylim(-0.5, 0.2) + xlim(-0.5,0.4) + # Remove this line for auto limits
    theme_bw() + 
    labs(
      title = title,
      subtitle = subtitle,
      x = xlabel, 
      y = ylabel) +
    scale_color_discrete('Party')
  return (scatter_plot)
}

create_mds_projections = function(df_input, year) {
  votes_dist_df = df_input %>%
    select(-c('id')) %>%
    mutate_all(standardize_categorical_cols) %>%
    daisy(metric = "gower") %>%
    as.matrix()
  
  mds_reduced_data = votes_dist_df %>% 
    cmdscale() %>%
    data.frame()
  
  return (mds_reduced_data)
}

path = "/Users/yash/Desktop/yash/IU Semester 2/Exploratory Data Analysis/Data"
setwd(path)
file_directory = 'congress'
members_data = 'members.csv'
votes_data = 'votes.csv'


# year 1989
year = '1989'
votes_in_1989  = func_load_data(year)
#head(votes_in_1989)
mds_1989 = create_mds_projections(votes_in_1989, year)
title_1 = paste0('Projections of Senators using Multidimensional Scaling')
subtitle_1 = paste0(year)
distance_1 = "Dimension 1"
distance_2 = "Dimension 2"
plot_mds_1989 = func_gen_scatterplot(votes_in_1989, mds_1989, year, 
                                      title_1, subtitle_1, distance_1, distance_2)

plot_mds_1989

# year 2014
year = '2014'
votes_in_2014  = func_load_data(year)
mds_2014 = create_mds_projections(votes_in_2014, year)
title_1 = paste0('Projections of Senators after Multidimensional Scaling')
subtitle_1 = paste0(year)
distance_1 = "Dimension 1"
distance_2 = "Dimension 2"
plot_mds_2014 = func_gen_scatterplot(votes_in_2014, mds_2014, year, 
                                      title_1, subtitle_1, distance_1, distance_2)
plot_mds_2014


#-------------------------------------------------------------------------------
#--------------------------------- PART 2 --------------------------------------
#-------------------------------------------------------------------------------


# Q2

# https://www.geeksforgeeks.org/handling-errors-in-r-programming/

new_df = function(colnames){
  tryCatch({
    data.frame(matrix(ncol=length(colnames), nrow=0, dimnames=list(NULL,colnames)))
  },
  error=function(e){
    cat('\n Error while creating a new Dataframe for: ',colnames)
    print(e)
  })
}


func_cluster_for_year = function(mds_points, df_votes, agg_col) {
  mds_points['id'] = df_votes$id
  mds_points['year'] = df_votes$year
  mds_points['party'] = df_votes$party
  
  agg_df = mds_points %>%
    filter(party %in% c('R', 'D')) %>%
      group_by(party) %>%
      summarise(mean=mean(X1), mean2=mean(X2))  %>%
      pivot_wider(names_from=party, values_from=c(mean, mean2))
  agg_df = agg_df %>% 
    cbind(mds_points %>% 
            select(X1, X2, party, year)
    )
  
  colnames(agg_df) = agg_col
  return (agg_df)
}

agg_col = c("Democrats_x1", "Republicans_x1", "Democrats_x2", "Republicans_x2",
                         "Senator_x1", "Senator_x2", "Party", "year")

clusters_data = new_df(agg_col)
years = sapply(c(1989:2014), as.character)

for (year in years){
  votes_df  = func_load_data(year)
  mds_points = create_mds_projections(votes_df, year)
  tryCatch({
    agg_df = func_cluster_for_year(mds_points, votes_df, agg_col)
    clusters_data = clusters_data %>% rbind(agg_df)
  },
  error=function(e){
    cat('\nError during aggregation the data for the year:', year)
    print(e)
  })
  print(paste0('Data for the year ', year, ' was successfully extracted.'))
}

clusters_data = clusters_data %>%
  mutate(
    distances =
      abs((Democrats_x1 - Republicans_x1) + (Democrats_x2 - Republicans_x2)),
  )


ggplot(clusters_data) +
geom_line(mapping=aes(x=year, y=distances, group=Party), size=1, color = 'red') +
theme_bw() +
theme(axis.text.x = element_text(angle=45, hjust=1)) +
labs(
  title='Polarization trend over time 1989-2014',
  subtitle=paste0('(Polarization metric => Manhattan distance between MDS clusters)'),
  x='year', y='Distance metric'
)

#-------------------------------------------------------------------------------
#--------------------------------- PART 3 --------------------------------------
#-------------------------------------------------------------------------------


cluster_over_year_given_sentence = function(mds_points, votes_df, senator_id, agg_col) {
  mds_points['id'] = votes_df$id
  mds_points['year'] = votes_df$year
  mds_points['party'] = votes_df$party
  
  agg_df = mds_points %>%
    filter(party %in% c('R', 'D')) %>%
      group_by(party) %>%
      summarise(mean=mean(X1), mean2=mean(X2))  %>%
      pivot_wider(names_from=party, values_from=c(mean, mean2))
  
  agg_df = agg_df %>%
    cbind(mds_points %>% 
            filter(id==senator_id) %>%
            select(X1, X2, party, year)
    )
  
  colnames(agg_df) = agg_col
  return (agg_df)
}

senator_id = 'S161'
#senator_id = 'S090'
#senator_id = 'S209'
year = '1989'
votes_in_1989  = func_load_data(year)
senator_name = paste0(
  votes_in_1989[votes_in_1989$id==senator_id,]$first_name, 
  ' ', 
  votes_in_1989[votes_in_1989$id==senator_id,]$last_name)
years = sapply(c(1989:2014), as.character)
agg_col = c("Democrats_x1", "Republicans_x1", "Democrats_x2", "Republicans_x2",
                         "Senator_x1", "Senator_x2", "Party", "year")
cluster_given_senator = new_df(agg_col)

for (year in years){
  votes_df  = func_load_data(year)
  mds_points = create_mds_projections(votes_df, year)
  tryCatch({
    agg_df = cluster_over_year_given_sentence(mds_points, votes_df, senator_id, agg_col)
    print(agg_df)
    cluster_given_senator = cluster_given_senator %>%
      rbind(agg_df)
  },
  error=function(e){
    cat('\nSomething went wrong while aggregating the data for year:', year)
    print(e)
  })
}

ggplot(cluster_given_senator) +
geom_path(mapping=aes(x=year, y=Democrats_x1, group=Party, color='Democrats'), size=1) +
geom_path(mapping=aes(x=year, y=Republicans_x1, group=Party, color='Republicans'), size=1) +
geom_path(mapping=aes(x=year, y=Senator_x1, group=Party, color='Senator'), size=1) +
theme_bw() +
theme(axis.text.x = element_text(angle=60, hjust=1)) +
scale_colour_manual('Legend', values=c('Democrats'='blue','Republicans'='red','Senator'='dark green')) +
labs(
  title= paste0('Ideology trends over time 1989-2014 for the Sen. ', senator_name),
  x='Years', y='MDS Distance 1'
)


