
# https://socviz.co/maps.html
# https://github.com/kjhealy/socviz

# https://cran.r-project.org/web/packages/usmap/vignettes/mapping.html

#****************************************************************************************************
#                Libraries ####
#****************************************************************************************************
library(maps)
# https://cran.r-project.org/web/packages/usmap/vignettes/mapping.html
library(usmap)
library(gridExtra)


#****************************************************************************************************
#                Functions ####
#****************************************************************************************************
theme_map <- function(base_size=9, base_family="") {
  # see:
  # https://socviz.co/maps.html
  # https://github.com/kjhealy/socviz
  require(grid)
  theme_bw(base_size=base_size, base_family=base_family) %+replace%
    theme(axis.line=element_blank(),
          axis.text=element_blank(),
          axis.ticks=element_blank(),
          axis.title=element_blank(),
          panel.background=element_blank(),
          panel.border=element_blank(),
          panel.grid=element_blank(),
          panel.spacing=unit(0, "lines"),
          plot.background=element_blank(),
          legend.justification = c(0,0),
          legend.position = c(0,0)
    )
}

get_mdata <- function(data){
  # df must have a column named stabbr
  mdata <- left_join(usmap::us_map() %>% arrange(full, piece, order), 
                     data %>% rename(abbr=stabbr),
                     by="abbr")
  return(mdata)
}

get_stateplot <- function(data, fillvar){
  # data is a data frame
  # fillvar is a character value with the name of the variable that will determine the fill color
  mdata <- get_mdata(data)
  p <- ggplot(data = mdata, aes(x = long, y = lat, group = group)) +
    geom_polygon(aes(fill=mdata[[fillvar]]), color = "gray90", size = 0.1, na.rm=TRUE) +
    coord_equal() + 
    theme_map() +
    theme(legend.position = "right")
  return(p)
}

#****************************************************************************************************
#                Data ####
#****************************************************************************************************
f <- function(x) scale(x, center=FALSE, scale=max(x))[, 1]
agepop <- wpop %>%
  filter(stabbr %in% c("DC", state.abb), sex=="Total", year %in% c(2020, 2040), popgroup %in% c(globals$wc_older, "poptot")) %>%
  mutate(older=ifelse(popgroup %in% globals$wc_older, "older", "total")) %>%
  group_by(stabbr, year, older) %>%
  summarise(pop=sum(value)) %>%
  ungroup %>%
  spread(older, pop) %>%
  mutate(pctold=older / total * 100,
         pctolds=f(pctold)) %>%
  select(stabbr, year, pctold, pctolds)
glimpse(agepop)


agepop %>%
  group_by(year) %>%
  do(qtiledf(.$pctold, probs=0:10/10))

#****************************************************************************************************
#                Draw maps ####
#****************************************************************************************************
# group the age data
# cuts <- c(0, 16, 18, 20, 22, 30)
scale(1:10, center=FALSE, scale=10)[, 1]
f <- function(x) scale(x, center=FALSE, scale=max(x))[, 1]
f(1:10)
# INCLUDING DC IS CRITICAL IF IT IS IN THE MAPDATA (else leave it out of map data) -- else we will get an NA facet


# cuts <- c(0, 14, 18, 20, 24, 30); cutlabs <- c("    <= 14%", "> 14% to 18%", "> 18% to 20%", "> 20% to 24%", "     > 24%")
# cuts <- c(0, 15, 17.5, 20, 25, 30); cutlabs <- c("    <= 14%", "> 14% to 18%", "> 18% to 20%", "> 20% to 24%", "     > 24%")
#cutlabs <- c("    <= 14%", "> 14% to 18%", "> 18% to 20%", "> 20% to 24%", "     > 24%")
# cutlabs <- c("    <= 14%", "> 14% to 18%", "> 18% to 20%", "> 20% to 24%", "     > 24%")

cuts <- c(0, 14, 17, 19, 21, 24, 30)
agepop$agroup <- cut(agepop$pctold, breaks=cuts)
count(agepop, year, agroup) %>% spread(year, n) # we have missing groups, which causes problem with facet

cutlabs <- c("<= 14%", levels(agepop$agroup)[2:5], "> 24%")
cbind(levels(agepop$agroup), cutlabs)
agepop <- agepop %>%
  mutate(agroup=factor(agroup, labels=cutlabs))
count(agepop, agroup)

capt <- str_wrap(paste0("Source: Author's analysis of projections from ", wc_src), 90)

# cred <- c('#f1eef6','#bdc9e1','#74a9cf','#2b8cbe','#045a8d')
# cmulti <- c('#d7191c','#fdae61','#ffffbf','#abdda4','#2b83ba') %>% rev
# 
# 
# 
# cyell <- c('#ffffb2','#fecc5c','#fd8d3c','#f03b20','#bd0026')
# clr2 <- c('#f1eef6','#d7b5d8','#df65b0','#dd1c77','#980043')

cmulti2 <- c('#d7191c','#fecc5c','#ffffbf','#b2e2e2','#2ca25f') %>% rev
colors <- cmulti2
colors <- c('#d73027','#fc8d59','#fee08b','#d9ef8b','#91cf60','#1a9850') %>% rev
colors <- c('#31a354','#f0f9e8','#ffffb2','#fee391','#feb24c','#f03b20')
p1 <- get_stateplot(agepop, "agroup") + 
  scale_fill_manual(values=colors, drop=FALSE)+
  theme(legend.position = "right") +
  guides(fill=guide_legend(title="% age 65+")) +
  geom_polygon(color = "black", fill=NA) +
  facet_wrap(~year, ncol=1) +
  ggtitle("Percent of population aged 65+") +
  theme(plot.title = element_text(size = 12, face = "bold"),
        strip.background = element_rect(colour="lightgrey", fill=NA),
        strip.text = element_text(size = 10, face="bold")) +
  labs(caption=capt) +
  theme(plot.caption = element_text(hjust=0, size=8))
p1
ggsave("./results/pctold_map.png", p1, width=8, height=6.5, units="in")


#****************************************************************************************************
#                alternative approach ####
#****************************************************************************************************
p2 <- get_stateplot(agepop, "pctold") + 
  theme(legend.position = "right") +
  guides(fill=guide_legend(title="% age 65+")) +
  facet_wrap(~year, ncol=1) +
  ggtitle("Percent of population aged 65+") +
  theme(plot.title = element_text(size = 12, face = "bold"),
        strip.background = element_rect(colour="lightgrey", fill=NA),
        strip.text = element_text(size = 10, face="bold")) +
  labs(caption=capt) +
  theme(plot.caption = element_text(hjust=0, size=8))
p2


p2 + scale_fill_continuous(type = "viridis")
p2 + scale_fill_continuous(type = "gradient")
p2 + scale_fill_gradient(low="blue", high="red")
p2 + scale_fill_gradient2(low="green", high="red")




ml <- marrangeGrob(list(p1, p2), nrow=2, ncol=1)
print(ml)
dev.off()

ggsave("./results/maps.png", ml, width=8, height=6.5, units="in")

ml <- marrangeGrob(list(p1, p2, p3, p4), 
                   layout_matrix = rbind(c(1,2),
                                         c(3,4)),
                   
                   nrow=3, ncol=2, top = NULL)
print(ml)
dev.off()



# https://stackoverflow.com/questions/54426144/how-to-remove-na-from-facet-wrap-in-ggplot2
# in facet_wrap -- Including rows for each variable and municipality no matter if the variable of interest is NA or not,
# finally solves the problem. Here is what I was looking for:


#****************************************************************************************************
#                OLD STUFF BELOW HERE ####
#****************************************************************************************************
# library(socviz)

us_states <- map_data("state")
str(us_states)
head(us_states)

party_colors <- c("#2E74C0", "#CB454A") 

p0 <- ggplot(data = subset(election, st %nin% "DC"),
             mapping = aes(x = r_points,
                           y = reorder(state, r_points),
                           color = party))

p1 <- p0 + geom_vline(xintercept = 0, color = "gray30") +
  geom_point(size = 2)

p2 <- p1 + scale_color_manual(values = party_colors)

p3 <- p2 + scale_x_continuous(breaks = c(-30, -20, -10, 0, 10, 20, 30, 40),
                              labels = c("30\n (Clinton)", "20", "10", "0",
                                         "10", "20", "30", "40\n(Trump)"))

p3 + facet_wrap(~ census, ncol=1, scales="free_y") +
  guides(color=FALSE) + labs(x = "Point Margin", y = "") +
  theme(axis.text=element_text(size=8))


p <- ggplot(data = us_states,
            mapping = aes(x = long, y = lat,
                          group = group))

p + geom_polygon(fill = "white", color = "black")

p <- ggplot(data = us_states,
            aes(x = long, y = lat,
                group = group, fill = region))

p + geom_polygon(color = "gray90", size = 0.1) + guides(fill = FALSE)

p <- ggplot(data = us_states,
            mapping = aes(x = long, y = lat,
                          group = group, fill = region))

p + geom_polygon(color = "gray90", size = 0.1) +
  coord_map(projection = "albers", lat0 = 39, lat1 = 45) +
  guides(fill = FALSE)

election$region <- tolower(election$state)
us_states_elec <- left_join(us_states, election)

p <- ggplot(data = us_states_elec,
            aes(x = long, y = lat,
                group = group, fill = party))

p + geom_polygon(color = "gray90", size = 0.1) +
  coord_map(projection = "albers", lat0 = 39, lat1 = 45) 


p0 <- ggplot(data = us_states_elec,
             mapping = aes(x = long, y = lat,
                           group = group, fill = party))

p1 <- p0 + geom_polygon(color = "gray90", size = 0.1) +
  coord_map(projection = "albers", lat0 = 39, lat1 = 45) 

p2 <- p1 + scale_fill_manual(values = party_colors) +
  labs(title = "Election Results 2016", fill = NULL)

p2 + theme_map() 



p <- plot_usmap(data = statepop, values = "pop_2015", lines = "grey") + 
  scale_fill_continuous(name = "Population (2015)", label = scales::comma) + 
  theme(legend.position = "right")
p
d <- ggplot_build(p)
str(d)


plot_usmap(data = statepop, values = "pop_2015", lines = "red") + 
  scale_fill_continuous(
    low = "white", high = "red", name = "Population (2015)", label = scales::comma
  ) + theme(legend.position = "right")

glimpse(statepop)

plot_usmap(data = df, values = "pctold_2020", lines = "red", labels=TRUE) + 
  scale_fill_continuous(name = "Percent old (2020)", low = "green", high = "red", label = scales::comma) +
  theme(legend.position = "right")

plot_usmap(data = df %>% gather(year, pct, starts_with ("pct")), values ="pct", lines = "red", labels=TRUE) + 
  scale_fill_continuous(name = "Percent old", low = "green", high = "red", label = scales::comma) +
  theme(legend.position = "right") +
  facet_wrap(~year, ncol=1)

plot_usmap(data = df %>% filter(stabbr %in% state.abb) %>% gather(year, pct, starts_with ("pct")), values ="pct", lines = "red", labels=TRUE) +
  facet_wrap(~year, ncol=1)


dta <- df %>% filter(stabbr %in% state.abb) %>% gather(year, pct, starts_with ("pct"))



# df <- us_map(regions = "counties")
# west_coast <- us_map(include = c("CA", "OR", "WA"))

statesakhi <- usmap::us_map()
str(statesakhi)

agepop <- wpop %>%
  filter(stabbr %in% c("DC", state.abb), sex=="Total", year %in% c(2020, 2040), popgroup %in% c(globals$wc_older, "poptot")) %>%
  mutate(older=ifelse(popgroup %in% globals$wc_older, "older", "total")) %>%
  group_by(stabbr, year, older) %>%
  summarise(pop=sum(value)) %>%
  ungroup %>%
  spread(older, pop) %>%
  mutate(pctold=older / total * 100,
         fips=getstfips(stabbr)) %>%
  select(fips, stabbr, year, pctold)
glimpse(agepop)

# group the age data

agepop$agroup <- cut(agepop$pctold, 4)
count(agepop, agroup)

p <- plot_usmap(data = agepop, values ="pctold") +
  theme(legend.position = "right") +
  facet_wrap(~year, ncol=1)
# p
# p
d <- ggplot_build(p)
str(d)

# result[order(result$full, result$piece, result$order)

mdata <- left_join(usmap::us_map() %>% arrange(full, piece, order), 
                   agepop %>% rename(abbr=stabbr))# %>% filter(year==2020))
ht(mdata)
count(mdata, year)

p <- ggplot(data = mdata, aes(x = long, y = lat, group = group, fill = agroup))

p + geom_polygon(color = "gray90", size = 0.1) +
  coord_equal() + theme_map() +
  theme(legend.position = "right") +
  facet_wrap(~year, ncol=1) 
  # DO NOT DO THIS coord_map(projection = "albers", lat0 = 39, lat1 = 45) 

map_df <- us_map()
polygon_layer <- geom_polygon(ggplot2::aes(x = map_df$long, 
                                                    y = map_df$lat, group = map_df$group), colour = lines, 
                                       fill = "white", size = 0.4)


poly <- geom_polygon(aes(x = mdata$long, 
                         y = mdata$lat, 
                         group = mdata$group,
                         colour = "black", 
                         fill = agroup, 
                         size = 0.4))

ggplot(data=mdata) + poly + coord_equal() + theme_map()



poly <- geom_polygon(aes(x = mdata$long, 
                         y = mdata$lat, 
                         group = mdata$group,
                         colour = "black", 
                         fill = "white", 
                         size = 0.4))



p <- ggplot(data = mdata,
            aes(x = long, y = lat,
                group = group, fill = agroup))

p + geom_polygon(color = "gray90", size = 0.1) +
  coord_map(projection = "albers", lat0 = 39, lat1 = 45) 



  

function (regions = c("states", "state", "counties", "county"), 
          include = c(), data = data.frame(), values = "values", theme = theme_map(), 
          lines = "black", labels = FALSE, label_color = "black") 
{
  if (!requireNamespace("ggplot2", quietly = TRUE)) {
    stop("Please install `ggplot2`. Use: install.packages(\"ggplot2\")")
  }
  regions_ <- match.arg(regions)
  if (nrow(data) == 0) {
    map_df <- us_map(regions = regions_, include = include)
    polygon_layer <- ggplot2::geom_polygon(ggplot2::aes(x = map_df$long, 
                                                        y = map_df$lat, group = map_df$group), colour = lines, 
                                           fill = "white", size = 0.4)
  }
  else {
    map_df <- map_with_data(data, values = values, include = include)
    polygon_layer <- ggplot2::geom_polygon(ggplot2::aes(x = map_df$long, 
                                                        y = map_df$lat, group = map_df$group, fill = map_df[, 
                                                                                                            values]), colour = lines, size = 0.4)
  }
  if (labels) {
    if (regions_ == "county" | regions_ == "counties") {
      warning("`labels` is currently only supported for state maps. It has no effect on county maps.")
      label_layer <- ggplot2::geom_blank()
    }
    else {
      centroid_labels <- utils::read.csv(system.file("extdata", 
                                                     paste0("us_", regions_, "_centroids.csv"), package = "usmap"), 
                                         colClasses = c("numeric", "numeric", "character", 
                                                        "character", "character"), stringsAsFactors = FALSE)
      if (length(include) > 0) {
        centroid_labels <- centroid_labels[centroid_labels$full %in% 
                                             include | centroid_labels$abbr %in% include | 
                                             centroid_labels$fips %in% include, ]
      }
      label_layer <- ggplot2::geom_text(data = centroid_labels, 
                                        ggplot2::aes(x = centroid_labels$x, y = centroid_labels$y, 
                                                     label = centroid_labels$abbr), colour = label_color)
    }
  }
  else {
    label_layer <- ggplot2::geom_blank()
  }
  ggplot2::ggplot(data = map_df) + polygon_layer + label_layer + 
    ggplot2::coord_equal() + theme
}

