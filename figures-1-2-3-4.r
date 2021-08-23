# Figures 1-4. Commitee co-membership networks in astrophysics
# Bès, Marie-Pierre, Lamy, Jérôme et Maisonobe, Marion. 2021. Supplementary material for Quantitative Science Studies research article: "Peer-making: the interconnections between PhD Thesis Committee membership and co-publishing".

library(tidyverse)
library(igraph)
library(ggraph)

#############################################################################################################

d <- read_tsv("data/ASTRO/jury_composition.tsv") %>%
  left_join(
    select(read_tsv("datignore/ASTRO/jury_name.tsv"), 
           id_jury, name_jury))  

g <- d %>% 
  select(id_phd, name_jury) %>%
  graph_from_data_frame() %>%
  bipartite_projection(types = str_detect(V(.)$name, "\\d"), 
                       which = F,
                       remove.type = F)

l <- layout_with_fr(g)

##############################################################################################################
# Figure 1
##############################################################################################################

g %>%
  ggraph(layout = l) + 
  geom_edge_link(edge_colour = "grey") +
  geom_node_point(aes(size = degree(g)), shape = 22, fill = "#037E40", stroke = 0.2) + # "darkgreen"
  geom_node_text(aes(filter = degree(g) >= 20, label = name), fontface = "bold", size = 1.5) +
  geom_node_text(aes(filter = degree(g) < 20, label = name), size = 1, repel = TRUE, min.segment.length = Inf, max.overlaps = Inf) +
  theme_void() +
  labs(size = "Jury members'\ndegree") +
  theme(legend.title = element_text(size = 5),
        legend.text = element_text(size = 4),
        legend.position = "bottom")

ggsave("plots/figure-1.pdf")
ggsave("plots/figure-1.png")

#############################################################################################################
# Figure 2
#############################################################################################################

g %>%
  ggraph(layout = l) + 
  geom_edge_link(aes(filter = E(g)$weight >= 2), edge_colour = "grey") +
  geom_node_point(aes(size = degree(g)), shape = 22, fill = "#037E40", stroke = 0.2) + # "darkgreen"
  geom_node_text(aes(filter = degree(g) >= 20, label = name), fontface = "bold", size = 1.5) +
  geom_node_text(aes(filter = degree(g) < 20, label = name), size = 1, repel = TRUE, min.segment.length = Inf, max.overlaps = Inf) +
  # geom_node_text(aes(filter = degree(g) >= 2, label = name), size = 1.2, repel = TRUE,
  #            segment.color = "#037E40") +
  theme_void() +
  labs(size = "Jury members'\ndegree") +
  theme(legend.title = element_text(size = 5),
        legend.text = element_text(size = 4),
        legend.position = "bottom")

ggsave("plots/figure-2.pdf")
ggsave("plots/figure-2.png")

#############################################################################################################
# Figure 3
#############################################################################################################

d <- read_tsv("data/ASTRO/jury_publications.tsv") %>%
  left_join(
    select(read_tsv("datignore/ASTRO/jury_name.tsv"), 
           id_jury, name_jury))

h <- d %>% 
  select(id_publi, name_jury) %>%
  graph_from_data_frame() %>%
  bipartite_projection(types = str_detect(V(.)$name, "\\d"), 
                       which = F,
                       remove.type = F)

i <- intersection(g, h, byname = "auto",
                  keep.all.vertices = TRUE)

i %>%
  ggraph(layout = l) + 
  geom_edge_link(edge_colour = "grey") +
  geom_node_point(aes(size = degree(g)), shape = 22, fill = "#037E40", stroke = 0.2) + # "darkgreen"
  geom_node_text(aes(filter = degree(g) >= 20, label = name), fontface = "bold", size = 1.5) +
  geom_node_text(aes(filter = degree(g) < 20, label = name), size = 1, repel = TRUE, min.segment.length = Inf, max.overlaps = Inf) +
  theme_void() +
  labs(size = "Jury members'\ndegree") +
  theme(legend.title = element_text(size = 5),
        legend.text = element_text(size = 4),
        legend.position = "bottom")

ggsave("plots/figure-3.pdf")
ggsave("plots/figure-3.png")

write_tsv(as.data.frame(l), "layout/layout_astro.tsv")

#############################################################################################################
# Figure 4
#############################################################################################################

n <- ego(g, order = 1, nodes = V(g)[name %in% c("Rème", "Von Ballmoos")], mode = c("all"),
    mindist = 0) %>% unlist()

s <- induced_subgraph(g, n)

S <- make_ego_graph(g, order = 0, V(g)[name %in% c("Rème", "Von Ballmoos")], mode = c("all"))

s <- union(S[[1]], S[[2]])

plot(S[[1]])

e <- incident_edges(g, V(g)[name %in% c("Rème", "Von Ballmoos")], mode = c("all"))

s <- subgraph.edges(g, unlist(e))

s %>%
  ggraph(layout = "fr") + 
  geom_edge_link(edge_colour = "grey") +
  geom_node_point(aes(filter = degree(s) >= 20, size = degree(s)), show.legend = F, shape = 21, fill = "red", stroke = 0.2) + # "darkgreen"
  geom_node_point(aes(filter = degree(s) < 20, size = degree(s)), show.legend = F, shape = 22, fill = "blue", stroke = 0.2) + # "darkgreen"
  geom_node_text(aes(filter = degree(s) >= 20, label = name), fontface = "bold", size = 3) +
  geom_node_text(aes(filter = degree(s) < 20, label = name), size = 2, repel = TRUE, min.segment.length = Inf, max.overlaps = Inf) +
  theme_void() +
  labs(size = "Jury members'\ndegree") +
  theme(legend.title = element_text(size = 5),
        legend.text = element_text(size = 4),
        legend.position = "bottom")

ggsave("plots/figure-4.pdf")
ggsave("plots/figure-4.png")

