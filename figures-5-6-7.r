# Figures 5-7. Commitee co-membership networks in archaeology
# Bès, Marie-Pierre, Lamy, Jérôme et Maisonobe, Marion. 2021. Supplementary material for Quantitative Science Studies research article: "Peer-making: the interconnections between PhD Thesis Committee membership and co-publishing".

library(tidyverse)
library(igraph)
library(ggraph)

#############################################################################################################

d <- read_tsv("data/ARCHEO/jury_composition.tsv") %>%
  left_join(
    select(read_tsv("datignore/ARCHEO/jury_name.tsv"), 
           id_jury, name_jury))  

g <- d %>% 
  select(id_phd, name_jury) %>%
  graph_from_data_frame() %>%
  bipartite_projection(types = str_detect(V(.)$name, "\\d"), 
                       which = F,
                       remove.type = F)

l <- layout_with_fr(g)

##############################################################################################################
# Figure 5
##############################################################################################################

g %>%
  ggraph(layout = l) + 
  geom_edge_link(edge_colour = "grey") +
  geom_node_point(aes(size = degree(g)), shape = 22, fill = "#037E40", stroke = 0.2) + # "darkgreen", #018300
  geom_node_text(aes(filter = degree(g) >= 20, label = name), fontface = "bold", size = 1.5) +
  geom_node_text(aes(filter = degree(g) < 20, label = name), repel = TRUE, min.segment.length = Inf,  size = 1.5) +
  theme_void() +
  labs(size = "Jury members'\ndegree") +
  theme(legend.title = element_text(size = 5),
        legend.text = element_text(size = 4),
        legend.position = "bottom")

ggsave("plots/figure-5.pdf")
ggsave("plots/figure-5.png")

#############################################################################################################
# Figure 6
#############################################################################################################

g %>%
  ggraph(layout = l) + 
  geom_edge_link(aes(filter = E(g)$weight >= 2), edge_colour = "grey") +
  geom_node_point(aes(size = degree(g)), shape = 22, fill = "#037E40", stroke = 0.2) + # "darkgreen"
  geom_node_text(aes(filter = degree(g) >= 20, label = name), fontface = "bold", size = 1.5) +
  geom_node_text(aes(filter = degree(g) < 20, label = name), repel = TRUE, min.segment.length = Inf,  size = 1.5) +
  theme_void() +
  labs(size = "Jury members'\ndegree") +
  theme(legend.title = element_text(size = 5),
        legend.text = element_text(size = 4),
        legend.position = "bottom")

ggsave("plots/figure-6.pdf")
ggsave("plots/figure-6.png")

#############################################################################################################
# Figure 7
#############################################################################################################

d <- read_tsv("data/ARCHEO/jury_publications.tsv") %>%
  left_join(
    select(read_tsv("datignore/ARCHEO/jury_name.tsv"), 
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
  geom_node_text(aes(filter = degree(g) < 20, label = name), repel = TRUE, min.segment.length = Inf,  size = 1.5) +
  theme_void() +
  labs(size = "Jury members'\ndegree") +
  theme(legend.title = element_text(size = 5),
        legend.text = element_text(size = 4),
        legend.position = "bottom")

ggsave("plots/figure-7.pdf")
ggsave("plots/figure-7.png")
