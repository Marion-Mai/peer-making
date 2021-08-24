# Figures 8-10. Commitee co-membership networks in economics
# Bès, Marie-Pierre, Lamy, Jérôme et Maisonobe, Marion. 2021. Supplementary material for Quantitative Science Studies research article: "Peer-making: the interconnections between PhD Thesis Committee membership and co-publishing".

library(tidyverse)
library(igraph)
library(ggraph)

#############################################################################################################

d <- read_tsv("data/ECO/jury_composition.tsv") %>%
  left_join(
    select(read_tsv("datignore/ECO/jury_name.tsv"), 
           id_jury, name_jury))  

g <- d %>% 
  select(id_phd, name_jury) %>%
  graph_from_data_frame() %>%
  bipartite_projection(types = str_detect(V(.)$name, "\\d"), 
                       which = F,
                       remove.type = F)

l <- layout_with_fr(g)

##############################################################################################################
# Figure 8
##############################################################################################################

g %>%
  ggraph(layout = l) + 
  geom_edge_link(edge_colour = "grey") +
  geom_node_point(aes(size = degree(g)), shape = 22, fill = "#037E40", stroke = 0.2) + # "darkgreen"
  geom_node_text(aes(label = name, fontface = ifelse(degree(g) >= 40, "bold", "plain")), size = ifelse(degree(g) >= 40, 3, 2), colour = "black", repel = T, show.legend = F) + # ,  repel = TRUE, min.segment.length = Inf, max.overlaps = Inf / size = ifelse(degree(g) >= 20, 5, 4)
  theme_void() +
  labs(size = "Jury members'\ndegree") +
  theme(legend.title = element_text(size = 6),
        legend.text = element_text(size = 5),
        legend.position = "bottom")

ggsave("plots/figure-8.pdf")
ggsave("plots/figure-8.png")

#############################################################################################################
# Figure 9
#############################################################################################################

g %>%
  ggraph(layout = l) + 
  geom_edge_link(aes(filter = E(g)$weight >= 2), edge_colour = "grey") +
  geom_node_point(aes(size = degree(g)), shape = 22, fill = "#037E40", stroke = 0.2) + # "darkgreen"
  geom_node_text(aes(label = name, fontface = ifelse(degree(g) >= 40, "bold", "plain")), size = ifelse(degree(g) >= 40, 3, 2), colour = "black", repel = T, show.legend = F) + # ,  repel = TRUE, min.segment.length = Inf, max.overlaps = Inf / size = ifelse(degree(g) >= 20, 5, 4)
  # geom_node_text(aes(filter = degree(g) >= 2, label = name), size = 1.2, repel = TRUE,
     #            segment.color = "#037E40") +
  theme_void() +
  labs(size = "Jury members'\ndegree") +
  theme(legend.title = element_text(size = 6),
        legend.text = element_text(size = 5),
        legend.position = "bottom")

ggsave("plots/figure-9.pdf")
ggsave("plots/figure-9.png")

#############################################################################################################
# Figure 10
#############################################################################################################

d <- read_tsv("data/ECO/jury_publications.tsv") %>%
  left_join(
    select(read_tsv("datignore/ECO/jury_name.tsv"), 
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
  geom_node_text(aes(label = name, fontface = ifelse(degree(i) >= 20, "bold", "plain")), size = ifelse(degree(i) >= 20, 3, 2), colour = "black", repel = T, show.legend = F) + # ,  repel = TRUE, min.segment.length = Inf, max.overlaps = Inf / size = ifelse(degree(g) >= 20, 5, 4)
  theme_void() +
  labs(size = "Jury members'\ndegree") +
  theme(legend.title = element_text(size = 6),
        legend.text = element_text(size = 5),
        legend.position = "bottom")

ggsave("plots/figure-10.pdf")
ggsave("plots/figure-10.png")

write_tsv(as.data.frame(l), "layout/layout_eco.tsv")


