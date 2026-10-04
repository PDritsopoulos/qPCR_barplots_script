library(ggplot2)

# 1. Δεδομένα με προσθήκη στήλης significance
df <- data.frame(
  replicate = factor(c("b1", "b2", "b3"), levels = c("b1", "b2", "b3")),
  fold_change = c(0.59858,0.55445,0.39689),
  sd = c(0.06716,0.12723,0.21664),
  signif = c("", "", "") # Τα αστεράκια για κάθε επανάληψη
)

# 2. Δημιουργία γραφήματος
p <- ggplot(df, aes(x = replicate, y = fold_change)) +
  geom_col(
    fill = "darkgrey", 
    color = "black", 
    linewidth = 1.2, 
    width = 0.5
  ) +
  geom_errorbar(
    aes(ymin = fold_change - sd, ymax = fold_change + sd),
    width = 0.15, 
    linewidth = 0.8, 
    color = "black"
  ) +
  # Αστεράκια σημαντικότητας πάνω από το άνω άκρο του error bar
  geom_text(
    aes(y = fold_change + sd + 0.15, label = signif),
    size = 7,
    fontface = "bold",
    color = "black"
  ) +
  # Επέκταση ορίου άξονα Y για να χωράνε άνετα τα αστεράκια
  scale_y_continuous(
    limits = c(0, 1),
    breaks = seq(0,3, by = 0.5),
    expand = c(0, 0)
  ) +
  # Τίτλος, ετικέτες και υπόμνημα p-values κάτω δεξιά
  labs(
    title = "VSP2 expression after 5 days with SRL248",
    x = NULL,
    y = "Fold Change\n(Treated/Control)",
    caption = "* p < 0.05,  ** p < 0.01,  *** p < 0.001"
  ) +
  theme_classic(base_size = 18) +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold", size = 22, margin = margin(b = 15)),
    
    # Μορφοποίηση υπομνήματος κάτω δεξιά
    plot.caption = element_text(
      hjust = 1,          # Στοίχιση δεξιά
      size = 12, 
      face = "italic", 
      color = "black", 
      margin = margin(t = 12)
    ),
    
    axis.line = element_line(linewidth = 1.2, color = "black"),
    axis.ticks = element_line(linewidth = 1.2, color = "black"),
    axis.ticks.length = unit(0.25, "cm"),
    axis.text.x = element_text(face = "bold", size = 20, color = "black", margin = margin(t = 8)),
    axis.text.y = element_text(face = "bold", size = 18, color = "black"),
    axis.title.y = element_text(face = "bold", size = 20, margin = margin(r = 15)),
    plot.margin = margin(20, 20, 10, 20)
  )

print(p)