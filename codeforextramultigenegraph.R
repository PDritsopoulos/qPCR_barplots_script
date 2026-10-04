library(ggplot2)

# 1. Δεδομένα
df <- data.frame(
  Gene = c("PR5", "PDF1.2b", "PR2", "AGO4", "SID2", "NPR1", "GRX480",
           "MET1", "NPRE1", "PDF1.2a", "SDG8", "HAC1", "PR1 like", "OPR3", "CRT3", "MPK3"),
  FoldChange = c(7.87626, 7.62087, 29.48063, 1.37844, 0.83597, 
                 1.06831, 1.78681, 0.97982, 1.18196, 8.10186, 0.89722, 0.79233, 0.93093, 2.43098, 2.08222, 2.94163),
  SEM = c(1.99002, 2.19611, 8.10725, 0.33254, 0.08773,
          0.07887, 0.36239, 0.35442, 0.13279, 2.21738, 0.11096, 0.22172, 0.16379, 0.37548, 0.22745, 0.35956),
  signif = c("***", "***", "***", "", "", "", "*", "", "", "**", "", "", "", "***", "***", "***")
)

# 2. Κατηγοριοποίηση
df$Regulation <- ifelse(df$FoldChange >= 1, "Up-regulated", "Down-regulated")

# 3. Δημιουργία γραφήματος
ggplot(df, aes(x = reorder(Gene, -FoldChange), y = FoldChange, fill = Regulation)) +
  geom_col(width = 0.65, color = "black", linewidth = 0.3, alpha = 0.85) +
  geom_errorbar(
    aes(ymin = pmax(0, FoldChange - SEM), ymax = FoldChange + SEM),
    width = 0.25,
    linewidth = 0.6,
    color = "black"
  ) +
  geom_hline(yintercept = 1, linetype = "dashed", color = "gray30", linewidth = 0.6) +
  
  scale_fill_manual(values = c("Down-regulated" = "gray80", "Up-regulated" = "gray40")) +
  scale_y_continuous(
    limits = c(0, 42),
    breaks = seq(0, 40, by = 5),
    expand = c(0, 0)
  ) +
  geom_text(
    aes(y = FoldChange + SEM + 0.8, label = signif),
    size = 4,
    fontface = "bold",
    color = "black"
  ) +
  labs(
    title = "Relative gene expression in SRL248 treated samples of b2 after 10 days",
    x = "Genes",
    y = "Fold Change (Treated / Control)",
    fill = "Regulation",
    caption = "Dashed line indicates Control baseline (Fold Change = 1)\n* p < 0.05,   ** p < 0.01,   *** p < 0.001"
  ) +
  theme_classic(base_size = 14) +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold", size = 16, margin = margin(b = 15)),
    axis.text.x = element_text(angle = 45, hjust = 1, face = "italic", size = 11, color = "black"),
    axis.text.y = element_text(size = 11, color = "black"),
    axis.title = element_text(face = "bold", size = 13),
    legend.position = "top",
    plot.caption = element_text(
      hjust = 1,
      size = 10.5,
      face = "italic",
      color = "black",
      lineheight = 1.2,
      margin = margin(t = 12)
    )
  )