# Môn học: Quá trình ngẫu nhiên - C03021
# Mô phỏng hàm trung bình (Mean function) - ví dụ 14
# Tối ưu hóa cho Positron IDE

# 1. KHỞI TẠO GÓI TIN 
required_packages <- c("ggplot2", "dplyr", "tidyr", "gridExtra", "scales", "latex2exp", "grid")
new_packages <- required_packages[!(required_packages %in% installed.packages()[, "Package"])]
if (length(new_packages)) install.packages(new_packages)
invisible(lapply(required_packages, library, character.only = TRUE))

# 2. THIẾT LẬP THAM SỐ
set.seed(12345) 
n_simulations <- 10000 
n_max <- 30           
r_min <- 0.04         
r_max <- 0.05         
initial_amount <- 1000 
n_vector <- 0:n_max

# 3. TÍNH TOÁN LÝ THUYẾT (TỐI ƯU HÓA) 
# Thay vì dùng sapply và tích phân số trị lặp lại, ta dùng công thức tích phân giải tích:
# E[(1+R)^n] = [ (1+r_max)^(n+1) - (1+r_min)^(n+1) ] / [ (n+1)(r_max - r_min) ]

calc_theory <- function(n, r_min, r_max, initial) {
  if (n == 0) return(initial)
  # Công thức tích phân của (1+r)^n từ r_min đến r_max
  val <- ((1 + r_max)^(n + 1) - (1 + r_min)^(n + 1)) / ((n + 1) * (r_max - r_min))
  return(initial * val)
}

calc_theory_sq <- function(n, r_min, r_max, initial) {
  if (n == 0) return(initial^2)
  # Công thức cho E[X_n^2] tương tự với bậc 2n
  val <- ((1 + r_max)^(2*n + 1) - (1 + r_min)^(2*n + 1)) / ((2*n + 1) * (r_max - r_min))
  return(initial^2 * val)
}

theoretical_means <- sapply(n_vector, calc_theory, r_min, r_max, initial_amount)
theoretical_vars  <- sapply(n_vector, calc_theory_sq, r_min, r_max, initial_amount) - theoretical_means^2
theoretical_sd    <- sqrt(pmax(0, theoretical_vars))

# 4. MÔ PHỎNG MONTE CARLO (VECTORIZED) 
cat("Bắt đầu mô phỏng Monte Carlo...\n")

# Tạo lãi suất ngẫu nhiên
R_values <- runif(n_simulations, min = r_min, max = r_max)

# Tối ưu: Sử dụng phép nhân ngoài (Outer product) để tránh vòng lặp for lồng nhau
# Công thức: initial * (1 + R)^n
# X_matrix sẽ có kích thước [n_simulations x (n_max + 1)]
exponent_matrix <- outer(R_values, n_vector, function(r, n) (1 + r)^n)
X_matrix <- initial_amount * exponent_matrix

# 5. THỐNG KÊ KẾT QUẢ 
simulated_means <- colMeans(X_matrix)
simulated_sd    <- apply(X_matrix, 2, sd)
se_means        <- simulated_sd / sqrt(n_simulations)

summary_stats <- data.frame(
  n = n_vector,
  simulated_mean = simulated_means,
  theoretical_mean = theoretical_means,
  simulated_sd = simulated_sd,
  theoretical_sd = theoretical_sd,
  ci_lower = simulated_means - 1.96 * se_means,
  ci_upper = simulated_means + 1.96 * se_means,
  relative_error = abs((simulated_means - theoretical_means) / theoretical_means) * 100
)

# 6. VẼ BIỂU ĐỒ (GGPLOT2) 
main_plot <- ggplot(summary_stats, aes(x = n)) +
  geom_ribbon(aes(ymin = ci_lower, ymax = ci_upper), fill = "lightblue", alpha = 0.4) +
  geom_line(aes(y = simulated_mean, color = "Mô phỏng"), linewidth = 1) +
  geom_point(aes(y = simulated_mean, color = "Mô phỏng"), size = 1.5) +
  geom_line(aes(y = theoretical_mean, color = "Lý thuyết"), linetype = "dashed", linewidth = 1) +
  scale_color_manual(values = c("Mô phỏng" = "darkblue", "Lý thuyết" = "darkred")) +
  scale_y_continuous(labels = label_dollar()) +
  labs(x = "Năm (n)", y = "E[X_n]", color = "Loại") +
  theme_minimal() +
  theme(legend.position = "inside", legend.position.inside = c(0.15, 0.85),
        legend.background = element_rect(fill="white", color="gray"))

# Biểu đồ sai số
plot_error <- ggplot(summary_stats, aes(x = n, y = relative_error)) +
  geom_line(color = "purple3", linewidth = 1) +
  geom_point(color = "purple4", size = 2) +
  geom_hline(yintercept = mean(summary_stats$relative_error), 
             color = "darkgreen", linetype = "dotted", linewidth = 0.8) +
  annotate("text", x = n_max * 0.7, y = mean(summary_stats$relative_error),
           label = paste("TB:", round(mean(summary_stats$relative_error), 3), "%"),
           vjust = -1, color = "darkgreen", size = 3) +
  labs(title = "Sai số tương đối (%)", x = "Năm (n)", y = "Sai số (%)") +
  scale_x_continuous(breaks = seq(0, n_max, by = 5)) +
  theme_minimal(base_size = 10) +
  theme(plot.title = element_text(face = "bold", hjust = 0.5))

# Biểu đồ phân phối R
plot_r_dist <- ggplot(data.frame(R = R_values), aes(x = R)) +
  geom_histogram(aes(y = after_stat(density)), bins = 30, fill = "steelblue", color = "white") +
  geom_density(color = "red") +
  theme_minimal() +
  labs(title = "Phân phối lãi suất R")

# 7. HIỂN THỊ VÀ LƯU KẾT QUẢ ==================================================
# Positron hỗ trợ hiển thị grid tốt
final_layout <- grid.arrange(
  main_plot, 
  arrangeGrob(plot_r_dist, plot_error, ncol = 1), 
  ncol = 2, widths = c(2, 1),
  top = textGrob("KẾT QUẢ MÔ PHỎNG QUÁ TRÌNH NGẪU NHIÊN", gp = gpar(fontsize=15, font=2))
)

ggsave("simulation_result.png", final_layout, width = 12, height = 7, dpi = 300)

# In bảng tóm tắt nhanh
print(summary_stats[summary_stats$n %in% c(0, 10, 20, 30), c("n", "simulated_mean", "theoretical_mean", "relative_error")])