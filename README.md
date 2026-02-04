# Mean Function Simulator (Example 14)

Dự án này thực hiện mô phỏng **Monte Carlo** để tìm hàm trung bình (Mean function) của một quá trình ngẫu nhiên, dựa trên nội dung môn học **Quá trình ngẫu nhiên (C03021)**.

## 1. Đề bài (Example 14)
Tìm hàm trung bình, phương sai và độ lệch chuẩn cho quá trình ngẫu nhiên $X_n$ được cho bởi:

$$X_n = 1000(1 + R)^n, \quad \text{với } n = 0, 1, 2, \dots$$

Trong đó, lãi suất $R$ là một biến ngẫu nhiên có phân phối đều: $R \sim U(0.04, 0.05)$.

## 2. Mục tiêu mô phỏng
* **Thực hiện mô phỏng:** 10,000 kịch bản biến ngẫu nhiên $R$.
* **Tính toán:** Giá trị kỳ vọng $E[X_n]$ (Mean function) qua các năm (từ $n=0$ đến $n=30$).
* **So sánh:** Đối chiếu kết quả mô phỏng thực tế với kết quả tính toán bằng công thức lý thuyết (tích phân).
* **Đánh giá:** Tính toán sai số tương đối để xác nhận độ tin cậy của mô hình.

## 3. Cấu trúc mã nguồn
* **Ngôn ngữ:** R.
* **Thư viện chính:** `ggplot2`, `dplyr`, `tidyr`, `gridExtra`, `latex2exp`.
* **Thuật toán:**
    * Sử dụng hàm `runif()` để tạo mẫu cho biến ngẫu nhiên $R$.
    * Tối ưu hóa tính toán bằng ma trận để xử lý 10,000 kịch bản nhanh chóng.
    * Sử dụng hàm `integrate()` để giải quyết bài toán tích phân lý thuyết phục vụ đối chiếu.

## 4. Cách chạy dự án
1. Đảm bảo máy tính đã cài đặt **R** và các thư viện cần thiết.
2. Mở thư mục dự án bằng **RStudio** hoặc **VS Code**.
3. Chạy file `vd14.R`.
4. Kết quả sẽ hiển thị bảng thống kê tại Terminal và biểu đồ tại khung Plots.

---
*Dự án phục vụ mục đích nghiên cứu học thuật - Chương 1 mục 5, tài liệu học tập Quá trình ngẫu nhiên.*