import matplotlib.pyplot as plt

# 定義變數名稱和對應的迴歸係數、標準誤、exp(beta) 值
variables = ['trustngo', 'gender', 'children', 'education', 'agegroup', 'ruralurban', 'GDP', 'BMI']
coefficients = [4.99e-01, 2.76e-01, 1.68e-01, 9.44e-02, 6.60e-02, 5.85e-02, 2.22e-05, -2.02e-01]
exp_beta = [1.647703, 1.317774, 1.18331, 1.098985, 1.068244, 1.06024, 1.000022, 0.817133]
std_error = [3.36e-02, 3.39e-02, 3.97e-02, 6.39e-03, 1.17e-02, 2.12e-02, 1.19e-06, 3.18e-02]

# 建立條形圖
plt.figure(figsize=(10, 6))
bars = plt.bar(variables, coefficients, yerr=std_error, capsize=5, color='lightgreen')  # 畫條形圖，並顯示誤差條
plt.xlabel('varables')  # X軸標籤
plt.ylabel('regression coefficient')  # Y軸標籤
plt.title('regression models variables & exp(beta)')  # 圖片標題

# 在條形上方顯示 exp(beta) 的值
for bar, exp_val in zip(bars, exp_beta):
    height = bar.get_height()
    plt.text(bar.get_x() + bar.get_width() / 2.0, height, f'exp({exp_val:.2f})', ha='center', va='bottom')

plt.axhline(0, color='black', linewidth=0.8)  # 添加一條0基線
plt.tight_layout()  # 自動調整佈局，避免標籤重疊
plt.show()  # 顯示圖表
