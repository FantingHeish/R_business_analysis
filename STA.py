import pandas as pd
import matplotlib.pyplot as plt
from math import pi

# 使用表格數據
data = {
    'Group': [1, 2, 3, 4],
    'Age_Group': [2.84, 1.66, 2.03, 2.09],
    'Gender': [1.74, 1.66, 1.62, 1.71],
    'Salary': [2.45, 2.29, 1.83, 2.57],
    'Education': [2.36, 1.93, 2.27, 2.66],
    'Employment': [2.55, 2.07, 2.82, 2.00],
    'Location by region': [1.35, 1.23, 1.51, 1.14],
    'Choco consumption': [3.04, 2.99, 3.01, 2.88],
    'Sustainability score': [-0.27, -0.31, 0.16, 0.43]
}

df = pd.DataFrame(data)

# 定義雷達圖類別
categories = list(df.columns[1:])
N = len(categories)

# 繪製雷達圖的函數
def create_radar_chart(df, group, ax):
    values = df.loc[group].drop('Group').values.flatten().tolist()
    values += values[:1]  # 關閉雷達圖循環
    angles = [n / float(N) * 2 * pi for n in range(N)]
    angles += angles[:1]

    # 設定雷達圖
    ax.set_theta_offset(pi / 2)
    ax.set_theta_direction(-1)

    # 繪製類別標籤
    plt.xticks(angles[:-1], categories)

    # 繪製數據
    ax.plot(angles, values, linewidth=2, linestyle='solid', label=f'Group {df.loc[group]["Group"]}')
    ax.fill(angles, values, alpha=0.4)

# 繪製雷達圖
fig, ax = plt.subplots(figsize=(8, 8), subplot_kw=dict(polar=True))

# 對每個群組繪製圖表
for i in range(len(df)):
    create_radar_chart(df, i, ax)

# 設置標題與圖例
plt.title('Radar Chart of Consumer Groups')
plt.legend(loc='upper right', bbox_to_anchor=(1.1, 1.1))

# 調整圖表布局以避免被截斷
plt.tight_layout()

# 顯示圖表
plt.show()
