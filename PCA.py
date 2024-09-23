import pandas as pd
import matplotlib.pyplot as plt

# 假設以下數據是從你的表格中提取的
data = {
    'Component': ['Comp.1', 'Comp.2', 'Comp.3', 'Comp.4', 'Comp.5', 'Comp.6', 'Comp.7', 'Comp.8', 'Comp.9'],
    'Proportion of Variance': [0.4218449, 0.1947797, 0.1237576, 0.09993709, 0.07958722, 0.03719878, 0.01953307, 0.01585152, 0.007510126],
    'Cumulative Proportion': [0.4218449, 0.6166246, 0.7403822, 0.84031927, 0.9199065, 0.95710528, 0.97663835, 0.99249887, 1.00000000]
}

# 轉換為 DataFrame
df = pd.DataFrame(data)

# 繪製條形圖來展示每個成分的變異數比例和累積變異數比例
fig, ax = plt.subplots(figsize=(10, 6))
ax.bar(df['Component'], df['Proportion of Variance'], color='b', label='Proportion of Variance')
ax.plot(df['Component'], df['Cumulative Proportion'], color='r', marker='o', label='Cumulative Proportion')

# 增加一些圖形格式化
ax.set_xlabel('Components')
ax.set_ylabel('Proportion')
ax.set_title('PCA Component Importance')
ax.legend()

# 顯示圖表
plt.show()
