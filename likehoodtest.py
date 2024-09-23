import matplotlib.pyplot as plt
import numpy as np

# 模擬數據
attributes = ["Tokens", "Organic", "Premium", "Fairtrade"]
log_likelihood_diff = [960.6, 966.21, 407.08, 1017.7]  # 模擬的loglikelihood差異數據

# 創建條形圖
plt.figure(figsize=(10, 6))
bars = plt.barh(attributes, log_likelihood_diff, color='skyblue')
plt.xlabel('Log-likelihood difference (Δχ²)')
plt.title('Likelihood Ratio Test Results - Impact of Removing Attributes')

# 顯示數值
for bar in bars:
    plt.text(bar.get_width(), bar.get_y() + bar.get_height()/2, f'{bar.get_width():.2f}', va='center')

plt.show()
