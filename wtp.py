import matplotlib.pyplot as plt

# Data from your analysis
labels = ['OriginVenezuela', 'ManufactureDeveloped', 'NutsNuts and Fruit', 'TokensDonate',
          'EnergyHigh', 'OrganicYes', 'PremiumYes', 'FairtradeYes', 'SugarHigh']
values = [-2.74862, 3.098304, 0.9784127, 4.680652, 5.038907, 4.788499, 16.96076, 5.638683, 7.425811]

# Creating the bar chart
plt.figure(figsize=(10, 6))
plt.barh(labels, values, color='skyblue')
plt.xlabel('Willingness to Pay (in price units)')
plt.title('Willingness to Pay for Different Product Attributes')

# Displaying the plot
plt.tight_layout()
plt.show()
