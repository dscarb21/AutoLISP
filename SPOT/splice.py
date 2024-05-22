import pandas as pd

# Read the CSV file into a DataFrame
df = pd.read_csv('points_data_large.csv')

# Keep only the first 20,000 rows
df = df.head(10000)

# Save the DataFrame to a new CSV file
df.to_csv('data_small.csv', index=False)
