import pandas as pd
import os

# Define paths
master_file = r'd:\PhD\Projects\WUR_projects\iSSA\results\Ecological_Behavior_V5\Elephant_Behavioral_Points_Final_V5.csv'
output_dir = r'd:\PhD\Projects\WUR_projects\iSSA\web_visualization\data\behavioral_points'

# Create output directory if it doesn't exist
if not os.path.exists(output_dir):
    os.makedirs(output_dir)

# Read the master file
df = pd.read_csv(master_file)

# Get unique elephants
elephants = df['Elephant'].unique()

# Split and save
for elephant in elephants:
    ele_df = df[df['Elephant'] == elephant]
    output_path = os.path.join(output_dir, f'{elephant}_behavioral_points.csv')
    ele_df.to_csv(output_path, index=False)
    print(f"Saved {elephant} to {output_path}")

print("Splitting complete.")
