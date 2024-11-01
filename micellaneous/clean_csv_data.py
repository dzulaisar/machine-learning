import os
import sys
import json
import ast
import pandas as pd
import re

def parse_data(s):
    s = s.replace("None", "null")
    s = s.replace("True", "true")
    s = s.replace("False", "false")
    s = re.sub(r"datetime\.datetime\([^)]+\)", "null", s)
    s = s.replace("'", '"')
    return json.loads(s)

def flatten_json(nested_json, exclude_keys=['']):
    out = {}

    def flatten(x, name='', exclude=exclude_keys):
        if type(x) is dict:
            for a in x:
                if a not in exclude:
                    flatten(x[a], name + a + '_')
        elif type(x) is list:
            i = 0
            for a in x:
                flatten(a, name + str(i) + '_')
                i += 1
        else:
            out[name[:-1]] = x

    flatten(nested_json)
    return out

def clean_csv_data(input_file, output_file):
    df = pd.read_csv(input_file)

    # Replace NaN with None
    df = df.where(pd.notnull(df), None)

    # Parse JSON strings into dictionaries
    df['key'] = df['key'].apply(parse_data)
    df['value'] = df['value'].apply(parse_data)

    # Flatten the nested dictionaries
    df_key = pd.json_normalize(df['key'])
    df_value = pd.json_normalize(df['value'])

    # Merge the flattened data back together
    cleaned_df = pd.concat([df_key, df_value, df['timestamp']], axis=1)

    # Write the cleaned DataFrame to a new CSV file
    cleaned_df.to_csv(output_file, index=False)
    print(f"Cleaned {input_file} and saved as {output_file}")

def batch_clean_csv_data(start_dir):
    for root, dirs, files in os.walk(start_dir):
        for file in files:
            if file.endswith(".csv"):
                input_file = os.path.join(root, file)
                output_file = os.path.splitext(input_file)[0] + "_cleaned.csv"
                clean_csv_data(input_file, output_file)

if __name__ == "__main__":
    if len(sys.argv) != 2:
        print("Usage: python clean_csv_data.py <start_directory>")
    else:
        start_dir = sys.argv[1]
        batch_clean_csv_data(start_dir)
