import os
import sys
import csv
from datetime import datetime

def concatenate_cleaned_csv_files(start_dir, output_file):
    # Step 1: Collect unique headers from all the input files
    unique_headers = set()

    for root, dirs, files in os.walk(start_dir):
        for file in files:
            if file.endswith(".csv"):
                input_file = os.path.join(root, file)

                with open(input_file, 'r', encoding='utf-8') as in_csv_file:
                    csv_reader = csv.reader(in_csv_file)

                    try:
                        header = next(csv_reader)
                    except StopIteration:
                        print(f"Skipping empty file: {input_file}")
                        continue

                    unique_headers.update(header)

    # Step 2: Create a dictionary to map header names to their corresponding column indices
    header_map = {name: index for index, name in enumerate(sorted(unique_headers))}

    # Step 3: Write the new header to the output file
    with open(output_file, 'w', newline='', encoding='utf-8') as out_csv_file:
        csv_writer = csv.writer(out_csv_file)
        csv_writer.writerow(header_map.keys())

    # Step 4: For each row in the input files, create a new row with the correct structure
    for root, dirs, files in os.walk(start_dir):
        for file in files:
            if file.endswith(".csv"):
                input_file = os.path.join(root, file)

                # Skip the output file
                if input_file == output_file:
                    continue

                with open(input_file, 'r', encoding='utf-8') as in_csv_file:
                    csv_reader = csv.reader(in_csv_file)
                    input_header = next(csv_reader)

                    input_header_indices = {name: index for index, name in enumerate(input_header)}

                    with open(output_file, 'a', newline='', encoding='utf-8') as out_csv_file:
                        csv_writer = csv.writer(out_csv_file)

                        for row in csv_reader:
                            new_row = [''] * len(header_map)
                            for name, index in input_header_indices.items():
                                new_row[header_map[name]] = row[index]

                            csv_writer.writerow(new_row)

                print(f"Concatenated {input_file}")

    print(f"Concatenated all .csv files and saved as {output_file}")
    
start_dir = os.getcwd()
today = datetime.today().strftime("%Y-%m-%d")
output_file = f"data_{today}.csv"
concatenate_cleaned_csv_files(start_dir, output_file)