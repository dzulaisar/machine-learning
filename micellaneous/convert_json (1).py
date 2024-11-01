import os
import sys
import csv
import json

import os
import sys
import csv
import json
from datetime import datetime


def flatten_dict(d, parent_key='', sep='_'):
    items = []
    for k, v in d.items():
        new_key = f"{parent_key}{sep}{k}" if parent_key else k
        if isinstance(v, dict):
            items.extend(flatten_dict(v, new_key, sep=sep).items())
        elif isinstance(v, list):
            for i, elem in enumerate(v):
                if isinstance(elem, dict):
                    items.extend(flatten_dict(elem, f"{new_key}{sep}{i}", sep=sep).items())
                else:
                    items.append((f"{new_key}{sep}{i}", elem))
        else:
            items.append((new_key, v))
    return dict(items)


def json_to_csv(input_file, output_file):
    with open(input_file, 'r', encoding='utf-8') as json_file:

        with open(output_file, 'w', newline='', encoding='utf-8') as csv_file:
            writer = csv.DictWriter(csv_file, fieldnames=None, extrasaction='ignore')

            header_written = False

            for line in json_file:
                record = json.loads(line)
                flattened_record = flatten_dict(record)

                if not header_written:
                    sorted_fieldnames = sorted(flattened_record.keys())  # Add this line
                    writer.fieldnames = sorted_fieldnames  # Update this line
                    writer.writeheader()
                    header_written = True

                writer.writerow(flattened_record)


def batch_convert_json_to_csv(start_dir):
    for root, dirs, files in os.walk(start_dir):
        for file in files:
            if file.endswith(".json"):
                input_file = os.path.join(root, file)
                output_file = os.path.splitext(input_file)[0] + ".csv"
                json_to_csv(input_file, output_file)
                print(f"Converted {input_file} to {output_file}")

# Change directory to call function
start_dir = os.getcwd()
batch_convert_json_to_csv(start_dir)