import os
import sys
import csv
import fastavro

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

def avro_to_csv(input_file, output_file):
    with open(input_file, 'rb') as avro_file:
        reader = fastavro.reader(avro_file)
        schema = reader.writer_schema

        with open(output_file, 'w', newline='', encoding='utf-8') as csv_file:
            writer = csv.DictWriter(csv_file, fieldnames=None, extrasaction='ignore')

            header_written = False

            for record in reader:
                unnested_record = {}
                for k, v in record['key'].items():
                    unnested_record[k] = v
                for k, v in record['value'].items():
                    unnested_record[k] = v
                
                flattened_record = flatten_dict(unnested_record)
                
                if not header_written:
                    writer.fieldnames = flattened_record.keys()
                    writer.writeheader()
                    header_written = True

                writer.writerow(flattened_record)

def batch_convert_avro_to_csv(start_dir):
    for root, dirs, files in os.walk(start_dir):
        for file in files:
            if file.endswith(".avro"):
                input_file = os.path.join(root, file)
                output_file = os.path.splitext(input_file)[0] + ".csv"
                avro_to_csv(input_file, output_file)
                print(f"Converted {input_file} to {output_file}")

# Change directory to call function
start_dir = os.getcwd()
batch_convert_avro_to_csv(start_dir)