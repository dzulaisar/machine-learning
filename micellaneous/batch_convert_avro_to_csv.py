import os
import sys
import csv
import fastavro

def avro_to_csv(input_file, output_file):
    with open(input_file, 'rb') as avro_file:
        reader = fastavro.reader(avro_file)
        schema = reader.writer_schema

        with open(output_file, 'w', newline='', encoding='utf-8') as csv_file:
            writer = csv.DictWriter(csv_file, fieldnames=[field['name'] for field in schema['fields']])
            writer.writeheader()

            for record in reader:
                writer.writerow(record)

def batch_convert_avro_to_csv(start_dir):
    for root, dirs, files in os.walk(start_dir):
        for file in files:
            if file.endswith(".avro"):
                input_file = os.path.join(root, file)
                output_file = os.path.splitext(input_file)[0] + ".csv"
                avro_to_csv(input_file, output_file)
                print(f"Converted {input_file} to {output_file}")

if __name__ == "__main__":
    if len(sys.argv) != 2:
        print("Usage: python batch_convert_avro_to_csv.py <start_directory>")
    else:
        start_dir = sys.argv[1]
        batch_convert_avro_to_csv(start_dir)
