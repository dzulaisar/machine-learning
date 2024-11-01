import os
import subprocess
from datetime import datetime
import argparse

def execute_python_scripts(directory, scripts, script_directory):
    for script in scripts:
        script_path = os.path.join(script_directory, script)
        if not os.path.exists(script_path):
            print(f"Cannot find script: {script_path}")
            continue
        subprocess.run(f'python {script_path}', cwd=directory)

if __name__ == "__main__":
    # Argument parser setup
    parser = argparse.ArgumentParser(description='Run Python scripts for a given date')
    parser.add_argument('-d', '--date', type=str, help='Date in ddMMyyyy format')

    # Parse arguments
    args = parser.parse_args()

    # If a date is specified, use that. Otherwise, use today's date.
    if args.date:
        try:
            folder_name = datetime.strptime(args.date, '%d%m%Y').strftime('%d%m%Y')
        except ValueError:
            print("Invalid date format. Please enter the date in ddMMyyyy format.")
            exit(1)
    else:
        # Get today's date
        today = datetime.now()
        folder_name = today.strftime('%d%m%Y')

    # Directory and script setup
    base_directory = r"C:\Users\muhammadadlan\Downloads\customer_ultimate_beta"

    sub_directories = [
        "bronze_balances",
        "bronze_customer",
        "bronze_ftv",
        "bronze_payments",
        "bronze_reversal",
        "bronze_saving_acc",
        "bronze_savings_pot",
        "bronze_address",
        "bronze_employment",
        "bronze_profile",
        "bronze_crr",
        "bronze_complyadv",
    ]

    scripts = [
        ["convert_json.py", "batch_concat.py"],
        ["batch_convert_avro.py", "batch_concat.py"],
        ["payments.py", "batch_concat.py"],
        ["payments.py", "batch_concat.py"],
        ["payments.py", "batch_concat.py"],
        ["batch_convert_avro.py", "batch_concat.py"],
        ["batch_convert_avro.py", "batch_concat.py"],
        ["batch_convert_avro.py", "batch_concat.py"],
        ["batch_convert_avro.py", "batch_concat.py"],
        ["batch_convert_avro.py", "batch_concat.py"],
        ["batch_convert_avro.py", "batch_concat.py"],
        ["batch_convert_avro.py", "batch_concat.py"],
    ]

    # Execution of scripts
    for sub_directory, script in zip(sub_directories, scripts):
        script_directory = os.path.join(base_directory, sub_directory)
        full_directory_path = os.path.join(script_directory, folder_name)
        execute_python_scripts(full_directory_path, script, script_directory)
