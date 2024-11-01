import os
import subprocess
from datetime import datetime, timedelta

def create_folder_in_directories(folder_name, directories):
    for directory in directories:
        path = os.path.join(directory, folder_name)
        try:
            os.makedirs(path)
            print(f"Directory {path} created")
        except FileExistsError:
            print(f"Directory {path} already exists")

def aws_s3_copy(directory, bucket_path, dates):
    for date in dates:
        cmd = [
            'aws', 's3', 'cp', 
            bucket_path, 
            directory, 
            '--recursive',
            '--exclude', '*',
            '--include', f'*ingestion_timestamp={date}*'
        ]
        result = subprocess.run(cmd, capture_output=True, text=True)
        
        print(f"Running command: {' '.join(cmd)}")
        print(result.stdout)  # Print command's output
        print(result.stderr)  # Print errors

if __name__ == "__main__":
    today = datetime.now()
    dates = [(today - timedelta(days=i)).strftime('%Y%m%d') for i in range(1, 4)]
    folder_name = today.strftime('%d%m%Y')

    directories = [
        r"C:\Users\muhammadadlan\Downloads\customer_ultimate_beta\bronze_balances",
        r"C:\Users\muhammadadlan\Downloads\customer_ultimate_beta\bronze_customer",
        r"C:\Users\muhammadadlan\Downloads\customer_ultimate_beta\bronze_ftv",
        r"C:\Users\muhammadadlan\Downloads\customer_ultimate_beta\bronze_payments",
        r"C:\Users\muhammadadlan\Downloads\customer_ultimate_beta\bronze_reversal",
        r"C:\Users\muhammadadlan\Downloads\customer_ultimate_beta\bronze_saving_acc",
        r"C:\Users\muhammadadlan\Downloads\customer_ultimate_beta\bronze_savings_pot",
        r"C:\Users\muhammadadlan\Downloads\customer_ultimate_beta\bronze_address",
        r"C:\Users\muhammadadlan\Downloads\customer_ultimate_beta\bronze_employment",
        r"C:\Users\muhammadadlan\Downloads\customer_ultimate_beta\bronze_profile",
        r"C:\Users\muhammadadlan\Downloads\customer_ultimate_beta\bronze_crr",
        r"C:\Users\muhammadadlan\Downloads\customer_ultimate_beta\bronze_complyadv",
    ]

    bucket_paths = [
        "s3://prod-912407396582-dlakebronze1-balances-s3/vault-core-api/streaming/balance-events/api_version=1.0/vault.core_api.v1.balances.balance.events/",
        "s3://prod-912407396582-dlakebronze1-customers-s3/customers-service/customer-snapshot/api_version=1.0/customers.customer.snapshot.v1/",
        "s3://prod-912407396582-dlakebronze1-payments-s3/payments-service/transfer-inbound-initial/api_version=1.0/payments.transfer.inbound.initial.v1/",
        "s3://prod-912407396582-dlakebronze1-payments-s3/onboarding-service/initial-payment-verifications/api_version=1.0/onboarding.payments-service.initial-payment-verifications.v1/",
        "s3://prod-912407396582-dlakebronze1-payments-s3/payments-service/transfer-reversal-completed/api_version=2.0/payments.transfer.reversal.completed.v2/",
        "s3://prod-912407396582-dlakebronze1-deposits-s3/deposits-service/saving-account-snapshot/api_version=1.1/deposit-account-service.saving-account.snapshot.v1/",
        "s3://prod-912407396582-dlakebronze1-deposits-s3/deposits-service/saving-pot-snapshot/api_version=1.1/deposit-account-service.saving-pot.snapshot.v1/",
        "s3://prod-912407396582-dlakebronze1-customers-s3/customers-service/address-snapshot/api_version=1.0/customers.address.snapshot.v1/",
        "s3://prod-912407396582-dlakebronze1-customers-s3/customers-service/employment-snapshot/api_version=1.0/customers.employment.snapshot.v1/",
        "s3://prod-912407396582-dlakebronze1-customers-s3/customers-service/profile-snapshot/api_version=1.0/customers.profile.snapshot.v1/",
        "s3://prod-912407396582-dlakebronze1-framl-s3/framl-service/crr-result-snapshot/api_version=1.0/framl.crr-result.snapshot.v1/",
        "s3://prod-912407396582-dlakebronze1-framl-s3/framl-service/detailed-comply-advantage-snapshot/api_version=1.0/framl.detailed_comply_advantage.snapshot.v1/",
    ]

    create_folder_in_directories(folder_name, directories)

    for directory, bucket_path in zip(directories, bucket_paths):
        full_dir_path = os.path.join(directory, folder_name)
        aws_s3_copy(full_dir_path, bucket_path, dates)
