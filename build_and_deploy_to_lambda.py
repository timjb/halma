#! /usr/bin/env nix-shell
#! nix-shell -i python3 -p "[python37 docker stack awscli]"

import argparse
import json
import os
import os.path
import subprocess
import shutil

STACK_OPTS = ["--no-nix", "--docker", "--docker-stack-exe", "download", "--no-docker-set-user"]

def build():
  subprocess.run(["stack", "build"] + STACK_OPTS + ["halma-telegram-bot"], check=True)

def package(code_s3_bucket):
  deploy_config_file = "deploy.yaml"
  stack_local_path = subprocess.run(
    ["stack", "path", "--local-install-root"] + STACK_OPTS,
    stdout=subprocess.PIPE,
    check=True,
    encoding='ascii'
  ).stdout.strip()
  bin_path = stack_local_path + "/bin/aws-lambda-halma-telegram-bot"
  os.makedirs("build", exist_ok=True)
  shutil.copy2(bin_path, "build/bootstrap")
  subprocess.run([
    "aws", "cloudformation", "package",
    "--template-file", "template.yaml",
    "--s3-bucket", code_s3_bucket,
    "--output-template-file", deploy_config_file
  ], check=True)
  return deploy_config_file

def deploy(telegram_token, stack_name, config_file):
  print(f"Testing whether stack '{stack_name}' already exists ...")
  stack_exists = subprocess.run([
    "aws", "cloudformation", "describe-stacks",
    "--stack-name", stack_name
  ], stdout=subprocess.DEVNULL, stderr=subprocess.DEVNULL).returncode == 0
  if stack_exists:
    print("Stack exists already => using 'aws cloudformation deploy' to update")
    subprocess.run([
      "aws", "cloudformation", "deploy",
      "--template-file", config_file,
      "--stack-name", stack_name,
      "--capabilities", "CAPABILITY_IAM", "CAPABILITY_AUTO_EXPAND",
      "--parameter-overrides", "ParameterKey=TelegramTokenParam,ParameterValue=" + telegram_token,
      "--no-fail-on-empty-changeset"
    ], check=True)
  else:
    print("Stack doesn't exist yet => creating with 'aws cloudformation create-stack'")
    subprocess.run([
      "aws", "cloudformation", "create-stack",
      "--template-body", "file://" + config_file,
      "--stack-name", stack_name,
      "--capabilities", "CAPABILITY_IAM", "CAPABILITY_AUTO_EXPAND",
      "--parameters", "ParameterKey=TelegramTokenParam,ParameterValue=" + telegram_token
    ], check=True)

def info(stack_name):
  describe_stacks_process = subprocess.run([
    "aws", "cloudformation", "describe-stacks",
    "--stack-name", stack_name
  ], capture_output=True)
  if describe_stacks_process.returncode != 0: return
  stack_infos = json.loads(describe_stacks_process.stdout)
  for output in stack_infos['Stacks'][0]['Outputs']:
    if output['OutputKey'] == 'WebhookUrl':
      webhook_url = output['OutputValue']
      print(f"Webhook URL: {webhook_url}")

def pipeline(telegram_token, stack_name, code_s3_bucket):
  build()
  config_file = os.path.abspath(package(code_s3_bucket))
  deploy(telegram_token, stack_name, config_file)
  info(stack_name)

parser = argparse.ArgumentParser(description="Build and deploy lambda function")
parser.add_argument('--telegram-token', required=True)
parser.add_argument('--stack-name', required=True)
parser.add_argument('--code-s3-bucket', required=True)

def main():
  args = parser.parse_args()
  pipeline(
    telegram_token=args.telegram_token,
    stack_name=args.stack_name,
    code_s3_bucket=args.code_s3_bucket
  )

if __name__ == '__main__':
  main()
