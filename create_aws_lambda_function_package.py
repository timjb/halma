#! /usr/bin/env nix-shell
#! nix-shell -i python3 -p "[python3 docker stack]"

import subprocess
from zipfile import ZipFile, ZIP_DEFLATED

ZIP_FILE_NAME = "aws-lambda-halma-telegram-bot.zip"
STACK_OPTS = ["--no-nix", "--docker", "--docker-stack-exe", "download", "--no-docker-set-user"]

def main():
  subprocess.run(["stack", "build"] + STACK_OPTS + ["halma-telegram-bot"], check=True)
  stack_local_path = subprocess.run(
    ["stack", "path", "--local-install-root"] + STACK_OPTS,
    stdout=subprocess.PIPE,
    check=True,
    encoding='ascii'
  ).stdout.strip()
  bin_path = stack_local_path + "/bin/aws-lambda-halma-telegram-bot"
  with ZipFile(ZIP_FILE_NAME, mode='w', compression=ZIP_DEFLATED) as zf:
    print(f"Writing {ZIP_FILE_NAME} ...")
    zf.write(bin_path, arcname="bootstrap")

if __name__ == '__main__':
  main()