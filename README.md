# Halma [![Build Status][travis-image]][travis-url] [![Hackage version][hackage-image]][hackage-url] [![Hackage dependencies][hackage-deps-image]][hackage-deps-url] [![Code Climate][codeclimate-image]][codeclimate-url]

![Halma Game][game-image]
![Halma Menu][menu-image]

## Installation

Make sure you have GTK and Cairo installed, then run

```bash
$ cabal install halma-gui
$ halma-gui
```

If cabal fails to solve the version constraints, try using [Stackage](http://www.stackage.org/).

## Documentation

See the docs on [Hackage][hackage-url].

## Deploying the Telegram bot on AWS Lambda

First, you need to build a Linux executable using Docker with Stack:

```bash
$ stack docker pull
$ stack build halma-telegram-bot --docker
```

The last command takes some while. It produces the name of a directory where you can find the compiled executable. Now, we must bundle that executable together with a wrapper written with node:

```bash
$ zip -j lambda-halma-telegram-bot.zip halma-telegram-bot/lambda-handler.js .stack-work/install/x86_64-linux-dkda49f7ca9b244180d3cfb1987cbc9743/lts-8.8/8.0.2/bin/halma-telegram-bot-serverless
```

Create a bucket on S3. In the following, this bucket is called `halma-telegram-bucket`. This bucket will be used for saving the game state.

Now create a new Lambda function using `lambda-halma-telegram-bot.zip` and these settings:

* **Runtime**: Node.js 6.10
* **Trigger**: Use API Gateway as a trigger. Choose "open" as the security setting, so that Telegram can call it.
* **Handler**: Enter `lambda-handler.handler`
* **Role**: The lambda function needs have permissions to call `GetObject`, `PutObject`, `ListBucket` on your newly created bucket. Additionally, as all lambda functions, it must be able to create logs.
  ```json
  {
    "Version": "2017-04-04",
    "Statement": [
      {
        "Effect": "Allow",
        "Action": ["logs:CreateLogGroup", "logs:CreateLogStream", "logs:PutLogEvents"],
        "Resource": "arn:aws:logs:*:*:*"
      },
      {
        "Effect": "Allow",
        "Action": ["s3:ListBucket"],
        "Resource": "arn:aws:s3:::halma-telegram-bucket"
      },
      {
        "Effect": "Allow",
        "Action": ["s3:PutObject", "s3:GetObject"],
        "Resource": "arn:aws:s3:::halma-telegram-bucket/*"
      }
    ]
  }
  ```
* **Environment variables**:
  * `TELEGRAM_TOKEN`: the token you got from the Botfather (e.g. `bot191051711:AAHJAuoSfO_jF254ola6RPhTX2dopur8U8w`)
  * `HALMA_S3_BUCKET`: the name of the bucket created above (e.g. `halma-telegram-bucket`)
* **Timeout**: As rendering the halma board can take a while, make sure to give the function a sufficient timeout (at least 20 seconds).

Finally, configure the Telegram Bot API to call the created lambda function when there is an update: 

```bash
curl -X POST \
  --url 'https://api.telegram.org/bot123456:ABC-DEF1234ghIkl-zyx57W2v1u123ew11/setWebhook' \
  -d 'url=https://abcdefghijk.execute-api.eu-central-1.amazonaws.com/prod/halma_telegram_bot_handler'
```

[travis-image]: https://img.shields.io/travis/timjb/halma.svg
[travis-url]: http://travis-ci.org/timjb/halma
[hackage-image]: https://img.shields.io/hackage/v/halma.svg?style=flat
[hackage-url]: http://hackage.haskell.org/package/halma
[hackage-deps-image]: https://img.shields.io/hackage-deps/v/halma.svg?style=flat
[hackage-deps-url]: http://packdeps.haskellers.com/feed?needle=halma
[codeclimate-image]: https://codeclimate.com/github/timjb/halma/badges/gpa.svg
[codeclimate-url]: https://codeclimate.com/github/timjb/halma

[game-image]: https://cdn.rawgit.com/timjb/halma/master/images/halma-game.png
[menu-image]: https://cdn.rawgit.com/timjb/halma/master/images/halma-menu.png
