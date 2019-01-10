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

<img align="right" src="https://cdn.rawgit.com/timjb/halma/master/images/telegram-chat.jpg" alt="Chatbot screenshot" />

## The Telegram Chatbot

There is a chatbot running on AWS Lambda: [@halma_bot](http://telegram.me/halma_bot)

(Please be patient, it sometimes needs a few seconds to respond.)

### Building and deploying the Telegram bot on AWS Lambda

The following command builds and deploys the Telegram bot on AWS Lambda:

```bash
$ python3 build_and_deploy_to_lambda.py \
    --telegram-token 123456:ABC-DEF1234ghIkl-zyx57W2v1u123ew11 \
    --stack-name telegram-halma-stack \
    --code-s3-bucket my-code-bucket # the executable will be uploaded to this bucket
```

This Python 3.7 script depends on `stack`, `docker` and `awscli` being installed (and in the `PATH`). When `nix-shell` is installed, this script can also be called using

```bash
$ ./build_and_deploy_to_lambda.py [...]
```

This will automatically install the required dependencies on the first run. Before the first run, you also need to run:

```bash
$ stack docker pull # download the Docker image for building
$ aws configure # configure authentication and default region (if you haven't done so already)
```

If deployment is successful, the script prints out the URL where the Lambda function can be invoked:

```
[...]
Webhook URL: https://abcdefghijk.execute-api.eu-central-1.amazonaws.com/Prod/halma-telegram-bot
```

Now, you may run a simple smoke test:

```bash
$ curl --header "Content-Type: application/json" \
       --request POST \
       --data '{"update_id":123412341234}' \
       https://abcdefghijk.execute-api.eu-central-1.amazonaws.com/Prod/halma-telegram-bot
```

To configure the Telegram Bot API to call the created lambda function when there is an update run

```bash
curl -X POST \
  --url 'https://api.telegram.org/bot123456:ABC-DEF1234ghIkl-zyx57W2v1u123ew11/setWebhook' \
  -d 'url=https://abcdefghijk.execute-api.eu-central-1.amazonaws.com/Prod/halma-telegram-bot'
```

with `123456:ABC-DEF1234ghIkl-zyx57W2v1u123ew11` replaced by your Telegram API access token.

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
