# notion-cli-hs

## install

```bash
git clone git@github.com:kurubushi/notion-cli-hs.git
cd notion-cli-hs
stack install
```

## setup

Create a config file `~/.notion-cli.conf`:

```
[Cookie]
token_v2 = xxxxxxxxxx
```

## usage

Upload a file to S3 bucket.

```bash
export PATH=$HOME/.local/bin/PATH:$PATH
notion-cli-exec s3upload gongon.jpg
```

response:

```
File: gongon.jpg
URL: "https://s3-us-west-2.amazonaws.com/secure.notion-static.com/xxxxxxxx-xxxx-xxxx-xxxx-xxxxxxxxxxxx/gongon.jpg"
```

Embed the URL into an image block on Notion :+1:
