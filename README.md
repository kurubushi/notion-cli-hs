# notion-cli-hs

## install

```bash
git clone git@github.com:kurubushi/notion-cli-hs.git
cd notion-cli-hs
stack install
export PATH=$HOME/.local/bin:$PATH
```

## usage

Upload a file to S3 bucket.

```bash
export NOTION_TOKEN_V2="xxxxxxxxxx"
notion-cli-exec s3upload gongon.jpg
```

response:

```
File: gongon.jpg
URL: "https://s3-us-west-2.amazonaws.com/secure.notion-static.com/xxxxxxxx-xxxx-xxxx-xxxx-xxxxxxxxxxxx/gongon.jpg"
```

Embed the URL into an image block on Notion :+1:
