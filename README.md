# notion-cli-hs

## install

Install GHC and Cabal to build this tool. If you use [GHCup](https://www.haskell.org/ghcup/), you can install with:

```bash
ghcup install       # to install GHC
ghcup install-cabal # to install Cabal
export PATH=$HOME/.ghcup/bin:$PATH
```

Build a binary.

```bash
git clone git@github.com:kurubushi/notion-cli-hs.git
cd notion-cli-hs
make notion-cli
```

Install the created binary `./notion-cli`. For instance:

```bash
sudo install -Dm755 notion-cli /usr/local/bin
export PATH=/usr/local/bin:$PATH
```

## setup

Create a config file `~/.notion-cli.conf`:

```
[Cookie]
token_v2 = xxxxxxxxxx
```

## usage

### Upload files to a Database on Notion.

Obtain the UUID of a database from Network logs on user's web browser.
When a user accesses a database page, a JSON data is sent to the server.

For instance, a POST packet to https://www.notion.so/api/v3/queryCollection with 

```json
{
  "collectionId": "xxxxxxxx-xxxx-xxxx-xxxx-xxxxxxxxxxxx",
  "collectionViewId": "yyyyyyyy-yyyy-yyyy-yyyy-yyyyyyyyyyyy",
  "query": {},
  "loader": {
    "type": "table",
    "limit": 50,
    "searchQuery": "",
    "userTimeZone": "Asia/Tokyo",
    "loadContentCover": true
  }
}
```

is found. The `collectionId` is the UUID of the database.

Upload files to the database:

```bash
notion-cli upload --database-uuid xxxxxxxx-xxxx-xxxx-xxxx-xxxxxxxxxxxx --record-title pinogon pino.jpg gongon.jpg 
```

The command inserts a new record "pinogon" to the database and appends pino.jpg and gongon.jpg to the record page.
Check your database :+1:

### Upload files to a page on Notion.

Upload files to the page https://www.notion.so/user-name/page_title-xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx

```bash
notion-cli upload --page-url https://www.notion.so/user-name/page_title-xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx pino.jpg gongon.jpg 
```

The command appends pino.jpg and gongon.jpg to the page.

### Upload a file to S3 bucket.

Upload a file to S3:

```bash
notion-cli s3upload gongon.jpg
```

response:

```
File: gongon.jpg
URL: "https://s3-us-west-2.amazonaws.com/secure.notion-static.com/xxxxxxxx-xxxx-xxxx-xxxx-xxxxxxxxxxxx/gongon.jpg"
```

Embed the URL into an image block on Notion :+1:
