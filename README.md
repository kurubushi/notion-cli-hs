# notion-cli-hs

## install

```bash
git clone git@github.com:kurubushi/notion-cli-hs.git
cd notion-cli-hs
stack install
export PATH=$HOME/.local/bin/PATH:$PATH
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

POST https://www.notion.so/api/v3/queryCollection with 

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

The `collectionId` is the UUID of the database.

Upload files to the database:

```bash
notion-cli-exe upload --title pinogon --database-uuid xxxxxxxx-xxxx-xxxx-xxxx-xxxxxxxxxxxx pino.jpg gongon.jpg 
```

The command inserts a new record "pinogon" to the database and appends pino.jpg and gongon.jpg to the record page.
Check your database :+1:

### Upload a file to S3 bucket.

Upload a file to S3:

```bash
notion-cli-exe s3upload gongon.jpg
```

response:

```
File: gongon.jpg
URL: "https://s3-us-west-2.amazonaws.com/secure.notion-static.com/xxxxxxxx-xxxx-xxxx-xxxx-xxxxxxxxxxxx/gongon.jpg"
```

Embed the URL into an image block on Notion :+1:
