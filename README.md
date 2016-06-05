glot-snippets
=============

[![Build Status](https://travis-ci.org/prasmussen/glot-snippets.svg?branch=master)](https://travis-ci.org/prasmussen/glot-snippets)

## Overview
glot-snippets provides a http api for storing and managing snippets.
Snippets can be stored anonymously or as a user by including an api token
with the request. CouchDB is used as the datastore.
The api is described [here](https://github.com/prasmussen/glot-snippets/tree/master/api_docs).

## Run
The download above is a standard erlang release that includes a start script.
To start glot-snippets in the foreground type: `glot/bin/glot foreground`.

## Environment variables
glot-snippets takes it's configuration from environment variables.
All vars needs to be set, no default values are provided.

| Variable name        | Allowed values                | Example                  | Description                                                  |
|:---------------------|:------------------------------|:-------------------------|:-------------------------------------------------------------|
| API_ENVIRONMENT      | development &#124; production | production               | Development mode will enable auto compiling of changed files |
| API_HTTP_LISTEN_IP   | &lt;ipv4 address&gt;          | 0.0.0.0                  | Listen ip                                                    |
| API_HTTP_LISTEN_PORT | 1-65535                       | 8090                     | Listen port                                                  |
| LOG_PATH             | &lt;filepath&gt;              | /home/app/log/           | Path to save logs                                            |
| BASE_URL             | &lt;url&gt;                   | https://snippets.glot.io | Base url to where the api is hosted                          |
| ADMIN_TOKEN          | &lt;string&gt;                | some-secret              | Admin token used to access the /admin endpoints              |
| DB_URL               | &lt;url&gt;                   | http://10.0.0.9:5984     | Url to CouchDB                                               |
| DB_USER              | &lt;string&gt;                | glot                     | CouchDB user                                                 |
| DB_PASSWORD          | &lt;string&gt;                | secret-password          | CouchDB password                                             |

## Api users
Users can be created with the `/admin/users` endpoint.
See the [api docs](https://github.com/prasmussen/glot-snippets/tree/master/api_docs/admin) for more details.
