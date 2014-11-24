Distributed static content app.
======================================


## Overview #
Alum provides a static web content mult host server and features horizontal scalability and high availability.


Alum is an open source Erlang application that is distributed using the [riak_core](https://github.com/basho/riak_core) Erlang library, and ensures strong consistency for the data stored within the nodes using the [riak_ensemble](https://github.com/basho/riak_ensemble) Erlang library.



API
---

## Basic Usage #


### Configuring a new host #
### http://localhost:9090/hosts/{name} #

```shell
curl -XPUT "http://localhost:9090/hosts/static.alum.com" -d '{ "cache" : 3600 }'
```

###  Storing content #
###  http://localhost:9090/content/{host}/{filepath} #

```shell
curl -XPUT "http://localhost:9090/static.alum.com/images/avatar.jpg" -d @path-to-avatar.jpg
```

###  Serving static files #
###  http://{host}/{filepath} #

```shell
curl -XGET "http://static.alum.com/images/avatar.jpg"
```
