# Erlang.org website
Source code for the Erlang.org website.

## Setting up Erlang.org locally

Erlang.org uses [Cowboy](https://github.com/ninenines/cowboy) for web server support and [ErlyDTL](https://github.com/erlydtl/erlydtl) for rendering the web pages. It uses [sumo_db](https://github.com/inaka/sumo_db) to connect to a PostgreSQL database.

### Setting up Erlang.org using docker

Run:

   docker-compiler up

Then connect to http://localhost:8080 and enjoy.

### Erlang/OTP

Use Erlang/OTP 18.3. Follow the instructions on https://github.com/kerl/kerl to install Erlang/OTP.

### Configure ops.config

Edit and rename the file `rel/ops.config.template` to `rel/ops.config`. It contains the configuration information and tells the application which port (by default `8080`) to host the website on and which database to pull information from.

### Database 
By default, Erlang.org connects to a PostgresSQL database called `erlang_org`, using the username `postgres` and password `postgres`. 

Create a database `erlang_org`:
```sql
CREATE DATABASE erlang_org;
```
Import the sample data:
```
$ psql erlang_org < erlang_org_data
```

### Running Erlang.org
Run the following command to start the server:
```
$ make run
```
The website will be available at http://localhost:8080.

### Templates

The templates for rendering the web pages are located at `templates/*.dtl`. Learn more about the [ErlyDTL](https://github.com/erlydtl/erlydtl/wiki) templates at https://github.com/erlydtl/erlydtl/wiki.

### Project Structure

Since this project is built with [sumo_db](http://github.com/inaka/sumo_db), it's code structure uses the _repository pattern_. Therefore, the code is organized in the following folders:

* **handlers:** In it you will find handlers for cowboy. For example, `erlang_docs_handler` is the one used to serve the `/docs` page.
* **models:** In this folder you will find _sumo_db_ models and repos. For each entity in the system you'll find two modules:
  - one of them represents the Abstract Data Type that models the entity (e.g. `erlorg_articles` contains all the functions that allow you to manipulate entities with type `erlorg_articles:article()`, but there is no business logic in it).
  - the other one (with suffix `_repo`) contains the business logic associated with the entity. For example, in `erlorg_articles_repo` you'll find functions to _create_, _fetch_, _list_, etc. entities with type `erlorg_articles:article()`. These functions only manipulate the internal data for those entities using the functions in the `erlorg_articles` module. And they talk to the database using functions in the `sumo` model, like `sumo:persist/2` to store objects.
* **stores:** In this folder you will find _sumo_db_ stores. This is the place where database specific logic is written. Functions that are specific to the underlying persistence tool we're using (in this case postgreSQL) and not generic are written in `erlorg_store_pgsql`.
* **utils:** In this folder we have utility functions to deal with some non-sumo-specific datatypes like binaries, datetimes and cowboy requests.

For more documentation on SumoDB, you can check its [hex.pm page](http://hex.pm/packages/sumo_db) and if you want to contribute to this project and you are unsure about where to put your code or how to write it don't hesitate to contact [Inaka](http://inaka.net) through their [public hipchat room](http://inaka.net/hipchat).

---

## License

```
Copyright 2016 Industrial Erlang User Group

Licensed under the Apache License, Version 2.0 (the "License");
you may not use this file except in compliance with the License.
You may obtain a copy of the License at

    http://www.apache.org/licenses/LICENSE-2.0

Unless required by applicable law or agreed to in writing, software
distributed under the License is distributed on an "AS IS" BASIS,
WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
See the License for the specific language governing permissions and
limitations under the License.
```
