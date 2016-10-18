# Erlang.org website
Source code for the Erlang.org website.

## Setting up Erlang.org locally

Erlang.org uses [Cowboy](https://github.com/ninenines/cowboy) for web server support and [ErlyDTL](https://github.com/erlydtl/erlydtl) for rendering the web pages. It uses [sumo_db](https://github.com/inaka/sumo_db) to connect to a PostgreSQL database.

### Erlang/OTP

Use Erlang/OTP 17.5. Follow the instructions on https://github.com/kerl/kerl to install Erlang/OTP.

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
