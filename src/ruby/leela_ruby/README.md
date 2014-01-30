# leela-ruby

ruby-leela is the ruby gem responsible to provide ruby bindings for
leela client C library.

## Installation

First you need to install leela C library, there are available packages for
debian squeeze/wheezy named leela-c and the package source is available to build
on leela repository.

After installing leela-ruby (libleela.so on your /usr/lib path), add this line
to your application's Gemfile:

        gem 'leela_ruby'

And then execute:

        $ bundle

Or install it yourself as:

        $ gem install leela_ruby


## Usage

Everything is done by using the connection class of leela ruby:

        # more than 1 endpoints are available
        endpoints = [
          "tcp://leela1.locaweb.com.br:4080",
          "tcp://leela2.locaweb.com.br:4080"
        ]

        conn = Leela::Connection.new(endpoints, "user", "password")


Using the the open connection the execute method will be available to execute
queries on leela:

        result = conn.execute("using (namespace) stat")


and to stream data block syntax are also available:

        conn.execute("using (namespace) stat") do |row|
          puts row
        end

at the end the connection should be closed:

        conn.close


## Contributing

1. Fork leela project
2. Work on it
3. Make all tests pass
4. Create new Pull Request
