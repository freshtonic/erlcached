# Erlcached

An implementation of memcached in Erlang, supporting only the ASCII
protocol storage command subset. Should work with any ASCII protocol
client as per the Memcached ASCII protocol spec circa mid-2007. However,
this implementation does not implement the stats commands, only the
storage manipulation commands (GET, SET, INC etc).

# Why?

I wanted to learn Erlang and OTP. Memcached was all the rage and the
protocol was well-specified. This small project was the outcome of
implementing the protocol in Erlang.

I don't expect anyone to use it (Memcached can do things that this
implementation can't, such as control memory allocation and layout etc,
because it's written in C and has access to low-level functionality).
However, it might be useful to someone for learning purposes, e.g. it
would be really cool to see someone bring the protocol up to date and
implement the memcached binary protocol.

# How come you only just open-sourced it? It's, like, 2010 already!

No excuses really. But what prompted me to get off my arse and release
it was a promise I made with @GarryDanger at #rorosyd after too much beer.
The crazy man remembered!

# License

This project is licensed under the MIT License.

# Contact

Erlcached was written by me (James Sadler AKA @freshtonic) in mid-2007.

email: freshtonic@gmail.com
Twitter: http://twitter.com/freshtonic

