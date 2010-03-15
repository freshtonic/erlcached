== Erlcached

An implementation of memcached in Erlang, supporting only the ASCII
protocol storage command subset. Should work with any ASCII protocol
client as per the Memcached ASCII protocol spec circa mid-2007. However,
this implementation does not implement the stats commands, only the
storage manipulation commands (GET, SET, INC etc).

== Why?

I wanted to learn Erlang and OTP. Memcached was all the rage and the
protocol was well-specified. This small project was the outcome of
implementing the protocol in Erlang.

== License

This project is licensed under the MIT License.

== Contact

Erlcached was written by me (James Sadler AKA @freshtonic) in mid-2007.

email: freshtonic@gmail.com
Twitter: http://twitter.com/freshtonic

