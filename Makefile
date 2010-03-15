.SUFFIXES: .erl .beam 

.erl.beam:
	erlc -W $<

ERL = erl -boot start_clean

MODS = erlcached_server \
			 erlcached_memcached_ascii_protocol \
			 erlcached_supervisor \
			 erlcached_app \
			 erlcached_launch \
			 erlcached_debug

all: compile tags

compile: ${MODS:%=%.beam}

launch_app: compile
	${ERL} -s erlcached_launch

appmon: compile
	${ERL} -s erlcached_launch start_with_appmon

clean:
	rm -rf *.beam *.dump tags

tags:
	ctags --no-warn \
		`find /usr/lib/erlang/lib/kernel-2.11.2/src -name \*.erl` *.erl
