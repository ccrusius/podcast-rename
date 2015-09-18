all:
	./rebar get-deps compile

eunit:
	./rebar get-deps compile eunit

release:
	./rebar get-deps compile generate

clean:
	./rebar clean
	rm -rf rel/podcast_rename

node:
	cd rel && ../rebar create-node -f nodeid=podcast_rename
