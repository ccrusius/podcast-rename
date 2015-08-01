all:
	./rebar get-deps compile

eunit:
	./rebar get-deps compile eunit

release:
	./rebar get-deps compile generate
