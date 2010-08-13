all: clean compile

clean:
	rm -f src/.\#*
	rm -rf ebin/
	rm -f *.dump

compile:
	mkdir -p ebin/
	erl -pa ebin -make

debug: all
	cd ebin
	@echo ""
	@echo ""
	erl -noshell -pa ebin -s youji start
