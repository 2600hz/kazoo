all:
	@test -d ebin || mkdir ebin
	@cp src/oauth.app.src ebin/oauth.app
	@erl -make

clean:
	@rm -rf ebin/* erl_crash.dump
