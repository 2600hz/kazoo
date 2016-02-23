task :clean do
  sh "rebar clean"
end

task :build => :clean do
  sh "rebar compile"
end

task :shell do
  sh "erl -pa ebin deps/*/ebin"
end

task :getdeps do
  sh "rebar get-deps"
end

task :doc do
  sh "rebar doc"
end

task :gettestdeps do
  sh "rebar -C rebar.tests.config get-deps"
end

task :features do
  sh "rebar -C rebar.tests.config compile run-features path=test/acceptance skip_deps=true"
end

task :spec do
  sh "rebar -C rebar.tests.config compile && ERL_LIBS='deps/' ./espec test/spec/"
end

task :default => :build
