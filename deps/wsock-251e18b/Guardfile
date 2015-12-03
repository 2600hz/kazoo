guard :shell do
  watch(%r{src/.+\.erl}) {|m| `rebar compile && dialyzer --plt plt ebin` }
end
