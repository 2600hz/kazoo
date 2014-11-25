Nonterminals
  addresses
  address
  name
  names
  email.

Terminals
  string
  ',' '<' '>'.
  
Rootsymbol
  addresses.

Endsymbol
  '$end'.

addresses -> address : ['$1'].
addresses -> address ',' addresses : ['$1' | '$3'].
addresses -> '$empty' : [].

address -> email : {undefined, '$1'}.
address -> '<' email '>' : {undefined, '$2'}.
address -> names '<' email '>' : {lists:flatten('$1'), '$3'}.

email -> string : element(3, '$1').
names -> name : '$1'.
names -> name names : ['$1', " " | '$2'].
name -> string : element(3, '$1').