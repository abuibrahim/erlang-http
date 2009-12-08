-module(persons).
-export([get/0]).

get() ->
    [{object, <<"Person">>,
      [{firstName, <<"Barack">>},
       {lastName, <<"Obama">>},
       {age, 47},
       {phone, <<"333.444.4321">>},
       {email, <<"president@whitehouse.gov">>}]},
     {object, <<"Person">>,
      [{firstName, <<"Joe">>},
       {lastName, <<"Biden">>},
       {age, 66},
       {phone, <<"444.222.1234">>},
       {email, <<"vp@whitehouse.gov">>}]},
     {object, <<"Person">>,
      [{firstName, <<"Sarah">>},
       {lastName, <<"Palin">>},
       {age, 44},
       {phone, <<"123.332.2312">>},
       {email, <<"governor@gov.state.ak.us">>}]},
     {object, <<"Person">>,
      [{firstName, <<"John">>},
       {lastName, <<"McCain">>},
       {age, 72},
       {phone, <<"666.111.2312">>},
       {email, <<"mccain@senate.gov">>}]}].
