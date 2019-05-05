-module(perm).
-export([perm/2]).

-import(lists,[nth/2,nthtail/2,append/2,reverse/1]).

-include_lib("proper/include/proper.hrl").
-include_lib("eunit/include/eunit.hrl").


perm(In, Out) ->
  perm_a(In,[],Out).

perm_a(In, Stack, Out) ->
  case {In, Stack, Out} of
    {In, [], []} -> true;
    {In, Stack, []} ->
                case (nth(1,In) == nth(1,Stack)) of
                  false -> false;
                  true  -> perm_a(nthtail(1, In),
                                  nthtail(1, Stack),[])
                end;
    {In, [], Out} ->
                X = nth(1,Out),
                Xs = nthtail(1,Out),
                perm_a(In,[X],Xs);
    {In, Stack, Out} ->
      I = nth(1,In),
      S = nth(1,Stack),
      O = nth(1,Out),
      In_tail = nthtail(1, In),
      Stack_tail = nthtail(1, Stack),

      case I == S of
        true -> perm_a(In_tail,Stack_tail,Out);
        false -> Stack_O = append([O],Stack),
                 Out_tail = nthtail(1, Out),
                 perm_a(In,Stack_O,Out_tail)
      end
    end.


perm_test() ->
  [simple_test(), same_input_list(), prop_test_1(),prop_rev()].

prop_test_1() ->
  ?FORALL({L1,L2}, {list_no_dupls(integer()), list_no_dupls(integer())},
          perm(L1,L2) == true).

same_input_list() ->
  ?FORALL(L, list(integer()), perm(L,L) == true).

prop_rev() ->
  ?FORALL(L, list_no_dupls(integer()),
    perm(L,lists:reverse(L)) == true).

simple_test() ->
  ?assertEqual(true, perm([1,2,3],[3,2,1])),
  ?assertEqual(false, perm([1,2,3],[2,3,1])),
  ok.

list_no_dupls(T) ->
  ?LET(L, list(T), remove_duplicates(L)).

remove_duplicates([]) -> [];
remove_duplicates([A|T]) ->
  case lists:member(A, T) of
    true -> remove_duplicates(T);
    false -> [A|remove_duplicates(T)]
  end.
