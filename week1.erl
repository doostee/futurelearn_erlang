-module(week1).
-export([perimeter/1, area/1, enclose/1, bits_tail_recurse/1, bits_direct_recurse/1]).
-include_lib("eunit/include/eunit.hrl").

perimeter(S) ->
    1.

area(S) ->
    1.

enclose(S) ->
    {}.

%% @doc Calculate the sum of the bits using tail recursion
%% @param N must be a positive integer
%% @returns an integer which is the sum of the bits
bits_tail_recurse(N) when N > 0 ->
    bits_tail_recurse(N, 0).

bits_tail_recurse(0, P) ->
    P;
bits_tail_recurse(V, P) when V band 1 == 1 ->
    bits_tail_recurse(V div 2, P + 1);
bits_tail_recurse(V, P) ->
    bits_tail_recurse(V div 2, P).

%% @doc Calculate the sum of the bits using direct recursion
%% @param N must be a positive integer
%% @returns an integer which is the sum of the bits
bits_direct_recurse(N) when N > 0 ->
    bits_direct_recurse_sub(N).

bits_direct_recurse_sub(N) when N == 0 ->
    0;
bits_direct_recurse_sub(N) ->
    N band 1 + bits_direct_recurse_sub(N div 2).

%% Unit Tests
bits_tail_recurse_test() ->
    [?assert(bits_tail_recurse(7) =:= 3),
     ?assert(bits_tail_recurse(8) =:= 1),
     ?assert(bits_tail_recurse(1) =:= 1),
     % the assignment states that N must be positive
     ?assertException(error, function_clause, bits_tail_recurse(0)),
     ?assertException(error, function_clause, bits_tail_recurse(-6))
    ].

bits_direct_recurse_test() ->
    [?assert(bits_direct_recurse(7) =:= 3),
     ?assert(bits_direct_recurse(8) =:= 1),
     ?assert(bits_direct_recurse(1) =:= 1),
     % the assignment states that N must be positive
     ?assertException(error, function_clause, bits_direct_recurse(0)),
     ?assertException(error, function_clause, bits_direct_recurse(-8))
    ].
