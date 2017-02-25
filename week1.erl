-module(week1).
-export([perimeter/1, area/1, enclose/1, bits/1, bits_direct_recurse/1]).
-include_lib("eunit/include/eunit.hrl").

%%
%% @doc Calculate the perimeter of a shape
%% @param shape Must be a tuple begining with 'circle', 'square', or 'triangle'
%%
perimeter({circle, {_X, _Y}, R}) when R >= 0 ->
    2*math:pi()*R;
perimeter({rectangle, {_X, _Y}, H, W}) when (H >= 0) and (W >= 0) ->
    2*H + 2*W;
perimeter({triangle, P1, P2, P3}) ->
    distance(P1, P2) + distance(P2, P3) + distance(P3, P1).

%%
%% @doc Calculate the area of a shape
%% @param shape Must be a tuple begining with 'circle', 'square', or 'triangle'
%%
area({circle, {_X, _Y}, R})  when R >= 0 ->
    math:pi()*square(R);
area({rectangle, {_X, _Y}, H, W}) when (H >= 0) and (W >= 0) ->
    H * W;
area({triangle, P1, P2, P3}) ->
    % using Heron's formula
    A = distance(P1, P2),
    B = distance(P2, P3),
    C = distance(P3, P1),
    S = (A + B + C)/2,
    math:sqrt(S*(S-A)*(S-B)*(S-C)).

%%
%% @doc get an enclosing rectangle of the passed shape
%%
enclose({circle, {X, Y}, R})  when R >= 0 ->
    {rectangle, {X, Y}, 2*R, 2*R};
enclose({rectangle, {X, Y}, H, W}) when (H >= 0) and (W >= 0) ->
    {rectangle, {X, Y}, H, W};
enclose({triangle, {X1, Y1}, {X2, Y2}, {X3, Y3}}) ->
    XMIN = min(X1, X2, X3),
    XMAX = max(X1, X2, X3),
    YMIN = min(Y1, Y2, Y3),
    YMAX = max(Y1, Y2, Y3),
    {rectangle, {(XMAX - XMIN)/2, (YMAX - YMIN)/2}, YMAX - YMIN, XMAX - XMIN}.

%%
%% @doc Calculate the sum of the bits using tail recursion
%% @param N must be a positive integer
%% @returns an integer which is the sum of the bits
%%
bits(N) when N > 0 ->
    bits_tail_recurse(N, 0).

bits_tail_recurse(0, P) ->
    P;
bits_tail_recurse(V, P) when V band 1 == 1 ->
    bits_tail_recurse(V div 2, P + 1);
bits_tail_recurse(V, P) ->
    bits_tail_recurse(V div 2, P).

%%
%% @doc Calculate the sum of the bits using direct recursion
%% @param N must be a positive integer
%% @returns an integer which is the sum of the bits
%%
bits_direct_recurse(N) when N > 0 ->
    bits_direct_recurse_sub(N).

bits_direct_recurse_sub(N) when N == 0 ->
    0;
bits_direct_recurse_sub(N) ->
    N band 1 + bits_direct_recurse_sub(N div 2).

%%
%% Private, not exported functions
%%

%% @doc square a number
square(X) ->
    X * X.

%% @doc Calculate the distance between two cartesian coordinates
distance({X1, Y1}, {X2, Y2}) ->
    math:sqrt(square(X2 - X1) + square(Y2 - Y1)).

%% @doc get the min of 3 numbers
min(A, B, C) ->
    min2(min2(A, B), min2(B, C)).

min2(A, B) when A > B ->
    B;
min2(A, _B) ->
    A.

%% @doc get the max of 3 numbers
max(A, B, C) ->
    max2(max2(A, B), max2(B, C)).

max2(A, B) when A > B ->
    A;
max2(_A, B) ->
    B.


%%
%% Unit Tests
%%
bits_tail_recurse_test() ->
    [?assertEqual(3, bits(7)),
     ?assertEqual(1, bits(8)),
     ?assertEqual(1, bits(1)),
     % the assignment states that N must be positive
     ?assertException(error, function_clause, bits(0)),
     ?assertException(error, function_clause, bits(-6))
    ].

bits_direct_recurse_test() ->
    [?assertEqual(3, bits_direct_recurse(7)),
     ?assertEqual(1, bits_direct_recurse(8)),
     ?assertEqual(1, bits_direct_recurse(1)),
     % the assignment states that N must be positive
     ?assertException(error, function_clause, bits_direct_recurse(0)),
     ?assertException(error, function_clause, bits_direct_recurse(-8))
    ].

perimeter_circle_test() ->
    [?assert((perimeter({circle, {0, 0}, 5}) < 31.42) and (perimeter({circle, {0, 0}, 5}) > 31.41)),
     ?assertEqual(0.0, perimeter({circle, {0, 0}, 0})),
     ?assertException(error, function_clause, perimeter({circle, {0,0}, -1}))
    ].

perimeter_rect_test() ->
    [?assertEqual(18, perimeter({rectangle, {0, 0}, 5, 4})),
     ?assertEqual(20, perimeter({rectangle, {0, 0}, 0, 10})),
     ?assertEqual(20, perimeter({rectangle, {0, 0}, 10, 0})),
     ?assertException(error, function_clause, perimeter({rectangle, {0,0}, -1, -5}))
    ].

perimeter_triangle_test() ->
    [?assertEqual(12.0, perimeter({triangle, {0, 0}, {0, 4}, {3, 0}})),
     ?assertEqual(12.0, perimeter({triangle, {0, 0}, {0, -4}, {-3, 0}}))
    ].

area_circle_test() ->
    [?assert((area({circle, {0, 0}, 5}) < 78.54) and (area({circle, {0, 0}, 5}) > 78.53)),
     ?assertEqual(0.0, area({circle, {0, 0}, 0})),
     ?assertException(error, function_clause, area({circle, {0,0}, -1}))
    ].

area_rect_test() ->
    [?assertEqual(20, area({rectangle, {0, 0}, 5, 4})),
     ?assertEqual(0, area({rectangle, {0, 0}, 0, 10})),
     ?assertEqual(0, area({rectangle, {0, 0}, 10, 0})),
     ?assertException(error, function_clause, area({rectangle, {0,0}, -1, -5}))
    ].

area_triangle_test() ->
    [?assertEqual(6.0, area({triangle, {0, 0}, {0, 4}, {3, 0}})),
     ?assertEqual(6.0, area({triangle, {0, 0}, {0, -4}, {-3, 0}}))
    ].


enclose_circle_test() ->
    [?assertEqual({rectangle, {3, 4}, 10, 10}, enclose({circle, {3, 4}, 5})),
     ?assertEqual({rectangle, {0, 0}, 0, 0}, enclose({circle, {0, 0}, 0})),
     ?assertException(error, function_clause, enclose({circle, {0,0}, -1}))
    ].

enclose_rect_test() ->
    [?assertEqual({rectangle, {0, 0}, 5, 4}, enclose({rectangle, {0, 0}, 5, 4})),
     ?assertException(error, function_clause, enclose({rectangle, {0,0}, -1, 5})),
     ?assertException(error, function_clause, enclose({rectangle, {0,0}, 1, -5}))
    ].

enclose_triangle_test() ->
    [?assertEqual({rectangle, {1.5, 2.0}, 4, 3}, enclose({triangle, {0, 0}, {0, 4}, {3, 0}})),
     ?assertEqual({rectangle, {2.0, 1.5}, 3, 4}, enclose({triangle, {0, 0}, {0, -3}, {-4, 0}}))
    ].    