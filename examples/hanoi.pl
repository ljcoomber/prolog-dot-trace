% Towers of Hanoi example

:- use_module('dot_trace.pl').

move(1, X, Y, _) :-
    % TODO: Need to use explicit out stream to stop contaminating dot file
    format('//Move top disk from ~w to ~w~n', [X, Y]).
    
move(N, X, Y, Z) :- 
    N > 1, 
    M is N - 1, 
    move(M, X, Z, Y), 
    move(1, X, Y, _), 
    move(M, Z, Y, X).  

generate :-
    dot_trace:start_trace('examples/hanoi.dot', OldStr),
    move(3,left,right,center),
    dot_trace:stop_trace(OldStr).


