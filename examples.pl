:- use_module('dot_trace.pl').
    
factorial(0, R, R). 

factorial(N, A, R) :-  
    N > 0, 
    A1 is N * A, 
    N1 is N - 1, 
    factorial(N1, A1, R).

generate_factorial :-
    dot_trace:start_trace('examples/factorial.dot', OldStr),
    factorial(3, 1, _),
    dot_trace:stop_trace(OldStr).

/*

dump_frame_attribute(Frame, Key):-
    (prolog_frame_attribute(Frame, Key, Value) ; Value=no),
    format('  ~w: ~w: ~w~n', [Frame, Key, Value]).

prolog_trace_interception(Port, Frame, Choice, continue) :-
    format('~w: ~w: ~w~n', [Port, Frame, Choice]),
    dump_frame_attribute(Frame, alternative),
    dump_frame_attribute(Frame, has_alternatives),
    dump_frame_attribute(Frame, goal),
    %dump_frame_attribute(Frame, parent_goal),
    dump_frame_attribute(Frame, predicate_indicator),
    dump_frame_attribute(Frame, clause),
    dump_frame_attribute(Frame, level),
    dump_frame_attribute(Frame, parent),
    dump_frame_attribute(Frame, context_module),
    dump_frame_attribute(Frame, top),
    dump_frame_attribute(Frame, hidden),
    %dump_frame_attribute(Frame, skipped),
    dump_frame_attribute(Frame, pc),
    dump_frame_attribute(Frame, argument(1)).
 */

