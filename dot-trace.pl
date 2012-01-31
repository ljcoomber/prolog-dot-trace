% TODO: Module name
% TODO: Modularise
% TODO: How to document predicates
% TODO: Tests
% TODO: Cuts, redos / alternatives
% TODO: Put start node at top
start_trace(DstFile, OldStr) :-
    telling(OldStr),
    tell(DstFile),
    print('digraph prologTrace {'), nl,
    trace.

stop_trace(OldStr):-
    notrace,
    print('}'), nl,
    told,
    tell(OldStr).

% Do not display calls to stop_trace, and rename caller to 'start'
prolog_trace_interception(call, Frame, _Choice, continue):-
    prolog_frame_attribute(Frame, predicate_indicator, stop_trace/1),
    reference_parent_gen(Frame, ParentReference),
    format('    ~w [label="Start"];~n', [ParentReference]).

prolog_trace_interception(exit, Frame, _Choice, continue) :-
    prolog_frame_attribute(Frame, predicate_indicator, start_trace/2).

prolog_trace_interception(call, Frame, _Choice, continue):-
    reference_gen(Frame, Reference),
    prolog_frame_attribute(Frame, goal, Goal),
    format('    ~w [label="~W"];~n', [Reference, Goal,
                                      [ quoted(true),
                                        numbervars(true),
                                        portray(true) ]]),
    
    reference_parent_gen(Frame, ParentReference),
    format('    ~w -> ~w;~n', [ParentReference, Reference]).

prolog_trace_interception(exit, Frame, _Choice, continue):-
    prolog_frame_attribute(Frame, clause, _),
    prolog_frame_attribute(Frame, goal, Goal),
    reference_gen(Frame, Reference),
    reference_parent_gen(Frame, ParentReference),
    format('    ~w -> ~w [label="Exit: ~w"];~n', [Reference, ParentReference, Goal]).


prolog_trace_interception(_Port, _Frame, _PC, continue).

% TODO: Need separator in reference to stop clashes
% TODO: Check Prolog-y nature of names
reference_gen(Frame, Reference) :-
    prolog_frame_attribute(Frame, level, Level),    
    prolog_frame_attribute(Frame, pc, PC),
    atom_concat(Level, PC, Reference).

reference_parent_gen(Frame, Reference) :-
    prolog_frame_attribute(Frame, parent, Parent),
    reference_gen(Parent, Reference).


% Begin example code %
factorial(0, R, R). 

factorial(N, A, R) :-  
    N > 0, 
    A1 is N * A, 
    N1 is N - 1, 
    factorial(N1, A1, R).

test :-
    start_trace('test-out.dot', OldStr),
    factorial(3, 1, _),
    stop_trace(OldStr).

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

