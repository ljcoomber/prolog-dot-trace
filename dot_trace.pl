:- module(dot_trace, [start_trace/2, stop_trace/1, prolog_trace_interception/4]).

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

prolog_trace_interception(call, Frame, _Choice, continue):-
    % Do not display calls to stop_trace, but use the call to get a reference to
    % the start node, and rename it
    prolog_frame_attribute(Frame, predicate_indicator, dot_trace:stop_trace/1),
    reference_parent_gen(Frame, ParentReference),
    format('    ~w [label="Start"];~n', [ParentReference]).

prolog_trace_interception(exit, Frame, _Choice, continue) :-
    % Do not display exit from start_trace
    prolog_frame_attribute(Frame, predicate_indicator, dot_trace:start_trace/2).

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
