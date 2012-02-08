:- module(dot_trace, [start_trace/2, stop_trace/1, prolog_trace_interception/4]).

% TODO: Tests
% TODO: Cuts, redos / alternatives
% TODO: Put start node at top
% TODO: Trace with explicit call, use $visible as per tests to determine setting
% TODO: Use named stream
% TODO: Remove system: module from node labels
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
    generate_parent_node_ref(Frame, ParentReference),
    format('    "~w" [label="Start"];~n', [ParentReference]).

prolog_trace_interception(call, Frame, _Choice, continue):-
    generate_node_ref(Frame, Reference),
    prolog_frame_attribute(Frame, goal, Goal),
    format('    "~w" [label="~W"];~n', [Reference, Goal,
                                      [ quoted(true),
                                        numbervars(true),
                                        portray(true) ]]),
    
    generate_parent_node_ref(Frame, ParentReference),
	flag(node_id, N, N + 1),
    format('    "~w" -> "~w" [label="~w"];~n', [ParentReference, Reference, N]).

prolog_trace_interception(exit, Frame, _Choice, continue) :-
    % Do not display exit from start_trace
    prolog_frame_attribute(Frame, predicate_indicator, dot_trace:start_trace/2).

prolog_trace_interception(exit, Frame, _Choice, continue):-
    prolog_frame_attribute(Frame, clause, _),
    prolog_frame_attribute(Frame, goal, Goal),
    generate_node_ref(Frame, Reference),
    generate_parent_node_ref(Frame, ParentReference),
	flag(node_id, N, N + 1),        
    format('    "~w" -> "~w" [label="~w  Exit: ~w"];~n', [Reference, ParentReference, N, Goal]).

prolog_trace_interception(fail, Frame, _Choice, continue):-
    format('// fail Frame: ~w~n', Frame).

prolog_trace_interception(redo, Frame, _Choice, continue):-
    format('// redo Frame: ~w~n', Frame).

prolog_trace_interception(_Port, _Frame, _PC, continue).


generate_node_ref(Frame, Reference) :-
    % I could not tell from a quick look of the SWI-Prolog source code what a
    % good reference would be, so using a combination of frame and PC as a starting
    % point
    prolog_frame_attribute(Frame, pc, PC),
    atomic_list_concat([f, Frame, p, PC], Reference).

generate_parent_node_ref(Frame, Reference) :-
    prolog_frame_attribute(Frame, parent, Parent),
    generate_node_ref(Parent, Reference).
