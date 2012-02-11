:- module(dot_trace, [dot_trace_file/2, dot_trace_stream/2, prolog_trace_interception/4]).

% TODO: Tests
% TODO: Cuts, redos / alternatives
% TODO: Put start node at top
% TODO: Use named stream
% TODO: Remove system: module from node labels

dot_trace_file(DstFile, Goal) :-
    setup_call_cleanup(open(DstFile, write, Fd),
                       dot_trace_stream(Fd, Goal),
                       close(Fd)).

dot_trace_stream(Stream, Goal) :-
    recorda(trace_stream, Stream, Ref),
    print(Stream, 'digraph prologTrace {'), nl(Stream),

    prolog_current_frame(Frame),
    generate_node_ref(Frame, NodeRef),
    format(Stream, '    "~w" [label="Start"];~n', [NodeRef]),

    trace,
    call(Goal),
    notrace,
    print(Stream, '}'), nl(Stream),
    erase(Ref).

prolog_trace_interception(call, Frame, _Choice, continue):-
    generate_node_ref(Frame, Reference),
    prolog_frame_attribute(Frame, goal, Goal),
    recorded(trace_stream, Stream),
    format(Stream, '    "~w" [label="~W"];~n', [Reference, Goal,
                                                [ quoted(true),
                                                  numbervars(true),
                                                  portray(true) ]]),
    
    generate_parent_node_ref(Frame, ParentReference),
	flag(node_id, N, N + 1),
    format(Stream, '    "~w" -> "~w" [label="~w"];~n',
           [ParentReference, Reference, N]).

prolog_trace_interception(exit, Frame, _Choice, continue):-
    prolog_frame_attribute(Frame, clause, _),
    prolog_frame_attribute(Frame, goal, Goal),
    generate_node_ref(Frame, Reference),
    generate_parent_node_ref(Frame, ParentReference),
	flag(node_id, N, N + 1),
    recorded(trace_stream, Stream),
    format(Stream, '    "~w" -> "~w" [label="~w  Exit: ~w"];~n',
           [Reference, ParentReference, N, Goal]).

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
