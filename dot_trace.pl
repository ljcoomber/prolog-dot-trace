:- module(dot_trace, [dot_trace_file/2, dot_trace_stream/2, prolog_trace_interception/4]).

% TODO: Support portray_text, and find a way to escape quotes
% TODO: Tests
% TODO: Cuts, redos / alternatives
% TODO: Remove system: module from node labels

dot_trace_file(DstFile, Goal) :-
    setup_call_cleanup(open(DstFile, write, Fd),
                       dot_trace_stream(Fd, Goal),
                       close(Fd)).

% TODO: Turn off notrace if Goal fails
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
                                                [ portray(true),
                                                  quoted(true) ]]),
    track_ungrounded_args(1, Frame),
    generate_parent_node_ref(Frame, ParentReference),
	flag(node_id, N, N + 1),
    format(Stream, '    "~w" -> "~w" [label="~w"];~n',
           [ParentReference, Reference, N]).

prolog_trace_interception(exit, Frame, _Choice, continue):-
    prolog_frame_attribute(Frame, clause, _),
    generate_node_ref(Frame, Reference),
    generate_parent_node_ref(Frame, ParentReference),
	flag(node_id, N, N + 1),
    recorded(trace_stream, Stream),

    % TODO: Support multiple args
    prolog_frame_attribute(Frame, level, Level),    
    dot_dcg:varbound(Level, Idx, Arg),
    prolog_frame_attribute(Frame, argument(Idx), Val),

    format(Stream, '    "~w" -> "~w" [label="~w ~w=~W"];~n',
           [Reference, ParentReference, N, Arg, Val, [portray(true)]]).
    
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

track_ungrounded_args(Idx, Frame) :-
    prolog_frame_attribute(Frame, argument(Idx), _Arg),
    % TODO: Inline predicate
    assert_ungrounded_arg(Idx, Frame), 
    NewIdx is Idx + 1,
    track_ungrounded_args(NewIdx, Frame).

track_ungrounded_args(Idx, Frame) :-
    % No more arguments => stop recursing
    \+prolog_frame_attribute(Frame, argument(Idx), _).

assert_ungrounded_arg(Idx, Frame) :-
    prolog_frame_attribute(Frame, argument(Idx), Arg),
    \+ground(Arg) ->
    prolog_frame_attribute(Frame, level, Level),
    term_to_atom(Arg, Str),
    
    % TODO: Clean-up database assertions before and after    
    asserta(dot_dcg:varbound(Level, Idx, Str)) ; true.
