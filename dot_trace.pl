:- module(dot_trace, [dot_trace_file/2, dot_trace_stream/2, prolog_trace_interception/4]).

% TODO: Support portray_text, and find a way to escape quotes
% TODO: Cuts, redos / alternatives
% TODO: Remove system: module from node labels
dot_trace_file(DstFile, Goal) :-
    setup_call_cleanup(open(DstFile, write, Fd),
                       dot_trace_stream(Fd, Goal),
                       close(Fd)).

% TODO: Turn off notrace if Goal fails
dot_trace_stream(Stream, Goal) :-
    % Dummy predicate to stop later lookups failing when there are no unbound
    % vars in the program execution
    asserta(dot_trace:varbound(a, a, a)),
    
    recorda(trace_stream, Stream, Ref),
    
    print(Stream, 'digraph prologTrace {'), nl(Stream),

    prolog_current_frame(Frame),
    generate_node_ref(Frame, NodeRef),
    format(Stream, '    "~w" [label="Start"];~n', [NodeRef]),

    trace,
    call(Goal),
    notrace,
    print(Stream, '}'), nl(Stream),

    % Clean-up
    erase(Ref),
    flag(node_id, _, 1).

/*
prolog_trace_interception(Port, Frame, Choice, continue):-
    % Used for debugging
    format('Trace: Port: ~w Frame: ~w, PC: ~w~n', [Port, Frame, PC]),
    prolog_frame_attribute(Frame, goal, Goal),
    format('    Goal: ~w~n', [Goal]),
    prolog_choice_attribute(Choice, parent, Parent),
    prolog_choice_attribute(Choice, frame, Frame),
    prolog_choice_attribute(Choice, type, Type),
    format('    Choice: ~w, ~w, ~w~n', [Parent, Frame, Type]).
*/

prolog_trace_interception(Port, Frame, Choice, continue):-
    step(Port, Frame, Choice).

step(Port, Frame, Choice):-
	flag(node_id, N, N + 1),    
    prolog_frame_attribute(Frame, goal, Goal),
    recorded(trace_stream, Stream),
    track_ungrounded_args(1, Frame),    
    step(Port, Frame, Choice, N, Goal, Stream).

step(call, Frame, _Choice, N, Goal, Stream):-    
    generate_node_ref(Frame, Reference),
    generate_parent_node_ref(Frame, ParentReference),
    
    format(Stream, '    "~w" [label="~W"];~n', [Reference, Goal,
                                                [ portray(true),
                                                  quoted(true) ]]),
    format(Stream, '    "~w" -> "~w" [label="~w"];~n',
           [ParentReference, Reference, N]).

step(redo, Frame, _Choice, N, Goal, Stream):-    
    generate_node_ref(Frame, Reference),
    generate_parent_node_ref(Frame, ParentReference),
    
    format(Stream, '    "~w" [label="~W"];~n', [Reference, Goal,
                                                [ portray(true),
                                                  quoted(true) ]]),
    format(Stream, '    "~w" -> "~w" [label="~w",style="dashed"];~n',
           [ParentReference, Reference, N]).

step(exit, Frame, Choice, N, _Goal, Stream):-
    generate_node_ref(Frame, Reference),
    generate_parent_node_ref(Frame, ParentReference),

    (  prolog_choice_attribute(Choice, frame, Frame),
       prolog_choice_attribute(Choice, type, Type),
       \+Type = catch
    -> generate_node_ref(Frame, ChoiceFrameRef),

       prolog_choice_attribute(Choice, parent, ChoiceParent),
       prolog_choice_attribute(ChoiceParent, frame, ChoiceParentFrame),
       
       generate_node_ref(ChoiceParentFrame, ChoiceParentRef),

       prolog_frame_attribute(Frame, alternative, AltFrame),
       generate_node_ref(AltFrame, AltFrameRef),
       prolog_frame_attribute(Frame, goal, AltGoal),

       format(Stream, '    "~w" [label="~w", color="blue"];~n',
              [AltFrameRef, AltGoal]),
       
       format(Stream,
              '    "~w" -> "~w" [label="~w (~w)",style="dashed",color="blue"];~n',
              [ChoiceFrameRef, AltFrameRef, Type, N])
    ; true
    ),

    % TODO: Support multiple args
    prolog_frame_attribute(Frame, level, Level),    

    (  dot_trace:varbound(Level, Idx, Arg)
    -> prolog_frame_attribute(Frame, argument(Idx), Val),
       format(Stream, '    "~w" -> "~w" [label="~w ~w=~W"];~n',
              [Reference, ParentReference, N, Arg, Val, [portray(true)]]),
       retract(dot_trace:varbound(Level, Idx, Arg))
    ;  format(Stream, '    "~w" -> "~w" [label="~w"];~n',
              [Reference, ParentReference, N])
    ).

step(fail, Frame, _Choice, N, _Goal, Stream):-
    generate_node_ref(Frame, Reference),
    generate_parent_node_ref(Frame, ParentReference),
    format(Stream, '    "~w" -> "~w" [label="~w",color="red"];~n',
           [Reference, ParentReference, N]).

step(Port, Frame, _Choice, N, Goal, _Stream):-
    format('// XXX Catch-all  ~w Frame: ~w: N: ~w Goal: ~w~n', [Port, Frame, N, Goal]).

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
    (  \+ground(Arg)
    -> prolog_frame_attribute(Frame, level, Level),
       term_to_atom(Arg, Str),
       % TODO: Clean-up database assertions before and after    
       asserta(dot_trace:varbound(Level, Idx, Str))
    ;  true
    ).
