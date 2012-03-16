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
    retractall(dot_trace:varbound/3),
    asserta(dot_trace:varbound(a, a, a, a)),
    
    recorda(trace_stream, Stream, Ref),
    
    print(Stream, 'digraph prologTrace {'), nl(Stream),
    prolog_current_frame(Frame),

    generate_refs(Frame, FrameRef, _),
    format(Stream, '    "~w" [label="Start"];~n', [FrameRef]),

    trace,
    %visible(+all), visible(+cut),
    call(Goal),
    notrace,
    
    format(Stream, '}~n', []),

    % Clean-up
    erase(Ref),
    flag(node_id, _, 1).

prolog_trace_interception(Port, Frame, Choice, continue):-
    step(Port, Frame, Choice).

step(Port, Frame, Choice):-
	flag(node_id, N, N + 1),    
    prolog_frame_attribute(Frame, goal, Goal),
    recorded(trace_stream, Stream),
    track_ungrounded_args(1, Frame),

    generate_refs(Frame, FrameRef, _),
    prolog_frame_attribute(Frame, predicate_indicator, Pred),
    
    format(Stream, '    "~w" [shape="box",label="~w"];~n',
           [FrameRef, Pred]),

    step(Port, Frame, Choice, N, Goal, Stream).

step(call, Frame, _Choice, N, Goal, Stream):-
    generate_refs(Frame, FrameRef, ParentFrameRef),    
    format(Stream, '    "~w" -> "~w" [label="~w: ~W"];~n',
           [ParentFrameRef, FrameRef, N, Goal, [portray(true), quoted(true)]]).

step(redo, Frame, _Choice, N, Goal, Stream):-
    generate_refs(Frame, FrameRef, ParentFrameRef),    
    format(Stream, '    "~w" -> "~w" [label="~w: ~W", style="dashed"];~n',
           [ParentFrameRef, FrameRef, N, Goal, [portray(true), quoted(true)]]).

step(exit, Frame, _Choice, N, _Goal, Stream):-
    generate_refs(Frame, FrameRef, ParentFrameRef),    

    % Assemble args as values that have been bound
    findall(ArgVal,
            (dot_trace:varbound(Frame, Idx, Arg),
             ground(Arg),
             prolog_frame_attribute(Frame, argument(Idx), Val),
             \+Val = [],
             format(atom(ArgVal), '~w=~W ', [Arg, Val, [portray(true), quoted(true)]])),
            ArgVals),

    concat_atom(ArgVals, ArgValStr),
    
    format(Stream, '    "~w" -> "~w" [label="~w ~w"];~n',
           [FrameRef, ParentFrameRef, N, ArgValStr]),
    retractall(dot_trace:varbound(Frame, _, _)),

    % Choice points, disabled for now as don't add much value to diagram
    /*
    (  prolog_choice_attribute(Choice, frame, Frame),
       prolog_choice_attribute(Choice, type, Type),
       \+Type = catch
    -> atomic_list_concat([c, Choice], ChoiceRef),
       format(Stream, '    "~w" [label="~w",color="blue"];~n', [ChoiceRef, Type]),
       format(Stream, '    "~w" -> "~w" [color="blue"];~n', [ChoiceRef, FrameRef])
    ;  true
    ),
    */

    (  prolog_frame_attribute(Frame, alternative, AltFrame),

       % TODO: Work out why an alt frame is left to test library when
       % choicepoints should have been erased
       prolog_frame_attribute(AltFrame, context_module, AltModule),
       AltModule \= plunit
    -> atomic_list_concat([f, AltFrame], AltFrameRef),
       prolog_frame_attribute(AltFrame, goal, AltGoal),
       
       format(Stream, '    "~w" -> "~w" [label="(~w) ~W",shape="box",style="dashed",color="green"];~n',
              [FrameRef, AltFrameRef, N, AltGoal, [portray(true), quoted(true)]])
    ;  true
    ).
    
step(fail, Frame, _Choice, N, _Goal, Stream):-
    generate_refs(Frame, FrameRef, ParentFrameRef),    
    format(Stream, '    "~w" -> "~w" [label="~w", color="red"];~n',
           [FrameRef, ParentFrameRef, N]),

    % TODO: Is this useful?
    (  prolog_frame_attribute(Frame, alternative, AltFrame)
    -> atomic_list_concat([f, AltFrame], AltFrameRef),
       format(Stream, '    "~w" -> "~w" [color="blue"];~n', [FrameRef, AltFrameRef])
    ;  true
    ).
    
step(Port, Frame, _Choice, N, Goal, _Stream):-
    format('*** Missed: ~w / ~w / ~w / ~w~n', [Port, Frame, N, Goal]),
    fail.
    
generate_refs(Frame, FrameRef, ParentFrameRef) :-
    atomic_list_concat([f, Frame], FrameRef),
    prolog_frame_attribute(Frame, parent, ParentFrame),    
    atomic_list_concat([f, ParentFrame], ParentFrameRef).

track_ungrounded_args(Idx, Frame) :-
    prolog_frame_attribute(Frame, argument(Idx), _Arg),
    assert_ungrounded_arg(Idx, Frame), 
    NewIdx is Idx + 1,
    track_ungrounded_args(NewIdx, Frame).

track_ungrounded_args(Idx, Frame) :-
    % No more arguments => stop recursing
    \+prolog_frame_attribute(Frame, argument(Idx), _).

assert_ungrounded_arg(Idx, Frame) :-
    prolog_frame_attribute(Frame, argument(Idx), Arg),
    (  \+ground(Arg)
    -> term_to_atom(Arg, Str),
       asserta(dot_trace:varbound(Frame, Idx, Str))
    ;  true
    ).