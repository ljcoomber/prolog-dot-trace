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

    generate_node_ref(Frame, 0, NodeRef),
    format(Stream, '    "~w" [label="Start"];~n', [NodeRef]),

    trace,
    %visible(+all), visible(+cut),
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

    atomic_list_concat([f, Frame], FrameRef),
    atomic_list_concat([f, Frame, p, Port, c, Choice], PortRef),

    prolog_frame_attribute(Frame, parent, ParentFrame),    

    atomic_list_concat([f, ParentFrame], ParentFrameRef),

    prolog_frame_attribute(Frame, predicate_indicator, Pred),
    
    format(Stream, '    "~w" [shape="box",label="~w"];~n',
           [FrameRef, Pred]),

    step(Port, Frame, Choice, N, Goal, Stream).

step(call, Frame, Choice, N, Goal, Stream):-
    atomic_list_concat([f, Frame], FrameRef),
    atomic_list_concat([f, Frame, p, call, c, Choice], PortRef),

    prolog_frame_attribute(Frame, parent, ParentFrame),    

    atomic_list_concat([f, ParentFrame], ParentFrameRef),
    
    format(Stream, '    "~w" -> "~w" [label="~w: ~W"];~n',
           [ParentFrameRef, FrameRef, N, Goal, [portray(true), quoted(true)]]).

step(redo, Frame, Choice, N, Goal, Stream):-
    atomic_list_concat([f, Frame], FrameRef),
    atomic_list_concat([f, Frame, p, redo, c, Choice], PortRef),

    prolog_frame_attribute(Frame, parent, ParentFrame),    

    atomic_list_concat([f, ParentFrame], ParentFrameRef),
    
    format(Stream, '    "~w" -> "~w" [label="~w: ~W", style="dashed"];~n',
           [ParentFrameRef, FrameRef, N, Goal, [portray(true), quoted(true)]]).

step(exit, Frame, Choice, N, Goal, Stream):-
    atomic_list_concat([f, Frame], FrameRef),

    prolog_frame_attribute(Frame, parent, ParentFrame),    
    atomic_list_concat([f, ParentFrame], ParentFrameRef),

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
    
    %generate_node_ref(Frame, Choice, Reference),
    %generate_parent_node_ref(Frame, Choice, ParentReference),

    (  prolog_choice_attribute(Choice, frame, Frame),
       prolog_choice_attribute(Choice, type, Type),
       \+Type = catch
    -> %generate_node_ref(Frame, Choice, ChoiceFrameRef),

       /*
       prolog_choice_attribute(Choice, parent, ChoiceParent),
       prolog_choice_attribute(ChoiceParent, frame, ChoiceParentFrame),
       
       generate_node_ref(ChoiceParentFrame, Choice, ChoiceParentRef),

       prolog_frame_attribute(Frame, alternative, AltFrame),
       generate_node_ref(AltFrame, Choice, AltFrameRef),
       prolog_frame_attribute(Frame, goal, AltGoal),

       format(Stream, '    "~w" [label="~w", color="blue"];~n',
              [AltFrameRef, AltGoal]),
       
       format(Stream,
              '    "~w" -> "~w" [label="~w (~w)",style="dashed",color="blue"];~n',
              [ChoiceFrameRef, AltFrameRef, Type, N])
         */
       true
    ; true
    ).


    %format(Stream, '    "~w" -> "~w" [label="~w: ~w"];~n',
    %       [FrameRef, ParentFrameRef, N, Goal]).

step(fail, Frame, Choice, N, _Goal, Stream):-
    atomic_list_concat([f, Frame], FrameRef),
    atomic_list_concat([f, Frame, p, exit, c, Choice], PortRef),

    prolog_frame_attribute(Frame, parent, ParentFrame),    

    atomic_list_concat([f, ParentFrame], ParentFrameRef),

    format(Stream, '    "~w" -> "~w" [label="~w", color="red"];~n',
           [FrameRef, ParentFrameRef, N]).
    
step(Port, Frame, Choice, N, Goal, _Stream):-
    format('*** Missed: ~w / ~w / ~w / ~w~n', [Port, Frame, N, Goal]).
    
generate_node_ref(Frame, Choice, Reference) :-
    % I could not tell from a quick look of the SWI-Prolog source code what a
    % good reference would be, so using a combination of frame and PC as a starting
    % point

    % TODO: Remove, update comments
    %prolog_frame_attribute(Frame, pc, PC),
    atomic_list_concat([f, Frame], Reference).

generate_parent_node_ref(Frame, Choice, Reference) :-
    prolog_frame_attribute(Frame, parent, Parent),
    generate_node_ref(Parent, Choice, Reference).

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
       %format('asserting varbound(~w, ~w, ~w)~n', [Frame, Idx, Str]),
       asserta(dot_trace:varbound(Frame, Idx, Str))
    ;  true
    ).