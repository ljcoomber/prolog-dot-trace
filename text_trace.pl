% Utility for printing detailed text of trace interception

dump_frame_attribute(Frame, Key):-
    (prolog_frame_attribute(Frame, Key, Value) ; Value=no),
    format('  ~w: ~w: ~w~n', [Frame, Key, Value]).

% TODO: remove repetition
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
