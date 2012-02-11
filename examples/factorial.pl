:- use_module('dot_trace.pl').
    
factorial(0, R, R). 

factorial(N, A, R) :-  
    N > 0, 
    A1 is N * A, 
    N1 is N - 1, 
    factorial(N1, A1, R).

generate :-
    dot_trace:dot_trace_file('examples/factorial.dot', factorial(3, 1, _)).