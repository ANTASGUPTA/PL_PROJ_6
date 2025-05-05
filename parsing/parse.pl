% Entry point
parse(Input) :- lines(Input, []).

% Lines → Line ; Lines | Line
lines(Input, Rest) :-
    line(Input, Temp),
    ( Temp = [';'|Next] -> lines(Next, Rest)
    ; Rest = Temp ).

% Line → Num , Line | Num
line(Input, Rest) :-
    num(Input, Temp),
    ( Temp = [','|Next] -> line(Next, Rest)
    ; Rest = Temp ).

% Num → Digit | Digit Num
num([H|T], Rest) :-
    digit(H),
    num_tail(T, Rest).

num_tail([H|T], Rest) :-
    digit(H), !,
    num_tail(T, Rest).
num_tail(Rest, Rest).

% Digit → 0..9
digit(D) :- member(D, ['0','1','2','3','4','5','6','7','8','9']).


% Example execution:
% ?- parse(['3', '2', ',', '0', ';', '1', ',', '5', '6', '7', ';', '2']).
% true.
% ?- parse(['3', '2', ',', '0', ';', '1', ',', '5', '6', '7', ';', '2', ',']).
% false.
% ?- parse(['3', '2', ',', ';', '0']).
% false.
