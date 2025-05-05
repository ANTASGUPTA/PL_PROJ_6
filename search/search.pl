:- dynamic door/2.
:- dynamic locked_door/3.
:- dynamic key/2.
:- dynamic treasure/1.
:- dynamic initial/1.

% Entry point: search(-Actions)
search(Actions) :-
    initial(Start),
    treasure(Goal),
    bfs([(Start, [], [])], [], Goal, RevActions),
    reverse(RevActions, Actions).

% BFS loop
bfs([(Pos, Keys, Actions)|_], _, Pos, Actions) :- !.

bfs([(Pos, Keys, Actions)|Rest], Visited, Goal, FinalActions) :-
    findall((Next, Keys, [move(Pos, Next)|Actions]),
        (   (door(Pos, Next); door(Next, Pos)),
            \+ locked(Pos, Next, _),
            \+ member((Next, Keys), Visited)
        ), Moves),

    findall((Next, Keys, [move(Pos, Next)|Actions]),
        (   (locked(Pos, Next, Color)),
            member(Color, Keys),
            \+ member((Next, Keys), Visited)
        ), UnlockMoves),

    findall((Pos, [Color|Keys], [pickup(Pos, Color)|Actions]),
        (   key(Pos, Color),
            \+ member(Color, Keys)
        ), Pickups),

    append(Rest, Moves, R1),
    append(R1, UnlockMoves, R2),
    append(R2, Pickups, NewQueue),

    bfs(NewQueue, [(Pos, Keys)|Visited], Goal, FinalActions).

% Helper to check for locked doors in either direction
locked(A, B, C) :- locked_door(A, B, C).
locked(A, B, C) :- locked_door(B, A, C).
