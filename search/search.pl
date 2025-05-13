:- dynamic door/2.
:- dynamic locked_door/3.
:- dynamic key/2.
:- dynamic treasure/1.
:- dynamic initial/1.

search(Actions) :-
    initial(Start),
    treasure(Goal),
    bfs([(Start, [], [])], [], Goal, RevActions),
    reverse(RevActions, Actions).

bfs([(Pos, _Keys, Path)|_], _, Pos, Path) :- !.

bfs([(Pos, Keys, Path)|Rest], Visited, Goal, FinalPath) :-
    sort(Keys, SortedKeys),
    State = (Pos, SortedKeys),

    (member(State, Visited) -> bfs(Rest, Visited, Goal, FinalPath) ;
    (
        % Pickup keys if any
        findall((Pos, SortedNewKeys, Path),
            (   key(Pos, Color),
                \+ member(Color, Keys),
                append(Keys, [Color], NewKeys),
                sort(NewKeys, SortedNewKeys)
            ), PickupList),

        % Unlock and move in one combined step for locked doors
        findall((Next, SortedKeys, [unlock(Color), move(Pos, Next)|Path]),
            (   (locked_door(Pos, Next, Color); locked_door(Next, Pos, Color)),
                member(Color, SortedKeys)
            ), UnlockMoveList),

        % Move through only unlocked doors
        findall((Next, SortedKeys, [move(Pos, Next)|Path]),
            (   (door(Pos, Next); door(Next, Pos)),
                \+ is_locked(Pos, Next)
            ), MoveList),

        append(PickupList, UnlockMoveList, R1),
        append(R1, MoveList, NextStates),
        append(Rest, NextStates, NewQueue),

        bfs(NewQueue, [State|Visited], Goal, FinalPath)
    )).

is_locked(A, B) :- locked_door(A, B, _).
is_locked(A, B) :- locked_door(B, A, _).
