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
        % Pickup keys (internally only, not shown in output)
        findall((Pos, SortedNewKeys, Path),
            (key(Pos, Color),
             \+ member(Color, Keys),
             append(Keys, [Color], NewKeys),
             sort(NewKeys, SortedNewKeys)),
            PickupList),

        % Move through only unlocked doors
        findall((Next, SortedKeys, [move(Pos, Next)|Path]),
            (door(Pos, Next)),
            MoveList1),

        findall((Next, SortedKeys, [move(Pos, Next)|Path]),
            (door(Next, Pos)),
            MoveList2),

        % Locked doors must be unlocked and moved together (combined step only if have key)
        findall((Next, SortedKeys, [unlock(Color), move(Pos, Next)|Path]),
            (locked_door(Pos, Next, Color),
             member(Color, SortedKeys)),
            UnlockMoveList1),

        findall((Next, SortedKeys, [unlock(Color), move(Pos, Next)|Path]),
            (locked_door(Next, Pos, Color),
             member(Color, SortedKeys)),
            UnlockMoveList2),

        append(PickupList, UnlockMoveList1, R1),
        append(R1, UnlockMoveList2, R2),
        append(R2, MoveList1, R3),
        append(R3, MoveList2, NextStates),
        append(Rest, NextStates, NewQueue),

        bfs(NewQueue, [State|Visited], Goal, FinalPath)
    )).
