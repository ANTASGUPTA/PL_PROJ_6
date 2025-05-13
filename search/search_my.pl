:- dynamic door/2.
:- dynamic locked_door/3.
:- dynamic key/2.
:- dynamic treasure/1.
:- dynamic initial/1.

% Entry point: returns only move(...) steps
search(MovesOnly) :-
    initial(Start),
    treasure(Goal),
    bfs([(Start, [], [])], [], Goal, RevActions),
    reverse(RevActions, Actions),
    include(is_move, Actions, MovesOnly).

% Helper: keep only move(...) actions
is_move(move(_, _)).

% Goal reached
bfs([(Pos, _Keys, Path)|_], _, Pos, Path) :- !.

bfs([(Pos, Keys, Path)|Rest], Visited, Goal, FinalPath) :-
    sort(Keys, SortedKeys),  % ensure consistent key order

    % Moves through unlocked doors
    findall((Next, SortedKeys, [move(Pos, Next)|Path]),
        (   (door(Pos, Next); door(Next, Pos)),
            \+ is_locked(Pos, Next),
            \+ member((Next, SortedKeys), Visited)
        ), MoveList),

    % Moves through locked doors if key is present
    findall((Next, SortedKeys, [move(Pos, Next)|Path]),
        (   (locked_door(Pos, Next, Color); locked_door(Next, Pos, Color)),
            member(Color, SortedKeys),
            \+ member((Next, SortedKeys), Visited)
        ), UnlockList),

    % Pick up keys (but these actions will be filtered later)
    findall((Pos, SortedNewKeys, [pickup(Pos, Color)|Path]),
        (   key(Pos, Color),
            \+ member(Color, Keys),
            append([Color], Keys, NewKeys),
            sort(NewKeys, SortedNewKeys)
        ), PickupList),

    append(Rest, MoveList, R1),
    append(R1, UnlockList, R2),
    append(R2, PickupList, NewQueue),

    bfs(NewQueue, [(Pos, SortedKeys)|Visited], Goal, FinalPath).

% Check if a door is locked in either direction
is_locked(A, B) :- locked_door(A, B, _).
is_locked(A, B) :- locked_door(B, A, _).
