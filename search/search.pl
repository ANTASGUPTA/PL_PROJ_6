:- dynamic door/2.
:- dynamic locked_door/3.
:- dynamic key/2.
:- dynamic treasure/1.
:- dynamic initial/1.

% Entry point: returns complete sequence (move + pickup)
search(Actions) :-
    initial(Start),
    treasure(Goal),
    bfs([(Start, [], [])], [], Goal, RevActions),
    reverse(RevActions, Actions).

% Goal reached
bfs([(Pos, _Keys, Path)|_], _, Pos, Path) :- !.

% BFS main loop
bfs([(Pos, Keys, Path)|Rest], Visited, Goal, FinalPath) :-
    sort(Keys, SortedKeys),
    State = (Pos, SortedKeys),

    % Skip if already visited in this key state
    member(State, Visited), !,
    bfs(Rest, Visited, Goal, FinalPath).

bfs([(Pos, Keys, Path)|Rest], Visited, Goal, FinalPath) :-
    sort(Keys, SortedKeys),

    % Get next moves through open doors
    findall((Next, SortedKeys, [move(Pos, Next)|Path]),
        (   (door(Pos, Next); door(Next, Pos)),
            \+ is_locked(Pos, Next)
        ), MoveList),

    % Get moves through locked doors if key exists
    findall((Next, SortedKeys, [move(Pos, Next)|Path]),
        (   (locked_door(Pos, Next, Color); locked_door(Next, Pos, Color)),
            member(Color, SortedKeys)
        ), UnlockList),

    % Pickup key if available and not already held
    findall((Pos, SortedNewKeys, [pickup(Pos, Color)|Path]),
        (   key(Pos, Color),
            \+ member(Color, Keys),
            append(Keys, [Color], NewKeys),
            sort(NewKeys, SortedNewKeys)
        ), PickupList),

    append(PickupList, MoveList, L1),
    append(L1, UnlockList, NextStates),
    append(Rest, NextStates, NewQueue),

    bfs(NewQueue, [State|Visited], Goal, FinalPath).

% Check if locked door
is_locked(A, B) :- locked_door(A, B, _).
is_locked(A, B) :- locked_door(B, A, _).
