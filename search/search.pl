:- dynamic door/2.
:- dynamic locked_door/3.
:- dynamic key/2.
:- dynamic treasure/1.
:- dynamic initial/1.

search(Actions) :-
    initial(Start),
    collect_keys(Start, [], CollectedKeys),
    bfs_queue([state(Start, CollectedKeys, [])], [], Actions).

% When goal is reached
bfs_queue([state(Room, _, Path)|_], _, Path) :-
    treasure(Room), !.

% Skip visited states
bfs_queue([state(Room, Keys, _)|Rest], Visited, Actions) :-
    member(visited(Room, Keys), Visited), !,
    bfs_queue(Rest, Visited, Actions).

% BFS explore next states
bfs_queue([state(Room, Keys, Path)|Rest], Visited, Actions) :-
    findall(
        state(NextRoom, NextKeys, NewPath),
        (
            expand_state(Room, Keys, NextRoom, NextKeys, ActionSequence),
            append(Path, ActionSequence, NewPath)
        ),
        NextStates
    ),
    append(Rest, NextStates, NewQueue),
    bfs_queue(NewQueue, [visited(Room, Keys)|Visited], Actions).

% Handling next steps when door is open
expand_state(Room, Keys, Next, KeysAfter, [move(Room, Next)]) :-
    collect_keys(Room, Keys, KeysHere),
    (door(Room, Next); door(Next, Room)),
    KeysAfter = KeysHere.

% Handling next steps when door is locked, requires unlock
expand_state(Room, Keys, Next, KeysAfter, [unlock(Color), move(Room, Next)]) :-
    collect_keys(Room, Keys, KeysHere),
    (locked_door(Room, Next, Color); locked_door(Next, Room, Color)),
    member(Color, KeysHere),
    KeysAfter = KeysHere.

% Collect keys if present (internally only, not shown in output)
collect_keys(Room, OldKeys, UpdatedKeys) :-
    findall(Color, (key(Room, Color), \+ member(Color, OldKeys)), NewColors),
    append(NewColors, OldKeys, Combined),
    sort(Combined, UpdatedKeys).
