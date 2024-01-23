:- initialization main, halt.
:- dynamic black/2.
:- dynamic white/2.
:- dynamic step1/2.
:- dynamic step2/2.

isEmpty(X, Y) :-
    \+white(X, Y),
    \+black(X, Y).

isValid(X, Y) :-
    X > 0, X < 16,
    Y > 0, Y < 16.

%	horizontal
relative(X1, Y1, X2, Y2) :-
    between(-4, 4, D),
    D \= 0,
    X2 is X1 + D,
    Y2 is Y1,
    isValid(X2, Y2).
%	vertical
relative(X1, Y1, X2, Y2) :-
    between(-4, 4, D),
    D \= 0,
    X2 is X1,
    Y2 is Y1 + D,
    isValid(X2, Y2).
%	r = + (rtop ldown)
relative(X1, Y1, X2, Y2) :-
    between(-4, 4, D),
    D \= 0,
    X2 is X1 + D,
    Y2 is Y1 + D,
    isValid(X2, Y2).
%	r = - (ltop rdown)
relative(X1, Y1, X2, Y2) :-
    between(-4, 4, D),
    D \= 0,
    X2 is X1 + D,
    Y2 is Y1 - D,
    isValid(X2, Y2).
relative(0, 0, _, _).

%	horizontal: 1
type(1, X1, _, X2, _) :-
	DX is X2 - X1, DX = 0.
%	vertical: 2
type(2, _, Y1, _, Y2) :-
    DY is Y2 - Y1, DY = 0.
%	r = +: 3 
type(3, X1, Y1, X2, Y2) :-
    DX is X2 - X1, DY is Y2 - Y1, DX is DY.
% 	r = -: 4
type(4, X1, Y1, X2, Y2) :-
    DX is X2 - X1, DY is Y2 - Y1, DX is -DY.

valid(Player, X, Y) :-
	member(Player, [black, white]),
	isEmpty(X, Y),
	isValid(X, Y).

read_predicates([H|T]) :-
    read_line_to_codes(user_input, H), 
    H \= end_of_file,
    string_codes(Input, H),
    term_string(Term, Input),
    assert(Term), % 將 Term assert 進 KB
    read_predicates(T).
read_predicates([]).

side(black) :-
    aggregate_all(count, black(_, _), Bn),
    aggregate_all(count, white(_, _), Wn),
    Bn = Wn.
side(white) :-
	\+side(black).

opponent(black, white).
opponent(white, black).

fourtowin(Player, L, A) :- % L = 4 to win list, A = answer list
    call(Player, X1, Y1),

    relative(X1, Y1, X2, Y2),
    (	X2 > X1
	; 	(X1 = X2, Y2 > Y1)),
    call(Player, X2, Y2),
    type(N, X1, Y1, X2, Y2),

    relative(X1, Y1, X3, Y3),
    (	X3 > X2 
	; 	(X2 = X3, Y3 > Y2)),
    call(Player, X3, Y3),
    type(N, X1, Y1, X3, Y3),

    relative(X1, Y1, X4, Y4),
    (	X4 > X3 
	;	(X3 = X4, Y4 > Y3)),
    call(Player, X4, Y4),
    type(N, X1, Y1, X4, Y4),

    relative(X1, Y1, AX1, AY1),
    relative(X4, Y4, AX1, AY1),
    type(N, X1, Y1, AX1, AY1),
    valid(Player, AX1, AY1),

    list_to_set([[X1,Y1],[X2,Y2],[X3,Y3],[X4,Y4]], L), % L = 4 to win list
    list_to_set([[AX1,AY1]], A). % A = answer list

writeMove(Player, X, Y) :-
    write(Player), write("("), write(X), write(", "), write(Y), write(")\n").
addMove(Player, X, Y) :-
    atomic_list_concat([Player, "(", X, ",", Y, ")"], Fact),
    term_to_atom(Term, Fact),
    assert(Term).
undoMove(Player, X, Y) :-
    atomic_list_concat([Player, "(", X, ",", Y, ")"], Fact),
    term_to_atom(Term, Fact),
    retract(Term).

three(Player, L, A, RX, RY) :-
    (	(RX = X1, RY = Y1)
	;	relative(RX, RY, X1, Y1)), 
    call(Player, X1, Y1),

    (	(RX = X2, RY = Y2) 
	;	relative(RX, RY, X2, Y2)), 
    relative(X1, Y1, X2, Y2),
    (	X2 > X1
	; 	(X1 = X2, Y2 > Y1)),
    call(Player, X2, Y2),
    type(N, X1, Y1, X2, Y2),

    (	(RX = X3, RY = Y3)
	;	relative(RX, RY, X3, Y3)), 
    relative(X1, Y1, X3, Y3),
    (	X3 > X2
	;	(X2 = X3, Y3 > Y2)),
    call(Player, X3, Y3),
    type(N, X1, Y1, X3, Y3),

    relative(X1, Y1, AX1, AY1),
    relative(X3, Y3, AX1, AY1),
    type(N, X1, Y1, AX1, AY1),
    valid(Player, AX1, AY1),

    relative(AX1, AY1, AX2, AY2),
    relative(X1, Y1, AX2, AY2),
    relative(X3, Y3, AX2, AY2),
    (	AX2 > AX1
	;	(AX1 = AX2, AY2 > AY1)),
    type(N, X1, Y1, AX2, AY2),
    valid(Player, AX2, AY2),

    list_to_set([[X1,Y1], [X2,Y2], [X3,Y3]], L),
    list_to_set([[AX1,AY1], [AX2,AY2]], A).

winSecure(Player) :-
	(fourtowin(Player, _, A), member([X, Y], A), opponent(Player, Other), \+valid(Other, X, Y)).

main :-
    read_predicates(_),
    side(Player),
    opponent(Player, Opponent),
%	if player got 4, next step win
	(
        fourtowin(Player, L, A), member([X, Y], A)
    ->  writeMove(Player, X, Y), halt
    ;	true
	),
    forall(fourtowin(Opponent, _, A), (member([X, Y], A), addMove(step1, X, Y))),
    forall((call(Opponent, X, Y), three(Opponent, _, A, X, Y), member([TX, TY], A)),
(	step1(TX, TY)
->	true
;	addMove(step1, TX, TY)
)),
	forall((call(Player, X, Y), three(Player, _, A, X, Y), member([TX, TY], A)),
(	step1(TX, TY)
->	true
;	addMove(step1, TX, TY)
)),
    step1(X, Y), 
    (
        addMove(Player, X, Y),
        (
            fourtowin(Opponent, _, _) 
        ->  (undoMove(Player, X, Y))
        ;   (	(three(Opponent, _, _, 0, 0), \+fourtowin(Player, _, _))
            ->  (
                    forall((three(Opponent, L, A, 0, 0), member([X1, Y1], A)), (step1(X1, Y1)
				->	true
				;	addMove(step2, X1, Y1))),
                    forall(step2(X1, Y1),
                        (
                            addMove(Opponent, X1, Y1),
                        (	winSecure(Opponent)
                        ->  undoMove(Opponent, X1, Y1), false
                        ;   true), 
							undoMove(Opponent, X1, Y1)
                        )
                    )
                    ->  retractall(step2(_, _)), undoMove(Player, X, Y), writeMove(Player, X, Y)
                    ;   retractall(step2(_, _)), undoMove(Player, X, Y)
                )
            )
        ;   undoMove(Player, X, Y), writeMove(Player, X, Y)
        )
    ).
