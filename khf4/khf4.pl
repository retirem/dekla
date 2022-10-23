satrak_mx(RowNum-ColNum, Trees, Dirs, Rows) :- 
    create_every_tent(RowNum, ColNum, Trees, Dirs, CurrentTents),
    no_collision(CurrentTents),
    create_matrice(RowNum, ColNum, CurrentTents, Rows),!.    

create_matrice(RowNum, ColNum, CurrentTents, Rows) :-
    build_rows(RowNum, ColNum, 1, CurrentTents, Rows).

build_rows(MaxRow, MaxCol, CurrentRow, CurrentTents, Rows) :-
    (CurrentRow =< MaxRow,
    build_cols(MaxCol, CurrentRow, 1, CurrentTents, Row),
    Rows = [Row|NewRows],
    TempRow is CurrentRow + 1,
    build_rows(MaxRow, MaxCol, TempRow, CurrentTents, NewRows))
    ;
    (
    	Rows = []
    ).

build_cols(MaxCol, CurrentRow, CurrentCol, [], Row) :-
    (CurrentCol =< MaxCol,
    Row = [0|NewElements],
    TempCol is CurrentCol + 1,
    build_cols(MaxCol, CurrentRow, TempCol, [], NewElements))
    ;
    (
    	Row = []
    ).

build_cols(MaxCol, CurrentRow, CurrentCol, CurrentTents, Row) :-
    (CurrentCol =< MaxCol,
    (memberchk(CurrentRow-CurrentCol, CurrentTents) ->  
    	Row = [1|NewElements]
    ;   
    	Row = [0|NewElements]
    ),
    TempCol is CurrentCol + 1,
    build_cols(MaxCol, CurrentRow, TempCol, CurrentTents, NewElements))
    ;
    (CurrentCol > MaxCol, 
    	Row = []
    ).

create_every_tent(RowNum, ColNum, [HeadTree|TailTrees], [HeadDir|TailDirs], CurrentTents) :-
    create_tent(HeadTree, HeadDir, Tent),
    placeable_tent(RowNum, ColNum, Tent),
    CurrentTents = [Tent|NewCurrentTents],
    create_every_tent(RowNum, ColNum, TailTrees, TailDirs, NewCurrentTents).

create_every_tent(_, _, [], [], CurrentTents) :-
    CurrentTents = [].

placeable_tent(RowNum, ColNum, Tent) :- 
    tent_inside(RowNum, ColNum, Tent).

no_collision([HeadTent|TailTents]) :-
    \+memberchk(HeadTent, TailTents),
    no_collision(TailTents).

no_collision([]) :-
    true.

no_collision([_|[]]) :-
    true.

create_tent(TreeRow-TreeCol, Dir, Tent) :-
    (Dir == n , NewRow is TreeRow - 1 , NewRow-TreeCol = Tent);
    (Dir == s , NewRow is TreeRow + 1 , NewRow-TreeCol = Tent);
    (Dir == w , NewCol is TreeCol - 1 , TreeRow-NewCol = Tent);
    (Dir == e , NewCol is TreeCol + 1 , TreeRow-NewCol = Tent).

tent_inside(RowNum, ColNum, TentRow-TentCol) :-
    (TentRow > 0 , TentRow =< RowNum , TentCol > 0 , TentCol =< ColNum).
 