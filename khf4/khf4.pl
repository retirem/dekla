satrak_mx(RowNum-ColNum, Trees, Dirs, boolMx) :- 
    [] = CurrentTents,
    [] = Rows,
    create_every_tent(RowNum, ColNum, Trees, Dirs, CurrentTents),
    create_matrice(RowNum, ColNum, CurrentTents, Rows).    

create_matrice(RowNum, ColNum, CurrentTents, Rows) :-
    build_rows(RowNum, ColNum, 0, CurrentTents, Rows).

build_rows(MaxRow, MaxCol, CurrentRow, CurrentTents, Rows) :-
    build_cols(MaxCol, CurrentRow, 0, CurrentTents, Row),
    build_rows(MaxRow, MaxCol, CurrentRow + 1, CurrentTents, [Row|Rows]).

build_rows(MaxRow, _, CurrentRow, _, _) :-
    CurrentRow == MaxRow, !.

build_cols(MaxCol, CurrentRow, CurrentCol, CurrentTents, Row) :-
    (memberchk(CurrentRow-CurrentCol, CurrentTents) , 
        build_cols(MaxCol, CurrentRow, CurrentCol + 1, CurrentTents, ['1'|Row]));
    (\+memberchk(CurrentRow-CurrentCol, CurrentTents) , 
        build_cols(MaxCol, CurrentRow, CurrentCol + 1, CurrentTents, ['0'|Row])).
    
build_cols(MaxCol, CurrentCol, _, _, _) :-
    CurrentCol == MaxCol, !.

create_every_tent(RowNum, ColNum, [HeadTree|TailTrees], [HeadDir|TailDirs], CurrentTents) :-
    placeable_tent(RowNum, ColNum, HeadTree, HeadDir, CurrentTents),
    create_every_tent(RowNum, ColNum, TailTrees, TailDirs, CurrentTents).

create_every_tent(_, _, [], [], CurrentTents) :-
    CurrentTents == CurrentTents.

placeable_tent(RowNum, ColNum, TreeRow-TreeCol, Dir, CurrentTents) :- 
    create_tent(TreeRow, TreeCol, Dir, Tent),
    tent_inside(RowNum, ColNum, Tent),
    \+memberchk(Tent, CurrentTents).

create_tent(TreeRow, TreeCol, Dir, Tent) :-
    (Dir == n , NewRow is TreeRow - 1 , NewRow-TreeCol = Tent);
    (Dir == s , NewRow is TreeRow + 1 , NewRow-TreeCol = Tent);
    (Dir == w , NewCol is TreeCol - 1 , TreeRow-NewCol = Tent);
    (Dir == e , NewCol is TreeCol + 1 , TreeRow-NewCol = Tent).

tent_inside(RowNum, ColNum, TentRow-TentCol) :-
    (TentRow > 0 , TentRow =< RowNum , TentCol > 0 , TentCol =< ColNum).
 