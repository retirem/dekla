satrak_mx(RowNum-ColNum, [HeadTreeRow-HeadTreeCol|TailTrees], [HeadDir|TailDirs], boolMx) :- 

placeable_tent(RowNum, ColNum, TreeRow-TreeCol, Dir, CurrentTents) :- 
    dir_inside(RowNum, ColNum, create_tent(TreeRow, TreeCol, Dir)),
    no_collision(create_tent(TreeRow, TreeCol, Dir, Tent), CurrentTents).

create_tent(TreeRow, TreeCol, Dir, Tent) :-
    (Dir == n , NewRow = TreeRow - 1 , NewRow-TreeCol is Tent);
    (Dir == s , NewRow = TreeRow + 1 , NewRow-TreeCol is Tent);
    (Dir == w , NewCol = TreeCol - 1 , TreeRow-NewCol is Tent);
    (Dir == e , NewCol = TreeCol + 1 , TreeRow-NewCol is Tent).

dir_inside(RowNum, ColNum, TentRow-TentCol) :-
    (TentRow > 0 , TentRow <= RowNum , TentCol > 0 , TentCol <= ColNum).

no_collision(TentRow-TentCol, [HeadCurrentTentRow-HeadCurrentTentCol|TailCurrentTents]) :-
    not(TentRow = HeadCurrentTentRow),
    no_collision(TentRow-TentCol, TailCurrentTents).

no_collision(TentRow-TentCol, []) :-
    1 == 1.
 