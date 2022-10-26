sator_szukites(Trees, SpecialTreeIndex, TreesDirLists, ShrunkLists) :-
    ValidIndex is SpecialTreeIndex - 1,
    nth0(ValidIndex, TreesDirLists, DirsForIndexedTree),
    check_nth_element(DirsForIndexedTree),
    nth0(ValidIndex, Trees, Tree),
    [Dir|_] = DirsForIndexedTree,
    create_tent(Tree, Dir, Tent),
    ShrunkLists = NewList,
    magic(Tent, TreesDirLists, NewList).

sator_szukites(_, _, [], ShrunkList) :-
    ShrunkList = [].

magic(Tent, TreesDirLists, ShrunkLists) :-
    ShrunkLists = [Tent|Coords],
    collect_coords(Tent, Coords, 0).
    /*ShrunkLists = ReturnedLists,
    shrunk_lists(Coords, TreesDirLists, ReturnedLists).*/

collect_coords(Tent, Coords, Index) :-
    coord_by_index(Tent, Index, Coord),
    NextIndex is Index + 1,
    Coords = [Coord|NewCoords],
    collect_coords(Tent, NewCoords, NextIndex).

collect_coords(_, NewCoords, 8) :-
    NewCoords = [].

coord_by_index(TentX-TentY, Index, Coord) :- 
    (Index == 0 , NewX is TentX + 1 , Coord = NewX-TentY); /* middle bottom */
    (Index == 1 , NewX is TentX - 1 , Coord = NewX-TentY); /* middle top */
    (Index == 2 , NewY is TentY + 1 , Coord = TentX-NewY); /* middle right */
    (Index == 3 , NewY is TentY - 1 , Coord = TentX-NewY); /* middle left */
    (Index == 4 , NewX is TentX + 1 , NewY is TentY + 1 , Coord = NewX-NewY); /* corner bottom right */
    (Index == 5 , NewX is TentX + 1 , NewY is TentY - 1 , Coord = NewX-NewY); /* corner bottom left */
    (Index == 6 , NewX is TentX - 1 , NewY is TentY + 1 , Coord = NewX-NewY); /* corner top right */
    (Index == 7 , NewX is TentX - 1 , NewY is TentY - 1 , Coord = NewX-NewY). /* corner top left */

create_tent(TreeRow-TreeCol, Dir, Tent) :-
    (Dir == n , NewRow is TreeRow - 1 , NewRow-TreeCol = Tent);
    (Dir == s , NewRow is TreeRow + 1 , NewRow-TreeCol = Tent);
    (Dir == w , NewCol is TreeCol - 1 , TreeRow-NewCol = Tent);
    (Dir == e , NewCol is TreeCol + 1 , TreeRow-NewCol = Tent).

check_nth_element(DirList) :-
    length(DirList, Length),
    Length == 1.