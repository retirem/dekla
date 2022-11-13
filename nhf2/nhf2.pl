satrak(RowNumbers, ColNumbers, Trees, Solution) :-
    length(RowNumbers, RowLength),
    length(ColNumbers, ColLength),
    iranylistak(RowLength-ColLength, Trees, DirectionsLists),
    length(Trees, TreeLength),
    do_shrunks(Trees, TreeLength, DirectionsLists, RowNumbers, RowLength, ColNumbers, ColLength, ShrunkedDirs),
    solve(Trees, TreeLength, ShrunkedDirs, RowNumbers, RowLength, ColNumbers, ColLength, Solution).

solve(Trees, TreeLength, ShrunkedDirs, RowNumbers, RowLength, ColNumbers, ColLength, Solution) :- 
    sub_solve(Trees, TreeLength, ShrunkedDirs, ShrunkedDirs, RowNumbers, RowLength, ColNumbers, ColLength, 0, TempSolution, Solution).

sub_solve(Trees, TreeLength, [_|TailDirs], ShrunkedDirs, RowNumbers, RowLength, ColNumbers, ColLength, Index, TempSolution, Solution) :-
    nth0(Index, ShrunkedDirs, SubDirs),
    length(SubDirs, Length),
    (Length == 1 -> 
        TempSolution = [SubDirs|Ret],
        TempIndex is Index + 1,
        sub_solve(Trees, TreeLength, TailDirs, ShrunkedDirs, RowNumbers, RowLength, ColNumbers, ColLength, TempIndex, Ret, Solution)
    ;
        multi_solve(Trees, TreeLength, SubDirs, ShrunkedDirs, RowNumbers, RowLength, ColNumbers, ColLength, Index, TempSolution, Solution)
    ).

multi_solve(Trees, TreeLength, [HeadSub|TailSub], ShrunkedDirs, RowNumbers, RowLength, ColNumbers, ColLength, Index, TempSolution, Solution) :-
    replace_at(ShrunkedDirs, Index, HeadSub, NewShrunked),
    sublist_from(NewShrunked, Index, 0, NewSub),
    sub_solve(Trees, TreeLength, NewSub, NewShrunked, RowNumbers, RowLength, ColNumbers, ColLength, Index, TempSolution, Solution).

multi_solve(_, _, [], _, _, _, _, _, _, _, _) :- false.

replace_at([Head|Tail], CurrentIndex, Index, Value, NewList) :-
    TempIndex is CurrentIndex + 1,
    (CurrentIndex == Index ->
        NewList = [Head|Ret],
        replace_at(Tail, TempIndex, Index, Value, Ret)
    ;
        replace_at(Tail, TempIndex, Index, Value, NewList)
    ).

replace_at([], _, _, _, NewList) :-
    NewList = [].

/* Count starts with index 0 */ 
sublist_from([Head|Tail], IndexFrom, Index, SubList) :-
    TempIndex is Index + 1,
    ( Index >= IndexFrom ->
        SubList = [Head|Ret],
        sublist_from(Tail, IndexFrom, TempIndex, Ret)
    ;
        sublist_from(Tail, IndexFrom, TempIndex, SubList)
    ).

sublist_from([], _, _, SubList) :-
    SubList = [].

sub_solve(_, _, [], _, _, _, _, _, TempSolution, Solution) :-
    Solution = [TempSolution|[]].

do_shrunks(Trees, TreeLength, DirectionLists, RowNumbers, RowLength, ColNumbers, ColLength, ShrunkedList) :-
    shrunk_trees(Trees, TreeLength, DirectionLists, ShrunkedDirs),
    shrunk_sums(Trees, RowNumbers, RowLength, ColNumbers, ColLength, ShrunkedDirs, ShrunkedList).

shrunk_sums(Trees, RowNumbers, RowLength, ColNumbers, ColLength, DirectionLists, SumShrunkedDirs) :-
    do_shrunk_row(Trees, DirectionLists, RowNumbers, RowLength, 1, TempShrunkedLists),
    do_shrunk_col(Trees, TempShrunkedLists, ColNumbers, ColLength, 1, SumShrunkedDirs).

do_shrunk_col(Trees, DirectionLists, ColNumbers, ColLength, Index, ShrunkedDirs) :-
    (Index =< ColLength ->
        ValidIndex is Index - 1,
        nth0(ValidIndex, ColNumbers, Db),
        osszeg_szukites(Trees, oszl(Index, Db), DirectionLists, Shrunked),
        TempIndex is Index + 1,
        do_shrunk_col(Trees, Shrunked, ColNumbers, ColLength, TempIndex, ShrunkedDirs)
    ;
        ShrunkedDirs = DirectionLists
    ).

do_shrunk_row(Trees, DirectionLists, RowNumbers, RowLength, Index, ShrunkedDirs) :-
    (Index =< RowLength -> 
        ValidIndex is Index - 1,
        nth0(ValidIndex, RowNumbers, Db),
        osszeg_szukites(Trees, sor(Index, Db), DirectionLists, Shrunked),
        TempIndex is Index + 1,
        do_shrunk_row(Trees, Shrunked, RowNumbers, RowLength, TempIndex, ShrunkedDirs)
    ;
        ShrunkedDirs = DirectionLists
    ).


shrunk_trees(Trees, TreeLength, DirectionsLists, Return) :-
    do_shrunk_dir(Trees, 1, TreeLength, DirectionsLists, Shrunked),
    (DirectionsLists /= Shrunked -> 
        shrunk_trees(Trees, TreeLength, Shrunked, Return)
    ;
        Return = Shrunked
    ).

do_shrunk_dir(Trees, Index, Length, DirectionLists, Ret) :-
    (Index =< Length -> 
        sator_szukites(Trees, Index, DirectionLists, Shrunked),
        TempIndex is Index + 1,
        do_shrunk_dir(Trees, TempIndex, Length, Shrunked, Ret)
    ;
        Ret = DirectionLists
    ).



/* ------------------------- LEGACY ---------------------------- */

iranylistak(RowCol, Trees, DirectionList) :-
    create_dir_lists(RowCol, Trees, Trees, TempList),
    (\+memberchk([], TempList) ->  
    	DirectionList = TempList
    ; 
    	DirectionList = []
    ).

iranylistak(_, [], DirectionList) :-
    DirectionList = [].

create_dir_lists(RowCol, Trees, CurrentTrees, DirectionList) :-
    [HeadTree|TailTree] = CurrentTrees,
    check_dirs_for_tree(RowCol, [n,s,w,e], HeadTree, Trees,  ListForTree),
    sort(ListForTree, Sorted),
    DirectionList = [Sorted|NewDirList],
    create_dir_lists(RowCol, Trees, TailTree, NewDirList).

create_dir_lists(_, _, [], NewDirList) :-
    NewDirList = [].

check_dirs_for_tree(RowNum-ColNum, [AllDirHead|AllDirTail], Tree, Trees, CurrentDirs) :-
    (create_tent(Tree, AllDirHead, Tent),
    tent_inside(RowNum, ColNum, Tent),
    \+memberchk(Tent, Trees) ->  
    	CurrentDirs = [AllDirHead|NewCurrentDirs],
        check_dirs_for_tree(RowNum-ColNum, AllDirTail, Tree, Trees, NewCurrentDirs)
    ;   
    	CurrentDirs = NewCurrentDirs,
    	check_dirs_for_tree(RowNum-ColNum, AllDirTail, Tree, Trees, NewCurrentDirs)
    ).

check_dirs_for_tree(_, [], _, _, NewCurrentDirs) :-
    NewCurrentDirs = [].

create_tent(TreeRow-TreeCol, Dir, Tent) :-
    (Dir == n , NewRow is TreeRow - 1 , NewRow-TreeCol = Tent);
    (Dir == s , NewRow is TreeRow + 1 , NewRow-TreeCol = Tent);
    (Dir == w , NewCol is TreeCol - 1 , TreeRow-NewCol = Tent);
    (Dir == e , NewCol is TreeCol + 1 , TreeRow-NewCol = Tent).

tent_inside(RowNum, ColNum, TentRow-TentCol) :-
    (TentRow > 0 , TentRow =< RowNum , TentCol > 0 , TentCol =< ColNum).




sator_szukites(Trees, SpecialTreeIndex, TreesDirLists, ShrunkLists) :-
    ValidIndex is SpecialTreeIndex - 1,
    nth0(ValidIndex, TreesDirLists, DirsForIndexedTree),
    check_nth_element(DirsForIndexedTree),
    nth0(ValidIndex, Trees, Tree),
    [Dir|_] = DirsForIndexedTree,
    create_tent(Tree, Dir, Tent),
    magic(Tent, Tree, Trees, TreesDirLists, NewList),
    (\+memberchk([], NewList) -> 
        ShrunkLists = NewList
    ;
        ShrunkLists = []
    ).

sator_szukites(_, _, [], ShrunkList) :-
    ShrunkList = [].

magic(Tent, Tree, Trees, TreesDirLists, ShrunkLists) :-
    collect_coords(Tent, Coords, 0),
    CheckingCoords = [Tent|Coords],
    ShrunkLists = ReturnedLists,
    shrunk_lists(CheckingCoords, Tree, Trees, TreesDirLists, ReturnedLists).

shrunk_lists(CheckingCoords, SelectedTree, [HeadTree|TailTrees], [HeadTreeDirsList|TailTreeDirsLists], ReturnedLists) :-
    (SelectedTree == HeadTree -> 
        ReturnedLists = [HeadTreeDirsList|NewReturnedLists],
        shrunk_lists(CheckingCoords, SelectedTree, TailTrees, TailTreeDirsLists, NewReturnedLists)
    ;
        ReturnedLists = [Sorted|NewReturnedLists],
        new_dirs_tree(CheckingCoords, HeadTree, HeadTreeDirsList, NewDirList),
        sort(NewDirList, Sorted),
        shrunk_lists(CheckingCoords, SelectedTree, TailTrees, TailTreeDirsLists, NewReturnedLists)
    ).

shrunk_lists(_, _, [], [], ReturnedLists) :-
    ReturnedLists = [].

new_dirs_tree(CheckingCoords, Tree, [HeadDir|TailDirs], DirList) :-
    create_tent(Tree, HeadDir, Tent),
    (memberchk(Tent, CheckingCoords) ->
        DirList = NewDirList,
        new_dirs_tree(CheckingCoords, Tree, TailDirs, NewDirList)
    ;
        DirList = [HeadDir|NewDirList],
        new_dirs_tree(CheckingCoords, Tree, TailDirs, NewDirList)
    ).

new_dirs_tree(_, _, [], NewDirList) :-
    NewDirList = [].


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

check_nth_element(DirList) :-
    length(DirList, Length),
    Length == 1.








osszeg_szukites(Trees, SumCondition, TreeDirs, ShrunkDirs) :-
    get_sure_trees(Trees, SumCondition, TreeDirs, Sures),
    get_perhaps_trees(Trees, SumCondition, TreeDirs, Perhapses),
    shrunk(Trees, Sures, Perhapses, SumCondition, TreeDirs, ShrunkDirs),!.

shrunk(Trees, SureTrees, PerhapsTrees, SumCondition, OriginalDirList, Shrunks) :-
    length(SureTrees, SureLength),
    length(PerhapsTrees, PerhapsLength),
    decide_on_length(Trees, SureLength, PerhapsLength, PerhapsTrees, OriginalDirList, SumCondition, Shrunks).

decide_on_length(Trees, SureLength, PerhapsLength, PerhapsTrees, OriginalDirList, sor(RowCount, RowNum), Shrunks) :-
    (SureLength > RowNum , Shrunks = []);
    (SureLength == RowNum , 
        no_perhapses_row(Trees, PerhapsTrees, OriginalDirList, RowCount, Shrunks)
    );
    (TempSum is SureLength + PerhapsLength,
        TempSum < RowNum,
        Shrunks = []
    );
    (TempSum is SureLength + PerhapsLength,
        TempSum == RowNum,
        all_perhapses_row(Trees, PerhapsTrees, OriginalDirList, RowCount, Shrunks)
    ).

decide_on_length(Trees, SureLength, PerhapsLength, PerhapsTrees, OriginalDirList, oszl(ColCount, ColNum), Shrunks) :-
    (SureLength > ColNum , Shrunks = []);
    (SureLength == ColNum , 
        no_perhapses_col(Trees, PerhapsTrees, OriginalDirList, ColCount, Shrunks)
    );
    (TempSum is SureLength + PerhapsLength,
        TempSum < ColNum,
        Shrunks = []
    );
    (TempSum is SureLength + PerhapsLength,
        TempSum == ColNum,
        all_perhapses_col(Trees, PerhapsTrees, OriginalDirList, ColCount, Shrunks)
    ).

get_no_correct_dirs_col([_-HeadTentCol|TailTents], [HeadOriginalDir|TailOriginalDirs], ColCount, Dirs) :-
    (HeadTentCol \= ColCount,
        Dirs = [HeadOriginalDir|Ret],
        get_no_correct_dirs_col(TailTents, TailOriginalDirs, ColCount, Ret)
    ;
        get_no_correct_dirs_col(TailTents, TailOriginalDirs, ColCount, Dirs)
    ).

get_no_correct_dirs_col([], [], _, Dirs) :-
    Dirs = [].

get_no_correct_dirs_row([HeadTentRow-_|TailTents], [HeadOriginalDir|TailOriginalDirs], RowCount, Dirs) :-
    (HeadTentRow \= RowCount,
        Dirs = [HeadOriginalDir|Ret],
        get_no_correct_dirs_row(TailTents, TailOriginalDirs, RowCount, Ret)
    ;
        get_no_correct_dirs_row(TailTents, TailOriginalDirs, RowCount, Dirs)
    ).

get_no_correct_dirs_row([], [], _, Dirs) :-
    Dirs = [].

get_all_correct_dirs_col([_-HeadTentCol|TailTents], [HeadOriginalDir|TailOriginalDirs], ColCount, Dirs) :-
    (HeadTentCol == ColCount,
        Dirs = [HeadOriginalDir|Ret],
        get_all_correct_dirs_col(TailTents, TailOriginalDirs, ColCount, Ret)
    ;
        get_all_correct_dirs_col(TailTents, TailOriginalDirs, ColCount, Dirs)
    ).

get_all_correct_dirs_col([], [], _, Dirs) :-
    Dirs = [].

get_all_correct_dirs_row([HeadTentRow-_|TailTents], [HeadOriginalDir|TailOriginalDirs], RowCount, Dirs) :-
    (HeadTentRow == RowCount,
        Dirs = [HeadOriginalDir|Ret],
        get_all_correct_dirs_row(TailTents, TailOriginalDirs, RowCount, Ret)
    ;
        get_all_correct_dirs_row(TailTents, TailOriginalDirs, RowCount, Dirs)
    ).

get_all_correct_dirs_row([], [], _, Dirs) :-
    Dirs = [].

no_perhapses_col([HeadTree|TailTrees], PerhapsTrees, [OriginalHeadDirs|OriginalTailDirs], ColCount, ShrunkDirs) :-
    [HeadPerhapsTree|TailPerhapsTrees] = PerhapsTrees,
    (HeadTree == HeadPerhapsTree ->
        create_tents(HeadTree, OriginalHeadDirs, Tents),
        get_no_correct_dirs_col(Tents, OriginalHeadDirs, ColCount, NewDirs),
        ShrunkDirs = [NewDirs|Ret],
        no_perhapses_col(TailTrees, TailPerhapsTrees, OriginalTailDirs, ColCount, Ret)
    ;
        ShrunkDirs = [OriginalHeadDirs|Ret],
        no_perhapses_col(TailTrees, PerhapsTrees, OriginalTailDirs, ColCount, Ret)
    ).

no_perhapses_col(_, [], Dirs, _, ShrunkDirs) :-
    ShrunkDirs = Dirs.

no_perhapses_col([], _, _, _, ShrunkDirs) :-
    ShrunkDirs = [].

no_perhapses_row([HeadTree|TailTrees], PerhapsTrees, [OriginalHeadDirs|OriginalTailDirs], RowCount, ShrunkDirs) :-
    [HeadPerhapsTree|TailPerhapsTrees] = PerhapsTrees,
    (HeadTree == HeadPerhapsTree ->
        create_tents(HeadTree, OriginalHeadDirs, Tents),
        get_no_correct_dirs_row(Tents, OriginalHeadDirs, RowCount, NewDirs),
        ShrunkDirs = [NewDirs|Ret],
        no_perhapses_row(TailTrees, TailPerhapsTrees, OriginalTailDirs, RowCount, Ret)
    ;
        ShrunkDirs = [OriginalHeadDirs|Ret],
        no_perhapses_row(TailTrees, PerhapsTrees, OriginalTailDirs, RowCount, Ret)
    ).

no_perhapses_row(_, [], Dirs, _, ShrunkDirs) :-
    ShrunkDirs = Dirs.

no_perhapses_row([], _, _, _, ShrunkDirs) :-
    ShrunkDirs = [].

all_perhapses_col([HeadTree|TailTrees], PerhapsTrees, [OriginalHeadDirs|OriginalTailDirs], ColCount, ShrunkDirs) :-
    [HeadPerhapsTree|TailPerhapsTrees] = PerhapsTrees,
    (HeadTree == HeadPerhapsTree ->
        create_tents(HeadTree, OriginalHeadDirs, Tents),
        get_all_correct_dirs_col(Tents, OriginalHeadDirs, ColCount, NewDirs),
        ShrunkDirs = [NewDirs|Ret],
        all_perhapses_col(TailTrees, TailPerhapsTrees, OriginalTailDirs, ColCount, Ret)
    ;
        ShrunkDirs = [OriginalHeadDirs|Ret],
        all_perhapses_col(TailTrees, PerhapsTrees, OriginalTailDirs, ColCount, Ret)
    ).

all_perhapses_col(_, [], Dirs, _, ShrunkDirs) :-
    ShrunkDirs = Dirs.

all_perhapses_col([], _, _, _, ShrunkDirs) :-
    ShrunkDirs = [].

all_perhapses_row([HeadTree|TailTrees], PerhapsTrees, [OriginalHeadDirs|OriginalTailDirs], RowCount, ShrunkDirs) :-
    [HeadPerhapsTree|TailPerhapsTrees] = PerhapsTrees,
    (HeadTree == HeadPerhapsTree ->
        create_tents(HeadTree, OriginalHeadDirs, Tents),
        get_all_correct_dirs_row(Tents, OriginalHeadDirs, RowCount, NewDirs),
        ShrunkDirs = [NewDirs|Ret],
        all_perhapses_row(TailTrees, TailPerhapsTrees, OriginalTailDirs, RowCount, Ret)
    ;
        ShrunkDirs = [OriginalHeadDirs|Ret],
        all_perhapses_row(TailTrees, PerhapsTrees, OriginalTailDirs, RowCount, Ret)
    ).

all_perhapses_row(_, [], Dirs, _, ShrunkDirs) :-
    ShrunkDirs = Dirs.

all_perhapses_row([], _, _, _, ShrunkDirs) :-
    ShrunkDirs = [].

no_collision_tent_col([_-HeadTentCol|TailTents], ColCount) :-
    HeadTentCol \= ColCount,
    no_collision_tent_col(TailTents, ColCount).

no_collision_tent_col([], _) :-
    true.

no_collision_tent_row([HeadTentRow-_|TailTents], RowCount) :-
    HeadTentRow \= RowCount,
    no_collision_tent_row(TailTents, RowCount).

no_collision_tent_row([], _) :-
    true.

create_tents(Tree, [HeadDir|TailDirs], Tents) :-
    create_tent(Tree, HeadDir, Tent),
    Tents = [Tent|Ret],
    create_tents(Tree, TailDirs, Ret).

create_tents(_, [], Tents) :-
    Tents = [].

get_perhaps_trees([HeadTree|TailTrees], sor(RowCount, RowNum), [HeadTreeDirs|TailTreeDirs], Perhapses) :-
    (check_tree_perhaps_row(HeadTree, RowCount, HeadTreeDirs) ->
        Perhapses = [HeadTree|Ret],
        get_perhaps_trees(TailTrees, sor(RowCount, RowNum), TailTreeDirs, Ret)
    ;
        get_perhaps_trees(TailTrees, sor(RowCount, RowNum), TailTreeDirs, Perhapses)
    ).

get_perhaps_trees([HeadTree|TailTrees], oszl(ColCount, ColNum), [HeadTreeDirs|TailTreeDirs], Perhapses) :-
    (check_tree_perhaps_col(HeadTree, ColCount, HeadTreeDirs) ->
        Perhapses = [HeadTree|Ret],
        get_perhaps_trees(TailTrees, oszl(ColCount, ColNum), TailTreeDirs, Ret)
    ;
        get_perhaps_trees(TailTrees, oszl(ColCount, ColNum), TailTreeDirs, Perhapses)
    ).

get_perhaps_trees([], _, [], Perhapses) :-
    Perhapses = [].

check_tree_perhaps_col(Tree, ColCount, Dirs) :-
    \+check_tree_sure_col(Tree, ColCount, Dirs),
    create_tents(Tree, Dirs, Tents),
    \+no_collision_tent_col(Tents, ColCount).

check_tree_perhaps_row(Tree, RowCount, Dirs) :-
    \+check_tree_sure_row(Tree, RowCount, Dirs),
    create_tents(Tree, Dirs, Tents),
    \+no_collision_tent_row(Tents, RowCount).

get_sure_trees([HeadTree|TailTrees], oszl(ColCount, ColNum), [HeadTreeDirs|TailTreeDirs], Sures) :-
    (check_tree_sure_col(HeadTree, ColCount, HeadTreeDirs) ->
        Sures = [HeadTree|Ret],
        get_sure_trees(TailTrees, oszl(ColCount, ColNum), TailTreeDirs, Ret)
    ;
        get_sure_trees(TailTrees, oszl(ColCount, ColNum), TailTreeDirs, Sures)
    ).

get_sure_trees([HeadTree|TailTrees], sor(RowCount, RowNum), [HeadTreeDirs|TailTreeDirs], Sures) :-
    (check_tree_sure_row(HeadTree, RowCount, HeadTreeDirs) ->
        Sures = [HeadTree|Ret],
        get_sure_trees(TailTrees, sor(RowCount, RowNum), TailTreeDirs, Ret)
    ;
        get_sure_trees(TailTrees, sor(RowCount, RowNum), TailTreeDirs, Sures)
    ).

get_sure_trees([], _, [], Sures) :-
    Sures = [].

check_tree_sure_col(Tree, ColCount, Dirs) :-
    length(Dirs, Length),
    Length =< 2,
    tents_are_right_col(Tree, ColCount, Dirs).

tents_are_right_col(Tree, ColCount, [HeadDir|TailDirs]) :-
    create_tent(Tree, HeadDir, Tent),
    _-TentCol = Tent,
    TentCol == ColCount,
    tents_are_right_col(Tree, ColCount, TailDirs).

tents_are_right_col(_, _, []) :-
    true.

check_tree_sure_row(Tree, RowCount, Dirs) :-
    length(Dirs, Length),
    Length =< 2,
    tents_are_right_row(Tree, RowCount, Dirs).

tents_are_right_row(Tree, RowCount, [HeadDir|TailDirs]) :-
    create_tent(Tree, HeadDir, Tent),
    TentRow-_ = Tent,
    TentRow = RowCount,
    tents_are_right_row(Tree, RowCount, TailDirs).

tents_are_right_row(_, _, []) :-
    true.
