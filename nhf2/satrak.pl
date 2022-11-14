:- use_module(library(lists), [nth0/3, nth0/4]).

satrak(satrak(RowNumbers, ColNumbers, Trees), Solution) :-
    length(RowNumbers, RowLength),
    length(ColNumbers, ColLength),
    iranylistak(RowLength-ColLength, Trees, DirectionsLists),
   	do_shrunks(Trees, DirectionsLists, RowNumbers, ColNumbers, ShrunkedDirs),
    solve(Trees, ShrunkedDirs, RowNumbers, ColNumbers, Return),
    flatten(Return, Solution).    

solve(Trees, ShrunkedDirs, RowNumbers, ColNumbers, Solution) :- 
    sub_solve(Trees, ShrunkedDirs, ShrunkedDirs, RowNumbers, ColNumbers, 0, Solution).

sub_solve(Trees, [HeadDirs|TailDirs], ShrunkedDirs, RowNumbers, ColNumbers, ValidIndex, Solution) :-
    length(HeadDirs, Length),
    NextValidIndex is ValidIndex + 1, 
    (Length == 1 -> 
        Solution = [HeadDirs|Ret],
        sub_solve(Trees, TailDirs, ShrunkedDirs, RowNumbers, ColNumbers, NextValidIndex, Ret)
    ;
    	member(DirElement, HeadDirs),
        nth0(ValidIndex, ShrunkedDirs, _, Rest),
        nth0(ValidIndex, NewShrunked, [DirElement], Rest),  
    	do_shrunks(Trees, NewShrunked, RowNumbers, ColNumbers, ShrunkedAgain),
        length(Pref, ValidIndex),
        append(Pref, SubShrunked, ShrunkedAgain),
        sub_solve(Trees, SubShrunked, ShrunkedAgain, RowNumbers, ColNumbers, ValidIndex, Solution)
	).

sub_solve(_, [], _, _, _, _, Solution) :-
    Solution = [].

do_shrunks(Trees, DirectionLists, RowNumbers, ColNumbers, ShrunkedList) :-
    do_shrunk_row(Trees, DirectionLists, RowNumbers, 1, TempShrunkedLists),
    shrunk_trees(Trees, TempShrunkedLists, ShrunkedDirs),
    do_shrunk_col(Trees, ShrunkedDirs, ColNumbers, 1, ShrunkedList).

do_shrunk_col(Trees, DirectionLists, [HeadColNum|TailColNums], Index, ShrunkedDirs) :-
    NextIndex is Index + 1,
    (HeadColNum >= 0 ->  
    	(osszeg_szukites(Trees, oszl(Index, HeadColNum), DirectionLists, Shrunked) ->
        	Shrunked \= [],
        	do_shrunk_col(Trees, Shrunked, TailColNums, NextIndex, ShrunkedDirs)
        ;   
        	do_shrunk_col(Trees, DirectionLists, TailColNums, NextIndex, ShrunkedDirs)
        )
    ;   
    	do_shrunk_col(Trees, DirectionLists, TailColNums, NextIndex, ShrunkedDirs)
    ).

do_shrunk_col(_,  ShrunkedList, [], _, ShrunkedDirs) :-
    ShrunkedDirs = ShrunkedList.

do_shrunk_row(Trees, DirectionLists, [HeadRowNum|TailRowNums], Index, ShrunkedDirs) :-
    NextIndex is Index + 1,
    (HeadRowNum >= 0 ->  
    	(osszeg_szukites(Trees, sor(Index, HeadRowNum), DirectionLists, Shrunked) ->  
        	Shrunked \= [],
    		do_shrunk_row(Trees, Shrunked, TailRowNums, NextIndex, ShrunkedDirs)
    	;   
    		do_shrunk_row(Trees, DirectionLists, TailRowNums, NextIndex, ShrunkedDirs)
    	)
    ;   
    	do_shrunk_row(Trees, DirectionLists, TailRowNums, NextIndex, ShrunkedDirs)
    ).

do_shrunk_row(_, ShrunkedList, [], _, ShrunkedDirs) :-
    ShrunkedDirs = ShrunkedList.
    
shrunk_trees(Trees, DirectionsLists, Return) :-
    do_shrunk_dir(Trees, Trees, 1, DirectionsLists, Shrunked),
    (DirectionsLists \= Shrunked -> 
        shrunk_trees(Trees, Shrunked, Return)
    ;
        Return = Shrunked
    ).

do_shrunk_dir(Trees, [_|TailTrees], Index, DirectionLists, Return) :-
    NextIndex is Index + 1,
    (sator_szukites(Trees, Index, DirectionLists, Shrunked) ->  
    	Shrunked \= [],
    	do_shrunk_dir(Trees, TailTrees, NextIndex, Shrunked, Return)
    ;
    	do_shrunk_dir(Trees, TailTrees, NextIndex, DirectionLists, Return)
    ).

do_shrunk_dir(_, [], _, FinalShrunkedList, ReturnList) :-
    ReturnList = FinalShrunkedList.

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

create_dir_lists(RowCol, Trees, [HCurrentTree|TCurrentTrees], DirectionList) :-
    dirlist_single_tree(RowCol, [n,s,w,e], HCurrentTree, Trees,  ListForTree),
    sort(ListForTree, Sorted),
    DirectionList = [Sorted|NewDirList],
    create_dir_lists(RowCol, Trees, TCurrentTrees, NewDirList).

create_dir_lists(_, _, [], NewDirList) :-
    NewDirList = [].

dirlist_single_tree(RowNum-ColNum, [HDir|TDirs], Tree, Trees, CurrentDirs) :-
    create_tent(Tree, HDir, Tent),
    ((tent_inside(RowNum, ColNum, Tent), \+memberchk(Tent, Trees)) ->  
    	CurrentDirs = [HDir|NewCurrentDirs],
        dirlist_single_tree(RowNum-ColNum, TDirs, Tree, Trees, NewCurrentDirs)
    ;   
    	dirlist_single_tree(RowNum-ColNum, TDirs, Tree, Trees, CurrentDirs)
    ).

dirlist_single_tree(_, [], _, _, NewCurrentDirs) :-
    NewCurrentDirs = [].

tent_inside(RowNum, ColNum, TentRow-TentCol) :-
    (TentRow > 0 , TentRow =< RowNum , TentCol > 0 , TentCol =< ColNum).

/* sator_szukites */

sator_szukites(Trees, SpecialTreeIndex, TreesDirLists, ShrunkLists) :-
    ValidIndex is SpecialTreeIndex - 1,
    nth0(ValidIndex, TreesDirLists, DirForIndexedTree),
    check_length_one(DirForIndexedTree),
    nth0(ValidIndex, Trees, Tree),
    [Dir|_] = DirForIndexedTree,
    create_tent(Tree, Dir, Tent),
    magic(Tent, Tree, Trees, TreesDirLists, NewList),
    (\+memberchk([], NewList) -> 
        ShrunkLists = NewList
    ;
        ShrunkLists = []
    ).

check_length_one([_X|[]]).

magic(Tent, Tree, Trees, TreesDirLists, ShrunkLists) :-
    collect_coords(Tent, Coords, 0),
    CheckingCoords = [Tent|Coords],
    shrunk_lists(CheckingCoords, Tent, Tree, Trees, TreesDirLists, ShrunkLists).

shrunk_lists(CheckingCoords, Tent, SelectedTree, [HeadTree|TailTrees], [HeadTreeDirsList|TailTreeDirsLists], ReturnedLists) :-
    (below_current(Tent, HeadTree) -> 
    	ReturnedLists = [HeadTreeDirsList|TailTreeDirsLists]
    ;   
    	(SelectedTree == HeadTree -> 
          	ReturnedLists = [HeadTreeDirsList|NewReturnedLists],
          	shrunk_lists(CheckingCoords, Tent, SelectedTree, TailTrees, TailTreeDirsLists, NewReturnedLists)
        ;
        	(in_distance(Tent, HeadTree) -> 
                new_dirs_tree(CheckingCoords, HeadTree, HeadTreeDirsList, NewDirList),
                ReturnedLists = [NewDirList|NewReturnedLists],
                shrunk_lists(CheckingCoords, Tent, SelectedTree, TailTrees, TailTreeDirsLists, NewReturnedLists)
            ;
                ReturnedLists = [HeadTreeDirsList|NewReturnedLists],
                shrunk_lists(CheckingCoords, Tent, SelectedTree, TailTrees, TailTreeDirsLists, NewReturnedLists)
            )
        )
	).

shrunk_lists(_, _, _, [], [], ReturnedLists) :-
    ReturnedLists = [].

below_current(STreetX-_, CTreeX-_) :-
    HorizontalDist is CTreeX - STreetX,
    HorizontalDist >= 3.

in_distance(STentX-STentY, CTreeX-CTreeY) :-
    HorizontalDist is STentX-CTreeX,
    VerticalDist is STentY-CTreeY,
    HorizontalAbs is abs(HorizontalDist),
    VerticalAbs is abs(VerticalDist),
    ((HorizontalAbs =< 2 , VerticalAbs =< 1) ; (HorizontalAbs =< 1 , VerticalAbs =< 2)), !.

new_dirs_tree(CheckingCoords, Tree, [HeadDir|TailDirs], DirList) :-
    create_tent(Tree, HeadDir, Tent),
    (memberchk(Tent, CheckingCoords) ->
        new_dirs_tree(CheckingCoords, Tree, TailDirs, DirList)
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

coord_by_index(X-Y, 0, NewX-Y) :- NewX is X + 1. /* middle bottom */
coord_by_index(X-Y, 1, NewX-Y) :- NewX is X - 1. /* middle top */
coord_by_index(X-Y, 2, X-NewY) :- NewY is Y + 1. /* middle right */
coord_by_index(X-Y, 3, X-NewY) :- NewY is Y - 1. /* middle left */
coord_by_index(X-Y, 4, NewX-NewY) :- NewX is X + 1, NewY is Y + 1. /* corner bottom right */
coord_by_index(X-Y, 5, NewX-NewY) :- NewX is X + 1, NewY is Y - 1. /* corner bottom left */
coord_by_index(X-Y, 6, NewX-NewY) :- NewX is X - 1, NewY is Y + 1. /* corner top right */
coord_by_index(X-Y, 7, NewX-NewY) :- NewX is X - 1, NewY is Y - 1. /* corner top left */

/* osszeg_szukites */

osszeg_szukites(Trees, LineLimit, TreeDirs, ShrunkDirs) :-
    get_sure_trees(Trees, LineLimit, TreeDirs, Sures),
    get_perhaps_trees(Trees, LineLimit, TreeDirs, Perhapses),
    shrunk(Trees, Sures, Perhapses, LineLimit, TreeDirs, ShrunkDirs),!.

shrunk(Trees, SureTrees, PerhapsTrees, LineLimit, OriginalDirList, Shrunks) :-
    length(SureTrees, SureLength),
    length(PerhapsTrees, PerhapsLength),
    decide_on_length(Trees, SureLength, PerhapsLength, PerhapsTrees, OriginalDirList, LineLimit, Shrunks).

decide_on_length(Trees, SureLength, PerhapsLength, PerhapsTrees, OriginalDirList, sor(Index, Db), Shrunks) :-
    TempSum is SureLength + PerhapsLength,
    (TempSum < Db, Shrunks = []);
    (TempSum == Db,
        all_perhapses(Trees, PerhapsTrees, OriginalDirList, sor(Index, Db), Shrunks)
    );
    (SureLength == Db , 
        no_perhapses_row(Trees, PerhapsTrees, OriginalDirList, Index, Shrunks)
    );
    (SureLength > Db , Shrunks = []).

decide_on_length(Trees, SureLength, PerhapsLength, PerhapsTrees, OriginalDirList, oszl(Index, Db), Shrunks) :-
    TempSum is SureLength + PerhapsLength,
    (TempSum < Db, Shrunks = []);
    (TempSum == Db,
        all_perhapses(Trees, PerhapsTrees, OriginalDirList, oszl(Index, Db), Shrunks)
    );
    (SureLength == Db , 
        no_perhapses_col(Trees, PerhapsTrees, OriginalDirList, Index, Shrunks)
    );
    (SureLength > Db , Shrunks = []).

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

    

get_all_correct_dirs([HTentRow-_|TTents], [HOriginalDir|TOriginalDirs], sor(Index, _), Dirs) :-
    (HTentRow == Index ->
        Dirs = [HOriginalDir|Return],
        get_all_correct_dirs(TTents, TOriginalDirs, sor(Index, _), Return)
    ;
        get_all_correct_dirs(TTents, TOriginalDirs, sor(Index, _), Dirs)
    ).

get_all_correct_dirs([_-HTentCol|TTents], [HOriginalDir|TOriginalDirs], oszl(Index, _), Dirs) :-
    (HTentCol == Index ->
        Dirs = [HOriginalDir|Return],
        get_all_correct_dirs(TTents, TOriginalDirs, oszl(Index, _), Return)
    ;
        get_all_correct_dirs(TTents, TOriginalDirs, oszl(Index, _), Dirs)
    ).

get_all_correct_dirs([], [], _, Dirs) :- Dirs = [].

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

all_perhapses([HTree|TTrees], [HPerhapsTree|TPerhapsTrees], [HOriginalDirs|TOriginalDirs], LineLimit, ShrunkDirs) :-
    (HTree == HPerhapsTree ->
        create_tents(HTree, HOriginalDirs, Tents),
        get_all_correct_dirs(Tents, HOriginalDirs, LineLimit, NewDirs),
        ShrunkDirs = [NewDirs|Return],
        all_perhapses(TTrees, TPerhapsTrees, TOriginalDirs, LineLimit, Return)
    ;
        ShrunkDirs = [HOriginalDirs|Return],
        all_perhapses(TTrees, TPerhapsTrees, TOriginalDirs, LineLimit, Return)
    ).

all_perhapses(_, [], Dirs, _, ShrunkDirs) :-
    ShrunkDirs = Dirs.

all_perhapses([], _, _, _, ShrunkDirs) :-
    ShrunkDirs = [].

no_collision_tent([HTentRow-_|TailTents], sor(Index, _)) :-
    HTentRow \= Index,
    no_collision_tent(TailTents, sor(Index, _)).

no_collision_tent([_-HTentCol|TailTents], oszl(Index, _)) :-
    HTentCol \= Index,
    no_collision_tent(TailTents, oszl(Index, _)).

no_collision_tent([], _) :- true.

create_tents(Tree, [HeadDir|TailDirs], Tents) :-
    create_tent(Tree, HeadDir, Tent),
    Tents = [Tent|Ret],
    create_tents(Tree, TailDirs, Ret).

create_tents(_, [], Tents) :-
    Tents = [].

get_perhaps_trees([HTreeRow-HTreeCol|TailTrees], sor(Index, Db), [HeadTreeDirs|TailTreeDirs], Perhapses) :-
    VerticalDist is HTreeRow - Index,
    VerticalAbs is abs(VerticalDist),
    (VerticalAbs =< 1 ->
        (check_tree_perhaps(HTreeRow-HTreeCol, sor(Index, Db), HeadTreeDirs) ->
            Perhapses = [HTreeRow-HTreeCol|Ret],
            get_perhaps_trees(TailTrees, sor(Index, Db), TailTreeDirs, Ret)
        ;
            get_perhaps_trees(TailTrees, sor(Index, Db), TailTreeDirs, Perhapses)
        )
    ;
        get_perhaps_trees(TailTrees, sor(Index, Db), TailTreeDirs, Perhapses)
    ).

get_perhaps_trees([HTreeRow-HTreeCol|TailTrees], oszl(Index, Db), [HeadTreeDirs|TailTreeDirs], Perhapses) :-
    HorizontalDist is HTreeCol - Index,
    HorizontalAbs is abs(HorizontalDist),
    (HorizontalAbs =< 1 ->    
        (check_tree_perhaps(HTreeRow-HTreeCol, oszl(Index, Db), HeadTreeDirs) ->
            Perhapses = [HTreeRow-HTreeCol|Ret],
            get_perhaps_trees(TailTrees, oszl(Index, Db), TailTreeDirs, Ret)
        ;
            get_perhaps_trees(TailTrees, oszl(Index, Db), TailTreeDirs, Perhapses)
        )
    ;
        get_perhaps_trees(TailTrees, oszl(Index, Db), TailTreeDirs, Perhapses)
    ).


get_perhaps_trees([], _, [], Perhapses) :-
    Perhapses = [].

check_tree_perhaps(Tree, LineLimit, Dirs) :-
    \+check_tree_for_sure(Tree, LineLimit, Dirs),
    create_tents(Tree, Dirs, Tents),
    \+no_collision_tent(Tents, LineLimit).

get_sure_trees([HTreeRow-HTreeCol|TailTrees], oszl(Index, Db), [HeadTreeDirs|TailTreeDirs], SureTrees) :- 
    HorizontalDist is HTreeCol - Index,
    HorizontalAbs is abs(HorizontalDist),
    (HorizontalAbs =< 1 ->
        (check_tree_for_sure(HTreeRow-HTreeCol, oszl(Index, Db), HeadTreeDirs) ->
            SureTrees = [HTreeRow-HTreeCol|Return],
            get_sure_trees(TailTrees, oszl(Index, Db), TailTreeDirs, Return)
        ;
            get_sure_trees(TailTrees, oszl(Index, Db), TailTreeDirs, SureTrees)
        )
    ;
        get_sure_trees(TailTrees, oszl(Index, Db), TailTreeDirs, SureTrees)
    ).

get_sure_trees([HTreeRow-HTreeCol|TailTrees], sor(Index, Db), [HeadTreeDirs|TailTreeDirs], SureTrees) :- 
    VerticalDist is HTreeRow - Index,
    VerticalAbs is abs(VerticalDist),
    (VerticalAbs =< 1 ->
        (check_tree_for_sure(HTreeRow-HTreeCol, sor(Index, Db), HeadTreeDirs) ->
            SureTrees = [HTreeRow-HTreeCol|Return],
            get_sure_trees(TailTrees, sor(Index, Db), TailTreeDirs, Return)
        ;
            get_sure_trees(TailTrees, sor(Index, Db), TailTreeDirs, SureTrees)
        )
    ;
        get_sure_trees(TailTrees, sor(Index, Db), TailTreeDirs, SureTrees)
    ).
    

get_sure_trees([], _, [], SureTrees) :-
    SureTrees = [].

check_tree_for_sure(Tree, LineLimit, Dirs) :-
    length(Dirs, Length),
    Length =< 2,
    tents_are_right(Tree, LineLimit, Dirs).

tents_are_right(Tree, sor(Index, _), [HeadDir|TailDirs]) :-
    create_tent(Tree, HeadDir, Tent),
    TentRow-_ = Tent,
    TentRow == Index,
    tents_are_right(Tree, sor(Index, _), TailDirs).

tents_are_right(Tree, oszl(Index, _), [HeadDir|TailDirs]) :-
    create_tent(Tree, HeadDir, Tent),
    _-TentCol = Tent,
    TentCol == Index,
    tents_are_right(Tree, oszl(Index, _), TailDirs).

tents_are_right(_, _, []) :- true.

create_tent(TreeRow-TreeCol, n, Tent) :- NewRow is TreeRow - 1, NewRow-TreeCol = Tent.
create_tent(TreeRow-TreeCol, s, Tent) :- NewRow is TreeRow + 1, NewRow-TreeCol = Tent.
create_tent(TreeRow-TreeCol, e, Tent) :- NewCol is TreeCol + 1, TreeRow-NewCol = Tent.
create_tent(TreeRow-TreeCol, w, Tent) :- NewCol is TreeCol - 1, TreeRow-NewCol = Tent.

flatten([], []).
flatten([[H]|T], [H|Tail]) :-
    flatten(T, Tail).
