:- use_module(library(lists), [nth0/3, nth0/4]).

satrak(satrak(RowNumbers, ColNumbers, Trees), Solution) :-
    length(RowNumbers, RowLength),
    length(ColNumbers, ColLength),
    iranylistak(RowLength-ColLength, Trees, DirectionsLists),
   	do_shrunks(Trees, DirectionsLists, RowNumbers, ColNumbers, ShrunkedDirs),
    solve(Trees, ShrunkedDirs, RowNumbers, ColNumbers, Return),
    flatten(Return, Solution).    

% Előkészíti a feladvány megoldását 
solve(Trees, ShrunkedDirs, RowNumbers, ColNumbers, Solution) :- 
    sub_solve(Trees, ShrunkedDirs, ShrunkedDirs, RowNumbers, ColNumbers, 0, Solution).

% A feldavány megoldásának motorja. Ha egyelemű az adott iránylista, akkor hozzávesszük a megoldáshoz, 
% majd rekurzívan továbbmegyünk, ha több elemű, akkor mindegyiket kiválasztva és lerögzítve az adott indexű helyen
% rekurzívan továbbmegyünk
sub_solve(_, [], _, _, _, _, Solution) :- Solution = [].
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

% A sor-, sátor- és oszlop szűkítések végrehajtása ebben a sorrendben
do_shrunks(Trees, DirectionLists, RowNumbers, ColNumbers, ShrunkedList) :-
    do_shrunk_row(Trees, DirectionLists, RowNumbers, 1, TempShrunkedLists),
    shrunk_trees(Trees, TempShrunkedLists, ShrunkedDirs),
    do_shrunk_col(Trees, ShrunkedDirs, ColNumbers, 1, ShrunkedList).

% A bemenő fák, iránylisták, oszlop kritériumok és index alapján az oszlop szűkítés elvégzése
do_shrunk_col(_,  ShrunkedList, [], _, ShrunkedDirs) :- ShrunkedDirs = ShrunkedList.
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

% A bemenő fák, iránylisták, sor kritériumok és index alapján az sor szűkítés elvégzése
do_shrunk_row(_, ShrunkedList, [], _, ShrunkedDirs) :- ShrunkedDirs = ShrunkedList.
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
    
% Szűkítés sátrak szerint rekurzívan, egészen addig, amíg lehet szűkíteni
shrunk_trees(Trees, DirectionsLists, Return) :-
    do_shrunk_dir(Trees, Trees, 1, DirectionsLists, Shrunked),
    (DirectionsLists \= Shrunked -> 
        shrunk_trees(Trees, Shrunked, Return)
    ;
        Return = Shrunked
    ).

% A bemenő fák, hátralevő fák, jelenlegi fa indexe 1-essel kezdve és iránylisták alapján minden fára elvégzi
% a sátor szűkítést
do_shrunk_dir(_, [], _, FinalShrunkedList, ReturnList) :- ReturnList = FinalShrunkedList.
do_shrunk_dir(Trees, [_|TailTrees], Index, DirectionLists, Return) :-
    NextIndex is Index + 1,
    (sator_szukites(Trees, Index, DirectionLists, Shrunked) ->  
    	Shrunked \= [],
    	do_shrunk_dir(Trees, TailTrees, NextIndex, Shrunked, Return)
    ;
    	do_shrunk_dir(Trees, TailTrees, NextIndex, DirectionLists, Return)
    ).

/* ------------------------- LEGACY ---------------------------- */

iranylistak(_, [], DirectionList) :- DirectionList = [].
iranylistak(RowCol, Trees, DirectionList) :-
    create_dir_lists(RowCol, Trees, Trees, TempList),
    (\+memberchk([], TempList) ->  
    	DirectionList = TempList
    ; 
    	DirectionList = []
    ).

% Létrehozza a fákhoz tartozó iránylistákat
create_dir_lists(_, _, [], NewDirList) :- NewDirList = [].
create_dir_lists(RowCol, Trees, [HCurrentTree|TCurrentTrees], DirectionList) :-
    dirlist_single_tree(RowCol, [n,s,w,e], HCurrentTree, Trees,  ListForTree),
    sort(ListForTree, Sorted),
    DirectionList = [Sorted|NewDirList],
    create_dir_lists(RowCol, Trees, TCurrentTrees, NewDirList).

% Kiválasztja az adott fához azokat az irányokat, hogy az azokkal képzett sátrak nem lógnak ki
% a mezőről és nem esnek egybe másik fával
dirlist_single_tree(_, [], _, _, NewCurrentDirs) :- NewCurrentDirs = [].
dirlist_single_tree(RowCol, [HDir|TDirs], Tree, Trees, CurrentDirs) :-
    create_tent(Tree, HDir, Tent),
    ((tent_inside(RowCol, Tent), \+memberchk(Tent, Trees)) ->  
    	CurrentDirs = [HDir|NewCurrentDirs],
        dirlist_single_tree(RowCol, TDirs, Tree, Trees, NewCurrentDirs)
    ;   
    	dirlist_single_tree(RowCol, TDirs, Tree, Trees, CurrentDirs)
    ).

% A kapott sor és oszlop szám alapján ellenőrzi, hogy a bemenő sátor a mezőn helyezkedik-e el
tent_inside(RowNum-ColNum, TentRow-TentCol) :-
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

% Ellenőrzi, hogy a kapott lista egyelemű-e
check_length_one([_X|[]]).

% Előkészíti a sátor szűkítést
magic(Tent, Tree, Trees, TreesDirLists, ShrunkLists) :-
    collect_coords(Tent, Coords, 0),
    CheckingCoords = [Tent|Coords],
    shrunk_lists(CheckingCoords, Tent, Tree, Trees, TreesDirLists, ShrunkLists).

% A bemenő koordináta halmaz, sátor, kijelölt fa, fák, iránylisták alapján szűkíti a 
% megfelelő fák iránylistáit
shrunk_lists(_, _, _, [], [], ReturnedLists) :- ReturnedLists = [].
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

% Ellenőrzi, hogy a bemenő kijelölt fától elég messze van-e a bemenő aktuális fa,
% mivel így már az aktuális fával nem kell szűkítést számolni
below_current(STreeX-_, CTreeX-_) :-
    HorizontalDist is CTreeX - STreeX,
    HorizontalDist >= 3.

% Ellenőrzi, hogy a bemenő sátor olyan közel van-e a bemenő fához, hogy érdemes-e a fával 
% szűkítést számolni
in_distance(STentX-STentY, CTreeX-CTreeY) :-
    HorizontalDist is STentX-CTreeX,
    VerticalDist is STentY-CTreeY,
    HorizontalAbs is abs(HorizontalDist),
    VerticalAbs is abs(VerticalDist),
    ((HorizontalAbs =< 2 , VerticalAbs =< 1) ; (HorizontalAbs =< 1 , VerticalAbs =< 2)), !.

% A bemenő sátor körülötti koordináta halmaz, fa és iránylista alapján szűkíti a fa iránylistáját
new_dirs_tree(_, _, [], NewDirList) :- NewDirList = [].
new_dirs_tree(CheckingCoords, Tree, [HeadDir|TailDirs], DirList) :-
    create_tent(Tree, HeadDir, Tent),
    (memberchk(Tent, CheckingCoords) ->
        new_dirs_tree(CheckingCoords, Tree, TailDirs, DirList)
    ;
        DirList = [HeadDir|NewDirList],
        new_dirs_tree(CheckingCoords, Tree, TailDirs, NewDirList)
    ).

% A bemenő sátor alapján összegyűjti a kürölötte lévő cellák koordinátáit
collect_coords(_, NewCoords, 8) :- NewCoords = [].
collect_coords(Tent, Coords, Index) :-
    coord_by_index(Tent, Index, Coord),
    NextIndex is Index + 1,
    Coords = [Coord|NewCoords],
    collect_coords(Tent, NewCoords, NextIndex).

% A kapott koordináták és index alapján visszaad egy új koordináta párost.
% A sátor körülötti cellák meghatározásához
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

% A bemenp fák, 'biztos' fa list, 'eseteg' fa list, oszlop/sor kritérium és iránylisták 
% segítségével előkészíti az összegek alapján való döntést
shrunk(Trees, SureTrees, PerhapsTrees, LineLimit, OriginalDirList, Shrunks) :-
    length(SureTrees, SureLength),
    length(PerhapsTrees, PerhapsLength),
    decide_on_length(Trees, SureLength, PerhapsLength, PerhapsTrees, OriginalDirList, LineLimit, Shrunks).

% A bemenő fák, 'biztos' fa össze, 'esetleg' fa összes, esetleg fák, iránylisták és oszlop/sor kritérium alapján 
% elvégzi az összegszűkítést
decide_on_length(Trees, SureLength, PerhapsLength, PerhapsTrees, OriginalDirList, sor(Index, Db), Shrunks) :-
    TempSum is SureLength + PerhapsLength,
    (TempSum < Db, Shrunks = []);
    (TempSum == Db,
        all_perhapses(Trees, PerhapsTrees, OriginalDirList, sor(Index, Db), Shrunks)
    );
    (SureLength == Db , 
        no_perhapses(Trees, PerhapsTrees, OriginalDirList, sor(Index, Db), Shrunks)
    );
    (SureLength > Db , Shrunks = []).

decide_on_length(Trees, SureLength, PerhapsLength, PerhapsTrees, OriginalDirList, oszl(Index, Db), Shrunks) :-
    TempSum is SureLength + PerhapsLength,
    (TempSum < Db, Shrunks = []);
    (TempSum == Db,
        all_perhapses(Trees, PerhapsTrees, OriginalDirList, oszl(Index, Db), Shrunks)
    );
    (SureLength == Db , 
        no_perhapses(Trees, PerhapsTrees, OriginalDirList, oszl(Index, Db), Shrunks)
    );
    (SureLength > Db , Shrunks = []).

% A bemenő sátrak, iránylisták és oszlop/sor kritérium alapján előállítja azokat az irányokat,
% amivel a sátrak nem tesznek eleget az oszlop/sor feltételnek
get_no_correct_dirs([], [], _, Dirs) :- Dirs = [].

get_no_correct_dirs([HTentRow-_|TTents], [HOriginalDir|TOriginalDirs], sor(Index, _), Dirs) :-
    (HTentRow \= Index,
        Dirs = [HOriginalDir|Return],
        get_no_correct_dirs(TTents, TOriginalDirs, sor(Index, _), Return)
    ;
        get_no_correct_dirs(TTents, TOriginalDirs, sor(Index, _), Dirs)
    ).

get_no_correct_dirs([_-HTentCol|TTents], [HOriginalDir|TOriginalDirs], oszl(Index, _), Dirs) :-
    (HTentCol \= Index,
        Dirs = [HOriginalDir|Return],
        get_no_correct_dirs(TTents, TOriginalDirs, oszl(Index, _), Return)
    ;
        get_no_correct_dirs(TTents, TOriginalDirs, oszl(Index, _), Dirs)
    ).

% A bemenő sátrak, iránylisták és oszlop/sor kritérium alapján előállítja azokat az irányokat,
% amivel a sátrak eleget tesznek az oszlop/sor feltételnek
get_all_correct_dirs([], [], _, Dirs) :- Dirs = [].

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

% A bemenő fák, 'esetleg' fák, fákhoz tartozó iránylisták és oszlop/sor kritérium alapján előállítja
% azt a szűkített listát, amibe egyik 'esetleg' fa sem tartozik bele
no_perhapses(_, [], Dirs, _, ShrunkDirs) :- ShrunkDirs = Dirs.
no_perhapses([], _, _, _, ShrunkDirs) :- ShrunkDirs = [].
no_perhapses([HTree|TTrees], [HPerhapsTree|TPerhapsTrees], [HOriginalDir|TOriginalDirs], LineLimit, ShrunkDirs) :-
    (HTree == HPerhapsTree ->
        create_tents(HTree, HOriginalDir, Tents),
        get_no_correct_dirs(Tents, HOriginalDir, LineLimit, NewDirs),
        ShrunkDirs = [NewDirs|Return],
        no_perhapses(TTrees, TPerhapsTrees, TOriginalDirs, LineLimit, Return)
    ;
        ShrunkDirs = [HOriginalDir|Return],
        no_perhapses(TTrees, [HPerhapsTree|TPerhapsTrees], TOriginalDirs, LineLimit, Return)
    ).

% A bemenő fák, 'esetleg' fák, fákhoz tartozó iránylisták és oszlop/sor kritérium alapján előállítja
% azt a szűkített listát, amibe minden 'esetleg' fa beletartoztik
all_perhapses(_, [], Dirs, _, ShrunkDirs) :- ShrunkDirs = Dirs.
all_perhapses([], _, _, _, ShrunkDirs) :- ShrunkDirs = [].
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

% A bemenő sátrak és oszlop/sor kritérium alapján ellenőrzi, hogy van-e olyan sátor, ami
% nem helyezkedik el a kritérium szerinti sorban/oszlopban
tent_is_out([], _) :- true.

tent_is_out([HTentRow-_|TailTents], sor(Index, _)) :-
    HTentRow \= Index,
    tent_is_out(TailTents, sor(Index, _)).

tent_is_out([_-HTentCol|TailTents], oszl(Index, _)) :-
    HTentCol \= Index,
    tent_is_out(TailTents, oszl(Index, _)).

% A bemenő fa és ahhoz tartozó iránylist alapján előállítja a fához tartozó sátrakat
create_tents(_, [], Tents) :- Tents = [].
create_tents(Tree, [HeadDir|TailDirs], Tents) :-
    create_tent(Tree, HeadDir, Tent),
    Tents = [Tent|Ret],
    create_tents(Tree, TailDirs, Ret).

% A bemenő fák, oszlop/sor kritérium, és iránylisták alapján összegyűjti az összes 'esetleg' fát
get_perhaps_trees([], _, [], Perhapses) :- Perhapses = [].

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
    
% A bemenő fa, oszlop/sor kritérium és fához tartozó irányok alapján ellenőrzi, hogy 
% a fa 'esetleg' fa-e
check_tree_perhaps(Tree, LineLimit, Dirs) :-
    \+check_tree_for_sure(Tree, LineLimit, Dirs),
    create_tents(Tree, Dirs, Tents),
    \+tent_is_out(Tents, LineLimit).

% A bemenő fák, oszlop/sor kritérium és iránylisták alapján összegyűjti az összes 'biztos' fát
get_sure_trees([], _, [], SureTrees) :- SureTrees = [].

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

% A bemenő fa, oszlop/sor kritérium és fához tartozó irányok alapján ellenőrzi, hogy 
% az irányokkal képzett sátrak alapján a fa 'biztos' fa-e
check_tree_for_sure(Tree, LineLimit, Dirs) :-
    length(Dirs, Length),
    Length =< 2,
    tents_are_right(Tree, LineLimit, Dirs).

% A bemenő fa, oszlop/sor kritérium, és fához tartozó irányok alapján ellenőrzi, hogy
% az irányokkal képzett sátrak megfelelnek a kritériumnak
tents_are_right(_, _, []).

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

% A bemenő fa és irány alapján előállítja a fa sátrát
create_tent(TreeRow-TreeCol, n, Tent) :- NewRow is TreeRow - 1, NewRow-TreeCol = Tent.
create_tent(TreeRow-TreeCol, s, Tent) :- NewRow is TreeRow + 1, NewRow-TreeCol = Tent.
create_tent(TreeRow-TreeCol, e, Tent) :- NewCol is TreeCol + 1, TreeRow-NewCol = Tent.
create_tent(TreeRow-TreeCol, w, Tent) :- NewCol is TreeCol - 1, TreeRow-NewCol = Tent.

% Listákat tartalmazó listából elemeket tartalmazó listát csinál
flatten([[H]|T], [H|Tail]) :- flatten(T, Tail).
flatten([], []).
