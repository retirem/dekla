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
