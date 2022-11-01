osszeg_szukites(Trees, SumCondition, TreeDirs, Sure, Perhapse) :-
    Sure = Sures,
    get_sure_dirs(Trees, SumCondition, TreeDirs, Sures),
    Perhapse = Perhapses,
    get_perhapse_dirs(Trees, SumCondition, TreeDirs, Perhapses).
    /*ShrunkDirs = ReturnShrunks,
    shrunk(Sures, Perhapses, TreeDirs, ReturnShrunks).*/

get_perhapse_dirs([HeadTree|TailTrees], oszl(ColCount, ColNum), [HeadTreeDirs|TailTreeDirs], Perhapses) :-
    (check_tree_perhaps_col(HeadTree, ColCount, HeadTreeDirs) ->
        Perhapses = [HeadTreeDirs|Ret],
        get_perhapse_dirs(TailTrees, oszl(ColCount, ColNum), TailTreeDirs, Ret)
    ;
        Perhapses = Ret,
        get_perhapse_dirs(TailTrees, oszl(ColCount, ColNum), TailTreeDirs, Ret)
    ).

check_tree_perhaps_col(Tree, ColCount, Dirs) :-
    \+check_tree_sure_col(Tree, ColCount, Dirs),
    create_tents(Tree, Dirs, Tents),
    no_collision_tent_col(Tents, ColCount).

check_tree_perhaps_row(Tree, RowCount, Dirs) :-
    \+check_tree_sure_row(Tree, RowCount, Dirs),
    create_tents(Tree, Dirs, Tents),
    no_collision_tent_row(Tents, RowCount).

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

get_perhapse_dirs([HeadTree|TailTrees], sor(RowCount, RowNum), [HeadTreeDirs|TailTreeDirs], Perhapses) :-
    (check_tree_perhaps_row(HeadTree, RowCount, HeadTreeDirs) ->
        Perhapses = [HeadTreeDirs|Ret],
        get_perhapse_dirs(TailTrees, row(RowCount, RowNum), TailTreeDirs, Ret)
    ;
        Perhapses = Ret,
        get_perhapse_dirs(TailTrees, row(RowCount, RowNum), TailTreeDirs, Ret)
    ).

get_perhapse_dirs([], _, [], Perhapses) :-
    Perhapses = [].

get_sure_dirs([HeadTree|TailTrees], oszl(ColCount, ColNum), [HeadTreeDirs|TailTreeDirs], Sures) :-
    (check_tree_sure_col(HeadTree, ColCount, HeadTreeDirs) ->
        Sures = [HeadTreeDirs|Ret],
        get_sure_dirs(TailTrees, oszl(ColCount, ColNum), TailTreeDirs, Ret)
    ;
        Sures = Ret,
        get_sure_dirs(TailTrees, oszl(ColCount, ColNum), TailTreeDirs, Ret)
    ).

get_sure_dirs([HeadTree|TailTrees], sor(RowCount, RowNum), [HeadTreeDirs|TailTreeDirs], Sures) :-
    (check_tree_sure_row(HeadTree, RowCount, HeadTreeDirs) ->
        Sures = [HeadTreeDirs|Ret],
        get_sure_dirs(TailTrees, sor(RowCount, RowNum), TailTreeDirs, Ret)
    ;
        Sures = Ret,
        get_sure_dirs(TailTrees, sor(RowCount, RowNum), TailTreeDirs, Ret)
    ).

get_sure_dirs([], _, [], Sures) :-
    Sures = [].

check_tree_sure_col(_-TreeCol, ColCount, Dirs) :-
    TreeCol == ColCount,
    length(Dirs, Length),
    Length =< 2,
    \+memberchk(Dirs, e),
    \+memberchk(Dirs, w).

check_tree_sure_row(TreeRow-_, RowCount, Dirs) :-
    TreeRow == RowCount,
    length(Dirs, Length),
    Length =< 2,
    \+memberchk(Dirs, n),
    \+memberchk(Dirs, s).

create_tent(TreeRow-TreeCol, Dir, Tent) :-
    (Dir == n , NewRow is TreeRow - 1 , NewRow-TreeCol = Tent);
    (Dir == s , NewRow is TreeRow + 1 , NewRow-TreeCol = Tent);
    (Dir == w , NewCol is TreeCol - 1 , TreeRow-NewCol = Tent);
    (Dir == e , NewCol is TreeCol + 1 , TreeRow-NewCol = Tent).
