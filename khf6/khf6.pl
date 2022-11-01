osszeg_szukites(Trees, SumCondition, TreeDirs, ShrunkDirs) :-
    get_sure_trees(Trees, SumCondition, TreeDirs, Sures),
    get_perhaps_trees(Trees, SumCondition, TreeDirs, Perhapses),
    shrunk(Trees, Sures, Perhapses, SumCondition, TreeDirs, ShrunkDirs).

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

check_tree_sure_col(_-TreeCol, ColCount, Dirs) :-
    TreeCol == ColCount,
    length(Dirs, Length),
    Length =< 2,
    \+memberchk(Dirs, [e]),
    \+memberchk(Dirs, [w]).

check_tree_sure_row(TreeRow-_, RowCount, Dirs) :-
    TreeRow == RowCount,
    length(Dirs, Length),
    Length =< 2,
    \+memberchk(Dirs, [n]),
    \+memberchk(Dirs, [s]).

create_tent(TreeRow-TreeCol, Dir, Tent) :-
    (Dir == n , NewRow is TreeRow - 1 , NewRow-TreeCol = Tent);
    (Dir == s , NewRow is TreeRow + 1 , NewRow-TreeCol = Tent);
    (Dir == w , NewCol is TreeCol - 1 , TreeRow-NewCol = Tent);
    (Dir == e , NewCol is TreeCol + 1 , TreeRow-NewCol = Tent).
