defmodule Nhf1 do

  @moduledoc """
  Kemping
  @author "Reiter Márton marci.reiter501@gmail.com"
  @date   "2022-10-15"
  ...
  """

  @type row   :: integer    # sor száma (1-től n-ig)
  @type col   :: integer    # oszlop száma (1-től m-ig)
  @type field :: {row, col} # egy parcella koordinátái

  @type tents_count_rows :: [integer] # a sátrak száma soronként
  @type tents_count_cols :: [integer] # a sátrak száma oszloponként

  @type trees       :: [field]   # a fákat tartalmazó parcellák koordinátái lexikálisan rendezve
  @type puzzle_desc :: {tents_count_rows, tents_count_cols, trees} # a feladványleíró hármas
  
  @type dir       :: :n | :e | :s | :w # a sátorpozíciók iránya: north, east, south, west
  @type tent_dirs :: [dir]             # a sátorpozíciók irányának listája a fákhoz képest

  @spec satrak(pd::puzzle_desc) :: tss::[tent_dirs]
  # tss a pd feladványleíróval megadott feladvány összes megoldásának listája, tetszőleges sorrendben

  @spec check_sol(pd::puzzle_desc, ds::tent_dirs) :: boolean()
  # Az {rs, cs, ts} = pd feladványleíró és a ds sátorirány-lista
  # alapján elvégzett ellenőrzés eredménye a cd hibaleíró, ahol
  #   rs a sátrak soronként elvárt számának a listája,
  #   cs a sátrak oszloponként elvárt számának a listája,
  #   ts a fákat tartalmazó parcellák koordinátájának a listája
  # Az {e_rows, e_cols, e_touch} = ed hármas elemei olyan
  # kulcs-érték párok, melyekben a kulcs a hiba jellegére utal, az
  # érték pedig a hibahelyeket felsoroló lista (üres, ha nincs hiba)

  @spec summary(numbers::[integer], row::boolean(), tents::[field]) :: [integer]
  # Összegzi az adott sorban vagy oszlopban található sátrak számát és
  # visszaadja egy listában

  @spec compare(numbers::[integer], summary::[integer]) :: [integer]
  # Összehasonlítja a várt és a kapott sátor mennyiséget adott sorban/oszlopban
  # és visszaadja a nem egyező sor- vagy oszlopszámokat

  @spec check_dir(rowNumbers::[integer], colNumbers::[integer], coord::field, trees::trees, val::dir) :: dir
  # Megvizsgálja, hogy a kapott val opcionális iránya-e a coord-nak. Tehát nem 
  # ütközik másik fával, és nem megy ki a mezőről

  @spec collision(new_tent::field, current_tents::[field]) :: boolean()
  # Kiszámolja a fákhoz tartozó sátrak helyét, amennyiben létezik a fához irány, majd megnézi, 
  # hogy a legutolsó sátor pozíciója ütközik-e valamelyik másik sátoréval

  @spec solve(trees::trees, rowNumbers::[integer], colNumbers::[integer], chosen_dirs::tent_dirs, pure_trees::trees, current_tents::[field]) :: [tent_dirs]
  # Segédfüggvénye a satrak-nak. Rekurzívan végigmegy a fák helyzetén, és a hozzájuk tartozó irányokon. 
  # Minden kombinációt letesztel, majd ha üres listára jut, mert végig ért az összes fán és 
  # mindegyikhez talált egy lehetséges irányt, akkor visszaadja a kigyűjtött irányokat. Ha valahol 
  # olyan irányra lépett, ami ütközéshez vezet, akkor üres listát ad vissza.

  @spec calculate_dirs(rowNumbers::[integer], colNumbers::[integer], tree::field, trees::trees) :: tent_dirs 

  @spec check_summary(tree::field, rowNumbers::[integer], colNumbers::[integer], current_tents::[field], dir::dir) :: boolean()

  @spec line_can_accept(line_value::integer, tents_in_line::[field]) :: boolean()

  defp compare(numbers, summary) do 
    for(
        index <- 0..length(numbers) - 1,
        num = Enum.at(numbers, index),
        sum = Enum.at(summary, index),
        num != sum,
        do: (
            index + 1
        )
    )
  end

  defp summary(numbers, row, tents) do
    for(
        index <- 0..length(numbers) - 1,
        num = Enum.at(numbers, index),
        do: (
            if num >= 0 do
                length(tents |> Enum.filter(&(elem(&1, row && 0 || 1) == index + 1)))
            else
                num
            end
        )
    )
  end

  def check_sol(pd, []) do 
    {rowNumbers, colNumbers, _} = pd
    rows_temp = for(
                    rowIndex <- 0..length(rowNumbers) - 1,
                    item = Enum.at(rowNumbers, rowIndex),
                    item != 0,
                    do: (
                        rowIndex + 1
                    )
                )

    col_temp = for(
                    colIndex <- 0..length(colNumbers) - 1,
                    item = Enum.at(colNumbers, colIndex),
                    item != 0,
                    do: (
                        colIndex + 1
                    )
                )
    length(rows_temp) == 0 && length(col_temp) == 0
  end

  def check_sol(pd, ds) do
        {rowNumbers, colNumbers, treeCells} = pd

        tents = for(
            treeIndex <- 0..length(treeCells) - 1,
            tree = Enum.at(treeCells, treeIndex),
            dir = Enum.at(ds, treeIndex),
            do: (
                cond do
                    dir == :w ->
                        {elem(tree, 0), elem(tree, 1) - 1}
                    dir == :e ->
                        {elem(tree, 0), elem(tree, 1) + 1}
                    dir == :s ->
                        {elem(tree, 0) + 1, elem(tree, 1)}
                    dir == :n ->
                        {elem(tree, 0) - 1, elem(tree, 1) }
                end
            )
        )

        rowSum = summary(rowNumbers, true, tents)
        colSum = summary(colNumbers, false, tents)
        badRows = compare(rowNumbers, rowSum)
        badCols = compare(colNumbers, colSum)
        
        touches = for(
            tent <- tents,
            tent2 <- tents,
            tent != tent2,
            do: (
                {x1, y1} = tent;
                {x2, y2} = tent2
                
                if (x1 + 1 == x2 && y1 == y2) ||  
                (x1 - 1 == x2 && y1 == y2) ||
                (y1 + 1 == y2 && x1 == x2) ||
                (y1 - 1 == y2 && x1 == x2) ||
                (x1 + 1 == x2 && y1 + 1 == y2) ||
                (x1 + 1 == x2 && y1 - 1 == y2) ||
                (x1 - 1 == x2 && y1 + 1 == y2) ||
                (x1 - 1 == x2 && y1 - 1 == y2) do
                    tent
                end
            ) 
        )
        sanitizedTouches = touches |> Enum.uniq |> Enum.reject(&is_nil/1) #|> Enum.sort_by(&(elem(&1, 0)))
        length(badRows) == 0 && length(badCols) == 0 && length(sanitizedTouches) == 0
  end

  defp check_dir(rowNumbers, colNumbers, coord, trees, val) when val == :n do 
    if elem(coord, 0) > 0 && Enum.at(rowNumbers, elem(coord, 0) - 1) != 0 && Enum.at(colNumbers, elem(coord, 1) - 1) != 0 && !(coord in trees) do
      val
    end
  end

  defp check_dir(rowNumbers, colNumbers, coord, trees, val) when val == :s do 
    if elem(coord, 0) <= length(rowNumbers) && Enum.at(rowNumbers, elem(coord, 0) - 1) != 0 && Enum.at(colNumbers, elem(coord, 1) - 1) != 0 && !(coord in trees) do
      val
    end
  end

  defp check_dir(rowNumbers, colNumbers, coord, trees, val) when val == :e do 
    if elem(coord, 1) <= length(colNumbers) && Enum.at(rowNumbers, elem(coord, 0) - 1) != 0 && Enum.at(colNumbers, elem(coord, 1) - 1) != 0 && !(coord in trees) do
      val
    end
  end

  defp check_dir(rowNumbers, colNumbers, coord, trees, val) when val == :w do 
    if elem(coord, 1) > 0 && Enum.at(rowNumbers, elem(coord, 0) - 1) != 0 && Enum.at(colNumbers, elem(coord, 1) - 1) != 0 && !(coord in trees) do
      val
    end
  end

  defp collision(new_tent, current_tents) do
    if(new_tent in current_tents) do
      true
    else
      touches = for(
            tent <- current_tents,
            {x2, y2} = new_tent,
            do: (
                {x1, y1} = tent;
                if (x1 + 1 == x2 && y1 == y2) ||  
                (x1 - 1 == x2 && y1 == y2) ||
                (y1 + 1 == y2 && x1 == x2) ||
                (y1 - 1 == y2 && x1 == x2) ||
                (x1 + 1 == x2 && y1 + 1 == y2) ||
                (x1 + 1 == x2 && y1 - 1 == y2) ||
                (x1 - 1 == x2 && y1 + 1 == y2) ||
                (x1 - 1 == x2 && y1 - 1 == y2) do
                    tent
                end
            ) 
        )
      length(Enum.filter(touches, fn(x) -> x != nil end)) > 0
    end
  end

  defp create_tent(tree, dir) do
    cond do
      dir === :n ->
        {elem(tree, 0) - 1, elem(tree, 1)}
      dir == :s ->
        {elem(tree, 0) + 1, elem(tree, 1)}
      dir == :e ->
        {elem(tree, 0), elem(tree, 1) + 1}
      dir == :w ->
        {elem(tree, 0), elem(tree, 1) - 1}
      true ->
        {}
    end
  end

  defp calculate_dirs(rowNumbers, colNumbers, tree, trees) do
    [check_dir(rowNumbers, colNumbers, {elem(tree, 0) - 1, elem(tree, 1)}, trees, :n), 
    check_dir(rowNumbers, colNumbers, {elem(tree, 0) + 1, elem(tree, 1)}, trees, :s), 
    check_dir(rowNumbers, colNumbers, {elem(tree, 0), elem(tree, 1) + 1}, trees, :e), 
    check_dir(rowNumbers, colNumbers, {elem(tree, 0), elem(tree, 1) - 1}, trees, :w)] 
    |> Enum.uniq |> Enum.reject(fn(x) -> is_nil(x) end)
  end

  defp line_can_accept(line_value, tents_in_line) do
    if line_value < 0 do
      true
    else
      if line_value == 0 do 
        false
      else
        if tents_in_line + 1 <= line_value do
          true
        else
          false
        end
      end
    end
  end

  defp check_summary(tree, rowNumbers, colNumbers, current_tents, dir) do
    possible_tent = create_tent(tree, dir)

    row_value = Enum.at(rowNumbers, elem(possible_tent, 0) - 1)
    col_value = Enum.at(colNumbers, elem(possible_tent, 1) - 1)

    tents_in_row = Enum.filter(current_tents, fn(x) -> elem(x, 0) == elem(possible_tent, 0) end)
    tents_in_col = Enum.filter(current_tents, fn(x) -> elem(x, 1) == elem(possible_tent, 1) end)

    line_can_accept(row_value, length(tents_in_row)) && line_can_accept(col_value, length(tents_in_col))
  end

  defp solve([], _rowNumbers, _colNumbers, chosen_dirs, _pure_trees, _current_tents) do
    chosen_dirs
  end

  defp solve(trees, rowNumbers, colNumbers, chosen_dirs, pure_trees, current_tents) do
    [tree | tail] = trees
    
    dirs_for_tree = calculate_dirs(rowNumbers, colNumbers, tree, pure_trees) 
    |> Enum.filter(fn(x) -> !collision(create_tent(tree, x), current_tents) end)
    |> Enum.filter(fn(x) -> check_summary(tree, rowNumbers, colNumbers, current_tents, x) end)

    for(
      dir <- dirs_for_tree,
      do: (
        tent = create_tent(tree, dir)
        if !collision(tent, current_tents) do
          solve(tail, rowNumbers, colNumbers, [dir | chosen_dirs], pure_trees, [tent | current_tents])
        else
          []
        end
      )
    )
  end

  def satrak(pd) do
    {rowNumbers, colNumbers, trees} = pd

    List.flatten(solve(trees, rowNumbers, colNumbers, [], trees, [])) |> Enum.reverse |> Enum.chunk_every(length(trees)) |> Enum.filter(fn(x) -> check_sol(pd, x) end)
  end
end

#IO.inspect Nhf1.satrak {[3, 3, 2, 3, 4, 3, 3, 1, 7, 1, 3, 2, 2, 4, 2],[2, 2, 1, 2, 4, 2, 4, 1, 2, 4, 0, 2, 1, 3, 1, 3, 2, 1, 2, 4],[{1, 3},{1, 6},{1, 9},{1, 14},{1, 19},{3, 1},{3, 11},{3, 16},{4, 5},{4, 13},{4, 20},{5, 11},{5, 16},{5, 19},{6, 7},{6, 13},{6, 18},{7, 6},{7, 10},{7, 12},{7, 20},{8, 2},{8, 5},{8, 9},{9, 16},{9, 19},{10, 4},{10, 6},{10, 14},{10, 20},{11, 3},{11, 11},{11, 17},{12, 5},{12, 19},{13, 3},{13, 7},{13, 9},{14, 2},{14, 8},{14, 9},{15, 4},{15, 16}]}
