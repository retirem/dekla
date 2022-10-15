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
  # Egy fához tartozó lehetséges irányok listáját adja meg, figyelembe véve, hogy ne menjen ki 
  # a jövőbeli sátor a mezőről, illetve, hogy egy sorba és oszlopba csak akkor lehessen letenni egy sátrat, 
  # ha van még hely

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

  defp check_sol(pd, ds) do
        {rowNumbers, colNumbers, treeCells} = pd

        tents = for(
            treeIndex <- 0..length(treeCells) - 1,
            tree = Enum.at(treeCells, treeIndex),
            dir = Enum.at(ds, treeIndex),
            do: (
              create_tent(tree, dir)
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
                
                if abs(x1 - x2) <= 1 && abs(y1 - y2) <= 1 do
                    tent
                end
            ) 
        )
        sanitizedTouches = touches |> Enum.uniq |> Enum.reject(&is_nil/1)
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
                if abs(x1 - x2) <= 1 && abs(y1 - y2) <= 1 do
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

  defp solve([], _rowNumbers, _colNumbers, chosen_dirs, _pure_trees, _current_tents) do
    chosen_dirs
  end

  defp solve(trees, rowNumbers, colNumbers, chosen_dirs, pure_trees, current_tents) do
    [tree | tail] = trees
    
    dirs_for_tree = calculate_dirs(rowNumbers, colNumbers, tree, pure_trees) 
    |> Enum.filter(fn(x) -> !collision(create_tent(tree, x), current_tents) end)

    for(
      dir <- dirs_for_tree,
      do: (
        tent = create_tent(tree, dir);
        solve(tail, 
        List.replace_at(rowNumbers, elem(tent, 0) - 1, Enum.at(rowNumbers, elem(tent, 0) - 1) - 1),
        List.replace_at(colNumbers, elem(tent, 1) - 1, Enum.at(colNumbers, elem(tent, 1) - 1) - 1),
        [dir | chosen_dirs], pure_trees, [tent | current_tents])
      )
    )
  end

  def satrak(pd) do
    {rowNumbers, colNumbers, trees} = pd
    List.flatten(solve(trees, rowNumbers, colNumbers, [], trees, [])) |> Enum.reverse |> Enum.chunk_every(length(trees)) |> Enum.filter(fn(x) -> check_sol(pd, x) end)
  end
end
