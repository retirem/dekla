defmodule Khf2 do

    @moduledoc """
    Kemping térképe
    @author "Reiter Márton marci.reiter501@gmail.com"
    @date   "2022-10-01"
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

    @spec to_external(pd::puzzle_desc, ds::tent_dirs, file::String.t) :: :ok
    # A pd = {rs, cs, ts} feladványleíró és a ds sátorirány-lista alapján
    # a feladvány szöveges ábrázolását írja ki a file fájlba, ahol
    #   rs a sátrak soronkénti számának a listája,
    #   cs a sátrak oszloponkénti számának a listája,
    #   ts a fákat tartalmazó parcellák koordinátájának listája

    @spec gen_matrice(rowNumbers::[integer], colNumbers::[integer], tentCells::[field]) :: [[String.t]]
    # Egy az kemping leírásának megfelelő mátrixot készít * és - jelölésekkel a megfelelő cellákban

    @spec to_directions(currentIndex::integer, listToModify::[String.t], tentCells::trees, ds::tent_dirs, columnWidth::integer) :: [String.t]
    # A kapott egyszerű listán a megfelelő elemeket a négy égtáj atomnak megfelelően lecseréli a paraméter 
    # sátor cellák alapján
    # Rekurzívan hívja saját magát. Ha már nincs több sátor cella, amit megnézzen, akkor visszaadja a módosított listát

    @spec to_file(file::String.t, colNumbers::[integer], rowNumbers::[integer], filledMatrice::[[String.t]]) :: :ok
    # Fájlba írja a sor- és oslopszámokat, illetve az égtájakkal kitöltött kemping térképet

    defp to_file(file, colNumbers, rowNumbers, filledMatrice) do
        File.write!(file, "   ")

        for(
            colNum <- colNumbers,
            do: (
                if colNum >= 0 do
                    File.write!(file, " ", [:append])
                end
                File.write!(file, Integer.to_string(colNum), [:append])
                File.write!(file, "  ", [:append])
            )
        )

        File.write!(file, "\n", [:append])

        for(
            index <- 0..length(rowNumbers) - 1,
            row = Enum.at(filledMatrice, index),
            rowNum = Enum.at(rowNumbers, index),
            do: (
                if rowNum >= 0 do
                    File.write!(file, " ", [:append])
                end
                File.write!(file, Integer.to_string(rowNum), [:append])
                File.write!(file, "  ", [:append])
                for(
                    element <- row,
                    do: (
                        File.write!(file, element, [:append])
                        File.write!(file, "   ", [:append])
                    )
                )
                if index < length(rowNumbers) - 1 do
                    File.write!(file, "\n", [:append])
                end
            )
        )
        :ok
    end

    defp to_directions(currentIndex, listToModify, tentCells, ds, columnWidth) when currentIndex > 0 do
        cell = Enum.at(tentCells, currentIndex)
        direction = Enum.at(ds, currentIndex)
        cond do 
            direction == :e ->
                to_directions(currentIndex - 1, List.replace_at(listToModify, (elem(cell, 0) - 1) * columnWidth + elem(cell, 1) - 1 + 1, "E"), tentCells, ds, columnWidth)
            direction == :s ->
                to_directions(currentIndex - 1, List.replace_at(listToModify, (elem(cell, 0) - 1) * columnWidth + elem(cell, 1) - 1 + columnWidth, "S"), tentCells, ds, columnWidth)
            direction == :w ->
                to_directions(currentIndex - 1, List.replace_at(listToModify, (elem(cell, 0) - 1) * columnWidth + elem(cell, 1) - 1 - 1, "W"), tentCells, ds, columnWidth)
            direction == :n ->
                to_directions(currentIndex - 1, List.replace_at(listToModify, (elem(cell, 0) - 1) * columnWidth + elem(cell, 1) - 1 - columnWidth, "N"), tentCells, ds, columnWidth)
        end
    end

    defp to_directions(0, listToModify, tentCells, ds, columnWidth) do
        cell = Enum.at(tentCells, 0)
        direction = Enum.at(ds, 0)
        cond do 
            direction == :e ->
                List.replace_at(listToModify, (elem(cell, 0) - 1) * columnWidth + elem(cell, 1) - 1 + 1, "E")
            direction == :s ->
                List.replace_at(listToModify, (elem(cell, 0) - 1) * columnWidth + elem(cell, 1) - 1 + columnWidth, "S")
            direction == :w ->
                List.replace_at(listToModify, (elem(cell, 0) - 1) * columnWidth + elem(cell, 1) - 1 - 1, "W")
            direction == :n ->
                List.replace_at(listToModify, (elem(cell, 0) - 1) * columnWidth + elem(cell, 1) - 1 - columnWidth, "N")
        end
    end

    defp gen_matrice(rowNumbers, colNumbers, tentCells) do
        for(
            rowIndex <- 0..length(rowNumbers) - 1,
            do: (
                for(
                    colIndex <- 0..length(colNumbers) - 1,
                    do: (
                        if Enum.member?(tentCells, {rowIndex + 1, colIndex + 1}) do 
                            "*"
                        else
                            "-"
                        end
                    )
                )
            )
        )
    end

    def to_external(pd, ds, file) do
        
        {rowNumbers, colNumbers, tentCells} = pd

        matrice = gen_matrice(rowNumbers, colNumbers, tentCells)

        flatMatrice = List.flatten(matrice)

        editedMatrice = to_directions(length(tentCells) - 1, flatMatrice, tentCells, ds, length(colNumbers))

        filledMatrice = editedMatrice |> Enum.chunk_every(length(colNumbers))
        
        to_file(file, colNumbers, rowNumbers, filledMatrice)
    end
end
