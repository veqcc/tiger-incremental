structure Main =
struct
  fun compile(file: string) =
    let
      val absyn = Parse.parse file
      val tree = Semant.transExp absyn
    in
      print "--\n";
      PrintAbsyn.print(TextIO.stdOut, absyn);
      print "--\n";
      PrintTree.print(TextIO.stdOut, tree)
  end

  fun main(cmd: string, args: string list): OS.Process.status = let in
    app compile args;
    0
  end
end
