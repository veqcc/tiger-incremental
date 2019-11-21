structure Main =
struct
  fun compile(file: string) = let in
    let val ast = Parse.parse file in
      print "parse =>\n";
      PrintAbsyn.print(TextIO.stdOut, ast)
    end
  end

  fun main(cmd: string, args: string list): OS.Process.status = let in
    app compile args;
    0
  end
end
