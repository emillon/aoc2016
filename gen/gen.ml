open Stdio

let () =
  for i = 1 to int_of_string Sys.argv.(1) do
    printf
      {|
(executable
 (name run%02d)
 (modules run%02d)
 (libraries aoc2016))

(rule
 (action
  (write-file run%02d.ml "let () = Aoc2016.Day%02d.run ()")))

(rule
 (action
  (with-stdout-to
   day%02d_p1.txt.gen
   (run ./run%02d.exe --part 1))))

(rule
 (alias runtest)
 (action
  (diff ../day%02d_p1.txt day%02d_p1.txt.gen)))

(rule
 (action
  (with-stdout-to
   day%02d_p2.txt.gen
   (run ./run%02d.exe --part 2))))

(rule
 (alias runtest)
 (action
  (diff ../day%02d_p2.txt day%02d_p2.txt.gen)))
  |}
      i
      i
      i
      i
      i
      i
      i
      i
      i
      i
      i
      i
  done
;;
