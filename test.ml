(* Define the data *)
let x = [| 1.0; 2.0; 3.0; 4.0; 5.0 |]
let y = [| 1.0; 4.0; 9.0; 16.0; 25.0 |]

(* Write the data to a file *)
let out_channel = open_out "data.txt"
let () =
  for i = 0 to Array.length x - 1 do
    Printf.fprintf out_channel "%f %f\n" x.(i) y.(i)
  done;
close_out out_channel

(* Plot the data using gnuplot *)
let () =
  let plot_cmd = "plot 'data.txt' with lines" in
  let gnuplot_cmd = Printf.sprintf "gnuplot -e \"%s\"" plot_cmd in
  ignore (Sys.command gnuplot_cmd)

(* Generate the image from the plot *)
let () =
  let convert_cmd = "convert -density 300 -quality 100 plot.eps plot.png" in
  ignore (Sys.command convert_cmd)
