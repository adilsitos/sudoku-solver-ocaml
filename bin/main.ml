let square grid x y = 
  let min_x = (x / 3) * 3 in 
  let min_y = (y / 3) * 3 in 
  let arr = Array.init 3 (fun _ -> Array.make 3 0) in
  let y_index = ref 0 in 
  (*rows*)
  for y_pos = min_y to min_y + 2 do 
    (*columns*)
    let x_index = ref 0 in 
    for x_pos = min_x to min_x + 2 do 
      arr.(!y_index).(!x_index) <- grid.(y_pos).(x_pos);
      x_index := !x_index + 1 
    done;
    y_index := !y_index + 1 
  done;
  arr

  

let squares grid = 
  let sub_arr = Array.init 3 (fun _ -> Array.make 3 0) in
  let arr = Array.init 9 (fun _ -> sub_arr) in 
  let index = ref 0 in
  for y = 0 to 2 do 
    for x = 0 to 2 do 
      let res = square grid (x * 3) (y*3) in 
      arr.(!index) <- res; 
      index := !index + 1;  
    done;
  done;
  arr


(* let is_valid grid row col num = 
  (*check if the number exists in the row*)
  let row_ok = 
    not (Array.exists (fun x -> x = num)  grid.(row)) 
  in 

  (*check if the number exists in the column*)
  let col_ok = 
    not (Array.exists (fun r -> r.(col) = num) grid) 
  in 

  let block_start_row = (row/3) * 3 in 
  let block_start_col = (col/3) * 3 in

  let block_ok = 
    let valid = ref true in 

  true *)



let print_grid grid =
  print_newline();
  for i = 0 to Array.length grid - 1 do
    for j = 0 to Array.length grid.(0) - 1 do
      Printf.printf "%d " grid.(i).(j)
    done;
    print_newline ();
  done


let () = 
  let grid = 
    [|
      [|5; 3; 0; 0; 7; 0; 0; 0; 0|];
      [|6; 0; 0; 1; 9; 5; 0; 0; 0|];
      [|0; 9; 8; 0; 0; 0; 0; 6; 0|];
      [|8; 0; 0; 0; 6; 0; 0; 0; 3|];
      [|4; 0; 0; 8; 0; 3; 0; 0; 1|];
      [|7; 0; 0; 0; 2; 0; 0; 0; 6|];
      [|0; 6; 0; 0; 0; 0; 2; 8; 0|];
      [|0; 0; 0; 4; 1; 9; 0; 0; 5|];
      [|0; 0; 0; 0; 8; 0; 0; 7; 9|]
    |]
  in 
  Array.iter print_grid (squares grid)  
  
  (* if solve grid then print_grid grid 
  else Printf.printf "No solution exists!!\n" *)