#require "core"

type table = string list list
type row = string list
type f_table = string
type ('a, 'b) t = ('a * 'b) list 
type dict = (string, row) t


let data_header = ["Proc"; "t_ready"; "t_cpu"; "started at"; "finished at"; "current state"]
let priority_data_header = ["Proc"; "t_start"; "t_cpu"; "priority"; "started at"; "finished at"]

let example = [ ["A"; "0"; "3"];
                ["B"; "1"; "2"];
                ["C"; "3"; "4"];
                ["D"; "4"; "2"];
                ["E"; "7"; "5"];
                ["F"; "9"; "3"];]

(*FUNCIONES DE FORMATO E IMPRESIÓN*)                     

(*Input: Lista de strings
  Output: Lista de largos de strings*)
let get_lengths (l : row) : int list = 
  List.map ~f:String.length l 

(*Input: Dos listas de enteros de igual tamaño
  Output: Lista con el máximo valor al comparar posición i-ésima de ambas listas*)
let max_lengths (l1 : int list) (l2 : int list) : int list =
  List.map2_exn ~f:Int.max l1 l2

(*Input: Una tabla o matriz
  Output: Los tamaños más grandes de strings encontrados para cada columna, almacenados en lista*)
let row_lengths (header : row) (table : table) : int list =
  let head_lengths = get_lengths header in
  let table_lengths = List.map ~f:get_lengths table in
  List.fold ~init:head_lengths ~f:max_lengths table_lengths

(*Input: Largos máximos de cada columna
  Output: String separador(de la tabla) acorde a los tamaños entregados.*)
let separator (widths : int list) : string =
  let blocks = List.map ~f:(fun x -> String.make (x+2) '-') widths in
  "|" ^ (String.concat ~sep:"+" blocks) ^ "|"  

(*Input: Un string y el tamaño máximo de string en su respectiva columna
  Output: El mismo string, formateado para que use cierto espacio y tenga un espacio al inicio*) 
let put_space (str : string) (len : int) : string =
  " " ^ str ^ String.make (len - (String.length str) + 1) ' '

(*Input: Una fila de la tabla, con los anchos máximos de cada columna
  Output: La fila formateada con separadores "|"*)  
let format_row (row : row) (widths : int list): string  =
  let frow = List.map2_exn row widths ~f:put_space  in
  "|" ^ (String.concat  ~sep:"|" frow) ^ "|"

(*Input: Tabla con cabecera
  Output: Tabla formateada*)
let format_table (header : row) (table : table) : string =
  let widths = row_lengths header table in
  let sep = separator widths in
  let formatted_header = format_row header widths in
  let formatted_table = List.map ~f:(fun r -> format_row r widths) table in
  (String.concat (formatted_header :: sep :: formatted_table) ~sep:"\n")

let print_table x = printf "%s\n" x 

(*IMPLEMENTACION FCFS*)

(*Las siguientes funciones asumen una lista de 3 elementos, con los dos ultimso
  siendo números representados como strings.*)

(*Input: 2 tablas a comparar.
  Output: Resultado de la comparación.*)
let row_compare (r1 : row) (r2 : row) : int =
  let [_;s1] = r1 in
  let [_;s2] = r2 in
  if (int_of_string s1 = int_of_string s2)
  then 0
  else if (int_of_string s1 < int_of_string s2)
  then 1
  else (-1)

(*Input: una tabla(de 2 columnas) y un instante de tiempo
  Output: tablas llenas de espacios blancos para los tiempos anteriores*) 
let rec pre_simulation (data : table) (sim: dict) (t : int) : dict =  
  let rec makeList a n =
    match n with
      | 0 -> [a]
      | _ -> " " :: (makeList a (n-1))
  in
  match data with
    | [] -> sim
    | [proc;_] :: xs -> (proc, (makeList proc t))  :: (pre_simulation xs sim t)  

let double_split (input : string) : table =
  List.map ~f:(String.split_on_chars ~on:[','])
    (String.split_on_chars input ~on:['|'])

let add_input_to_data (input : table) (data : dict) (time : int) : dict =
  let t = string_of_int time in
  let rec add_to_data inp = 
    match inp with
      | [] -> data
      | [proc; t_cpu] :: xs -> (proc,[proc; t; t_cpu; "-"; "-"; "waiting"]) :: add_to_data xs
  in
  add_to_data input 

let rec simulation_header n =
  match n with 
    | 0 -> ["Proc"]
    | _ -> (string_of_int (n-1)) :: (simulation_header (n-1)) 

let rec table_of_dict (d : dict) : table =
  match d with
    | [] -> []
    | (key, row) :: xs -> row :: (table_of_dict xs)

(*let fcfs (sim : dict) (data : dict) (proc : string) (time : int) : dict * dict = 
  *)


    


let main : unit =
  let flag = ref true in
  let current_time = ref 0 in
  let current_proc = ref "-" in
  let data_dict = ref [] in
  let data_table = ref [] in
  let sim_dict = ref [] in
  let sim_table = ref [] in
  let sim_header = ref [] in
  let input = ref [] in
  printf "FCFS algorithm simulation by: Cristian Urbina\n\n";
  printf "At every iteration(time) you can add how many processes as you want.\n";
  printf "The format is: name1,cpu_time1|name2,cpu_time2 ... etc.\n";
  printf "This method very easy and fast to use, but also very susceptible to fail\n";
  printf "if input doesn't match with the format, so be careful.\n\n";
  while !flag = true do
    printf "Current time = %d\n" !current_time;
    printf "Current proc = %s\n" !current_proc;
    printf "Processes to add: \n";

     (*Es necesario leer la entrada del usuario aqui*)
    input := double_split "SuperProc,2|AnotherProc,2|C,1|D,7";
    input := List.sort row_compare !input;

    data_dict := add_input_to_data !input !data_dict !current_time;
    data_table := List.rev (table_of_dict !data_dict);
    print_table (format_table data_header !data_table);
    printf "\n\n";

    sim_header := simulation_header !current_time;
    sim_dict := pre_simulation !input !sim_dict !current_time;
    sim_table := List.rev (table_of_dict !sim_dict); 
    print_table (format_table !sim_header !sim_table);

    incr current_time;
    flag := false
  done;