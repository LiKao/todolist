TYPE_CONV_PATH "Current.Module.Name"

type repetition = Daily
                | Weekly of Date.weekday
								| Monthly of Date.dayofmonth
								| Weekdays
								| Weekends
								with sexp

type todotime = Repeated of repetition
              | Single   of Date.date
							with sexp

type tododata = {duetime : todotime;
                 subject : string}
								with sexp

(* States of todos as phantom types to allow generic type*)
(* checked data bases *)																
type pending
type closed
						
(* The actual type of todos 'a can be pending or closed *)																								
type 'a todo = tododata with sexp

type 'a todolist = ('a todo) list with sexp

let add_todo todo todolist    = todo :: todolist
let delete_todo todo todolist = List.filter (fun x -> x != todo) todolist




              
         