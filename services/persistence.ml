(** Wrapper to persistent data structures, to allow abstraction **)

open Lwt

type 'a t = {data: 'a Ocsipersist.t Lwt.t;
             mutex: Lwt_mutex.t}
  
let store = Ocsipersist.open_store "TodoDB"
  
let create name default = 
  {data = Ocsipersist.make_persistent store name default;
   mutex = Lwt_mutex.create ()}
  
let access reference funct =
  reference.data >>= fun v ->
  Lwt_mutex.lock reference.mutex >>= fun () ->
  Ocsipersist.get v >>= fun value ->
  return (funct value) >>= fun res ->
  Lwt_mutex.unlock reference.mutex;
  return res
  
let change reference funct =
  reference.data >>= fun v ->
  Lwt_mutex.lock reference.mutex >>= fun () ->
  Ocsipersist.get v >>= fun value ->
  Ocsipersist.set v (funct value) >>= fun () ->
  Lwt_mutex.unlock reference.mutex;
  return ()