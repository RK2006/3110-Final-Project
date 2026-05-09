(* AF: a tabular Q-learner / SARSA learner. [q.(s).(a)] is our current guess of
   Q(s,a). [alpha] is the learning rate, [gamma] the discount, and [epsilon]
   the chance of acting randomly during training. The same Q-table supports
   both Q-learning ([update]) and SARSA ([update_sarsa]) updates.

   RI: [num_states, num_actions > 0]; [q] is shaped [num_states x num_actions];
   [alpha], [gamma], [epsilon] all live in [[0, 1]]. *)
type t = {
  num_states : int;
  num_actions : int;
  alpha : float;
  gamma : float;
  mutable epsilon : float;
  q : float array array;
}

let create ~num_states ~num_actions ~alpha ~gamma ~epsilon =
  {
    num_states;
    num_actions;
    alpha;
    gamma;
    epsilon;
    q = Array.init num_states (fun _ -> Array.make num_actions 0.0);
  }

let max_q t s = Array.fold_left Float.max Float.neg_infinity t.q.(s)

let act t ~state ~training =
  if training && Random.float 1.0 < t.epsilon then Random.int t.num_actions
  else begin
    let row = t.q.(state) in
    let best = ref 0 in
    let best_v = ref Float.neg_infinity in
    Array.iteri
      (fun a v ->
        if v > !best_v then begin
          best := a;
          best_v := v
        end)
      row;
    !best
  end

let update t ~state ~action ~reward ~next_state ~terminal =
  let q_sa = t.q.(state).(action) in
  let future = if terminal then 0.0 else t.gamma *. max_q t next_state in
  let td = reward +. future -. q_sa in
  t.q.(state).(action) <- q_sa +. (t.alpha *. td)

let update_sarsa t ~state ~action ~reward ~next_state ~next_action ~terminal =
  let q_sa = t.q.(state).(action) in
  let future =
    if terminal then 0.0 else t.gamma *. t.q.(next_state).(next_action)
  in
  let td = reward +. future -. q_sa in
  t.q.(state).(action) <- q_sa +. (t.alpha *. td)

let decay_epsilon t ~rate ~min_epsilon =
  t.epsilon <- Float.max min_epsilon (t.epsilon *. (1.0 -. rate))

let epsilon t = t.epsilon

let save t path =
  let oc = open_out path in
  Printf.fprintf oc "%d,%d,%h,%h,%h\n" t.num_states t.num_actions t.alpha
    t.gamma t.epsilon;
  for s = 0 to t.num_states - 1 do
    for a = 0 to t.num_actions - 1 do
      Printf.fprintf oc "%d,%d,%h\n" s a t.q.(s).(a)
    done
  done;
  close_out oc

let load path =
  let ic = open_in path in
  let hdr = input_line ic in
  let parts p = String.split_on_char ',' p in
  match parts hdr with
  | [ ns; na; al; gm; ep ] ->
      let num_states = int_of_string (String.trim ns) in
      let num_actions = int_of_string (String.trim na) in
      let alpha = float_of_string (String.trim al) in
      let gamma = float_of_string (String.trim gm) in
      let epsilon = float_of_string (String.trim ep) in
      let q = Array.init num_states (fun _ -> Array.make num_actions 0.0) in
      (try
         while true do
           let line = input_line ic in
           match parts line with
           | [ s; a; v ] ->
               q.(int_of_string (String.trim s)).(int_of_string (String.trim a)) <-
                 float_of_string (String.trim v)
           | _ -> failwith ("Malformed line: " ^ line)
         done
       with End_of_file -> ());
      close_in ic;
      { num_states; num_actions; alpha; gamma; epsilon; q }
  | _ ->
      close_in ic;
      failwith ("Malformed header in Q-table file: " ^ path)
