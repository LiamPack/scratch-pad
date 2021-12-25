(** Andrey, Vadim, Karpusenko (2013): "Test-Driving Intel Xeon-Phi Coprocessors
   with a Basic N-Body Simulation."  *)
(* #require "owl, owl-ode, owl-plplot";;*)

open Owl

(* make sure that the denominator of r^hat / r^3 doesn't bottom out to 0 *)
let numerical_zero = Mat.of_array [| 1E-13 |] 1 1
let masses = [| 1.; 0.1; 0.01|]

let bodies = Mat.of_arrays [| [| 0.; 0. |]; [| 1.; 0.|]; [| -1.; 0.|]|]
let bodiesVs = Mat.of_arrays [| [| 0.01; 0. |]; [| 0.; 1.|]; [| 0.; -1.|]|]


let f_1 planets =
  let force = Mat.(zeros (row_num planets) (col_num planets)) in
  for i = 0 to Mat.row_num planets - 1 do
    let open Mat in
    let dr = planets - row planets i in
    let dr_sqr = l2norm_sqr ~axis:1 dr in
    let dr3_recip = 1. $/ (max2 (dr_sqr * sqrt dr_sqr) numerical_zero) in
    force.${[]} <- force.${[]} - ((dr * dr3_recip) *$ (Array.get masses i))
  done;
  force


let step dt (ps, vs) =
  let f (bodies, _) _ = f_1 bodies in
  Owl_ode.Symplectic.D.Symplectic_Euler.step f ~dt (ps, vs) 0.0


let integrate t dt (ps, vs) =
  let rec integrate_helper t dt ps vs ts =
    if t <= 0.
    then ps, vs, ts
    else (
      let (p', v'), _ = step dt (List.hd ps, List.hd vs) in
      integrate_helper (t -. dt) dt ([ p' ] @ ps) ([ v' ] @ vs) ([ t -. dt ] @ ts))
  in
  integrate_helper t dt [ ps ] [ vs ] []


let plot_one_mat m scale ind =
  let h = Owl_plplot.Plot.create ("plots/" ^ string_of_int ind ^ ".png") in
  let open Owl_plplot.Plot in
  set_xrange h (scale *. -5.) (scale *. 5.);
  set_yrange h (scale *. -5.) (scale *. 5.);
  scatter ~h ~spec:[ Marker "#[0x229a]"; MarkerSize 5. ] (Mat.col m 0) (Mat.col m 1);
  output h


let plot duration =
  let dt = 0.1 in
  let y0 = bodies, bodiesVs in
  let ps, _, ts = integrate duration dt y0 in
  (* let h = .. *)
  let ind = ref (List.length ps) in
  let plot_mat m _ =
    plot_one_mat m 1. !ind;
    ind := !ind - 1;
  in
  Mat.print (List.nth ps (List.length ps - 1));
  let _ = List.map2 plot_mat ps ts in
  0.
