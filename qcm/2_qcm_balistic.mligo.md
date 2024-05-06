```
// #import "@ligo/math_lib/float/trigo_float.mligo" "TrigoFloat"
#import "../.ligo/source/i/ligo__s__math_lib__1.1.1__ffffffff/float/trigo_float.mligo" "TrigoFloat"

module Utils = struct
    [@inline]
    let rec zipWith (type a b c) (lst_a : a list) (lst_b : b list) (f: (a * b) -> c) : c list =
      match lst_a, lst_b with
      | [], _ -> []
      | _, [] -> []
```
-------------
QUESTION Q5: Complete the recursive call
- [ ]  | [x, xs], [y, ys] -> zipWith xs ys (f (x, y) )
- [ ]  | [x, xs], [y, ys] -> (f (x, y)) :: zipWith xs ys f
- [ ]  | [x; xs], [y; ys] -> (f x y ) :: zipWith xs ys f
- [ ]  | x::xs, y::ys -> zipWith xs ys f :: (f x y )
- [ ]  | [x; xs], [y; ys] -> zipWith xs ys (f x y )
- [ ]  | [x; xs], [y; ys] -> zipWith xs ys (f (x, y) )
- [ ]  | [x::xs, y::ys] -> zipWith xs ys (f x y )
- [ ]  | [x::xs, y::ys] -> zipWith xs ys f :: (f x y )
- [ ]  | x::xs, y::ys -> zipWith xs ys (f x y )
- [ ]  | [x::xs, y::ys] -> zipWith xs ys (f (x, y) )
- [ ]  | [x::xs, y::ys] -> (f x y ) :: zipWith xs ys f
- [ ]  | x::xs, y::ys -> (f (x, y)) :: zipWith xs ys f
- [ ]  | x::xs, y::ys -> zipWith xs ys (f (x, y) )
- [ ]  | x::xs, y::ys -> zipWith xs ys f :: (f (x, y) )
- [ ]  | [x; xs], [y; ys] -> zipWith xs ys f :: (f (x, y) )
-------------
```
    [@inline]
    let reverse (type a) (lst : a list) : a list =
      let rec rev (type b) ((lst1, res) : b list * b list) : b list =
        match lst1 with
        | [] -> res
        | hd :: tl -> rev (tl, (hd :: res))
      in
      rev (lst, ([] : a list))
end

module Balistic = struct
  module Float = TrigoFloat.Float
  type float = Float.t

  type fire_param = {
    power: float;
    angle: float
  }

  type storage = {
    cached_sinus: address;
    requests: (address, fire_param) big_map;
    results: (address, float) big_map;
    requests_multiple: (address, fire_param list) big_map;
    results_multiple: (address, float list) big_map
  }

  type ret = operation list * storage

  module Errors = struct
    let invalid_sinus_contract = "Invalid CachedSinus contract (entrypoint request not found)"
    let invalid_argument = "Invalid argument"
    let invalid_callback = "invalid_callback"
    let not_allowed = "NOT allowed"
    let unknown_request = "unknown_request"
  end

  type request_param = {
    input: float list;
    callback: float list contract
  }

  // d = v² * sin(2 * angle) / g 
  //with the gravity of earth, g = 9.8 m/s
  let distance (v, sin_double_angle: float * float) : float =
    let v_square = Float.mul v v in 
    let nominator = Float.mul sin_double_angle v_square in
```
-------------
QUESTION Q6: Define the variable `g` as a float
- [ ] let g = Float.new 9.8 0 in
- [ ] let g = Float.new 98 1 in
- [ ] let g = Float.new 98 -1 in
- [ ] let g = Float.new 98 (-1) in
- [ ] let g = Float.new 9.8 (0) in
- [ ] let g = Float.new 98 (1) in
- [ ] let g = Float.new 9,8 -1 in
-------------
```
    let d = Float.div nominator g in 
    d

  // The entrypoint Fire takes an initial vector and an angle as parameter. 
  // It asks CachedSinus contract to compute sinus of the given angle. 
  // The given initial vector (called `power`) is stored in a `requests` map (in the storage). 
  // This initial vector is used by the Apply_fire entrypoint once sinus value has been computed.
  // The given sinus sent by CachedSinus is processed in a second entrypoint Apply_fire. 
  [@entry] let fire (p : fire_param) (store : storage) : ret =
    let request_id = Tezos.get_source() in
    let {power = _power; angle} = p in
    let double_angle = Float.mul angle (TrigoFloat.two) in
    let e : request_param contract = match Tezos.get_contract_opt store.cached_sinus with
    | Some c -> c 
    | None -> failwith(Errors.invalid_sinus_contract) 
    in
```
-------------
QUESTION Q7: Define a variable `e_resp` by retrieving the contract interface of the Apply_fire entrypoint.
- [ ] let e_resp : float list contract = match Tezos.get_entrypoint_opt "Apply_fire" (Tezos.get_self_address()) with
- [ ] let e_resp : float list contract = match Tezos.get_entrypoint_opt "%apply_fire" (Tezos.get_self_address()) with
- [ ] let e_resp : float list contract = match Tezos.get_entrypoint_opt "apply_fire" (Tezos.get_self_address()) with
- [ ] let e_resp : float list contract = match Tezos.get_entrypoint_opt "Apply_fire" (store.cached_sinus) with
- [ ] let e_resp : float list contract = match Tezos.get_entrypoint_opt "%apply_fire" (store.cached_sinus) with
- [ ] let e_resp : float list contract = match Tezos.get_entrypoint_opt "apply_fire" (store.cached_sinus) with
- [ ] let e_resp : fire_param contract = match Tezos.get_entrypoint_opt "Apply_fire" (Tezos.get_self_address()) with
- [ ] let e_resp : fire_param contract = match Tezos.get_entrypoint_opt "%apply_fire" (Tezos.get_self_address()) with
- [ ] let e_resp : fire_param contract = match Tezos.get_entrypoint_opt "apply_fire" (Tezos.get_self_address()) with
- [ ] let e_resp : fire_param contract = match Tezos.get_entrypoint_opt "Apply_fire" (store.cached_sinus) with
- [ ] let e_resp : fire_param contract = match Tezos.get_entrypoint_opt "%apply_fire" (store.cached_sinus) with
- [ ] let e_resp : fire_param contract = match Tezos.get_entrypoint_opt "apply_fire" (store.cached_sinus) with
-------------
```
    | Some c -> c 
    | None -> failwith(Errors.invalid_callback) 
    in

    let param = { input = [double_angle]; callback = e_resp } in
```
-------------
QUESTION Q8: Define the variable `param` (of type request_param) which contains the request that will be sent to the `CachedSinus` contract.
- [ ] let param = ( double_angle; callback = e ) in
- [ ] let param = ( [double_angle]; callback = e ) in
- [ ] let param = ( double_angle; callback = e_resp ) in
- [ ] let param = ( [double_angle]; callback = e_resp ) in
- [ ] let param = ( double_angle; e ) in
- [ ] let param = ( [double_angle]; e ) in
- [ ] let param = ( double_angle; e_resp ) in
- [ ] let param = ( [double_angle]; e_resp ) in
- [ ] let param = { input = double_angle; callback = e_resp } in
- [ ] let param = { input = [double_angle]; callback = e_resp } in
- [ ] let param = { input = double_angle; callback = e } in
- [ ] let param = { input = [double_angle]; callback = e } in
- [ ] let param = { input = double_angle, callback = e_resp } in
- [ ] let param = { input = [double_angle], callback = e_resp } in
- [ ] let param = { input = double_angle, callback = e } in
-------------
```
    let op : operation = Tezos.transaction param 0tez e in 
    [op], { store with requests = Big_map.update request_id (Some p) store.requests }

  // The entrypoint Apply_fire takes a list of float (representing the sinus of given angles in entrypoint Fire) and compute the balistic distance. 
  // The result is stored in the `results` map of the storage.
  [@entry] let apply_fire (response : float list) (store : storage) : ret =
```
-------------
QUESTION Q9: Assert that this entrypoint can only be called by the `CachedSinus` contract.
- [ ] let () = assert_with_error (store.cached_sinus = Tezos.get_source()) Errors.not_allowed in
- [ ] let () = assert_with_error (store.cached_sinus = Tezos.get_sender()) Errors.not_allowed in
- [ ] let () = assert_with_error (cached_sinus = Tezos.get_source()) Errors.not_allowed in
- [ ] let () = assert_with_error (cached_sinus = Tezos.get_sender()) Errors.not_allowed in
-------------
```
    let request_id = Tezos.get_source() in
    // retrieve initial vector from `requests` map.
    let power = match Big_map.find_opt request_id store.requests with
      | None -> failwith(Errors.unknown_request)
      | Some rq -> rq.power
    in  
    // takes the first element of the response list
```
-------------
QUESTION Q10: Define a variable `sin_double_angle` which contains the first element of the float list `response`.
- [ ] let sin_double_angle = Option.unopt (List.head_opt response) in
- [ ] let sin_double_angle = Option.unopt (List.tail_opt response) in
- [ ] let sin_double_angle = List.head (Option.unopt (response)) in
- [ ] let sin_double_angle = List.tail (List.head_opt response) in
- [ ] let sin_double_angle = Option.unopt (List.head response) in
- [ ] let sin_double_angle = List.tail (List.head_opt response) in
- [ ] let sin_double_angle = List.head response in
-------------
```
    let result = distance(power, sin_double_angle) in
    [], { store with 
```
-------------
QUESTION Q11: Now that the fire request has been calculated, we can clean the `requests` big_map of the storage.
- [ ] requests = Big_map.clean store.requests;
- [ ] requests = Big_map.update request_id None store.requests;
- [ ] requests = Big_map.update request_id (None: address) store.requests;
- [ ] requests = Big_map.remove request_id store.requests;
-------------
```
```
-------------
QUESTION Q12: Now that the fire request has been calculated, we can store the result in the `results` big_map of the storage.
- [ ] results = Big_map.update request_id (Some result) store.results
- [ ] results = Big_map.update request_id result store.results
- [ ] results = Big_map.update request_id Some result store.results
- [ ] results = Big_map.update request_id Some(result) store.results
-------------
```
    }
  
  // The entrypoint Fire_multiple execute many action Fire in once (asks many sinuses to CachedSinus). 
  // It triggers a callback response which triggers the Apply_fire_multiple entrypoint
  [@entry] let fire_multiple (p : fire_param list) (store : storage) : ret =
```
-------------
QUESTION Q13: Define the `get_double_angle` function that is used in the next line
- [ ] let get_double_angle (acc, elt: float list * fire_param) : float list = (elt.angle * 2) :: acc in
- [ ] let get_double_angle (acc, elt: fire_param list * float) : fire_param list = (elt.angle * 2) :: acc in
- [ ] let get_double_angle (acc, elt: float list * fire_param) : float list = (Float.mul elt.angle (TrigoFloat.two)) :: acc in
- [ ] let get_double_angle (acc, elt: fire_param list * float) : fire_param list = (Float.mul elt.angle (TrigoFloat.two)) :: acc in
- [ ] let get_double_angle (acc, elt: float list * float) : float list = (Float.mul elt (TrigoFloat.two)) :: acc in
- [ ] let get_double_angle (acc, elt: fire_param list * float) : fire_param list  = (Float.mul elt (TrigoFloat.two)) :: acc in
- [ ] let get_double_angle (acc, elt: fire_param list * fire_param) : fire_param list  = (elt.angle * 2) :: acc in
- [ ] let get_double_angle (acc, elt: fire_param list * fire_param) : fire_param list  = (Float.mul elt (TrigoFloat.two)) :: acc in
-------------
```
    // Fold on the given fire requests to retrieve an inversed list of double angles. (Double angles means angles are multiplied by 2). 
    let reversed_args = List.fold get_double_angle p [] in
    let args = Utils.reverse(reversed_args) in 

    let request_id = Tezos.get_source() in
    let e : request_param contract = match Tezos.get_contract_opt store.cached_sinus with
    | Some c -> c 
    | None -> failwith(Errors.invalid_sinus_contract) 
    in
```
-------------
QUESTION Q7bis: Define a variable `e_resp` by retrieving the contract interface of the Apply_fire_multiple entrypoint.
- [ ] let e_resp : float list contract = match Tezos.get_entrypoint_opt "Apply_fire_multiple" (Tezos.get_self_address()) with
- [ ] let e_resp : float list contract = match Tezos.get_entrypoint_opt "%apply_fire_multiple" (Tezos.get_self_address()) with
- [ ] let e_resp : float list contract = match Tezos.get_entrypoint_opt "apply_fire_multiple" (Tezos.get_self_address()) with
- [ ] let e_resp : float list contract = match Tezos.get_entrypoint_opt "Apply_fire_multiple" (store.cached_sinus) with
- [ ] let e_resp : float list contract = match Tezos.get_entrypoint_opt "%apply_fire_multiple" (store.cached_sinus) with
- [ ] let e_resp : float list contract = match Tezos.get_entrypoint_opt "apply_fire_multiple" (store.cached_sinus) with
- [ ] let e_resp : fire_param contract = match Tezos.get_entrypoint_opt "Apply_fire_multiple" (Tezos.get_self_address()) with
- [ ] let e_resp : fire_param contract = match Tezos.get_entrypoint_opt "%apply_fire_multiple" (Tezos.get_self_address()) with
- [ ] let e_resp : fire_param contract = match Tezos.get_entrypoint_opt "apply_fire_multiple" (Tezos.get_self_address()) with
- [ ] let e_resp : fire_param contract = match Tezos.get_entrypoint_opt "Apply_fire_multiple" (store.cached_sinus) with
- [ ] let e_resp : fire_param contract = match Tezos.get_entrypoint_opt "%apply_fire_multiple" (store.cached_sinus) with
- [ ] let e_resp : fire_param contract = match Tezos.get_entrypoint_opt "apply_fire_multiple" (store.cached_sinus) with
-------------
```
    | Some c -> c 
    | None -> failwith(Errors.invalid_callback) 
    in
    let param = { input = args; callback = e_resp } in
    let op : operation = Tezos.transaction param 0tez e in 
    [op], { store with requests_multiple = Big_map.update request_id (Some p) store.requests_multiple }


  [@entry] let apply_fire_multiple (responses : float list) (store : storage) : ret =
    // responses contains the corresponding sinus of the double of the given angles. (i.e. responses =  [sin_2angle_1; sin_2angle_2; ... ])
```
-------------
QUESTION Q9bis: Assert that this entrypoint can only be called by the `CachedSinus` contract.
- [ ] let () = assert_with_error (store.cached_sinus = Tezos.get_source()) Errors.not_allowed in
- [ ] let () = assert_with_error (store.cached_sinus = Tezos.get_sender()) Errors.not_allowed in
- [ ] let () = assert_with_error (cached_sinus = Tezos.get_source()) Errors.not_allowed in
- [ ] let () = assert_with_error (cached_sinus = Tezos.get_sender()) Errors.not_allowed in
-------------
```
    let request_id = Tezos.get_source() in
    let requests : fire_param list = match Big_map.find_opt request_id store.requests_multiple with
      | None -> failwith(Errors.unknown_request)
      | Some rqs -> rqs
    in  
    let get_power(acc, elt : float list * fire_param) : float list = elt.power :: acc in
    let powers_inv = List.fold get_power requests [] in
    // powers = [power1; power2; ... ]
    let powers = Utils.reverse(powers_inv) in
    // Compute the distance for all fire requests. (i.e. result = [distance(power1,sin_2angle_1); distance(power2,sin_2angle_2); ... ])
```
-------------
QUESTION Q14: Define the `result` variable by computing the distance for all given fire requests. 
- [ ] let result : float list = List.map distance powers responses in
- [ ] let result : float list = List.fold distance powers responses in
- [ ] let result : float list = Utils.zipWith powers responses distance in
- [ ] let result : float list = List.fold (powers, responses) distance in
- [ ] let result : float list = Utils.zipWith ((a,b) -> distance(a,b)) (powers, responses) in
- [ ] let result : float list = List.fold distance (powers, responses) in
- [ ] let result : float list = Utils.zipWith distance (powers, responses) in
- [ ] let result : float list = List.map ((a,b) -> distance(a,b)) (powers, responses) in
- [ ] let result : float list = List.map distance (powers, responses) in
- [ ] let result : float list = Utils.zipWith distance (powers, responses) in
-------------
```
    [], { store with 
```
-------------
QUESTION Q11bis: Now that the fire request has been calculated, we can clean the `requests_multiple` big_map of the storage.
- [ ] requests_multiple = Big_map.clean store.requests_multiple;
- [ ] requests_multiple = Big_map.update request_id None store.requests_multiple;
- [ ] requests_multiple = Big_map.update request_id (None: address) store.requests_multiple;
- [ ] requests_multiple = Big_map.remove request_id store.requests_multiple;
-------------
```
```
-------------
QUESTION Q12bis: Now that the fire request has been calculated, we can store the result in the `results_multiple` big_map of the storage.
- [ ] results_multiple = Big_map.update request_id (Some result) store.results_multiple
- [ ] results_multiple = Big_map.update request_id result store.results_multiple
- [ ] results_multiple = Big_map.update request_id Some result store.results_multiple
- [ ] results_multiple = Big_map.update request_id Some(result) store.results_multiple
-------------
```
    }

end
```