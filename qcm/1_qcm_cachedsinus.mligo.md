```
#import "../.ligo/source/i/ligo__s__math_lib__1.1.1__ffffffff/float/trigo_float.mligo" "TrigoFloat"

module Utils = struct
    [@inline]
    let reverse (type a) (lst : a list) : a list =
      let rec rev (type b) ((lst1, res) : b list * b list) : b list =
        match lst1 with
        | [] -> res
        | hd :: tl -> rev (tl, (hd :: res))
      in
      rev (lst, ([] : a list))
end

module CachedSinus = struct
  type float = TrigoFloat.Float.t
  type storage = {
```
----------------------
QUESTION Q1: Inside the `CachedSinus` module, define inside the type `storage` a field named `cached` which keeps track of the given angle and its related computed sinus value.
- [ ] cached: (float, float) big_map
- [ ] cached: big_map (float, float) 
- [ ] cached: big_map <float, float>
- [ ] cached: <float, float> big_map
- [ ] cached: big_map <float; float>
- [ ] cached: (float, float)
- [ ] cached: [float, float]
- [ ] cached: [float; float]
- [ ] cached: {float; float}
------------------
```
  }
  type ret = operation list * storage

  module Errors = struct
    let invalid_argument = "Invalid argument"
  end

  type request_param = {
    input: float list;
    callback: float list contract
  }

  // The unique entrypoint of the contract that takes a list of angles as parameter and returns to the caller contract the sinuses of these given angles. Angles must be provided in radian (rad).
  [@entry] let request (p : request_param) (store : storage) : ret =
```
------------------
QUESTION Q2: Define two variables `inputs` and `callback` by deconstructing the parameter `p`.
- [ ] let {input = inputs; callback = _} = p in
- [ ] let {input = inputs; callback} = p in
- [ ] let (inputs; callback) = p in
- [ ] let (inputs, callback) = p in
- [ ] let {inputs; callback} = p in
- [ ] let {input = input; callback = callback} = p in
---------
```
    // function that check if the sinus of the given angle has already been computed (if it is the case reuses the value otherwise performs the computation and saves the computed sinus in the storage) 
    let get (acc, elt: (float list * (float, float) big_map) * float) : (float list * (float, float) big_map) = 
      match Big_map.find_opt elt acc.1 with
      | Some v -> (v :: acc.0, acc.1)
      | None -> 
        let sinus_value = TrigoFloat.sin(elt, 11n) in // PRECISION SET TO 11 (i.e. 10^(-11) )
        let new_cached = Big_map.add elt sinus_value acc.1 in 
        (sinus_value :: acc.0, new_cached)
    in
    // retrieve the list of angles and compute their sinuses (also updates the `cached` field of the storage).  
```
-------------------
QUESTION Q3: Fold on the given list of angles and retrieve their sinuses (use the previously defined `get` function).
- [ ] let (sinus_angles_inversed, new_cached) = List.fold get inputs store.cached [] in
- [ ] let (sinus_angles_inversed, new_cached) = List.fold get inputs [] store.cached in
- [ ] let (sinus_angles_inversed, new_cached) = List.fold (get, inputs, [], store.cached) in
- [ ] let (sinus_angles_inversed, new_cached) = List.fold (get inputs [] store.cached) in
- [ ] let (sinus_angles_inversed, new_cached) = List.fold get inputs ([], store.cached) in
- [ ] let (sinus_angles_inversed, new_cached) = List.fold (get, inputs, store.cached, []) in
- [ ] let (sinus_angles_inversed, new_cached) = List.fold (get inputs store.cached []) in
- [ ] let (sinus_angles_inversed, new_cached) = List.fold get inputs (store.cached, []) in
-----------------
```
    let responses = Utils.reverse sinus_angles_inversed in
    // Create an operation to send back the responses to the caller
    let op : operation = Tezos.transaction responses 0tez callback in 
```
------------
QUESTION Q4: The entrypoint must update its storage with newly computed sinuses and send back the result to the caller. Finish the "return" of the function.
- [ ] responses, { store with cached = new_cached}
- [ ] [responses], { store with cached = new_cached}
- [ ] responses, store.cached
- [ ] [responses], new_cached
- [ ] op, { store with cached = new_cached}
- [ ] [op], { store with cached = new_cached}
- [ ] op, store.cached
- [ ] [op], new_cached
---------------
```

  [@view] let get (input: float) (store: storage) : float option =
      Big_map.find_opt input store.cached

end
```