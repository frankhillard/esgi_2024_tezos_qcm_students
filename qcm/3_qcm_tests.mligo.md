```
#import "./helper/bootstrap.mligo" "Bootstrap"
#import "./helper/assert.mligo" "Assert"
#import "../lib/exo_8_solution_cachedsinus.mligo" "EXO_8_1"
#import "../lib/exo_8_solution_balistic.mligo" "EXO_8_2"

module CachedSinus = EXO_8_1.CachedSinus
module TrigoFloat = EXO_8_1.TrigoFloat
module Float = TrigoFloat.Float
module Balistic = EXO_8_2.Balistic

type float = EXO_8_1.CachedSinus.float

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

/////////////////////////////////////////////////////////////////////////////////////////////////////////
// TESTS
/////////////////////////////////////////////////////////////////////////////////////////////////////////

let test_exo_8_solution_check_sinus_initial_storage =
  let (_owner1, _owner2, _owner3, _, _, _, _) = Bootstrap.boot_accounts() in

  // DEPLOY CachedSinus
  let initial_storage = { 
```
----------------------
QUESTION Q15: In order to deploy `CachedSinus` contract, its initial storage must be prepared. 
Define a storage which has already calculated the sinus of PI/2.
- [ ] cached = Map.literal([(TrigoFloat.pi_half, TrigoFloat.one)])
- [ ] cached = [(TrigoFloat.pi_half, TrigoFloat.one)]
- [ ] cached = [TrigoFloat.pi_half; TrigoFloat.one]
- [ ] cached = {TrigoFloat.pi_half; TrigoFloat.one}
- [ ] cached = List.literal([(TrigoFloat.pi_half, TrigoFloat.one)])
- [ ] cached = Big_map.literal([(TrigoFloat.pi_half, TrigoFloat.one)])
------------------
```
  } in
  let {addr = cached_sinus_taddr; code = _ ; size = _} = Test.originate (contract_of CachedSinus) initial_storage 0tez in
  let cached_sinus_contract = Test.to_contract cached_sinus_taddr in
  let _cached_sinus_address : address = Tezos.address cached_sinus_contract in

  // VERIFY 
  let current_storage = Test.get_storage cached_sinus_taddr in
  let output_pi_half = match Big_map.find_opt TrigoFloat.pi_half current_storage.cached with
  | None -> failwith "Error: No value cached for pi_half"
  | Some res -> res
  in
  let () = assert (output_pi_half = TrigoFloat.one) in
  ()


let test_exo_8_solution_fire =
  let (owner1, owner2, _owner3, _, _, _, _) = Bootstrap.boot_accounts() in
  // DEPLOY CachedSinus
  let initial_storage = { 
    cached = (Big_map.empty: (float, float) big_map)
  } in
  let {addr = cached_sinus_taddr; code = _ ; size = _} = Test.originate (contract_of CachedSinus) initial_storage 0tez in
  let cached_sinus_contract = Test.to_contract cached_sinus_taddr in
  let cached_sinus_address : address = Tezos.address cached_sinus_contract in

  // DEPLOY Balistic
  let initial_storage_balistic = { 
    cached_sinus = cached_sinus_address;
    requests = (Big_map.empty : (address, Balistic.fire_param) big_map);
    results = (Big_map.empty : (address, float) big_map);
    requests_multiple = (Big_map.empty : (address, Balistic.fire_param list) big_map);
    results_multiple = (Big_map.empty : (address, float list) big_map)
  } in
  let {addr = balistic_taddr; code = _ ; size = _} = Test.originate (contract_of Balistic) initial_storage_balistic 0tez in
  let balistic_contract = Test.to_contract balistic_taddr in
  let _balistic_address : address = Tezos.address balistic_contract in

  // FIRE pre-computed value (sin(22.5°))
  let () = Test.set_source owner1 in 
  let angle = Float.div TrigoFloat.pi_quarter (Float.new 2 0) in 
```
----------------------
QUESTION Q16: In order to invoke the `Fire` entrypoint, the parameter must be prepared. 
Define a parameter `p` with an initial vector of 10 and an angle of PI/8.
- [ ] let p = { power = 10; angle = angle } in
- [ ] let p = { power = Float.new 10 0; angle } in
- [ ] let p = (10, angle) in
- [ ] let p = (Float.new 10 0; angle) in
------------------
```
  let r = Test.transfer_to_contract_exn balistic_contract (Fire p) 0tez in
  let () = Test.log("Cost of first Fire") in
  let () = Test.log(r) in

  // VERIFY that computed distance is correct (error lower than the precision threshold)
  let current_storage = Test.get_storage balistic_taddr in
  let value = match Big_map.find_opt owner1 current_storage.results with
  | Some v -> v
  | None -> failwith("no value returned")
  in
  let error_threshold = Float.new 1 (-10) in // SET PRECISION => error_threshold represent 0.0000000001
  let expected = Float.new 72153753182 (-10) in
  let diff = Float.sub value expected in
```
----------------------
QUESTION 17: Define the variable `diff_abs` as the absolute value of the variable `diff`.
- [ ] let diff_abs = Float.abs diff in
- [ ] let diff_abs = Float.new (abs diff.value) diff.pow in
- [ ] let diff_abs = Float.new (int (abs diff.value)) diff.pow in
- [ ] let diff_abs = Float.new (abs diff.value) (abs diff.pow) in
- [ ] let diff_abs = Float.new (int (abs diff.value)) (int (abs diff.pow)) in
------------------
```
  let () = assert (Float.lt diff_abs error_threshold) in

  // VERIFY that sin(PI/4) is stored in cache (in contract CachedSinus) .. actually it is the double of the given angle
  let sinus_storage = Test.get_storage cached_sinus_taddr in
  let cached_angle = Float.mul angle (Float.new 2 0) in 
  let _sin_pi_eigth = match Big_map.find_opt cached_angle sinus_storage.cached with
  | Some v -> v
  | None -> failwith("not in cache")
  in
  // FIRE (again) pre-computed value (sin(22.5°))
  let () = Test.set_source owner2 in 
  let r = Test.transfer_to_contract_exn balistic_contract (Fire p) 0tez in
  let () = Test.log("Cost of second Fire") in
  let () = Test.log(r) in
  ()


let test_exo_8_solution_fire_multiple =
  let (owner1, owner2, _owner3, _, _, _, _) = Bootstrap.boot_accounts() in
  // DEPLOY CachedSinus
  let initial_storage = { 
    cached = (Big_map.empty: (float, float) big_map)
  } in
  let {addr = cached_sinus_taddr; code = _ ; size = _} = Test.originate (contract_of CachedSinus) initial_storage 0tez in
  let cached_sinus_contract = Test.to_contract cached_sinus_taddr in
  let cached_sinus_address : address = Tezos.address cached_sinus_contract in

  // DEPLOY Balistic
  let initial_storage_balistic = { 
    cached_sinus = cached_sinus_address;
    requests = (Big_map.empty : (address, Balistic.fire_param) big_map);
    results = (Big_map.empty : (address, float) big_map);
    requests_multiple = (Big_map.empty : (address, Balistic.fire_param list) big_map);
    results_multiple = (Big_map.empty : (address, float list) big_map)
  } in
  let {addr = balistic_taddr; code = _ ; size = _} = Test.originate (contract_of Balistic) initial_storage_balistic 0tez in
  let balistic_contract = Test.to_contract balistic_taddr in
  let _balistic_address : address = Tezos.address balistic_contract in

  // FIRE pre-computed value (sin(22.5°))
  let () = Test.set_source owner1 in 
  let angle_pi_8th = Float.div TrigoFloat.pi_quarter (Float.new 2 0) in 
  let angle_pi_16th = Float.div angle_pi_8th (Float.new 2 0) in 
  let p1 = { power = Float.new 10 0; angle = angle_pi_8th } in
  let p2 = { power = Float.new 8 0; angle = angle_pi_16th } in
```
----------------------
QUESTION 18: Invoke the entrypoint Fire_multiple of the Balistic contract with the two Fire request `p1` and `p2`.
- [ ]   let r = Test.transfer_to_contract_exn balistic_contract (Fire_multiple [[p1; p2]]) 0tez in
- [ ]   let r = Test.transfer_to_contract_exn balistic_contract (Fire_multiple [[p1]; [p2]]) 0tez in
- [ ]   let r = Test.transfer_to_contract_exn balistic_contract (Fire_multiple [p1; p2]) 0tez in
- [ ]   let r = Test.transfer_to_contract_exn balistic_contract (Fire_multiple (p1, p2)) 0tez in
------------------
```
  let () = Test.log("Cost of first Fire multiple") in
  let () = Test.log(r) in

  // VERIFY Balistic distance 
  let current_storage = Test.get_storage balistic_taddr in
  let values = match Big_map.find_opt owner1 current_storage.results_multiple with
  | Some v -> v
  | None -> failwith("no value returned")
  in
  // check first value
  let error_threshold = Float.new 1 (-10) in // SET PRECISION => error_threshold represent 0.0000000001
  let value = Option.unopt(List.head_opt values) in
  let expected = Float.new 72153753182 (-10) in
  let diff = Float.sub value expected in
  let diff_abs = Float.new (int (abs diff.value)) diff.pow in
  let () = assert (Float.lt diff_abs error_threshold) in
  // check second value
  let value2 = Option.unopt(List.head_opt (Option.unopt(List.tail_opt(values)))) in
  let expected2 = Float.new 249915710932 (-11) in
  let diff2 = Float.sub value2 expected2 in
  let diff2_abs = Float.new (int (abs diff2.value)) diff2.pow in
  let () = assert (Float.lt diff2_abs error_threshold) in


  // VERIFY that sin(PI/4) is stored in cache (in contract CachedSinus) .. actually it is the double of the given angle
  let sinus_storage = Test.get_storage cached_sinus_taddr in
  let cached_angle_pi_4th = Float.mul angle_pi_8th (Float.new 2 0) in 
  let _sin_pi_4th = match Big_map.find_opt cached_angle_pi_4th sinus_storage.cached with
  | Some v -> v
  | None -> failwith("pi_4th not in cache")
  in
  // VERIFY that sin(PI/8) is stored in cache (in contract CachedSinus)
  let cached_angle_pi_8th = Float.mul angle_pi_16th (Float.new 2 0) in 
  let _sin_pi_8th = match Big_map.find_opt cached_angle_pi_8th sinus_storage.cached with
  | Some v -> v
  | None -> failwith("pi_8th not in cache")
  in
  // FIRE (again) 
  let () = Test.set_source owner2 in 
```
----------------------
QUESTION 18bis: Invoke the entrypoint Fire_multiple of the Balistic contract with the two Fire request `p1` and `p2`.
- [ ]   let r = Test.transfer_to_contract_exn balistic_contract (Fire_multiple (p1, p2)) 0tez in
- [ ]   let r = Test.transfer_to_contract_exn balistic_contract (Fire_multiple [p1; p2]) 0tez in
- [ ]   let r = Test.transfer_to_contract_exn balistic_contract (Fire_multiple [[p1; p2]]) 0tez in
- [ ]   let r = Test.transfer_to_contract_exn balistic_contract (Fire_multiple [[p1]; [p2]]) 0tez in
------------------
```
  let () = Test.log("Cost of second Fire multiple") in
  let () = Test.log(r) in
  ()
```