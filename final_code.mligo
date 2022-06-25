type token_id = nat

type transfer_destination =
[@layout:comb]
{
  to_ : address;
  token_id : token_id;
  amount : nat;
}

type transfer =
[@layout:comb]
{
  from_ : address;
  txs : transfer_destination list;
}

type balance_of_request =
[@layout:comb]
{
  owner : address;
  token_id : token_id;
}

type balance_of_response =
[@layout:comb]
{
  request : balance_of_request;
  balance : nat;
}

type balance_of_param =
[@layout:comb]
{
  requests : balance_of_request list;
  callback : (balance_of_response list) contract;
}

type operator_param =
[@layout:comb]
{
  owner : address;
  operator : address;
  token_id: token_id;
}

type update_operator =
[@layout:comb]
  | Add_operator of operator_param
  | Remove_operator of operator_param

type token_metadata =
[@layout:comb]
{
  token_id : token_id;
  token_info : (string, bytes) map;
}

type token_metadata_storage = (token_id, token_metadata) big_map

type token_metadata_param = 
[@layout:comb]
{
  token_ids : token_id list;
  handler : (token_metadata list) -> unit;
}

type mint_params =
[@layout:comb]
{
  link_to_metadata: bytes;
  owner: address;
}

type contract_metadata = (string, bytes) big_map

type transfer_destination_descriptor =
[@layout:comb]
{
  to_ : address option;
  token_id : token_id;
  amount : nat;
}

type transfer_descriptor =
[@layout:comb]
{
  from_ : address option;
  txs : transfer_destination_descriptor list
}

type transfer_descriptor_param =
[@layout:comb]
{
  batch : transfer_descriptor list;
  operator : address;
}
// Types for tickets
type owner = address
type event_id = nat
type mem_id = nat
//___________________________________________TYPES FOR STORAGE_____________________________________________
type ticket_id = token_id
type attendee = address
type token_id_set = token_id set

type in_events = event_id set

type ticket_features = 
[@layout:comb]
{
    ticket_extendable: bool;
    free_upgradation: bool;
    refreshments: bool;
    special_access: bool;
}

type ticket = 
[@layout:comb]
{   
    in_events: in_events;
    price: tez;
}
//That users can buy ticket for 
type event = 
[@layout:comb]
{
    owner:owner;
    accomodation_limit: nat;
    booked_tickets: nat;
    ticket_price: tez;
    description: string;
    date: timestamp;
    related_events: in_events;
    ticket_features: ticket_features;
    claimed: bool;
}
//Memberships user can buy
type membership = 
[@layout:comb]
{
    mem_id: mem_id;
    owner:owner;
    accomodation_limit: nat;
    price_month: tez;
    booked_mems: nat;
    claimed: token_id_set;
}

type create_eve_params = 
[@layout:comb]
{ 
    accomodation_limit: nat;
    description: string;
    ticket_price: tez;
    date: timestamp;
    ticket_extendable: bool;
    free_upgradation: bool;
    refreshments: bool; 
    special_access: bool;
    related_events: in_events;
}

type create_mem_params = 
[@layout:comb]
{
    accomodation_limit: nat;
    price_month: tez;
}

type buy_membership_params = 
[@layout:comb]
{
    mint_params: mint_params;
    mem_id: nat;
    start_date: timestamp;
    duration_mon: nat;
}

type verify_ticket_params = 
[@layout:comb]
{
    ticket_id: token_id;
    event_id: event_id;
}

type ticket_valid_response = 
[@layout:comb]
{
  verify_ticket_params : verify_ticket_params;
  valid: bool;
}

type ticket_valid_param =
[@layout:comb]
{
  verify_ticket_params : verify_ticket_params;
  callback : ticket_valid_response contract;
}

type verify_mem_params = 
[@layout:comb]
{
    mem_pass_id: token_id;
    mem_id: mem_id;
}

type mem_valid_response = 
[@layout:comb]
{
    verify_mem_params: verify_mem_params;
    valid: bool;
}

type mem_valid_param =
[@layout:comb]
{
  verify_mem_params : verify_mem_params;
  callback : mem_valid_response contract;
}

type membership_pass = 
[@layout:comb]
{
    mem_id: mem_id;
    start_date: timestamp;
    valid_till: timestamp;
    price: tez;
}

type refund_ticket_param = 
[@layout:comb]
{
    ticket_id: ticket_id;
    event_id: event_id;
}

type refund_membership_param = 
[@layout:comb]
{
    token_id: token_id;
    mem_id: mem_id;
}



type id_to_tickets = (token_id, ticket) big_map
type event_to_tickets = (event_id, token_id_set) big_map
type id_to_mem = (token_id, membership_pass) big_map
type mem_to_mem_pass = (mem_id, token_id_set) big_map
type memberships = (mem_id, membership) big_map
type events = (event_id, event) big_map

type ticket_storage = 
[@layout:comb]
{
    id_to_tickets: id_to_tickets;
    event_to_tickets: event_to_tickets;
    id_to_mem: id_to_mem;
    mem_to_mem_pass: mem_to_mem_pass;
    events: events;
    memberships: memberships;
    next_event_id: event_id;
    next_mem_id: mem_id;
}

type sell_ticket_params = 
[@layout:comb]
{
    event_id: event_id;
}

type buy_ticket_params = 
[@layout:comb]
{
    mint_params: mint_params;
    event_id: event_id;
    related_events: bool;
}

type claim_mem_params = 
[@layout:comb]
{
    mem_id: mem_id;
    mem_token_id: token_id;
}

type ticket_sys_entry_points =
  | Transfer of transfer list
  | Balance_of of balance_of_param
  | Update_operators of update_operator list
  | Mint of mint_params
  | Burn of token_id
  | BuyTicket of buy_ticket_params
  | CreateEvent of create_eve_params
  | CreateMembership of create_mem_params
  | BuyMembership of buy_membership_params
  | VerifyTicket of ticket_valid_param
  | VerifyMembership of mem_valid_param
  | RefundTicket of refund_ticket_param
  | RefundMembership of refund_membership_param
  | ClaimEventMoney of event_id
  | ClaimMemMoney of claim_mem_params

let fa2_token_undefined = "FA2_TOKEN_UNDEFINED" 
let fa2_insufficient_balance = "FA2_INSUFFICIENT_BALANCE"
let fa2_tx_denied = "FA2_TX_DENIED"
let fa2_not_owner = "FA2_NOT_OWNER"
let fa2_not_operator = "FA2_NOT_OPERATOR"
let fa2_operators_not_supported = "FA2_OPERATORS_UNSUPPORTED"
let fa2_receiver_hook_failed = "FA2_RECEIVER_HOOK_FAILED"
let fa2_sender_hook_failed = "FA2_SENDER_HOOK_FAILED"
let fa2_receiver_hook_undefined = "FA2_RECEIVER_HOOK_UNDEFINED"
let fa2_sender_hook_undefined = "FA2_SENDER_HOOK_UNDEFINED"

type operator_transfer_policy =
  [@layout:comb]
  | No_transfer
  | Owner_transfer
  | Owner_or_operator_transfer

type owner_hook_policy =
  [@layout:comb]
  | Owner_no_hook
  | Optional_owner_hook
  | Required_owner_hook

type custom_permission_policy =
[@layout:comb]
{
  tag : string;
  config_api: address option;
}

type permissions_descriptor =
[@layout:comb]
{
  operator : operator_transfer_policy;
  receiver : owner_hook_policy;
  sender : owner_hook_policy;
  custom : custom_permission_policy option;
}

type operator_storage = ((address * (address * token_id)), unit) big_map

let update_operators (update, storage : update_operator * operator_storage)
    : operator_storage =
  match update with
  | Add_operator op -> 
    Big_map.update (op.owner, (op.operator, op.token_id)) (Some unit) storage
  | Remove_operator op -> 
    Big_map.remove (op.owner, (op.operator, op.token_id)) storage

let validate_update_operators_by_owner (update, updater : update_operator * address)
    : unit =
  let op = match update with
  | Add_operator op -> op
  | Remove_operator op -> op
  in
  if op.owner = updater then unit else failwith fa2_not_owner

let fa2_update_operators (updates, storage
    : (update_operator list) * operator_storage) : operator_storage =
  let updater = Tezos.sender in
  let process_update = (fun (ops, update : operator_storage * update_operator) ->
    let _u = validate_update_operators_by_owner (update, updater) in
    update_operators (update, ops)
  ) in
  List.fold process_update updates storage

type operator_validator = (address * address * token_id * operator_storage)-> unit

let make_operator_validator (tx_policy : operator_transfer_policy) : operator_validator =
  let can_owner_tx, can_operator_tx = match tx_policy with
  | No_transfer -> (failwith fa2_tx_denied : bool * bool)
  | Owner_transfer -> true, false
  | Owner_or_operator_transfer -> true, true
  in
  (fun (owner, operator, token_id, ops_storage 
      : address * address * token_id * operator_storage) ->
    if can_owner_tx && owner = operator
    then unit (* transfer by the owner *)
    else if not can_operator_tx
    then failwith fa2_not_owner (* an operator transfer not permitted by the policy *)
    else if Big_map.mem  (owner, (operator, token_id)) ops_storage
    then unit (* the operator is permitted for the token_id *)
    else failwith fa2_not_operator (* the operator is not permitted for the token_id *)
  )

let default_operator_validator : operator_validator =
  (fun (owner, operator, token_id, ops_storage 
      : address * address * token_id * operator_storage) ->
    if owner = operator
    then unit (* transfer by the owner *)
    else if Big_map.mem (owner, (operator, token_id)) ops_storage
    then unit (* the operator is permitted for the token_id *)
    else failwith fa2_not_operator (* the operator is not permitted for the token_id *)
  )

let validate_operator (tx_policy, txs, ops_storage 
    : operator_transfer_policy * (transfer list) * operator_storage) : unit =
  let validator = make_operator_validator tx_policy in
  List.iter (fun (tx : transfer) -> 
    List.iter (fun (dst: transfer_destination) ->
      validator (tx.from_, Tezos.sender, dst.token_id ,ops_storage)
    ) tx.txs
  ) txs

type token_def =
[@layout:comb]
{
  from_ : nat;
  to_ : nat;
}

type nft_meta = (token_def, token_metadata) big_map

type token_storage = {
  token_defs : token_def set;
  next_token_id : token_id;
  metadata : nft_meta;
}

type ledger = (token_id, address) big_map
type reverse_ledger = (address, token_id list) big_map

type nft_token_storage = {
  ledger : ledger;
  operators : operator_storage;
  reverse_ledger: reverse_ledger;
  metadata: (string, bytes) big_map;
  token_metadata: token_metadata_storage;
  next_token_id: token_id;
  admin: address;
}


type ticket_system_storage = 
[@layout:comb]
{
    nft_token_storage: nft_token_storage;
    ticket_storage: ticket_storage; 
}

type return_ = (operation list) * ticket_system_storage

let get_balance (p, ledger : balance_of_param * ledger) : operation =
  let to_balance = fun (r : balance_of_request) ->
    let owner = Big_map.find_opt r.token_id ledger in
    match owner with
    | None -> (failwith fa2_token_undefined : balance_of_response)
    | Some o ->
      let bal = if o = r.owner then 1n else 0n in
      { request = r; balance = bal; }
  in
  let responses = List.map to_balance p.requests in
  Tezos.transaction responses 0mutez p.callback

let transfer (txs, validate_op, ops_storage, ledger, reverse_ledger
    : (transfer list) * operator_validator * operator_storage * ledger * reverse_ledger) : ledger * reverse_ledger =
  let make_transfer = (fun ((l, rv_l), tx : (ledger * reverse_ledger) * transfer) ->
    List.fold 
      (fun ((ll, rv_ll), dst : (ledger * reverse_ledger) * transfer_destination) ->
        if dst.amount = 0n
        then ll, rv_ll
        else if dst.amount <> 1n
        then (failwith fa2_insufficient_balance : ledger * reverse_ledger)
        else
          let owner = Big_map.find_opt dst.token_id ll in
          match owner with
          | None -> (failwith fa2_token_undefined : ledger * reverse_ledger)
          | Some o -> 
            if o <> tx.from_
            then (failwith fa2_insufficient_balance : ledger * reverse_ledger)
            else 
              begin
                let _u = validate_op (o, Tezos.sender, dst.token_id, ops_storage) in
                let new_ll = Big_map.update dst.token_id (Some dst.to_) ll in
                (* removes token id from sender *)
                let new_rv_ll = 
                  match Big_map.find_opt tx.from_ rv_ll with
                  | None -> (failwith fa2_insufficient_balance : reverse_ledger)
                  | Some tk_id_l -> 
                      Big_map.update 
                        tx.from_ 
                        (Some (List.fold (
                          fun (new_list, token_id: token_id list * token_id) ->
                            if token_id = dst.token_id
                            then new_list
                            else token_id :: new_list
                        ) tk_id_l ([]: token_id list))) 
                        rv_ll 
                in
                (* adds token id to recipient *)
                let updated_rv_ll = 
                  match Big_map.find_opt dst.to_ new_rv_ll with
                  | None -> Big_map.add dst.to_ [dst.token_id] new_rv_ll
                  | Some tk_id_l -> Big_map.update dst.to_ (Some (dst.token_id :: tk_id_l)) new_rv_ll in

                new_ll, updated_rv_ll
              end
      ) tx.txs (l, rv_l)
  )
  in 
    
  List.fold make_transfer txs (ledger, reverse_ledger)

(** Finds a definition of the token type (token_id range) associated with the provided token id *)
let find_token_def (tid, token_defs : token_id * (token_def set)) : token_def =
  let tdef = Set.fold (fun (res, d : (token_def option) * token_def) ->
    match res with
    | Some _ -> res
    | None ->
      if tid >= d.from_ && tid < d.to_
      then  Some d
      else (None : token_def option)
  ) token_defs (None : token_def option)
  in
  match tdef with
  | None -> (failwith fa2_token_undefined : token_def)
  | Some d -> d

let get_metadata (tokens, meta : (token_id list) * token_storage )
    : token_metadata list =
  List.map (fun (tid: token_id) ->
    let tdef = find_token_def (tid, meta.token_defs) in
    let meta = Big_map.find_opt tdef meta.metadata in
    match meta with
    | Some m -> { m with token_id = tid; }
    | None -> (failwith "NO_DATA" : token_metadata)
  ) tokens

let mint (p, s: mint_params * nft_token_storage): nft_token_storage =
  let { link_to_metadata; owner } = p in
  let token_id = s.next_token_id in
  (* Updates the ledger *)
  let new_ledger = Big_map.add token_id owner s.ledger in
  (* Updates the reverse ledger *)
  let new_reverse_ledger = 
    match Big_map.find_opt owner s.reverse_ledger with
    | None -> Big_map.add owner [token_id] s.reverse_ledger
    | Some l -> Big_map.update owner (Some (token_id :: l)) s.reverse_ledger in
  (* Stores the metadata *)
  let new_entry = { token_id = token_id; token_info = Map.literal [("", link_to_metadata)] } in
  
  { 
      s with 
          ledger = new_ledger;
          reverse_ledger = new_reverse_ledger;
          token_metadata = Big_map.add token_id new_entry s.token_metadata;
          next_token_id = token_id + 1n;
  }

let burn (p, s: token_id * nft_token_storage): nft_token_storage =
  (* removes token from the ledger *)
  let new_ledger: ledger =
    match Big_map.find_opt p s.ledger with
    | None -> (failwith "UNKNOWN_TOKEN": ledger)
    | Some owner ->
      if owner <> Tezos.sender
      then (failwith "NOT_TOKEN_OWNER": ledger)
      else
        Big_map.remove p s.ledger
  in
  (* removes token from the reverse ledger *)
  let new_reverse_ledger: reverse_ledger =
    match Big_map.find_opt Tezos.sender s.reverse_ledger with
    | None -> (failwith "NOT_A_USER": reverse_ledger)
    | Some tk_id_l -> 
      Big_map.update 
        Tezos.sender 
        (Some (List.fold (
          fun (new_list, token_id: token_id list * token_id) ->
            if token_id = p
            then new_list
            else token_id :: new_list
        ) tk_id_l ([]: token_id list))) 
        s.reverse_ledger
  in { s with ledger = new_ledger; reverse_ledger = new_reverse_ledger }

//_______________________________________Ticket Functions________________________________________________
let buy_ticket(buy_p, stor : buy_ticket_params * ticket_system_storage) : ticket_system_storage =
    let { mint_params; event_id; related_events } = buy_p in
    let { nft_token_storage; ticket_storage; } = stor in
    let ticket_id = nft_token_storage.next_token_id in
    let event_to_book : event =
        match Big_map.find_opt event_id ticket_storage.events with
        | None -> (failwith "EVENT NOT FOUND!" : event)
        | Some (_event)-> _event
    in
    if Tezos.amount < event_to_book.ticket_price
    then (failwith "THE TICKET PRICE IS NOT PAID": ticket_system_storage)
    else if Tezos.now > event_to_book.date
    then (failwith "THE EVENT ALREADY COMMENCED": ticket_system_storage)
    else if event_to_book.booked_tickets = event_to_book.accomodation_limit
    then (failwith "MAXIMUM OCCUPANCY REACHED!": ticket_system_storage)
    else
    let ticket_already_present: bool = 
        match Big_map.find_opt Tezos.sender nft_token_storage.reverse_ledger with 
        //NFT storage find a NFT token with owner's address
        | None -> false
        | Some(tick) -> true
    in
    if ticket_already_present 
    then (failwith "YOU HAVE ALREADY REGISTERED FOR EVENT!" : ticket_system_storage)
    else
    let related_events_no : nat = Set.cardinal event_to_book.related_events in
    let related_events_ticket : bool = (related_events && event_to_book.ticket_features.ticket_extendable) && (related_events_no <> 0n) in
    
    let add_op : bool = 
        if (related_events_ticket && not event_to_book.ticket_features.free_upgradation && (Tezos.amount < (event_to_book.ticket_price * 2n)))
        then (failwith "Not enough amount Paid for all tickets!" : bool)
        else if (related_events_ticket && (event_to_book.ticket_features.free_upgradation || Tezos.amount > event_to_book.ticket_price * 2n))
        then true
        else false
    in        
    let tick_in_events : in_events = 
        if add_op 
        then Set.add event_id event_to_book.related_events
        else Set.literal [event_id;]
    in
    let new_ticket : ticket = 
    {
        in_events = tick_in_events;
        price = Tezos.amount;
    } 
    in
    let event_ticket_set : token_id_set = 
        match Big_map.find_opt event_id ticket_storage.event_to_tickets with
        | None -> Set.literal [ticket_id]
        | Some(_eve_set) -> Set.add ticket_id _eve_set 
    in
    let new_event_to_tickets_map = Big_map.update event_id (Some event_ticket_set) ticket_storage.event_to_tickets 
    in 
    let new_id_to_tickets : id_to_tickets =
        Big_map.add ticket_id new_ticket ticket_storage.id_to_tickets
    in
    let new_booked_tickets : nat = event_to_book.booked_tickets + 1n in
    let updated_event_to_book : event = 
    {
        event_to_book with booked_tickets = new_booked_tickets
    }
    in
    let new_events : events = Big_map.update event_id (Some updated_event_to_book) ticket_storage.events in
    let new_ticket_storage : ticket_storage = 
        {
            ticket_storage with 
                id_to_tickets = new_id_to_tickets;
                event_to_tickets = new_event_to_tickets_map;
                events = new_events;
        }
    in
    let new_nft_token_storage = mint(mint_params, nft_token_storage) in
    {
        stor with
            ticket_storage = new_ticket_storage;
            nft_token_storage = new_nft_token_storage;
    }

let create_event(create_eve_p, tick_stor: create_eve_params * ticket_storage) : ticket_storage = 
    if Tezos.amount <> 5tz //require 5 tez for creation of event
    then (failwith "Not enough Fees for Creating event paid!" : ticket_storage)
    else
    let { accomodation_limit; description; ticket_price; date; ticket_extendable; free_upgradation; refreshments; special_access; related_events; } = create_eve_p in
    if date < Tezos.now 
    then (failwith "INVALID DATE PROVIDED!" : ticket_storage)
    else
    if accomodation_limit <= 0n
    then (failwith "INVALID ACCOMODATION LIMIT PROVIDED!" : ticket_storage)
    else if ticket_price <= 0tz
    then (failwith "INVALID PRICE PROVIDED!" : ticket_storage)
    else
    let event_ticket_features : ticket_features = 
    {
        ticket_extendable = ticket_extendable;
        free_upgradation = free_upgradation; 
        refreshments = refreshments;
        special_access = special_access;
    } in
    let new_event : event = 
    {
        owner = Tezos.sender;
        accomodation_limit = accomodation_limit;
        booked_tickets = 0n;
        ticket_price = ticket_price;
        description = description;
        date = date;
        related_events = related_events;
        ticket_features = event_ticket_features;
        claimed = false;
    } in 
    let eve_id = tick_stor.next_event_id in
    let new_events_map = Big_map.add eve_id new_event tick_stor.events in
    {
        tick_stor with 
            events = new_events_map;
            next_event_id = eve_id + 1n;
    }

let create_membership(create_mem_p, tick_stor: create_mem_params * ticket_storage) : ticket_storage = 
    if Tezos.amount <> 5tz //require 5 tez for creation of membership
    then(failwith "Not enough Fees for Creating Memebership paid!" : ticket_storage)
    else
    let { accomodation_limit; price_month } = create_mem_p in
    if accomodation_limit < 0n
    then (failwith "INVALID ACCOMODATION PROVIDED!" : ticket_storage) 
    else if price_month < 0tz
    then (failwith "INVALID PRICE PROVIDED!" : ticket_storage) 
    else
    let mem_id = tick_stor.next_mem_id in
    let new_membership : membership = 
    {
        mem_id = mem_id;
        owner = Tezos.sender;
        accomodation_limit = accomodation_limit;
        price_month = price_month;
        booked_mems = 0n;
        claimed = Set.literal [];
    } in
    let new_memberships = Big_map.add tick_stor.next_mem_id new_membership tick_stor.memberships in
    {
        tick_stor with
            memberships = new_memberships;
            next_mem_id = tick_stor.next_mem_id+1n;
    }


let buy_membership(buy_mem_p, stor: buy_membership_params * ticket_system_storage): ticket_system_storage = 
    let { nft_token_storage; ticket_storage; } = stor in
    let { mint_params; mem_id; start_date; duration_mon; } = buy_mem_p in
    let mem_to_take : membership =
        match Big_map.find_opt mem_id ticket_storage.memberships with
        | None -> (failwith "MEMBERSHIP WITH THIS ID NOT FOUND!" : membership)
        | Some (_mem)-> _mem
    in
    if mem_to_take.booked_mems = mem_to_take.accomodation_limit
    then (failwith "NO MORE MEMBERSHIP CAN BE TAKEN!" : ticket_system_storage) 
    else if (duration_mon < 0n) || (duration_mon > 12n)
    then (failwith "INVALID DURATION PROVIDED" : ticket_system_storage)
    else
    let price_to_pay = mem_to_take.price_month * duration_mon in
    if Tezos.amount < price_to_pay
    then (failwith "PLEASE PAY FULL AMOUNT TO TAKE MEMBERSHIP!": ticket_system_storage)
    else if start_date < Tezos.now 
    then (failwith "INVALID START DATE PROVIDED" : ticket_system_storage)
    else
    let time_sec : int = 2592000 * duration_mon in //30days have 2592000 seconds
    let valid_till_dur : timestamp = start_date + time_sec in
    let new_mem_ob : membership_pass = 
        {
            mem_id = mem_id;
            start_date = start_date;
            valid_till = valid_till_dur;
            price = Tezos.amount;
        } in
    let new_id_to_mem = Big_map.add nft_token_storage.next_token_id new_mem_ob ticket_storage.id_to_mem in
    let updated_mem_to_take = 
    {
        mem_to_take with 
            booked_mems = mem_to_take.booked_mems+1n;
    } in
    let updated_set : token_id_set= 
        match Big_map.find_opt mem_id ticket_storage.mem_to_mem_pass with
        | None -> Set.literal [nft_token_storage.next_token_id]    
        | Some(_mem_set) -> Set.add nft_token_storage.next_token_id _mem_set
    in
    let new_mem_set = Big_map.update mem_id (Some updated_set) ticket_storage.mem_to_mem_pass in
    let updated_memberships = Big_map.update mem_id (Some updated_mem_to_take) ticket_storage.memberships in
    let new_ticket_storage = {
        ticket_storage with 
            id_to_mem = new_id_to_mem;
            mem_to_mem_pass = new_mem_set;
            memberships = updated_memberships;
    } in
    let new_nft_token_storage = mint(mint_params, nft_token_storage) in
    {
        stor with
            nft_token_storage = new_nft_token_storage;
            ticket_storage = new_ticket_storage;
    }

let verify_ticket(ticket_valid_p, stor : ticket_valid_param * ticket_system_storage) : operation = 
    let { nft_token_storage; ticket_storage} = stor in
    let {ticket_id; event_id} = ticket_valid_p.verify_ticket_params in
    let ticket_already_present: bool = 
        match Big_map.find_opt Tezos.sender nft_token_storage.reverse_ledger with 
        //NFT storage find a NFT token with owner's address
        | None -> false
        | Some(tick) -> true
    in
    if not ticket_already_present 
    then (failwith "THERE IS NO TICKET ASSOCIATED WITH YOU!" : operation)
    else 
    let ticket_ob : ticket = 
        match Big_map.find_opt ticket_id ticket_storage.id_to_tickets with
        | None -> (failwith "NO TICKET DATA FOUND" : ticket)
        | Some(_tick)-> _tick
    in
    let ticket_valid_for_event : bool = 
        if Set.mem event_id ticket_ob.in_events
        then true
        else false
    in
    let ticket_valid_response : ticket_valid_response = 
        {
            verify_ticket_params = ticket_valid_p.verify_ticket_params;
            valid = ticket_valid_for_event;
        }
    in
    Tezos.transaction ticket_valid_response 0mutez ticket_valid_p.callback

let verify_memebership(mem_valid_p, stor : mem_valid_param * ticket_system_storage) : operation = 
    let { nft_token_storage; ticket_storage} = stor in
    let {mem_pass_id; mem_id} = mem_valid_p.verify_mem_params in
    let mem_token_already_present: bool = 
        match Big_map.find_opt Tezos.sender nft_token_storage.reverse_ledger with 
        //NFT storage find a NFT token with owner's address
        | None -> false
        | Some(tick) -> true
    in
    if not mem_token_already_present 
    then (failwith "THERE IS NO MEMBERSHIP TOKEN ASSOCIATED WITH YOU!" : operation)
    else 
    let mem_ob : membership_pass = 
        match Big_map.find_opt mem_pass_id ticket_storage.id_to_mem with
        | None -> (failwith "NO MEMBERSHIP DATA FOUND" : membership_pass)
        | Some(_mem)-> _mem
    in
    let mem_valid : bool = 
        if mem_ob.mem_id = mem_id && mem_ob.start_date > Tezos.now && mem_ob.valid_till < Tezos.now
        then true
        else false
    in
    let mem_valid_response : mem_valid_response = 
        {
            verify_mem_params = mem_valid_p.verify_mem_params;
            valid = mem_valid;
        }
    in
    Tezos.transaction mem_valid_response 0mutez mem_valid_p.callback
    
let refund_ticket(refund_p, stor : refund_ticket_param * ticket_system_storage) : return_ = 
    let { nft_token_storage; ticket_storage; } = stor in
    let { ticket_id; event_id; } = refund_p in
    let event_to_refund : event =
        match Big_map.find_opt event_id ticket_storage.events with
        | None -> (failwith "EVENT NOT FOUND!" : event)
        | Some (_event)-> _event
    in
    if Tezos.sender <> event_to_refund.owner
    then (failwith "You are Not Authorised to do this action" : return_)
    else if event_to_refund.date > Tezos.now
    then (failwith "CANNOT PROCESS REFUND AFTER COMMENCEMENT OF EVENT!" : return_)
    else
    let tickets_booked : token_id_set = 
        match Big_map.find_opt event_id ticket_storage.event_to_tickets with
        | None -> (failwith "EVENT TICKETS NOT FOUND!" : token_id_set)
        | Some (_token_id_set)-> _token_id_set
    in
    let ticket_in_list : bool = 
        Set.mem ticket_id tickets_booked
    in
    if not ticket_in_list
    then (failwith "TICKET NOT FOUND IN EVENTS LIST!" : return_ )
    else
    let ticket_ob : ticket = 
        match Big_map.find_opt ticket_id ticket_storage.id_to_tickets with
        | None -> (failwith "EVENT TICKETS NOT FOUND!" : ticket)
        | Some (ticket)-> ticket
    in
    let ticket_owner_address : address = 
        match Big_map.find_opt ticket_id nft_token_storage.ledger with
        | None -> (failwith "ADDRESS WITH TICKET NOT FOUND!" : address)
        | Some (addr)-> addr
    in
    let receiver : unit contract =
        match (Tezos.get_contract_opt ticket_owner_address : unit contract option) with
        | Some (contract) -> contract
        | None -> (failwith ("Not a contract") : (unit contract))
    in
    let transaction : operation = Tezos.transaction unit ticket_ob.price receiver in
    
    let updated_event_to_refund = 
    {
        event_to_refund with
            booked_tickets = abs(event_to_refund.booked_tickets - 1n);
    }
    in
    let updated_events : events = Big_map.update event_id (Some updated_event_to_refund) ticket_storage.events in
    let new_ticket_set = Set.remove ticket_id tickets_booked in
    let new_events_to_tickets = Big_map.update event_id (Some new_ticket_set) ticket_storage.event_to_tickets in
    let new_id_to_tickets = Big_map.remove ticket_id ticket_storage.id_to_tickets in 
    let new_ticket_storage = 
    {
        ticket_storage with
            id_to_tickets = new_id_to_tickets;
            event_to_tickets = new_events_to_tickets;
            events = updated_events;
    }in
    [transaction], {stor with ticket_storage=new_ticket_storage}
    

let refund_membership(refund_p, stor : refund_membership_param * ticket_system_storage) : return_ = 
    let { nft_token_storage; ticket_storage; } = stor in
    let { token_id; mem_id; } = refund_p in
    let mem_to_refund : membership =
        match Big_map.find_opt mem_id ticket_storage.memberships with
        | None -> (failwith "MEMBERSHIP NOT FOUND!" : membership)
        | Some (_mem)-> _mem
    in
    if Tezos.sender <> mem_to_refund.owner
    then (failwith "You are Not Authorised to do this action" : return_ )
    else
    let mems_bought : token_id_set = 
        match Big_map.find_opt mem_id ticket_storage.mem_to_mem_pass with
        | None -> (failwith "MEMBERSHIPS NOT FOUND!" : token_id_set)
        | Some (_token_id_set)-> _token_id_set
    in
    let mem_in_list : bool = 
        Set.mem token_id mems_bought
    in
    if not mem_in_list
    then (failwith "MEMBERSHIP NOT FOUND IN MEBERSHIPs LIST!" : return_)
    else
    let mem_ob : membership_pass = 
        match Big_map.find_opt token_id ticket_storage.id_to_mem with
        | None -> (failwith "MEMBERSHIP NOT FOUND!" : membership_pass)
        | Some (mem_pass)-> mem_pass
    in
    if mem_ob.valid_till > Tezos.now
    then (failwith "CANNOT PROCESS REFUND AFTER COMPLETION OF MEMBERSHIP!" : return_)
    else
    let mem_owner_address : address = 
        match Big_map.find_opt token_id nft_token_storage.ledger with
        | None -> (failwith "ADDRESS WITH TICKET NOT FOUND!" : address)
        | Some (addr)-> addr
    in
    let receiver : unit contract =
        match (Tezos.get_contract_opt mem_owner_address : unit contract option) with
        | Some (contract) -> contract
        | None -> (failwith ("Not a contract") : (unit contract))
    in
    let transaction : operation = Tezos.transaction unit mem_ob.price receiver in
    let new_mems_bought = 
        Set.remove token_id mems_bought 
    in
    let new_mem_to_mem_pass = Big_map.update mem_id (Some new_mems_bought) ticket_storage.mem_to_mem_pass in
    let new_id_to_mem = Big_map.remove token_id ticket_storage.id_to_mem in
    let new_mem_ob = 
    {
        mem_to_refund with 
            booked_mems = abs(mem_to_refund.booked_mems - 1n);
    }
    in
    let new_memberships = 
        Big_map.update mem_id (Some new_mem_ob) ticket_storage.memberships
    in
    let new_ticket_storage = 
    {
        ticket_storage with 
            id_to_mem = new_id_to_mem;
            mem_to_mem_pass = new_mem_to_mem_pass;
            memberships = new_memberships;
    }
    in
    [transaction], {stor with ticket_storage=new_ticket_storage}

let claim_eve_money(event_id, stor : event_id * ticket_system_storage) : return_ = 
    let { nft_token_storage; ticket_storage; } = stor in
    let event_to_claim : event =
        match Big_map.find_opt event_id ticket_storage.events with
        | None -> (failwith "EVENT NOT FOUND!" : event)
        | Some (_event)-> _event
    in
    if Tezos.sender <> event_to_claim.owner 
    then (failwith "You are not authorized to do this action!" : return_)
    else if event_to_claim.claimed 
    then (failwith "You already claimed money!" : return_)
    else if event_to_claim.date < Tezos.now 
    then (failwith "You cannt perform this action since event has not yet commenced!" : return_)
    else
    let amount = event_to_claim.booked_tickets * event_to_claim.ticket_price in
    let receiver : unit contract =
        match (Tezos.get_contract_opt event_to_claim.owner : unit contract option) with
        | Some (contract) -> contract
        | None -> (failwith ("Not a contract") : (unit contract))
    in
    let tr : operation = Tezos.transaction unit amount receiver in
    let updated_event_to_claim = 
    {
        event_to_claim with
            claimed=true;
    } in
    let updated_events = Big_map.update event_id (Some updated_event_to_claim) ticket_storage.events in
    let new_ticket_storage = 
    {
        ticket_storage with
            events = updated_events;
    }in
    [tr], {stor with ticket_storage=new_ticket_storage}
    
let claim_mem_money(claim_mem_p, stor : claim_mem_params * ticket_system_storage) : return_ =
    let { mem_id; mem_token_id; } = claim_mem_p in
    let { nft_token_storage; ticket_storage; } = stor in
    let mem_to_claim : membership_pass =
        match Big_map.find_opt mem_token_id ticket_storage.id_to_mem with
        | None -> (failwith "MEMBERSHIP PASS NOT FOUND!" : membership_pass)
        | Some (_mem_pass)-> _mem_pass
    in
    let mem_ship : membership = 
        match Big_map.find_opt mem_to_claim.mem_id ticket_storage.memberships with
        | None -> (failwith "MEMBERSHIP NOT FOUND!" : membership)
        | Some (_mem)-> _mem
    in
    if Tezos.sender <> mem_ship.owner 
    then (failwith "You are not authorized to do this action!" : return_)
    else if mem_to_claim.valid_till < Tezos.now 
    then (failwith "You cannt perform this action since membership has not yet ended!" : return_)
    else if Set.mem mem_token_id mem_ship.claimed
    then (failwith "ALREADY CLAIMED FOR THIS MEMEBRSHIP!" : return_)
    else
    let receiver : unit contract =
        match (Tezos.get_contract_opt mem_ship.owner : unit contract option) with
        | Some (contract) -> contract
        | None -> (failwith ("Not a contract") : (unit contract))
    in
    let tr = Tezos.transaction unit mem_to_claim.price receiver in
    let up_claimed = Set.add mem_token_id mem_ship.claimed in
    let updated_mem_ship = 
    {
        mem_ship with
            claimed=up_claimed;
    } in
    let updated_memberships = Big_map.update mem_id (Some updated_mem_ship) ticket_storage.memberships in
    let new_ticket_storage = 
    {
        ticket_storage with
            memberships = updated_memberships;
    } in
    [tr], {stor with ticket_storage=new_ticket_storage}


let ticket_main (param, storage : ticket_sys_entry_points * ticket_system_storage)
    : return_ =
  match param with
  | Transfer txs ->
    let {nft_token_storage; ticket_storage;} = storage in
    let (new_ledger, new_reverse_ledger) = transfer 
      (txs, default_operator_validator, nft_token_storage.operators, nft_token_storage.ledger, nft_token_storage.reverse_ledger) in
    let new_nft_storage = { nft_token_storage with ledger = new_ledger; reverse_ledger = new_reverse_ledger } in
    let new_storage = {storage with nft_token_storage=new_nft_storage; } in
    ([] : operation list), new_storage

  | Balance_of p ->
    let {nft_token_storage; ticket_storage;} = storage in
    let op = get_balance (p, nft_token_storage.ledger) in
    [op], storage

  | Update_operators updates ->
    let {nft_token_storage; ticket_storage;} = storage in
    let new_ops = fa2_update_operators (updates, nft_token_storage.operators) in
    let new_nft_storage = { nft_token_storage with operators = new_ops; } in
    let new_storage = {storage with nft_token_storage=new_nft_storage;} in
    ([] : operation list), new_storage

  | Mint p ->
    let { nft_token_storage; ticket_storage; } = storage in
    let new_nft_storage = mint (p, nft_token_storage) in
    let new_storage = {storage with nft_token_storage=new_nft_storage} in
    ([]: operation list), new_storage

  | Burn p ->
    let { nft_token_storage; ticket_storage; } = storage in
    let new_nft_storage = burn (p, nft_token_storage) in
    let new_storage = {storage with nft_token_storage=new_nft_storage} in
    ([]: operation list), new_storage
  | BuyTicket p ->
    ([]: operation list), buy_ticket(p, storage)  
  | CreateEvent p -> 
    let{ nft_token_storage; ticket_storage } = storage in
    let new_tick_storage = create_event(p, ticket_storage) in
    let new_storage = {storage with ticket_storage=new_tick_storage} in
    ([]: operation list), new_storage
  | CreateMembership p ->
    let {nft_token_storage;ticket_storage}= storage in
    let new_ticket_storage = create_membership(p, ticket_storage) in
    let new_storage = {storage with ticket_storage=new_ticket_storage} in
    ([]: operation list), new_storage
  | BuyMembership p ->
    let new_storage = buy_membership(p, storage) in
    ([]: operation list), new_storage

  | VerifyTicket p -> 
    let op = verify_ticket(p, storage) in
    [op], storage

  | VerifyMembership p -> 
    let op = verify_memebership(p, storage) in
    [op], storage

  | RefundTicket p -> refund_ticket(p, storage)

  | RefundMembership p -> refund_membership(p, storage)

  | ClaimEventMoney p -> claim_eve_money(p, storage)
    
  | ClaimMemMoney p -> claim_mem_money(p, storage)

//   | SellToken p -> 
