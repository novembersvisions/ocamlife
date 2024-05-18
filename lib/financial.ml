open ANSITerminal

exception CreditLimitReached

let day = string_of_int (Unix.localtime (Unix.time ())).tm_mday
let month = string_of_int ((Unix.localtime (Unix.time ())).tm_mon + 1)
let year = string_of_int ((Unix.localtime (Unix.time ())).tm_year + 1900)
let curr_date = day ^ "-" ^ month ^ "-" ^ year
let user_financial_file user = "data/" ^ user ^ "_financials.csv"

let load_financial_data user =
  let path = user_financial_file user in
  if Sys.file_exists path then Csv.load path else [ [ "tb"; "0.0" ] ]

let save_financial_data user data =
  let path = user_financial_file user in
  Csv.save path data

(* accounts *)

let string_financial sheet aspect =
  List.fold_right
    (fun lst acc ->
      match lst with
      | [] -> acc
      | h :: t when h = aspect ->
          if acc <> "" then String.concat " " t ^ "\n" ^ acc
          else String.concat " " t
      | _ :: _ -> acc)
    sheet ""
  ^ "\n"

let view_financial user aspect =
  let sheet = Csv.load (user_financial_file user) in
  let header =
    if aspect = "account" then "\nAccount | Balance"
    else "\nName | Limit | Debt"
  in
  print_endline header;
  print_endline (string_financial sheet aspect)

let add_account user name balance =
  let data = load_financial_data user in
  let new_data = [ "account"; name; string_of_float balance ] :: data in
  save_financial_data user new_data;
  print_string [ Foreground Green ] "\nNew account saved successfully!\n"

let rec prompt_add_account user =
  print_string [ Reset ]
    "\nEnter 'back' to go back to the menu. \nEnter account name: ";
  let account_name = read_line () in
  if account_name = "back" then ()
  else if Data.search2 "account" account_name (user_financial_file user) then (
    print_string [ Foreground Red ] "\nThis account already exists!\n";
    prompt_add_account user)
  else
    let () = print_string [ Reset ] "Enter initial balance: " in
    try
      let input = read_line () in
      if input = "back" then ()
      else
        let balance = float_of_string input in
        add_account user account_name balance
    with _ ->
      print_string [ Foreground Red ] "\nPlease enter a number!\n";
      prompt_add_account user

let rec modify_financial name operation amount data aspect =
  match data with
  | [] -> []
  | h :: (t : string list list) -> (
      match h with
      | [] -> modify_financial name operation amount t aspect
      | a :: b :: c :: d when aspect = "account" && a = aspect && b = name -> (
          match operation with
          | "add" ->
              ([ aspect; name; string_of_float (float_of_string c +. amount) ]
              @ d)
              :: t
          | "subtract" ->
              ([ aspect; name; string_of_float (float_of_string c -. amount) ]
              @ d)
              :: t
          | "set" -> ([ aspect; name; string_of_float amount ] @ d) :: t
          | _ -> h :: modify_financial name operation amount t aspect)
      | a :: b :: l :: c :: d
        when aspect = "credit_card" && a = aspect && b = name -> (
          match operation with
          | "add" ->
              ([
                 aspect; name; l; string_of_float (float_of_string c +. amount);
               ]
              @ d)
              :: t
          | "subtract" ->
              ([
                 aspect; name; l; string_of_float (float_of_string c -. amount);
               ]
              @ d)
              :: t
          | "set" -> ([ aspect; name; l; string_of_float amount ] @ d) :: t
          | _ -> h :: modify_financial name operation amount t aspect)
      | _ :: _ -> h :: modify_financial name operation amount t aspect)

let edit_account_balance user name operation amount =
  let data = load_financial_data user in
  let modified_data = modify_financial name operation amount data "account" in
  save_financial_data user modified_data

let rec prompt_edit_account_amt user name operation =
  let () = print_string [ Reset ] "Enter amount: " in
  let input = read_line () in
  if input = "back" then ()
  else
    let amount = float_of_string_opt input in
    match amount with
    | None ->
        print_string [ Foreground Red ] "\nPlease enter a numerical amount!\n";
        prompt_edit_account_amt user name operation
    | Some amount ->
        edit_account_balance user name operation amount;
        print_string [ Foreground Green ]
          "\nAccount balance modified successfully.\n"

let rec prompt_edit_account user =
  let () =
    print_string [ Reset ]
      "\nEnter 'back' to go back to the menu. \nEnter account name to edit: "
  in
  let account_name = read_line () in
  if account_name = "back" then ()
  else if not (Data.search2 "account" account_name (user_financial_file user))
  then (
    print_string [ Foreground Red ] "\nThis account does not exist.\n";
    prompt_edit_account user)
  else
    let () = print_string [ Reset ] "Select operation (add/subtract/set): " in
    let op = read_line () in
    if op = "back" then ()
    else if op <> "add" && op <> "subtract" && op <> "set" then (
      print_string [ Foreground Red ] "\nPlease input add, subtract, or set.\n";
      prompt_edit_account user)
    else prompt_edit_account_amt user account_name op

let rec remove_financial lst aspect name =
  match lst with
  | [] -> raise Not_found
  | h :: t -> (
      match h with
      | [] -> remove_financial t aspect name
      | a :: b :: _ when a = aspect && b = name -> t
      | _ -> h :: remove_financial t aspect name)

let rec remove_account user =
  let path = user_financial_file user in
  print_string [ Reset ]
    "\nEnter 'back' to go back to the menu. \nEnter account name: ";
  let account = read_line () in
  if account = "back" then ()
  else (
    if account = "" then (
      print_string [ Foreground Red ] "\nSorry, this account does not exist!\n";
      remove_account user)
    else
      print_string [ Reset ]
        ("\nAre you sure you want to remove account " ^ account ^ "? (y/n) ");
    let confirm = read_line () in
    if confirm <> "y" then ()
    else
      try
        Csv.save path
          (remove_financial (load_financial_data user) "account" account);
        print_string [ Foreground Green ] "\nRemoved account successfully.\n"
      with Not_found ->
        print_string [ Foreground Red ] "Sorry, this account does not exist!\n";
        remove_account user)

(* credit cards *)
let rec add_credit_card user =
  print_string [ Reset ]
    "\nEnter 'back' to go back to the menu.\nEnter credit card name: ";
  let name = read_line () in
  if name = "back" then ()
  else if Data.search2 "credit_card" name (user_financial_file user) then (
    print_string [ Foreground Red ] "\nThis credit card already exists!\n";
    add_credit_card user)
  else
    let () = print_string [ Reset ] "Enter credit limit: " in
    let input = read_line () in
    if input = "back" then ()
    else
      let limit = float_of_string_opt input in
      match limit with
      | None ->
          print_string [ Foreground Red ] "\nPlease enter a numerical limit!\n";
          add_credit_card user
      | Some limit ->
          let data = load_financial_data user in
          let new_data =
            [ "credit_card"; name; string_of_float limit; "0.0" ] :: data
          in
          save_financial_data user new_data;
          print_string [ Foreground Green ]
            "\nCredit card added successfully.\n"

let rec remove_credit user =
  let path = user_financial_file user in
  print_string [ Reset ]
    "\nEnter 'back' to go back to the menu. \nEnter credit card name: ";
  let card = read_line () in
  if card = "back" then ()
  else if card = "" then (
    print_string [ Foreground Red ]
      "\nSorry, this credit card does not exist!\n";
    remove_credit user)
  else (
    print_string [ Reset ]
      ("\nAre you sure you want to remove credit card " ^ card ^ "? (y/n) ");
    let confirm = read_line () in
    if confirm <> "y" then ()
    else
      try
        Csv.save path
          (remove_financial (load_financial_data user) "credit_card" card);
        print_string [ Foreground Green ]
          "\nRemoved credit card successfully.\n"
      with Not_found ->
        print_string [ Foreground Red ]
          "Sorry, this credit card does not exist!\n";
        remove_credit user)

let edit_credit_balance user name operation amount =
  let data = load_financial_data user in
  let modified_data =
    modify_financial name operation amount data "credit_card"
  in
  save_financial_data user modified_data

(* Cross functionality *)

let pay_credit_card_balance user credit_name account_name amount =
  let data = load_financial_data user in
  let credit_card, others =
    List.partition
      (fun row ->
        List.nth row 1 = credit_name && List.nth row 0 = "credit_card")
      data
  in
  let account, _ =
    List.partition
      (fun row -> List.nth row 1 = account_name && List.nth row 0 = "account")
      others
  in
  match (credit_card, account) with
  | [ [ "credit_card"; _; _; balance ] ], [ [ "account"; _; account_balance ] ]
    ->
      let str_balance = balance in
      let balance = float_of_string balance in
      if balance = 0. then
        print_string [ Foreground Red ]
          "\nYou do not have a balance to pay off!\n"
      else if amount > balance then
        print_string [ Foreground Red ]
          ("\nYou are paying too much! Please pay " ^ str_balance
         ^ " or less.\n")
      else
        let account_balance = float_of_string account_balance in
        let pay_amount = min balance (min amount account_balance) in
        if pay_amount > 0.0 && account_balance >= amount then (
          edit_account_balance user account_name "subtract" pay_amount;
          edit_credit_balance user credit_name "subtract" pay_amount;
          print_string [ Foreground Green ] "\nPayment successful.\n")
        else
          print_string [ Foreground Red ]
            "\nInvalid payment amount or insufficient funds.\n"
  | _ -> print_string [ Foreground Red ] "\nCredit card or account not found.\n"

let rec prompt_pay_credit user =
  try
    print_string [ Reset ]
      "\nEnter 'back' to go back to the menu. \nEnter credit card name: ";
    let card = read_line () in
    if card = "back" then ()
    else if
      card = ""
      || not (Data.search2 "credit_card" card (user_financial_file user))
    then (
      print_string [ Foreground Red ]
        "\n\
         Sorry, this credit card does not exist! If you do not have a card, \
         please add one.\n";
      prompt_pay_credit user)
    else (
      print_string [ Reset ] "\nEnter bank account name: ";
      let account = read_line () in
      if account = "back" then ()
      else if
        account = ""
        || not (Data.search2 "account" account (user_financial_file user))
      then (
        print_string [ Foreground Red ]
          "\n\
           Sorry, this bank account does not exist! If you do not have an \
           account, please add one.\n";
        prompt_pay_credit user)
      else (
        print_string [ Reset ] "\nEnter amount of money to transfer: ";
        let amount = read_line () in
        if amount = "back" then ()
        else
          let amount = float_of_string amount in
          pay_credit_card_balance user card account amount))
  with _ ->
    print_string [ Foreground Red ] "\nPlease enter a numerical amount.\n";
    prompt_pay_credit user

(* transactions *)

let user_transaction_log_file user = "data/" ^ user ^ "_transaction_log.csv"

let log_transaction user t_type date amount entity =
  let path = user_transaction_log_file user in
  let data = Csv.load path in
  let new_entry = [ date; t_type; string_of_float amount; entity ] in
  let updated_data = new_entry :: data in
  Csv.save path updated_data;
  print_string [ Foreground Green ] "\nTransaction made successfully!\n"

let modify_credit_data card amount data =
  List.map
    (fun row ->
      match row with
      | a :: b :: c :: d :: _ when a = "credit_card" && b = card ->
          if amount > float_of_string c -. float_of_string d then
            raise CreditLimitReached
          else [ a; b; c; string_of_float (float_of_string d +. amount) ]
      | _ -> row)
    data

let charge_credit_card user card typ amount entity =
  let data = load_financial_data user in
  try
    let modified_data = modify_credit_data card amount data in
    save_financial_data user modified_data;
    log_transaction user typ curr_date amount entity
  with
  | CreditLimitReached ->
      print_string [ Foreground Red ]
        "\n\
         You cannot spend money past your credit limit! Please try again with \
         another card, or pay this one off first.\n"
  | _ ->
      print_string [ Foreground Red ] "\nYou do not have a credit card yet.\n"

let rec make_transaction user =
  try
    print_string [ Reset ]
      "\nEnter 'back' to go back to the menu. \nEnter credit card name: ";
    let card = read_line () in
    if card = "back" then ()
    else if
      card = ""
      || not (Data.search2 "credit_card" card (user_financial_file user))
    then (
      print_string [ Foreground Red ]
        "\n\
         Sorry, this credit card does not exist! If you do not have a card, \
         please add one.\n";
      make_transaction user)
    else (
      print_string [ Reset ]
        "\nEnter type of transaction (bill, shopping, etc): ";
      let typ = read_line () in
      if typ = "back" then ()
      else (
        print_string [ Reset ] "\nEnter amount of money: ";
        let amount = read_line () in
        if amount = "back" then ()
        else
          let amount = float_of_string amount in
          print_string [ Reset ]
            "\nEnter the person/company receiving the money: ";
          let entity = read_line () in
          if entity = "back" then ()
          else charge_credit_card user card typ amount entity))
  with _ ->
    print_string [ Foreground Red ] "\nPlease enter a numerical amount.\n";
    make_transaction user

let view_transactions user =
  Data.see_history "\nDate | Type | Amount | Company/Person"
    (user_transaction_log_file user)
