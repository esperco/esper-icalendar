(*
   iCalendar recurrence rule parsing
   https://icalendar.org/iCalendar-RFC-5545/3-8-5-3-recurrence-rule.html
*)

open Batteries
open Icalendar_t
open Log

(* Utility function for Atdgen validation *)
let check f x =
  match f [] x with
  | None -> x
  | Some err -> invalid_arg (Ag_util.Validation.string_of_error err)

let handle_scanf_error f =
  try Some (f ())
  with Scanf.Scan_failure _ | Failure _ | End_of_file -> None


(* Parsing *)

let parse_int_list valid l =
  List.map (fun s ->
    let n = int_of_string s in
    check valid n
  ) (String.nsplit l ~by:",")

let positive_int_of_string s =
  let n = int_of_string s in
  check Icalendar_v.validate_positive_int n

let parse_freq = function
  | "SECONDLY" -> `Secondly
  | "MINUTELY" -> `Minutely
  | "HOURLY" -> `Hourly
  | "DAILY" -> `Daily
  | "WEEKLY" -> `Weekly
  | "MONTHLY" -> `Monthly
  | "YEARLY" -> `Yearly
  | s -> invalid_arg ("Unrecognized freq " ^ s)

let parse_date d =
  handle_scanf_error (fun () ->
    if String.length d = 8 then (* Date *)
      Scanf.sscanf d "%4s%2s%2s" (fun y m d ->
        `Date (Util_dateonly.of_string (String.concat "-" [y; m; d]))
      )
    else (* Date-Time *)
      Scanf.sscanf d "%4s%2s%2sT%2s%2s%2s%s" (fun yr mo dy hr mi sc _z ->
        `Date_time (Util_localtime.No_timezone.of_string (
          String.concat "-" [yr; mo; dy] ^ "T" ^
          String.concat ":" [hr; mi; sc]
        ))
      )
  )

let parse_weekday = function
  | "SU" -> `Sunday
  | "MO" -> `Monday
  | "TU" -> `Tuesday
  | "WE" -> `Wednesday
  | "TH" -> `Thursday
  | "FR" -> `Friday
  | "SA" -> `Saturday
  | s -> invalid_arg ("Unrecognized weekday " ^ s)

let parse_byseclist l = parse_int_list Icalendar_v.validate_seconds l

let parse_byminlist l = parse_int_list Icalendar_v.validate_minutes l

let parse_byhrlist l = parse_int_list Icalendar_v.validate_hour l

let parse_weekdaynum s =
  let ordwk_char = ['0'; '1'; '2'; '3'; '4'; '5'; '6';
                    '7'; '8'; '9'; '0'; '+'; '-'] in
  if List.exists (String.contains s) ordwk_char then
    handle_scanf_error (fun () ->
      Scanf.sscanf s "%d%s" (fun n wday ->
        let ordwk = check Icalendar_v.validate_ordwk n in
        { ord = Some ordwk; day = parse_weekday wday }
      )
    )
  else Some { ord = None; day = parse_weekday s }

let parse_bywdaylist l =
  let parts = String.nsplit l ~by:"," in
  let days = List.filter_map parse_weekdaynum parts in
  if List.length days = List.length parts then Some days else None

let parse_bymodaylist l = parse_int_list Icalendar_v.validate_ordmoday l

let parse_byyrdaylist l = parse_int_list Icalendar_v.validate_ordyrday l

let parse_bywknolist l = parse_int_list Icalendar_v.validate_weeknum l

let parse_bymolist l = parse_int_list Icalendar_v.validate_monthnum l

let parse_bysplist l = parse_int_list Icalendar_v.validate_setposday l

let parse_rrule (rrule : string) : recur =
  let parts = String.nsplit rrule ~by:";" in
  let is_freq p =
    match String.split p ~by:"=" with
    | ("FREQ", _) -> true
    | _ -> false
  in
  let (freq, others) =
    match List.partition is_freq parts with
    | ([], _) -> invalid_arg ("Missing FREQ in " ^ rrule)
    | ([f], rest) ->
        let freq = snd (String.split f ~by:"=") in
        (parse_freq freq, rest)
    | (fs, _) -> invalid_arg ("Multiple FREQs in " ^ rrule)
  in
  let recur = Icalendar_v.create_recur ~freq () in
  let error param =
    invalid_arg ("Multiple " ^ param ^ "s in " ^ rrule)
  in
  List.fold_left (fun accu part ->
    let (k, v) = String.split part ~by:"=" in
    match (k, v) with
    | ("FREQ", f) -> assert false
    | ("UNTIL", d) ->
        if accu.until = None && accu.count = None then
          (match parse_date d with
          | None ->
              failwith ("Invalid date " ^ d ^ " in RRULE " ^ rrule)
          | Some date ->
              { accu with until = Some date }
          )
        else if accu.until = None then
          invalid_arg ("Both UNTIL and COUNT given in " ^ rrule)
        else error k
    | ("COUNT", n) ->
        if accu.count = None && accu.until = None then
          { accu with count = Some (positive_int_of_string n) }
        else if accu.count = None then
          invalid_arg ("Both COUNT and UNTIL given in " ^ rrule)
        else error k
    | ("INTERVAL", n) ->
        if accu.interval = None then
          { accu with interval = Some (positive_int_of_string n) }
        else error k
    | ("BYSECOND", l) ->
        if accu.bysecond = [] then
          { accu with bysecond = parse_byseclist l }
        else error k
    | ("BYMINUTE", l) ->
        if accu.byminute = [] then
          { accu with byminute = parse_byminlist l }
        else error k
    | ("BYHOUR", l) ->
        if accu.byhour = [] then
          { accu with byhour = parse_byhrlist l }
        else error k
    | ("BYDAY", l) ->
        if accu.byday = [] then
          (match parse_bywdaylist l with
          | None ->
              failwith ("Invalid BYDAY " ^ l ^ " in RRULE " ^ rrule)
          | Some wdaylist ->
              { accu with byday = wdaylist }
          )

        else error k
    | ("BYMONTHDAY", l) ->
        if accu.bymonthday = [] then
          { accu with bymonthday = parse_bymodaylist l }
        else error k
    | ("BYYEARDAY", l) ->
        if accu.byyearday = [] then
          { accu with byyearday = parse_byyrdaylist l }
        else error k
    | ("BYWEEKNO", l) ->
        if accu.byweekno = [] then
          { accu with byweekno = parse_bywknolist l }
        else error k
    | ("BYMONTH", l) ->
        if accu.bymonth = [] then
          { accu with bymonth = parse_bymolist l }
        else error k
    | ("BYSETPOS", l) ->
        if accu.bysetpos = [] then
          { accu with bysetpos = parse_bysplist l }
        else error k
    | ("WKST", day) ->
        if accu.wkst = None then
          { accu with wkst = Some (parse_weekday day) }
        else error k
    | _ -> invalid_arg ("Unrecognized RRULE part " ^ part)
  ) recur others

let try_parse rule accu =
  if String.starts_with rule "RRULE" then
    let (_, parts) = String.split rule ~by:":" in
    { accu with rrule = parse_rrule parts :: accu.rrule }
  else if String.starts_with rule "EXDATE" then
    let (before, after) = String.split rule ~by:":" in
    let param =
      if before = "EXDATE" then None
      else Some (String.lchop before ~n:7)
    in
    { accu with exdate = (param, after) :: accu.exdate }
  else if String.starts_with rule "RDATE" then
    let (before, after) = String.split rule ~by:":" in
    let param =
      if before = "RDATE" then None
      else Some (String.lchop before ~n:6)
    in
    { accu with rdate = (param, after) :: accu.rdate }
  else
    invalid_arg ("Unrecognized recurrence component: " ^ rule)

let parse (rules : string list) : recurrence =
  let recurrence = Icalendar_v.create_recurrence () in
  List.fold_right (fun rule accu ->
    try try_parse rule accu
    with e ->
      let err =
        Printexc.to_string e
          ^ " raised during parsing of recurrence rule "
          ^ rule
      in
      invalid_arg err
  ) rules recurrence

(* Printing *)

let print_int_list l =
  String.concat "," (List.map (fun n -> string_of_int n) l)

let print_freq = function
  | `Secondly -> "SECONDLY"
  | `Minutely -> "MINUTELY"
  | `Hourly -> "HOURLY"
  | `Daily -> "DAILY"
  | `Weekly -> "WEEKLY"
  | `Monthly -> "MONTHLY"
  | `Yearly -> "YEARLY"

let print_date = function
  | `Date_time d ->
      let open Util_localtime in
      Printf.sprintf "%04d%02d%02dT%02d%02d%02dZ"
        d.year d.month d.day d.hour d.min (int_of_float d.sec)
  | `Date d ->
      let open Util_dateonly in
      Printf.sprintf "%04d%02d%02d" d.year d.month d.day

let print_weekday = function
  | `Sunday -> "SU"
  | `Monday -> "MO"
  | `Tuesday -> "TU"
  | `Wednesday -> "WE"
  | `Thursday -> "TH"
  | `Friday -> "FR"
  | `Saturday -> "SA"

let print_bywdaylist l =
  String.concat "," (
    List.map (function
      | { ord = None; day } -> print_weekday day
      | { ord = Some ordwk; day } -> string_of_int ordwk ^ print_weekday day
    ) l
  )

let print_rrule (recur : recur) : string =
  let rule = "FREQ=" ^ print_freq recur.freq in
  let rule =
    match (recur.until, recur.count) with
    | (None, None) -> rule
    | (Some d, None) -> rule ^ ";UNTIL=" ^ print_date d
    | (None, Some n) -> rule ^ ";COUNT=" ^ string_of_int n
    | _ -> invalid_arg ("Both COUNT and UNTIL given in " ^
                        Icalendar_j.string_of_recur recur)
  in
  let rule =
    match recur.interval with
    | None -> rule
    | Some n -> rule ^ ";INTERVAL=" ^ string_of_int n
  in
  let rule =
    match recur.bysecond with
    | [] -> rule
    | l -> rule ^ ";BYSECOND=" ^ print_int_list l
  in
  let rule =
    match recur.byminute with
    | [] -> rule
    | l -> rule ^ ";BYMINUTE=" ^ print_int_list l
  in
  let rule =
    match recur.byhour with
    | [] -> rule
    | l -> rule ^ ";BYHOUR=" ^ print_int_list l
  in
  let rule =
    match recur.byday with
    | [] -> rule
    | l -> rule ^ ";BYDAY=" ^ print_bywdaylist l
  in
  let rule =
    match recur.bymonthday with
    | [] -> rule
    | l -> rule ^ ";BYMONTHDAY=" ^ print_int_list l
  in
  let rule =
    match recur.byyearday with
    | [] -> rule
    | l -> rule ^ ";BYYEARDAY=" ^ print_int_list l
  in
  let rule =
    match recur.byweekno with
    | [] -> rule
    | l -> rule ^ ";BYWEEKNO=" ^ print_int_list l
  in
  let rule =
    match recur.bymonth with
    | [] -> rule
    | l -> rule ^ ";BYMONTH=" ^ print_int_list l
  in
  let rule =
    match recur.bysetpos with
    | [] -> rule
    | l -> rule ^ ";BYSETPOS=" ^ print_int_list l
  in
  let rule =
    match recur.wkst with
    | None -> rule
    | Some day -> rule ^ ";WKST=" ^ print_weekday day
  in
  rule

let print_date_rule name (param, date_rule) =
  match param with
  | None -> name ^ ":" ^ date_rule
  | Some p -> name ^ ";" ^ p ^ ":" ^ date_rule

let print (rules : recurrence) : string list =
  List.map (fun recur -> "RRULE:" ^ print_rrule recur) rules.rrule
    @ List.map (print_date_rule "EXDATE") rules.exdate
    @ List.map (print_date_rule "RDATE") rules.rdate


(* Human-readable descriptions of recurrences
   e.g., "Every 3 weeks on Monday and Wednesday, until Jan 3, 2015" *)

exception Unsupported
  (* If you can't make this rule in our frontend interface,
     then we won't bother trying to summarize it,
     because recurrence rules can get really complicated.
     Our interface is modeled after Google's,
     so we're unlikely to encounter rules we can't summarize. *)

let rec summarize_weekday = function
  | { ord = None; day = `Sunday } -> "Sunday"
  | { ord = None; day = `Monday } -> "Monday"
  | { ord = None; day = `Tuesday } -> "Tuesday"
  | { ord = None; day = `Wednesday } -> "Wednesday"
  | { ord = None; day = `Thursday } -> "Thursday"
  | { ord = None; day = `Friday } -> "Friday"
  | { ord = None; day = `Saturday } -> "Saturday"
  | { ord = Some n; day } ->
      let ord =
        match n with
        | 1 -> "first"
        | 2 -> "second"
        | 3 -> "third"
        | 4 -> "fourth"
        | 5 -> "fifth"
        | -1 -> "last"
        | -2 -> "second-to-last"
        | -3 -> "third-to-last"
        | _ -> raise Unsupported
      in
      "the " ^ ord ^ " " ^ summarize_weekday { ord = None; day }

let has_all_weekdays l =
  let weekdays = [
    { ord = None; day = `Monday };
    { ord = None; day = `Tuesday };
    { ord = None; day = `Wednesday };
    { ord = None; day = `Thursday };
    { ord = None; day = `Friday };
  ] in
  List.for_all (fun d -> List.mem d l) weekdays

let summarize_weekdays = function
  | [] -> assert false
  | [day] -> summarize_weekday day
  | [day1; day2] -> summarize_weekday day1 ^ " and " ^ summarize_weekday day2
  | many_days ->
      if has_all_weekdays many_days then "weekdays" else
      let rev = List.rev many_days in
      let last = List.hd rev in
      let rest = List.rev (List.tl rev) in
      String.concat ", " (List.map summarize_weekday rest)
        ^ ", and " ^ summarize_weekday last

let summarize_date =
  let months = [|"Jan"; "Feb"; "Mar"; "Apr"; "May"; "Jun";
                 "Jul"; "Aug"; "Sep"; "Oct"; "Nov"; "Dec"|] in
  function
  | `Date_time d ->
      let open Util_localtime in
      Printf.sprintf "%s %d, %d" months.(d.month - 1) d.day d.year
  | `Date d ->
      let open Util_dateonly in
      Printf.sprintf "%s %d, %d" months.(d.month - 1) d.day d.year

let extract_by_filter period rule =
  let other_filters = [
    rule.bysecond;
    rule.byminute;
    rule.byhour;
    rule.byyearday;
    rule.byweekno;
    rule.bymonth;
    rule.bysetpos;
  ] in
  let no_filter =
    List.for_all (( = ) []) (
      List.map (fun _ -> 0) rule.byday
        :: rule.bymonthday
        :: other_filters
    ) && (rule.wkst = None || rule.wkst = Some `Sunday)
  in
  let just_byday =
    if List.for_all (( = ) []) (rule.bymonthday :: other_filters)
    && rule.byday <> []
    && (rule.wkst = None || rule.wkst = Some `Sunday)
    then Some rule.byday else None
  in
  let just_bymonthday =
    if List.for_all (( = ) []) (
      List.map (fun _ -> 0) rule.byday :: other_filters
    ) && rule.bymonthday <> []
    && (rule.wkst = None || rule.wkst = Some `Sunday)
    then Some rule.bymonthday else None
  in
  match period with
  | `Daily when no_filter -> `No_filter
  | `Weekly when no_filter -> `No_filter
  | `Weekly ->
      (match just_byday with
      | None -> raise Unsupported
      | Some l -> `Byday l
      )
  | `Monthly ->
      (match (just_byday, just_bymonthday) with
      | (None, None) -> raise Unsupported
      | (Some l, None) -> `Byday l
      | (None, Some l) -> `Bymonthday l
      | (Some _, Some _) -> raise Unsupported
      )
  | `Yearly when no_filter -> `No_filter
  | _ -> raise Unsupported

let summarize_if_supported rule =
  let interval = match rule.interval with None -> 1 | Some i -> i in
  let period = rule.freq in
  let repeats =
    match (interval, period) with
    | (_, `Secondly) -> raise Unsupported
    | (_, `Minutely) -> raise Unsupported
    | (_, `Hourly) -> raise Unsupported
    | (1, `Daily) -> "Daily"
    | (1, `Weekly) -> "Weekly"
    | (1, `Monthly) -> "Monthly"
    | (1, `Yearly) -> "Annually"
    | (n, `Daily) -> "Every " ^ string_of_int n ^ " days"
    | (n, `Weekly) -> "Every " ^ string_of_int n ^ " weeks"
    | (n, `Monthly) -> "Every " ^ string_of_int n ^ " months"
    | (n, `Yearly) -> "Every " ^ string_of_int n ^ " years"
  in
  let on =
    let byxxx = extract_by_filter period rule in
    match period with
    | `Daily ->
        "" (* No BYxxx supported in our/Google's interface for DAILY *)
    | `Weekly ->
        (match byxxx with
        | `No_filter -> "" (* TODO Use DTSTART? *)
        | `Byday days -> " on " ^ summarize_weekdays days
        | _ -> assert false
        )
    | `Monthly ->
        (match byxxx with
        | `Byday days -> " on " ^ summarize_weekdays days
        | `Bymonthday [n] ->
            if n > 0 then " on day " ^ string_of_int n
            else raise Unsupported
        | `Bymonthday _ -> raise Unsupported
        | _ -> assert false
        )
    | `Yearly ->
        "" (* No BYxxx supported in our/Google's interface for YEARLY *)
        (* TODO Mention DTSTART? Google does... *)
    | _ -> assert false
  in
  let until =
    match rule.until with None -> None | Some d -> Some (summarize_date d)
  in
  let count =
    match rule.count with None -> None | Some n -> Some (string_of_int n)
  in
  let extent =
    match (until, count) with
    | (None, None) -> "" (* forever *)
    | (Some date, None) -> ", until " ^ date
    | (None, Some occurrences) -> ", " ^ occurrences ^ " times"
    | (Some _, Some _) ->
        invalid_arg (
          "Both UNTIL and COUNT are given in " ^ print_rrule rule
        )
  in
  repeats ^ on ^ extent

let summarize (rule : recur) : string =
  try summarize_if_supported rule
  with Unsupported -> "Custom rule"


(* Tests *)

module Test = struct
  type test_case = {
    printed : string; (* RECUR value in RFC format *)
    parsed : recur; (* Expected result of parse function *)
    summarized : string; (* Human-readable description *)
  }

  let r freq = Icalendar_v.create_recur ~freq

  let dt unix =
    `Date_time (
      Util_localtime.of_float
        ~timezone:Util_timezone.utc
        (float_of_int unix)
    )

  (* Examples from RFC 5545 sec. 3.8.5.3. Recurrence Rule *)
  let examples = [
    { printed = "RRULE:FREQ=DAILY;COUNT=10";
      parsed = r `Daily ~count:10 ();
      summarized = "Daily, 10 times" };

    { printed = "RRULE:FREQ=DAILY;UNTIL=19971224T000000Z";
      parsed = r `Daily ~until:(dt 882921600) ();
      summarized = "Daily, until Dec 24, 1997" };

    { printed = "RRULE:FREQ=DAILY;INTERVAL=2";
      parsed = r `Daily ~interval:2 ();
      summarized = "Every 2 days" };

    { printed = "RRULE:FREQ=DAILY;INTERVAL=10;COUNT=5";
      parsed = r `Daily ~interval:10 ~count:5 ();
      summarized = "Every 10 days, 5 times" };

    { printed = "RRULE:FREQ=DAILY;UNTIL=20000131T140000Z;BYMONTH=1";
      parsed = r `Daily ~until:(dt 949327200)
                 ~bymonth:[1] ();
      summarized = "Custom rule" };

    { printed = "RRULE:FREQ=YEARLY;UNTIL=20000131T140000Z;BYMONTH=1;\
                 BYDAY=SU,MO,TU,WE,TH,FR,SA";
      parsed = r `Yearly ~until:(dt 949327200)
                 ~bymonth:[1]
                 ~byday:[{ ord = None; day = `Sunday };
                         { ord = None; day = `Monday };
                         { ord = None; day = `Tuesday };
                         { ord = None; day = `Wednesday };
                         { ord = None; day = `Thursday };
                         { ord = None; day = `Friday };
                         { ord = None; day = `Saturday };
                        ] ();
      summarized = "Custom rule" };

    { printed = "RRULE:FREQ=WEEKLY;COUNT=10";
      parsed = r `Weekly ~count:10 ();
      summarized = "Weekly, 10 times" };

    { printed = "RRULE:FREQ=WEEKLY;UNTIL=19971224T000000Z";
      parsed = r `Weekly ~until:(dt 882921600) ();
      summarized = "Weekly, until Dec 24, 1997" };

    { printed = "RRULE:FREQ=WEEKLY;INTERVAL=2;WKST=SU";
      parsed = r `Weekly ~interval:2 ~wkst:`Sunday ();
      summarized = "Every 2 weeks" };

    { printed = "RRULE:FREQ=WEEKLY;UNTIL=19971007T000000Z;WKST=SU;BYDAY=TU,TH";
      parsed = r `Weekly ~until:(dt 876182400)
                 ~wkst:`Sunday
                 ~byday:[{ ord = None; day = `Tuesday };
                         { ord = None; day = `Thursday };
                        ] ();
      summarized = "Weekly on Tuesday and Thursday, until Oct 7, 1997" };

    { printed = "RRULE:FREQ=WEEKLY;COUNT=10;WKST=SU;BYDAY=TU,TH";
      parsed = r `Weekly ~count:10 ~wkst:`Sunday
                 ~byday:[{ ord = None; day = `Tuesday };
                         { ord = None; day = `Thursday }
                        ] ();
      summarized = "Weekly on Tuesday and Thursday, 10 times" };

    { printed = "RRULE:FREQ=WEEKLY;INTERVAL=2;UNTIL=19971224T000000Z;\
                 WKST=SU;BYDAY=MO,WE,FR";
      parsed = r `Weekly ~interval:2
                 ~until:(dt 882921600)
                 ~wkst:`Sunday
                 ~byday:[{ ord = None; day = `Monday };
                         { ord = None; day = `Wednesday };
                         { ord = None; day = `Friday };
                        ] ();
      summarized = "Every 2 weeks on Monday, Wednesday, and Friday, \
                    until Dec 24, 1997" };

    { printed = "RRULE:FREQ=WEEKLY;INTERVAL=2;COUNT=8;WKST=SU;BYDAY=TU,TH";
      parsed = r `Weekly ~interval:2 ~count:8 ~wkst:`Sunday
                 ~byday:[{ ord = None; day = `Tuesday };
                         { ord = None; day = `Thursday };
                        ] ();
      summarized = "Every 2 weeks on Tuesday and Thursday, 8 times" };

    { printed = "RRULE:FREQ=MONTHLY;COUNT=10;BYDAY=1FR";
      parsed = r `Monthly ~count:10
                 ~byday:[ { ord = Some 1; day = `Friday }] ();
      summarized = "Monthly on the first Friday, 10 times" };

    { printed = "RRULE:FREQ=MONTHLY;UNTIL=19971224T000000Z;BYDAY=1FR";
      parsed = r `Monthly ~until:(dt 882921600)
                 ~byday:[{ ord = Some 1; day = `Friday }] ();
      summarized = "Monthly on the first Friday, until Dec 24, 1997" };

    { printed = "RRULE:FREQ=MONTHLY;INTERVAL=2;COUNT=10;BYDAY=1SU,-1SU";
      parsed = r `Monthly ~interval:2 ~count:10
                 ~byday:[{ ord = Some 1; day = `Sunday };
                         { ord = Some (-1); day = `Sunday };
                        ] ();
      summarized = "Every 2 months on the first Sunday and the last Sunday, \
                    10 times" };

    { printed = "RRULE:FREQ=MONTHLY;COUNT=6;BYDAY=-2MO";
      parsed = r `Monthly ~count:6
                 ~byday:[{ ord = Some (-2); day = `Monday }] ();
      summarized = "Monthly on the second-to-last Monday, 6 times" };

    { printed = "RRULE:FREQ=MONTHLY;BYMONTHDAY=-3";
      parsed = r `Monthly ~bymonthday:[-3] ();
      summarized = "Custom rule" };

    { printed = "RRULE:FREQ=MONTHLY;COUNT=10;BYMONTHDAY=2,15";
      parsed = r `Monthly ~count:10 ~bymonthday:[2; 15] ();
      summarized = "Custom rule" };

    { printed = "RRULE:FREQ=MONTHLY;COUNT=10;BYMONTHDAY=1,-1";
      parsed = r `Monthly ~count:10 ~bymonthday:[1; -1] ();
      summarized = "Custom rule" };

    { printed = "RRULE:FREQ=MONTHLY;INTERVAL=18;COUNT=10;\
                 BYMONTHDAY=10,11,12,13,14,15";
      parsed = r `Monthly ~interval:18 ~count:10
                 ~bymonthday:[10; 11; 12; 13; 14; 15] ();
      summarized = "Custom rule" };

    { printed = "RRULE:FREQ=MONTHLY;INTERVAL=2;BYDAY=TU";
      parsed = r `Monthly ~interval:2
                 ~byday:[{ ord = None; day = `Tuesday }] ();
      summarized = "Every 2 months on Tuesday" };

    { printed = "RRULE:FREQ=YEARLY;COUNT=10;BYMONTH=6,7";
      parsed = r `Yearly ~count:10 ~bymonth:[6; 7] ();
      summarized = "Custom rule" };

    { printed = "RRULE:FREQ=YEARLY;INTERVAL=2;COUNT=10;BYMONTH=1,2,3";
      parsed = r `Yearly ~interval:2 ~count:10 ~bymonth:[1; 2; 3] ();
      summarized = "Custom rule" };

    { printed = "RRULE:FREQ=YEARLY;INTERVAL=3;COUNT=10;BYYEARDAY=1,100,200";
      parsed = r `Yearly ~interval:3 ~count:10 ~byyearday:[1; 100; 200] ();
      summarized = "Custom rule" };

    { printed = "RRULE:FREQ=YEARLY;BYDAY=20MO";
      parsed = r `Yearly ~byday:[{ ord = Some 20; day = `Monday}] ();
      summarized = "Custom rule" };

    { printed = "RRULE:FREQ=YEARLY;BYWEEKNO=20;BYDAY=MO";
      parsed = r `Yearly ~byweekno:[20]
                 ~byday:[{ ord = None; day = `Monday}] ();
      summarized = "Custom rule" };

    { printed = "RRULE:FREQ=YEARLY;BYMONTH=3;BYDAY=TH";
      parsed = r `Yearly ~bymonth:[3]
                 ~byday:[{ ord = None; day = `Thursday}] ();
      summarized = "Custom rule" };

    { printed = "RRULE:FREQ=YEARLY;BYDAY=TH;BYMONTH=6,7,8";
      parsed = r `Yearly ~byday:[{ ord = None; day = `Thursday}]
                 ~bymonth:[6; 7; 8] ();
      summarized = "Custom rule" };

    { printed = "RRULE:FREQ=MONTHLY;BYDAY=FR;BYMONTHDAY=13";
      parsed = r `Monthly ~byday:[{ ord = None; day = `Friday}]
                 ~bymonthday:[13] ();
      summarized = "Custom rule" };

    { printed = "RRULE:FREQ=MONTHLY;BYDAY=SA;BYMONTHDAY=7,8,9,10,11,12,13";
      parsed = r `Monthly ~byday:[{ ord = None; day = `Saturday}]
                 ~bymonthday:[7; 8; 9; 10; 11; 12; 13] ();
      summarized = "Custom rule" };

    { printed = "RRULE:FREQ=YEARLY;INTERVAL=4;BYMONTH=11;BYDAY=TU;\
               BYMONTHDAY=2,3,4,5,6,7,8";
      parsed = r `Yearly ~interval:4 ~bymonth:[11]
                 ~byday:[{ ord = None; day = `Tuesday}]
                 ~bymonthday:[2; 3; 4; 5; 6; 7; 8] ();
      summarized = "Custom rule" };

    { printed = "RRULE:FREQ=MONTHLY;COUNT=3;BYDAY=TU,WE,TH;BYSETPOS=3";
      parsed = r `Monthly ~count:3
                 ~byday:[{ ord = None; day = `Tuesday};
                         { ord = None; day = `Wednesday};
                         { ord = None; day = `Thursday}]
                 ~bysetpos:[3] ();
      summarized = "Custom rule" };

    { printed = "RRULE:FREQ=MONTHLY;BYDAY=MO,TU,WE,TH,FR;BYSETPOS=-2";
      parsed = r `Monthly ~byday:[{ ord = None; day = `Monday};
                                  { ord = None; day = `Tuesday};
                                  { ord = None; day = `Wednesday};
                                  { ord = None; day = `Thursday};
                                  { ord = None; day = `Friday}]
                 ~bysetpos:[-2] ();
      summarized = "Custom rule" };

    { printed = "RRULE:FREQ=HOURLY;INTERVAL=3;UNTIL=19970902T170000Z";
      parsed = r `Hourly ~interval:3
                 ~until:(dt 873219600) ();
      summarized = "Custom rule" };

    { printed = "RRULE:FREQ=MINUTELY;INTERVAL=15;COUNT=6";
      parsed = r `Minutely ~interval:15 ~count:6 ();
      summarized = "Custom rule" };

    { printed = "RRULE:FREQ=MINUTELY;INTERVAL=90;COUNT=4";
      parsed = r `Minutely ~interval:90 ~count:4 ();
      summarized = "Custom rule" };

    { printed = "RRULE:FREQ=DAILY;BYHOUR=9,10,11,12,13,14,15,16;\
                 BYMINUTE=0,20,40";
      parsed = r `Daily ~byhour:[9; 10; 11; 12; 13; 14; 15; 16]
                 ~byminute:[0; 20; 40] ();
      summarized = "Custom rule" };

    { printed = "RRULE:FREQ=MINUTELY;INTERVAL=20;\
                 BYHOUR=9,10,11,12,13,14,15,16";
      parsed = r `Minutely ~interval:20
                 ~byhour:[9; 10; 11; 12; 13; 14; 15; 16] ();
      summarized = "Custom rule" };

    { printed = "RRULE:FREQ=WEEKLY;INTERVAL=3;COUNT=4;BYDAY=TU,SU;WKST=MO";
      parsed = r `Weekly ~interval:3 ~count:4
                 ~byday:[{ ord = None; day = `Tuesday};
                         { ord = None; day = `Sunday}]
                 ~wkst:`Monday ();
      summarized = "Custom rule" };

    { printed = "RRULE:FREQ=WEEKLY;INTERVAL=2;COUNT=4;BYDAY=TU,SU;WKST=SU";
      parsed = r `Weekly ~interval:2 ~count:4
                 ~byday:[{ ord = None; day = `Tuesday};
                         { ord = None; day = `Sunday}]
                 ~wkst:`Sunday ();
      summarized = "Every 2 weeks on Tuesday and Sunday, 4 times" };

    { printed = "RRULE:FREQ=MONTHLY;BYMONTHDAY=15,30;COUNT=5";
      parsed = r `Monthly ~bymonthday:[15; 30] ~count:5 ();
      summarized = "Custom rule" };
  ]

  let test_parsing () =
    List.iter (fun { printed; parsed } ->
      let expected = Icalendar_v.create_recurrence ~rrule:[parsed] () in
      try assert (parse [printed] = expected)
      with e ->
        logf `Error "Wrong parsed value for %s" printed;
        Trax.raise __LOC__ e
    ) examples;
    true

  let test_printing () =
    List.iter (fun { printed; parsed } ->
      let output =
        try List.hd (print (Icalendar_v.create_recurrence ~rrule:[parsed] ()))
        with Failure _ ->
          logf `Error "No printed output for rule %s" printed;
          assert false
      in
      let output_parts = String.(nsplit (lchop output ~n:6) ~by:";") in
      let expected_parts = String.(nsplit (lchop printed ~n:6) ~by:";") in
      try
        assert (
          List.for_all (fun p -> List.mem p output_parts) expected_parts
        )
      with e ->
        logf `Error "Wrong printed output %s (expected %s)" output printed;
        Trax.raise __LOC__ e
    ) examples;
    true

  let test_summarizing () =
    List.iter (fun { printed; parsed; summarized } ->
      let summary = summarize parsed in
      try assert (summary = summarized)
      with e ->
        logf `Error "Wrong summary \"%s\" for %s (expected %s)"
          summary printed summarized;
        Trax.raise __LOC__ e
    ) examples;
    true

  let tests = [
    ("test_rrule_parsing", test_parsing);
    ("test_rrule_printing", test_printing);
    ("test_rrule_summarizing", test_summarizing);
  ]
end

let tests = Test.tests
