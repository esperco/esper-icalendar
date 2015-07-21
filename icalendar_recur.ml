open Batteries
open Icalendar_t
open Log

(* Utility function for Atdgen validation *)
let check f x =
  match f [] x with
  | None -> x
  | Some err ->
      raise (Invalid_argument (Ag_util.Validation.string_of_error err))


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
  | s -> raise (Invalid_argument ("Unrecognized freq " ^ s))

let parse_date d =
  if String.length d = 8 then (* Date *)
    Scanf.sscanf d "%4s%2s%2s" (fun y m d ->
      Util_localtime.of_string (String.concat "-" [y; m; d])
    )
  else (* Date-Time *)
    Scanf.sscanf d "%4s%2s%2sT%2s%2s%2s%s" (fun yr mo dy hr mi sc _z ->
      Util_localtime.of_string (
        String.concat "-" [yr; mo; dy] ^ "T" ^
        String.concat ":" [hr; mi; sc]
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
  | s -> raise (Invalid_argument ("Unrecognized weekday " ^ s))

let parse_byseclist l = parse_int_list Icalendar_v.validate_seconds l

let parse_byminlist l = parse_int_list Icalendar_v.validate_minutes l

let parse_byhrlist l = parse_int_list Icalendar_v.validate_hour l

let parse_weekdaynum s =
  let ordwk_char = ['0'; '1'; '2'; '3'; '4'; '5'; '6';
                    '7'; '8'; '9'; '0'; '+'; '-'] in
  if List.exists (String.contains s) ordwk_char then
    Scanf.sscanf s "%d%s" (fun n wday ->
      let ordwk = check Icalendar_v.validate_ordwk n in
      (Some ordwk, parse_weekday wday)
    )
  else (None, parse_weekday s)

let parse_bywdaylist l = List.map parse_weekdaynum (String.nsplit l ~by:",")

let parse_bymodaylist l = parse_int_list Icalendar_v.validate_ordmoday l

let parse_byyrdaylist l = parse_int_list Icalendar_v.validate_ordyrday l

let parse_bywknolist l = parse_int_list Icalendar_v.validate_weeknum l

let parse_bymolist l = parse_int_list Icalendar_v.validate_monthnum l

let parse_bysplist l = parse_int_list Icalendar_v.validate_setposday l

let parse_recur_rule_part p =
  match String.split p ~by:"=" with
  | ("FREQ", f) -> `Freq (parse_freq f)
  | ("UNTIL", d) -> `Until (parse_date d)
  | ("COUNT", n) -> `Count (positive_int_of_string n)
  | ("INTERVAL", n) -> `Interval (positive_int_of_string n)
  | ("BYSECOND", l) -> `Bysecond (parse_byseclist l)
  | ("BYMINUTE", l) -> `Byminute (parse_byminlist l)
  | ("BYHOUR", l) -> `Byhour (parse_byhrlist l)
  | ("BYDAY", l) -> `Byday (parse_bywdaylist l)
  | ("BYMONTHDAY", l) -> `Bymonthday (parse_bymodaylist l)
  | ("BYYEARDAY", l) -> `Byyearday (parse_byyrdaylist l)
  | ("BYWEEKNO", l) -> `Byweekno (parse_bywknolist l)
  | ("BYMONTH", l) -> `Bymonth (parse_bymolist l)
  | ("BYSETPOS", l) -> `Bysetpos (parse_bysplist l)
  | ("WKST", day) -> `Wkst (parse_weekday day)
  | _ -> raise (Invalid_argument ("Unrecognized recur_rule_part " ^ p))

let parse (rrule : string) : recur_rule_part list =
  let parts =
    if String.starts_with rrule "RRULE:"
    then String.lchop rrule ~n:6
    else ""
  in
  List.map parse_recur_rule_part (String.nsplit parts ~by:";")


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

let print_date d =
  let open Util_localtime in
  Printf.sprintf "%04d%02d%02dT%02d%02d%02dZ"
    d.year d.month d.day d.hour d.min (int_of_float d.sec)

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
      | (None, day) -> print_weekday day
      | (Some ordwk, day) -> string_of_int ordwk ^ print_weekday day
    ) l
  )

let print_recur_rule_part = function
  | `Freq f -> "FREQ=" ^ print_freq f
  | `Until d -> "UNTIL=" ^ print_date d
  | `Count n -> "COUNT=" ^ string_of_int n
  | `Interval n -> "INTERVAL=" ^ string_of_int n
  | `Bysecond l -> "BYSECOND=" ^ print_int_list l
  | `Byminute l -> "BYMINUTE=" ^ print_int_list l
  | `Byhour l -> "BYHOUR=" ^ print_int_list l
  | `Byday l -> "BYDAY=" ^ print_bywdaylist l
  | `Bymonthday l -> "BYMONTHDAY=" ^ print_int_list l
  | `Byyearday l -> "BYYEARDAY=" ^ print_int_list l
  | `Byweekno l -> "BYWEEKNO=" ^ print_int_list l
  | `Bymonth l -> "BYMONTH=" ^ print_int_list l
  | `Bysetpos l -> "BYSETPOS=" ^ print_int_list l
  | `Wkst day -> "WKST=" ^ print_weekday day

let print (rrule : recur_rule_part list) : string =
  "RRULE:" ^ String.concat ";" (List.map print_recur_rule_part rrule)


(* Human-readable descriptions of recurrences
   e.g., "Every 3 weeks on Monday and Wednesday, until Jan 3, 2015" *)

exception Unsupported
  (* If you can't make this rule in our frontend interface,
     then we won't bother trying to summarize it,
     because recurrence rules can get really complicated.
     Our interface is modeled after Google's,
     so we're unlikely to encounter rules we can't summarize. *)

let rec summarize_weekday = function
  | (None, `Sunday) -> "Sunday"
  | (None, `Monday) -> "Monday"
  | (None, `Tuesday) -> "Tuesday"
  | (None, `Wednesday) -> "Wednesday"
  | (None, `Thursday) -> "Thursday"
  | (None, `Friday) -> "Friday"
  | (None, `Saturday) -> "Saturday"
  | (Some n, day) ->
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
      "the " ^ ord ^ " " ^ summarize_weekday (None, day)

let has_all_weekdays l =
  let weekdays = [
    (None, `Monday);
    (None, `Tuesday);
    (None, `Wednesday);
    (None, `Thursday);
    (None, `Friday);
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

let summarize_date d =
  let open Util_localtime in
  let months = [|"Jan"; "Feb"; "Mar"; "Apr"; "May"; "Jun";
                 "Jul"; "Aug"; "Sep"; "Oct"; "Nov"; "Dec"|] in
  Printf.sprintf "%s %d, %d" months.(d.month - 1) d.day d.year

let extract_by_filter period rule =
  let byxxx =
    List.filter (function
      | `Byhour _ | `Byday _ | `Bymonthday _ | `Byyearday _
      | `Byweekno _ | `Bymonth _ | `Bysetpos _ ->
          true
      | `Wkst n when n <> `Sunday ->
          (* Affects ByXXX, so considered one *)
          true
      | _ -> false
    ) rule
  in
  match (period, byxxx) with
  | (`Daily, []) -> `No_filter
  | (`Weekly, []) -> `No_filter
  | (`Weekly, [`Byday l]) -> `Byday l
  | (`Monthly, [`Byday l]) -> `Byday l
  | (`Monthly, [`Bymonthday l]) -> `Bymonthday l
  | (`Yearly, []) -> `No_filter
  | _ -> raise Unsupported

let summarize_if_supported rule =
  let interval =
    try List.find_map (function `Interval i -> Some i | _ -> None) rule
    with Not_found -> 1
  in
  let period =
    try List.find_map (function `Freq f -> Some f | _ -> None) rule
    with Not_found -> raise (Failure ("Missing FREQ in " ^ print rule))
  in
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
    try
      List.find_map (function
        | `Until d -> Some (Some (summarize_date d))
        | _ -> None
      ) rule
    with Not_found -> None
  in
  let count =
    try
      List.find_map (function
        | `Count n -> Some (Some (string_of_int n))
        | _ -> None
      ) rule
    with Not_found -> None
  in
  let extent =
    match (until, count) with
    | (None, None) -> "" (* forever *)
    | (Some date, None) -> ", until " ^ date
    | (None, Some occurrences) -> ", " ^ occurrences ^ " times"
    | (Some _, Some _) ->
        raise (Failure ("Both UNTIL and COUNT are given in " ^ print rule))
  in
  repeats ^ on ^ extent

let summarize (rule : recur_rule_part list) : string =
  try summarize_if_supported rule
  with Unsupported -> "Custom rule"


(* More JS-friendly format *)

let js_recur_of_parts (rule : recur_rule_part list) : js_recur =
  let is_freq = function `Freq _ -> true | _ -> false in
  let (freq, others) =
    match List.partition is_freq rule with
    | ([], _) ->
        raise (Failure ("Missing FREQ in " ^ print rule))
    | ([f], rest) ->
        (match f with `Freq freq -> (freq, rest) | _ -> assert false)
    | (fs, _) ->
        raise (Failure ("Multiple FREQs in " ^ print rule))
  in
  let init = Icalendar_v.create_js_recur ~freq () in
  List.fold_left (fun accu -> function
    | `Freq _ -> assert false
    | `Until d ->
        if accu.until = None && accu.count = None then
          { accu with until = Some d }
        else if accu.until = None then
          raise (Failure ("Both UNTIL and COUNT given in " ^ print rule))
        else
          raise (Failure ("Multiple UNTILs in " ^ print rule))
    | `Count n ->
        if accu.count = None && accu.until = None then
          { accu with count = Some n }
        else if accu.count = None then
          raise (Failure ("Both COUNT and UNTIL given in " ^ print rule))
        else
          raise (Failure ("Multiple COUNTs in " ^ print rule))
    | `Interval n ->
        if accu.interval = None then { accu with interval = Some n }
        else raise (Failure ("Multiple INTERVALs in " ^ print rule))
    | `Bysecond l ->
        if accu.bysecond = [] then { accu with bysecond = l }
        else raise (Failure ("Multiple BYSECONDs in " ^ print rule))
    | `Byminute l ->
        if accu.byminute = [] then { accu with byminute = l }
        else raise (Failure ("Multiple BYMINUTEs in " ^ print rule))
    | `Byhour l ->
        if accu.byhour = [] then { accu with byhour = l }
        else raise (Failure ("Multiple BYHOURs in " ^ print rule))
    | `Byday l ->
        if accu.byday = [] then { accu with byday = l }
        else raise (Failure ("Multiple BYDAYs in " ^ print rule))
    | `Bymonthday l ->
        if accu.bymonthday = [] then { accu with bymonthday = l }
        else raise (Failure ("Multiple BYMONTHDAYs in " ^ print rule))
    | `Byyearday l ->
        if accu.byyearday = [] then { accu with byyearday = l }
        else raise (Failure ("Multiple BYYEARDAYs in " ^ print rule))
    | `Byweekno l ->
        if accu.byweekno = [] then { accu with byweekno = l }
        else raise (Failure ("Multiple BYWEEKNOs in " ^ print rule))
    | `Bymonth l ->
        if accu.bymonth = [] then { accu with bymonth = l }
        else raise (Failure ("Multiple BYMONTHs in " ^ print rule))
    | `Bysetpos l ->
        if accu.bysetpos = [] then { accu with bysetpos = l }
        else raise (Failure ("Multiple BYSETPOSs in " ^ print rule))
    | `Wkst day ->
        if accu.wkst = None then { accu with wkst = Some day }
        else raise (Failure ("Multiple WKSTs in " ^ print rule))
  ) init others

let parts_of_js_recur (js_rule : js_recur) : recur_rule_part list  =
  let rule = [`Freq js_rule.freq] in
  let rule =
    match (js_rule.until, js_rule.count) with
    | (None, None) -> rule
    | (Some n, None) -> `Until n :: rule
    | (None, Some n) -> `Count n :: rule
    | _ -> raise (Failure ("Both COUNT and UNTIL given in " ^
                            Icalendar_j.string_of_js_recur js_rule))
  in
  let rule =
    match js_rule.interval with None -> rule | Some n -> `Interval n :: rule
  in
  let rule =
    match js_rule.bysecond with [] -> rule | l -> `Bysecond l :: rule
  in
  let rule =
    match js_rule.byminute with [] -> rule | l -> `Byminute l :: rule
  in
  let rule =
    match js_rule.byhour with [] -> rule | l -> `Byhour l :: rule
  in
  let rule =
    match js_rule.byday with [] -> rule | l -> `Byday l :: rule
  in
  let rule =
    match js_rule.bymonthday with [] -> rule | l -> `Bymonthday l :: rule
  in
  let rule =
    match js_rule.byyearday with [] -> rule | l -> `Byyearday l :: rule
  in
  let rule =
    match js_rule.byweekno with [] -> rule | l -> `Byweekno l :: rule
  in
  let rule =
    match js_rule.bymonth with [] -> rule | l -> `Bymonth l :: rule
  in
  let rule =
    match js_rule.bysetpos with [] -> rule | l -> `Bysetpos l :: rule
  in
  let rule =
    match js_rule.wkst with None -> rule | Some day -> `Wkst day :: rule
  in
  List.rev rule


(* Tests *)

module Test = struct
  type test_case = {
    printed : string; (* RECUR value in RFC format *)
    parsed : recur_rule_part list; (* Expected result of parse function *)
    summarized : string; (* Human-readable description *)
  }

  (* Examples from RFC 5545 sec. 3.8.5.3. Recurrence Rule *)
  let examples = [
    { printed = "RRULE:FREQ=DAILY;COUNT=10";
      parsed = [`Freq `Daily; `Count 10];
      summarized = "Daily, 10 times" };

    { printed = "RRULE:FREQ=DAILY;UNTIL=19971224T000000Z";
      parsed = [`Freq `Daily; `Until (Util_localtime.of_float 882921600.)];
      summarized = "Daily, until Dec 24, 1997" };

    { printed = "RRULE:FREQ=DAILY;INTERVAL=2";
      parsed = [`Freq `Daily; `Interval 2];
      summarized = "Every 2 days" };

    { printed = "RRULE:FREQ=DAILY;INTERVAL=10;COUNT=5";
      parsed = [`Freq `Daily; `Interval 10; `Count 5];
      summarized = "Every 10 days, 5 times" };

    { printed = "RRULE:FREQ=DAILY;UNTIL=20000131T140000Z;BYMONTH=1";
      parsed = [`Freq `Daily; `Until (Util_localtime.of_float 949327200.);
                `Bymonth [1]];
      summarized = "Custom rule" };

    { printed = "RRULE:FREQ=YEARLY;UNTIL=20000131T140000Z;BYMONTH=1;\
                 BYDAY=SU,MO,TU,WE,TH,FR,SA";
      parsed = [`Freq `Yearly; `Until (Util_localtime.of_float 949327200.);
                `Bymonth [1]; `Byday [(None, `Sunday); (None, `Monday);
                                      (None, `Tuesday); (None, `Wednesday);
                                      (None, `Thursday); (None, `Friday);
                                      (None, `Saturday)]];
      summarized = "Custom rule" };

    { printed = "RRULE:FREQ=WEEKLY;COUNT=10";
      parsed = [`Freq `Weekly; `Count 10];
      summarized = "Weekly, 10 times" };

    { printed = "RRULE:FREQ=WEEKLY;UNTIL=19971224T000000Z";
      parsed = [`Freq `Weekly; `Until (Util_localtime.of_float 882921600.)];
      summarized = "Weekly, until Dec 24, 1997" };

    { printed = "RRULE:FREQ=WEEKLY;INTERVAL=2;WKST=SU";
      parsed = [`Freq `Weekly; `Interval 2; `Wkst `Sunday];
      summarized = "Every 2 weeks" };

    { printed = "RRULE:FREQ=WEEKLY;UNTIL=19971007T000000Z;WKST=SU;BYDAY=TU,TH";
      parsed = [`Freq `Weekly; `Until (Util_localtime.of_float 876182400.);
                `Wkst `Sunday; `Byday [(None, `Tuesday); (None, `Thursday)]];
      summarized = "Weekly on Tuesday and Thursday, until Oct 7, 1997" };

    { printed = "RRULE:FREQ=WEEKLY;COUNT=10;WKST=SU;BYDAY=TU,TH";
      parsed = [`Freq `Weekly; `Count 10; `Wkst `Sunday;
                `Byday [(None, `Tuesday); (None, `Thursday)]];
      summarized = "Weekly on Tuesday and Thursday, 10 times" };

    { printed = "RRULE:FREQ=WEEKLY;INTERVAL=2;UNTIL=19971224T000000Z;\
                 WKST=SU;BYDAY=MO,WE,FR";
      parsed = [`Freq `Weekly; `Interval 2;
                `Until (Util_localtime.of_float 882921600.);
                `Wkst `Sunday;
                `Byday [(None, `Monday); (None, `Wednesday);
                        (None, `Friday)]];
      summarized = "Every 2 weeks on Monday, Wednesday, and Friday, \
                    until Dec 24, 1997" };

    { printed = "RRULE:FREQ=WEEKLY;INTERVAL=2;COUNT=8;WKST=SU;BYDAY=TU,TH";
      parsed = [`Freq `Weekly; `Interval 2; `Count 8; `Wkst `Sunday;
                `Byday [(None, `Tuesday); (None, `Thursday)]];
      summarized = "Every 2 weeks on Tuesday and Thursday, 8 times" };

    { printed = "RRULE:FREQ=MONTHLY;COUNT=10;BYDAY=1FR";
      parsed = [`Freq `Monthly; `Count 10; `Byday [(Some 1, `Friday)]];
      summarized = "Monthly on the first Friday, 10 times" };

    { printed = "RRULE:FREQ=MONTHLY;UNTIL=19971224T000000Z;BYDAY=1FR";
      parsed = [`Freq `Monthly; `Until (Util_localtime.of_float 882921600.);
                `Byday [(Some 1, `Friday)]];
      summarized = "Monthly on the first Friday, until Dec 24, 1997" };

    { printed = "RRULE:FREQ=MONTHLY;INTERVAL=2;COUNT=10;BYDAY=1SU,-1SU";
      parsed = [`Freq `Monthly; `Interval 2; `Count 10;
                `Byday [(Some 1, `Sunday); (Some (-1), `Sunday)]];
      summarized = "Every 2 months on the first Sunday and the last Sunday, \
                    10 times" };

    { printed = "RRULE:FREQ=MONTHLY;COUNT=6;BYDAY=-2MO";
      parsed = [`Freq `Monthly; `Count 6; `Byday [(Some (-2), `Monday)]];
      summarized = "Monthly on the second-to-last Monday, 6 times" };

    { printed = "RRULE:FREQ=MONTHLY;BYMONTHDAY=-3";
      parsed = [`Freq `Monthly; `Bymonthday [-3]];
      summarized = "Custom rule" };

    { printed = "RRULE:FREQ=MONTHLY;COUNT=10;BYMONTHDAY=2,15";
      parsed = [`Freq `Monthly; `Count 10; `Bymonthday [2; 15]];
      summarized = "Custom rule" };

    { printed = "RRULE:FREQ=MONTHLY;COUNT=10;BYMONTHDAY=1,-1";
      parsed = [`Freq `Monthly; `Count 10; `Bymonthday [1; -1]];
      summarized = "Custom rule" };

    { printed = "RRULE:FREQ=MONTHLY;INTERVAL=18;COUNT=10;\
                 BYMONTHDAY=10,11,12,13,14,15";
      parsed = [`Freq `Monthly; `Interval 18; `Count 10;
                `Bymonthday [10; 11; 12; 13; 14; 15]];
      summarized = "Custom rule" };

    { printed = "RRULE:FREQ=MONTHLY;INTERVAL=2;BYDAY=TU";
      parsed = [`Freq `Monthly; `Interval 2; `Byday [(None, `Tuesday)]];
      summarized = "Every 2 months on Tuesday" };

    { printed = "RRULE:FREQ=YEARLY;COUNT=10;BYMONTH=6,7";
      parsed = [`Freq `Yearly; `Count 10; `Bymonth [6; 7]];
      summarized = "Custom rule" };

    { printed = "RRULE:FREQ=YEARLY;INTERVAL=2;COUNT=10;BYMONTH=1,2,3";
      parsed = [`Freq `Yearly; `Interval 2; `Count 10; `Bymonth [1; 2; 3]];
      summarized = "Custom rule" };

    { printed = "RRULE:FREQ=YEARLY;INTERVAL=3;COUNT=10;BYYEARDAY=1,100,200";
      parsed = [`Freq `Yearly; `Interval 3; `Count 10;
                `Byyearday [1; 100; 200]];
      summarized = "Custom rule" };

    { printed = "RRULE:FREQ=YEARLY;BYDAY=20MO";
      parsed = [`Freq `Yearly; `Byday [(Some 20, `Monday)]];
      summarized = "Custom rule" };

    { printed = "RRULE:FREQ=YEARLY;BYWEEKNO=20;BYDAY=MO";
      parsed = [`Freq `Yearly; `Byweekno [20]; `Byday [(None, `Monday)]];
      summarized = "Custom rule" };

    { printed = "RRULE:FREQ=YEARLY;BYMONTH=3;BYDAY=TH";
      parsed = [`Freq `Yearly; `Bymonth [3]; `Byday [(None, `Thursday)]];
      summarized = "Custom rule" };

    { printed = "RRULE:FREQ=YEARLY;BYDAY=TH;BYMONTH=6,7,8";
      parsed = [`Freq `Yearly; `Byday [(None, `Thursday)];
                `Bymonth [6; 7; 8]];
      summarized = "Custom rule" };

    { printed = "RRULE:FREQ=MONTHLY;BYDAY=FR;BYMONTHDAY=13";
      parsed = [`Freq `Monthly; `Byday [(None, `Friday)]; `Bymonthday [13]];
      summarized = "Custom rule" };

    { printed = "RRULE:FREQ=MONTHLY;BYDAY=SA;BYMONTHDAY=7,8,9,10,11,12,13";
      parsed = [`Freq `Monthly; `Byday [(None, `Saturday)];
                `Bymonthday [7; 8; 9; 10; 11; 12; 13]];
      summarized = "Custom rule" };

    { printed = "RRULE:FREQ=YEARLY;INTERVAL=4;BYMONTH=11;BYDAY=TU;\
               BYMONTHDAY=2,3,4,5,6,7,8";
      parsed = [`Freq `Yearly; `Interval 4; `Bymonth [11];
                `Byday [(None, `Tuesday)];
                `Bymonthday [2; 3; 4; 5; 6; 7; 8]];
      summarized = "Custom rule" };

    { printed = "RRULE:FREQ=MONTHLY;COUNT=3;BYDAY=TU,WE,TH;BYSETPOS=3";
      parsed = [`Freq `Monthly; `Count 3;
                `Byday [(None, `Tuesday); (None, `Wednesday);
                        (None, `Thursday)];
                `Bysetpos [3]];
      summarized = "Custom rule" };

    { printed = "RRULE:FREQ=MONTHLY;BYDAY=MO,TU,WE,TH,FR;BYSETPOS=-2";
      parsed = [`Freq `Monthly; `Byday [(None, `Monday); (None, `Tuesday);
                                        (None, `Wednesday); (None, `Thursday);
                                        (None, `Friday)];
                `Bysetpos [-2]];
      summarized = "Custom rule" };

    { printed = "RRULE:FREQ=HOURLY;INTERVAL=3;UNTIL=19970902T170000Z";
      parsed = [`Freq `Hourly; `Interval 3;
                `Until (Util_localtime.of_float 873219600.)];
      summarized = "Custom rule" };

    { printed = "RRULE:FREQ=MINUTELY;INTERVAL=15;COUNT=6";
      parsed = [`Freq `Minutely; `Interval 15; `Count 6];
      summarized = "Custom rule" };

    { printed = "RRULE:FREQ=MINUTELY;INTERVAL=90;COUNT=4";
      parsed = [`Freq `Minutely; `Interval 90; `Count 4];
      summarized = "Custom rule" };

    { printed = "RRULE:FREQ=DAILY;BYHOUR=9,10,11,12,13,14,15,16;\
                 BYMINUTE=0,20,40";
      parsed = [`Freq `Daily; `Byhour [9; 10; 11; 12; 13; 14; 15; 16];
                `Byminute [0; 20; 40]];
      summarized = "Custom rule" };

    { printed = "RRULE:FREQ=MINUTELY;INTERVAL=20;\
                 BYHOUR=9,10,11,12,13,14,15,16";
      parsed = [`Freq `Minutely; `Interval 20;
                `Byhour [9; 10; 11; 12; 13; 14; 15; 16]];
      summarized = "Custom rule" };

    { printed = "RRULE:FREQ=WEEKLY;INTERVAL=3;COUNT=4;BYDAY=TU,SU;WKST=MO";
      parsed = [`Freq `Weekly; `Interval 3; `Count 4;
                `Byday [(None, `Tuesday); (None, `Sunday)]; `Wkst `Monday];
      summarized = "Custom rule" };

    { printed = "RRULE:FREQ=WEEKLY;INTERVAL=2;COUNT=4;BYDAY=TU,SU;WKST=SU";
      parsed = [`Freq `Weekly; `Interval 2; `Count 4;
                `Byday [(None, `Tuesday); (None, `Sunday)]; `Wkst `Sunday];
      summarized = "Every 2 weeks on Tuesday and Sunday, 4 times" };

    { printed = "RRULE:FREQ=MONTHLY;BYMONTHDAY=15,30;COUNT=5";
      parsed = [`Freq `Monthly; `Bymonthday [15; 30]; `Count 5];
      summarized = "Custom rule" };
  ]

  let test_parsing () =
    List.iter (fun { printed; parsed } ->
      try assert (parse printed = parsed)
      with e -> logf `Error "Wrong parsed value for %s" printed; raise e
    ) examples;
    true

  let test_printing () =
    List.iter (fun { printed; parsed } ->
      try assert (print parsed = printed)
      with e -> logf `Error "Wrong printed output for %s" printed; raise e
    ) examples;
    true

  let test_summarizing () =
    List.iter (fun { printed; parsed; summarized } ->
      try assert (summarize parsed = summarized)
      with e -> logf `Error "Wrong summary for %s" printed; raise e
    ) examples;
    true

  let tests = [
    ("test_rrule_parsing", test_parsing);
    ("test_rrule_printing", test_printing);
    ("test_rrule_summarizing", test_summarizing);
  ]
end

let tests = Test.tests
