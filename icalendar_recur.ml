open Batteries
open Icalendar_t

(* Utility function for Atdgen validation *)
let check f x =
  match f [] x with
  | None -> x
  | Some err ->
      raise (Invalid_argument (Ag_util.Validation.string_of_error err))

let positive_int_of_string s =
  let n = int_of_string s in
  check Icalendar_v.validate_positive_int n

(* Simple comma-separated lists of numbers *)

let parse_int_list valid l =
  List.map (fun s ->
    let n = int_of_string s in
    check valid n
  ) (String.nsplit l ~by:",")

let print_int_list l =
  String.concat "," (List.map (fun n -> string_of_int n) l)


(* Parsing *)

let rec parse (rrule : string) : recur =
  List.map parse_recur_rule_part (String.nsplit rrule ~by:";")

and parse_recur_rule_part p =
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
  | (name, value) ->
      raise (Invalid_argument ("Unrecognized recur_rule_part " ^ p))

and parse_freq = function
  | "SECONDLY" -> `Secondly
  | "MINUTELY" -> `Minutely
  | "HOURLY" -> `Hourly
  | "DAILY" -> `Daily
  | "WEEKLY" -> `Weekly
  | "MONTHLY" -> `Monthly
  | "YEARLY" -> `Yearly
  | s -> raise (Invalid_argument ("Unrecognized freq " ^ s))

and parse_date d =
  if String.length d = 8 then (* Date *)
    Scanf.sscanf d "%4s%2s%2s" (fun y m d ->
      Util_localtime.of_string (String.concat "-" [y; m; d])
    )
  else (* Date-Time *)
    Scanf.sscanf d "%4s%2s%2sT%2s%2s%2s%s" (fun y mo d h mi s _z ->
      Util_localtime.of_string (
        String.concat "-" [y; mo; d] ^ "T" ^
        String.concat ":" [h; mi; s]
      )
    )

and parse_byseclist l = parse_int_list Icalendar_v.validate_seconds l

and parse_byminlist l = parse_int_list Icalendar_v.validate_minutes l

and parse_byhrlist l = parse_int_list Icalendar_v.validate_hour l

and parse_weekdaynum s =
  let ordwk_char = ['0'; '1'; '2'; '3'; '4'; '5'; '6';
                    '7'; '8'; '9'; '0'; '+'; '-'] in
  if List.exists (String.contains s) ordwk_char then
    Scanf.sscanf s "%d%s" (fun n wday ->
      let ordwk = check Icalendar_v.validate_ordwk n in
      (Some ordwk, parse_weekday wday)
    )
  else (None, parse_weekday s)

and parse_bywdaylist l = List.map parse_weekdaynum (String.nsplit l ~by:",")

and parse_bymodaylist l = parse_int_list Icalendar_v.validate_ordmoday l

and parse_byyrdaylist l = parse_int_list Icalendar_v.validate_ordyrday l

and parse_bywknolist l = parse_int_list Icalendar_v.validate_weeknum l

and parse_bymolist l = parse_int_list Icalendar_v.validate_monthnum l

and parse_bysplist l = parse_int_list Icalendar_v.validate_setposday l

and parse_weekday = function
  | "SU" -> `Sunday
  | "MO" -> `Monday
  | "TU" -> `Tuesday
  | "WE" -> `Wednesday
  | "TH" -> `Thursday
  | "FR" -> `Friday
  | "SA" -> `Saturday
  | s -> raise (Invalid_argument ("Unrecognized weekday " ^ s))

let of_string = parse
let wrap = parse


(* Printing *)

let rec print (rrule : recur) : string =
  String.concat ";" (List.map print_recur_rule_part rrule)

and print_recur_rule_part = function
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

and print_freq = function
  | `Secondly -> "SECONDLY"
  | `Minutely -> "MINUTELY"
  | `Hourly -> "HOURLY"
  | `Daily -> "DAILY"
  | `Weekly -> "WEEKLY"
  | `Monthly -> "MONTHLY"
  | `Yearly -> "YEARLY"

and print_date d =
  let open Util_localtime in
  Printf.sprintf "%04d%02d%02dT%02d%02d%02dZ"
    d.year d.month d.day d.hour d.min (int_of_float d.sec)

and print_bywdaylist l =
  String.concat "," (
    List.map (function
      | (None, day) -> print_weekday day
      | (Some ordwk, day) -> string_of_int ordwk ^ print_weekday day
    ) l
  )

and print_weekday = function
  | `Sunday -> "SU"
  | `Monday -> "MO"
  | `Tuesday -> "TU"
  | `Wednesday -> "WE"
  | `Thursday -> "TH"
  | `Friday -> "FR"
  | `Saturday -> "SA"

let to_string = print
let unwrap = print


(* Human-readable descriptions of recurrences
   e.g., "Every 3 weeks on Monday and Wednesday until Jan 3, 2015" *)

exception Unsupported
  (* If you can't make this rule in our frontend interface,
     then we won't bother trying to summarize it,
     because recurrence rules can get really complicated.
     Our interface is modeled after Google's,
     so we're unlikely to encounter rules we can't summarize. *)

let rec summarize_if_supported rule =
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

and extract_by_filter period rule =
  let byxxx =
    List.filter (function
      | `Byhour _ | `Byday _ | `Bymonthday _ | `Byyearday _
      | `Byweekno _ | `Bymonth _ | `Bysetpos _ | `Wkst _ ->
          true (* Extract all the BYxxx parameters *)
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

and summarize_weekdays = function
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

and summarize_weekday = function
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
        | _ -> raise Unsupported
      in
      "the " ^ ord ^ " " ^ summarize_weekday (None, day)

and has_all_weekdays l =
  let weekdays = [
    (None, `Monday);
    (None, `Tuesday);
    (None, `Wednesday);
    (None, `Thursday);
    (None, `Friday);
  ] in
  List.for_all (fun d -> List.mem d l) weekdays

and summarize_date d =
  let open Util_localtime in
  let months = [|"Jan"; "Feb"; "Mar"; "Apr"; "May"; "Jun";
                 "Jul"; "Aug"; "Sep"; "Oct"; "Nov"; "Dec"|] in
  Printf.sprintf "%s %d, %d" months.(d.month - 1) d.day d.year

let summarize rule =
  try summarize_if_supported rule
  with Unsupported -> "Custom rule"


(* Tests *)

module Test = struct
  type test_case = {
    printed : string; (* RECUR value in RFC format *)
    parsed : recur;
    summarized : string; (* Human-readable description *)
  }

  (* Examples from RFC 5545 sec. 3.8.5.3. Recurrence Rule *)
  let examples = [
    { printed = "FREQ=DAILY;COUNT=10";
      parsed = [`Freq `Daily; `Count 10];
      summarized = "Daily, 10 times" };

    { printed = "FREQ=DAILY;UNTIL=19971224T000000Z";
      parsed = [`Freq `Daily; `Until (Util_localtime.of_float 882921600.)];
      summarized = "Daily, until Dec 24, 1997" };

    { printed = "FREQ=DAILY;INTERVAL=2";
      parsed = [`Freq `Daily; `Interval 2];
      summarized = "Every 2 days" };

    { printed = "FREQ=DAILY;INTERVAL=10;COUNT=5";
      parsed = [`Freq `Daily; `Interval 10; `Count 5];
      summarized = "Every 10 days, 5 times" };

    { printed = "FREQ=DAILY;UNTIL=20000131T140000Z;BYMONTH=1";
      parsed = [`Freq `Daily; `Until (Util_localtime.of_float 949327200.);
                `Bymonth [1]];
      summarized = "Custom rule" };

    { printed = "FREQ=YEARLY;UNTIL=20000131T140000Z;BYMONTH=1;\
                 BYDAY=SU,MO,TU,WE,TH,FR,SA";
      parsed = [`Freq `Yearly; `Until (Util_localtime.of_float 949327200.);
                `Bymonth [1]; `Byday [(None, `Sunday); (None, `Monday);
                                      (None, `Tuesday); (None, `Wednesday);
                                      (None, `Thursday); (None, `Friday);
                                      (None, `Saturday)]];
      summarized = "Custom rule" };

    { printed = "FREQ=WEEKLY;COUNT=10";
      parsed = [`Freq `Weekly; `Count 10];
      summarized = "Weekly, 10 times" };

    { printed = "FREQ=WEEKLY;UNTIL=19971224T000000Z";
      parsed = [`Freq `Weekly; `Until (Util_localtime.of_float 882921600.)];
      summarized = "Weekly, until Dec 24, 1997" };

    { printed = "FREQ=WEEKLY;INTERVAL=2;WKST=SU";
      parsed = [`Freq `Weekly; `Interval 2; `Wkst `Sunday];
      summarized = "Every 2 weeks" };

    { printed = "FREQ=WEEKLY;UNTIL=19971007T000000Z;WKST=SU;BYDAY=TU,TH";
      parsed = [`Freq `Weekly; `Until (Util_localtime.of_float 876182400.);
                `Wkst `Sunday; `Byday [(None, `Tuesday); (None, `Thursday)]];
      summarized = "Weekly on Tuesday and Thursday, until Oct 7, 1997" };

    { printed = "FREQ=WEEKLY;COUNT=10;WKST=SU;BYDAY=TU,TH";
      parsed = [`Freq `Weekly; `Count 10; `Wkst `Sunday;
                `Byday [(None, `Tuesday); (None, `Thursday)]];
      summarized = "Weekly on Tuesday and Thursday, 10 times" };

    { printed = "FREQ=WEEKLY;INTERVAL=2;UNTIL=19971224T000000Z;\
                 WKST=SU;BYDAY=MO,WE,FR";
      parsed = [`Freq `Weekly; `Interval 2;
                `Until (Util_localtime.of_float 882921600.);
                `Wkst `Sunday;
                `Byday [(None, `Monday); (None, `Wednesday);
                        (None, `Friday)]];
      summarized = "Every 2 weeks on Monday, Wednesday, and Friday, \
                    until Dec 24, 1997" };

    { printed = "FREQ=WEEKLY;INTERVAL=2;COUNT=8;WKST=SU;BYDAY=TU,TH";
      parsed = [`Freq `Weekly; `Interval 2; `Count 8; `Wkst `Sunday;
                `Byday [(None, `Tuesday); (None, `Thursday)]];
      summarized = "Every 2 weeks on Tuesday and Thursday, 8 times" };

    { printed = "FREQ=MONTHLY;COUNT=10;BYDAY=1FR";
      parsed = [`Freq `Monthly; `Count 10; `Byday [(Some 1, `Friday)]];
      summarized = "Monthly on the first Friday, 10 times" };

    { printed = "FREQ=MONTHLY;UNTIL=19971224T000000Z;BYDAY=1FR";
      parsed = [`Freq `Monthly; `Until (Util_localtime.of_float 882921600.);
                `Byday [(Some 1, `Friday)]];
      summarized = "Monthly on the first Friday, until Dec 24, 1997" };

    { printed = "FREQ=MONTHLY;INTERVAL=2;COUNT=10;BYDAY=1SU,-1SU";
      parsed = [`Freq `Monthly; `Interval 2; `Count 10;
                `Byday [(Some 1, `Sunday); (Some (-1), `Sunday)]];
      summarized = "Every 2 months on the first Sunday and the last Sunday, \
                    10 times" };

    { printed = "FREQ=MONTHLY;COUNT=6;BYDAY=-2MO";
      parsed = [`Freq `Monthly; `Count 6; `Byday [(Some (-2), `Monday)]];
      summarized = "Monthly on the second-to-last Monday, 6 times" };

    { printed = "FREQ=MONTHLY;BYMONTHDAY=-3";
      parsed = [`Freq `Monthly; `Bymonthday [-3]];
      summarized = "Custom rule" };

    { printed = "FREQ=MONTHLY;COUNT=10;BYMONTHDAY=2,15";
      parsed = [`Freq `Monthly; `Count 10; `Bymonthday [2; 15]];
      summarized = "Custom rule" };

    { printed = "FREQ=MONTHLY;COUNT=10;BYMONTHDAY=1,-1";
      parsed = [`Freq `Monthly; `Count 10; `Bymonthday [1; -1]];
      summarized = "Custom rule" };

    { printed = "FREQ=MONTHLY;INTERVAL=18;COUNT=10;\
                 BYMONTHDAY=10,11,12,13,14,15";
      parsed = [`Freq `Monthly; `Interval 18; `Count 10;
                `Bymonthday [10; 11; 12; 13; 14; 15]];
      summarized = "Custom rule" };

    { printed = "FREQ=MONTHLY;INTERVAL=2;BYDAY=TU";
      parsed = [`Freq `Monthly; `Interval 2; `Byday [(None, `Tuesday)]];
      summarized = "Every 2 months on Tuesday" };

    { printed = "FREQ=YEARLY;COUNT=10;BYMONTH=6,7";
      parsed = [`Freq `Yearly; `Count 10; `Bymonth [6; 7]];
      summarized = "Custom rule" };

    { printed = "FREQ=YEARLY;INTERVAL=2;COUNT=10;BYMONTH=1,2,3";
      parsed = [`Freq `Yearly; `Interval 2; `Count 10; `Bymonth [1; 2; 3]];
      summarized = "Custom rule" };

    { printed = "FREQ=YEARLY;INTERVAL=3;COUNT=10;BYYEARDAY=1,100,200";
      parsed = [`Freq `Yearly; `Interval 3; `Count 10;
                `Byyearday [1; 100; 200]];
      summarized = "Custom rule" };

    { printed = "FREQ=YEARLY;BYDAY=20MO";
      parsed = [`Freq `Yearly; `Byday [(Some 20, `Monday)]];
      summarized = "Custom rule" };

    { printed = "FREQ=YEARLY;BYWEEKNO=20;BYDAY=MO";
      parsed = [`Freq `Yearly; `Byweekno [20]; `Byday [(None, `Monday)]];
      summarized = "Custom rule" };

    { printed = "FREQ=YEARLY;BYMONTH=3;BYDAY=TH";
      parsed = [`Freq `Yearly; `Bymonth [3]; `Byday [(None, `Thursday)]];
      summarized = "Custom rule" };

    { printed = "FREQ=YEARLY;BYDAY=TH;BYMONTH=6,7,8";
      parsed = [`Freq `Yearly; `Byday [(None, `Thursday)];
                `Bymonth [6; 7; 8]];
      summarized = "Custom rule" };

    { printed = "FREQ=MONTHLY;BYDAY=FR;BYMONTHDAY=13";
      parsed = [`Freq `Monthly; `Byday [(None, `Friday)]; `Bymonthday [13]];
      summarized = "Custom rule" };

    { printed = "FREQ=MONTHLY;BYDAY=SA;BYMONTHDAY=7,8,9,10,11,12,13";
      parsed = [`Freq `Monthly; `Byday [(None, `Saturday)];
                `Bymonthday [7; 8; 9; 10; 11; 12; 13]];
      summarized = "Custom rule" };

    { printed = "FREQ=YEARLY;INTERVAL=4;BYMONTH=11;BYDAY=TU;\
               BYMONTHDAY=2,3,4,5,6,7,8";
      parsed = [`Freq `Yearly; `Interval 4; `Bymonth [11];
                `Byday [(None, `Tuesday)];
                `Bymonthday [2; 3; 4; 5; 6; 7; 8]];
      summarized = "Custom rule" };

    { printed = "FREQ=MONTHLY;COUNT=3;BYDAY=TU,WE,TH;BYSETPOS=3";
      parsed = [`Freq `Monthly; `Count 3;
                `Byday [(None, `Tuesday); (None, `Wednesday);
                        (None, `Thursday)];
                `Bysetpos [3]];
      summarized = "Custom rule" };

    { printed = "FREQ=MONTHLY;BYDAY=MO,TU,WE,TH,FR;BYSETPOS=-2";
      parsed = [`Freq `Monthly; `Byday [(None, `Monday); (None, `Tuesday);
                                        (None, `Wednesday); (None, `Thursday);
                                        (None, `Friday)];
                `Bysetpos [-2]];
      summarized = "Custom rule" };

    { printed = "FREQ=HOURLY;INTERVAL=3;UNTIL=19970902T170000Z";
      parsed = [`Freq `Hourly; `Interval 3;
                `Until (Util_localtime.of_float 873219600.)];
      summarized = "Custom rule" };

    { printed = "FREQ=MINUTELY;INTERVAL=15;COUNT=6";
      parsed = [`Freq `Minutely; `Interval 15; `Count 6];
      summarized = "Custom rule" };

    { printed = "FREQ=MINUTELY;INTERVAL=90;COUNT=4";
      parsed = [`Freq `Minutely; `Interval 90; `Count 4];
      summarized = "Custom rule" };

    { printed = "FREQ=DAILY;BYHOUR=9,10,11,12,13,14,15,16;BYMINUTE=0,20,40";
      parsed = [`Freq `Daily; `Byhour [9; 10; 11; 12; 13; 14; 15; 16];
                `Byminute [0; 20; 40]];
      summarized = "Custom rule" };

    { printed = "FREQ=MINUTELY;INTERVAL=20;BYHOUR=9,10,11,12,13,14,15,16";
      parsed = [`Freq `Minutely; `Interval 20;
                `Byhour [9; 10; 11; 12; 13; 14; 15; 16]];
      summarized = "Custom rule" };

    { printed = "FREQ=WEEKLY;INTERVAL=2;COUNT=4;BYDAY=TU,SU;WKST=MO";
      parsed = [`Freq `Weekly; `Interval 2; `Count 4;
                `Byday [(None, `Tuesday); (None, `Sunday)]; `Wkst `Monday];
      summarized = "Custom rule" };

    { printed = "FREQ=WEEKLY;INTERVAL=2;COUNT=4;BYDAY=TU,SU;WKST=SU";
      parsed = [`Freq `Weekly; `Interval 2; `Count 4;
                `Byday [(None, `Tuesday); (None, `Sunday)]; `Wkst `Sunday];
      summarized = "Custom rule" };

    { printed = "FREQ=MONTHLY;BYMONTHDAY=15,30;COUNT=5";
      parsed = [`Freq `Monthly; `Bymonthday [15; 30]; `Count 5];
      summarized = "Custom rule" };
  ]

  let test_parsing () =
    List.iter (fun { printed; parsed } ->
      assert (parse printed = parsed)
    ) examples;
    true

  let test_printing () =
    List.iter (fun { printed; parsed } ->
      assert (print parsed = printed)
    ) examples;
    true

  let test_summarizing () =
    List.iter (fun { parsed; summarized } ->
      assert (summarize parsed = summarized)
    ) examples;
    true

  let tests = [
    ("test_rrule_parsing", test_parsing);
    ("test_rrule_printing", test_printing);
    ("test_rrule_summarizing", test_summarizing);
  ]
end

let tests = Test.tests
