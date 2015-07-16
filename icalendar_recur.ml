open Batteries
open Icalendar_t

(* Parsing *)

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


(* Tests *)

module Test = struct
  type test_case = {
    printed : string;
    parsed : recur;
  }

  (* Examples from RFC 5545 sec. 3.8.5.3. Recurrence Rule *)
  let examples = [
    { printed = "FREQ=DAILY;COUNT=10";
      parsed = [`Freq `Daily; `Count 10] };

    { printed = "FREQ=DAILY;UNTIL=19971224T000000Z";
      parsed = [`Freq `Daily; `Until (Util_localtime.of_float 882921600.)] };

    { printed = "FREQ=DAILY;INTERVAL=2";
      parsed = [`Freq `Daily; `Interval 2] };

    { printed = "FREQ=DAILY;INTERVAL=10;COUNT=5";
      parsed = [`Freq `Daily; `Interval 10; `Count 5] };

    { printed = "FREQ=DAILY;UNTIL=20000131T140000Z;BYMONTH=1";
      parsed = [`Freq `Daily; `Until (Util_localtime.of_float 949327200.);
                `Bymonth [1]] };

    { printed = "FREQ=YEARLY;UNTIL=20000131T140000Z;BYMONTH=1;\
               BYDAY=SU,MO,TU,WE,TH,FR,SA";
      parsed = [`Freq `Yearly; `Until (Util_localtime.of_float 949327200.);
                `Bymonth [1]; `Byday [(None, `Sunday); (None, `Monday);
                                      (None, `Tuesday); (None, `Wednesday);
                                      (None, `Thursday); (None, `Friday);
                                      (None, `Saturday)]] };

    { printed = "FREQ=WEEKLY;COUNT=10";
      parsed = [`Freq `Weekly; `Count 10] };

    { printed = "FREQ=WEEKLY;UNTIL=19971224T000000Z";
      parsed = [`Freq `Weekly; `Until (Util_localtime.of_float 882921600.)] };

    { printed = "FREQ=WEEKLY;INTERVAL=2;WKST=SU";
      parsed = [`Freq `Weekly; `Interval 2; `Wkst `Sunday] };

    { printed = "FREQ=WEEKLY;UNTIL=19971007T000000Z;WKST=SU;BYDAY=TU,TH";
      parsed = [`Freq `Weekly; `Until (Util_localtime.of_float 876182400.);
                `Wkst `Sunday; `Byday [(None, `Tuesday); (None, `Thursday)]] };

    { printed = "FREQ=WEEKLY;COUNT=10;WKST=SU;BYDAY=TU,TH";
      parsed = [`Freq `Weekly; `Count 10; `Wkst `Sunday;
                `Byday [(None, `Tuesday); (None, `Thursday)]] };

    { printed = "FREQ=WEEKLY;INTERVAL=2;UNTIL=19971224T000000Z;\
               WKST=SU;BYDAY=MO,WE,FR";
      parsed = [`Freq `Weekly; `Interval 2;
                `Until (Util_localtime.of_float 882921600.);
                `Wkst `Sunday;
                `Byday [(None, `Monday); (None, `Wednesday);
                        (None, `Friday)]] };

    { printed = "FREQ=WEEKLY;INTERVAL=2;COUNT=8;WKST=SU;BYDAY=TU,TH";
      parsed = [`Freq `Weekly; `Interval 2; `Count 8; `Wkst `Sunday;
                `Byday [(None, `Tuesday); (None, `Thursday)]] };

    { printed = "FREQ=MONTHLY;COUNT=10;BYDAY=1FR";
      parsed = [`Freq `Monthly; `Count 10; `Byday [(Some 1, `Friday)]] };

    { printed = "FREQ=MONTHLY;UNTIL=19971224T000000Z;BYDAY=1FR";
      parsed = [`Freq `Monthly; `Until (Util_localtime.of_float 882921600.);
                `Byday [(Some 1, `Friday)]] };

    { printed = "FREQ=MONTHLY;INTERVAL=2;COUNT=10;BYDAY=1SU,-1SU";
      parsed = [`Freq `Monthly; `Interval 2; `Count 10;
                `Byday [(Some 1, `Sunday); (Some (-1), `Sunday)]] };

    { printed = "FREQ=MONTHLY;COUNT=6;BYDAY=-2MO";
      parsed = [`Freq `Monthly; `Count 6; `Byday [(Some (-2), `Monday)]] };

    { printed = "FREQ=MONTHLY;BYMONTHDAY=-3";
      parsed = [`Freq `Monthly; `Bymonthday [-3]] };

    { printed = "FREQ=MONTHLY;COUNT=10;BYMONTHDAY=2,15";
      parsed = [`Freq `Monthly; `Count 10; `Bymonthday [2; 15]] };

    { printed = "FREQ=MONTHLY;COUNT=10;BYMONTHDAY=1,-1";
      parsed = [`Freq `Monthly; `Count 10; `Bymonthday [1; -1]] };

    { printed = "FREQ=MONTHLY;INTERVAL=18;COUNT=10;\
                 BYMONTHDAY=10,11,12,13,14,15";
      parsed = [`Freq `Monthly; `Interval 18; `Count 10;
                `Bymonthday [10; 11; 12; 13; 14; 15]] };

    { printed = "FREQ=MONTHLY;INTERVAL=2;BYDAY=TU";
      parsed = [`Freq `Monthly; `Interval 2; `Byday [(None, `Tuesday)]] };

    { printed = "FREQ=YEARLY;COUNT=10;BYMONTH=6,7";
      parsed = [`Freq `Yearly; `Count 10; `Bymonth [6; 7]] };

    { printed = "FREQ=YEARLY;INTERVAL=2;COUNT=10;BYMONTH=1,2,3";
      parsed = [`Freq `Yearly; `Interval 2; `Count 10; `Bymonth [1; 2; 3]] };

    { printed = "FREQ=YEARLY;INTERVAL=3;COUNT=10;BYYEARDAY=1,100,200";
      parsed = [`Freq `Yearly; `Interval 3; `Count 10;
                `Byyearday [1; 100; 200]] };

    { printed = "FREQ=YEARLY;BYDAY=20MO";
      parsed = [`Freq `Yearly; `Byday [(Some 20, `Monday)]] };

    { printed = "FREQ=YEARLY;BYWEEKNO=20;BYDAY=MO";
      parsed = [`Freq `Yearly; `Byweekno [20]; `Byday [(None, `Monday)]] };

    { printed = "FREQ=YEARLY;BYMONTH=3;BYDAY=TH";
      parsed = [`Freq `Yearly; `Bymonth [3]; `Byday [(None, `Thursday)]] };

    { printed = "FREQ=YEARLY;BYDAY=TH;BYMONTH=6,7,8";
      parsed = [`Freq `Yearly; `Byday [(None, `Thursday)];
                `Bymonth [6; 7; 8]] };

    { printed = "FREQ=MONTHLY;BYDAY=FR;BYMONTHDAY=13";
      parsed = [`Freq `Monthly; `Byday [(None, `Friday)]; `Bymonthday [13]] };

    { printed = "FREQ=MONTHLY;BYDAY=SA;BYMONTHDAY=7,8,9,10,11,12,13";
      parsed = [`Freq `Monthly; `Byday [(None, `Saturday)];
                `Bymonthday [7; 8; 9; 10; 11; 12; 13]] };

    { printed = "FREQ=YEARLY;INTERVAL=4;BYMONTH=11;BYDAY=TU;\
               BYMONTHDAY=2,3,4,5,6,7,8";
      parsed = [`Freq `Yearly; `Interval 4; `Bymonth [11];
                `Byday [(None, `Tuesday)];
                `Bymonthday [2; 3; 4; 5; 6; 7; 8]] };

    { printed = "FREQ=MONTHLY;COUNT=3;BYDAY=TU,WE,TH;BYSETPOS=3";
      parsed = [`Freq `Monthly; `Count 3;
                `Byday [(None, `Tuesday); (None, `Wednesday);
                        (None, `Thursday)];
                `Bysetpos [3]] };

    { printed = "FREQ=MONTHLY;BYDAY=MO,TU,WE,TH,FR;BYSETPOS=-2";
      parsed = [`Freq `Monthly; `Byday [(None, `Monday); (None, `Tuesday);
                                        (None, `Wednesday); (None, `Thursday);
                                        (None, `Friday)];
                `Bysetpos [-2]] };

    { printed = "FREQ=HOURLY;INTERVAL=3;UNTIL=19970902T170000Z";
      parsed = [`Freq `Hourly; `Interval 3;
                `Until (Util_localtime.of_float 873219600.)] };

    { printed = "FREQ=MINUTELY;INTERVAL=15;COUNT=6";
      parsed = [`Freq `Minutely; `Interval 15; `Count 6] };

    { printed = "FREQ=MINUTELY;INTERVAL=90;COUNT=4";
      parsed = [`Freq `Minutely; `Interval 90; `Count 4] };

    { printed = "FREQ=DAILY;BYHOUR=9,10,11,12,13,14,15,16;BYMINUTE=0,20,40";
      parsed = [`Freq `Daily; `Byhour [9; 10; 11; 12; 13; 14; 15; 16];
                `Byminute [0; 20; 40]] };

    { printed = "FREQ=MINUTELY;INTERVAL=20;BYHOUR=9,10,11,12,13,14,15,16";
      parsed = [`Freq `Minutely; `Interval 20;
                `Byhour [9; 10; 11; 12; 13; 14; 15; 16]] };

    { printed = "FREQ=WEEKLY;INTERVAL=2;COUNT=4;BYDAY=TU,SU;WKST=MO";
      parsed = [`Freq `Weekly; `Interval 2; `Count 4;
                `Byday [(None, `Tuesday); (None, `Sunday)]; `Wkst `Monday] };

    { printed = "FREQ=WEEKLY;INTERVAL=2;COUNT=4;BYDAY=TU,SU;WKST=SU";
      parsed = [`Freq `Weekly; `Interval 2; `Count 4;
                `Byday [(None, `Tuesday); (None, `Sunday)]; `Wkst `Sunday] };

    { printed = "FREQ=MONTHLY;BYMONTHDAY=15,30;COUNT=5";
      parsed = [`Freq `Monthly; `Bymonthday [15; 30]; `Count 5] };
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

  let tests = [
    ("test_rrule_parsing", test_parsing);
    ("test_rrule_printing", test_printing);
  ]
end

let tests = Test.tests
