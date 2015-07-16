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

let rec parse (rrule : string) : recur =
  List.map parse_recur_rule_part (String.nsplit rrule ~by:";")

and parse_recur_rule_part p =
  match String.split p ~by:"=" with
  | ("FREQ", f) -> `Freq (parse_freq f)
  | ("UNTIL", date) -> `Until (Util_time.of_string date)
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

let print (rrule : recur) : string = "" (* TODO *)

let of_string = parse
let to_string = print
