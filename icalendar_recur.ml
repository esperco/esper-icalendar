open Batteries
open Icalendar_t

let parse_freq f = assert false

let parse_byseclist l = assert false

let parse_byminlist l = assert false

let parse_byhrlist l = assert false

let parse_bywdaylist l = assert false

let parse_bymodaylist l = assert false

let parse_byyrdaylist l = assert false

let parse_bywknolist l = assert false

let parse_bymolist l = assert false

let parse_bysplist l = assert false

let parse_weekday day = assert false

let parse_recur_rule_part p =
  match String.split p ~by:"=" with
  | ("FREQ", f) -> `Freq (parse_freq f)
  | ("UNTIL", date) -> `Until (Util_time.of_string date)
  | ("COUNT", n) -> `Count (int_of_string n)
  | ("INTERVAL", n) -> `Interval (int_of_string n)
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

let parse (rrule : string) : recur =
  List.map parse_recur_rule_part (String.nsplit rrule ~by:";")

let print (rrule : recur) : string = "" (* TODO *)

let of_string = parse
let to_string = print
