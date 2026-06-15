
# Module `Js_of_ocaml.Intl`

Internationalization API

A code example:

```ocaml

open Js;;

let fc v = Console.console##debug v in

let jas a = array (Array.map (fun v -> string v) a) in

if Intl.is_supported ()
then (
  let intl = Intl.intl in
  fc (intl##getCanonicalLocales (jas [| "EN-US"; "Fr" |]));
  try
    let options = Intl.Collator.object_options () in
    options##.localeMatcher := string "lookup";
    fc
      (intl##._Collator##supportedLocalesOf
         (jas [| "ban"; "id-u-co-pinyin"; "de-ID" |])
         (def options));

    (* Note: the exact output may be browser-dependent *)
    let letterSort lang letters =
      letters##sort
        (wrap_callback (fun a b ->
             let collator =
               new%js Intl.collator_constr (def (array [| lang |])) undefined
             in
             Js.float (float_of_int (collator##.compare a b))));
      letters
    in
    let a = jas [| "a"; "z"; "ä" |] in
    fc (letterSort (string "de") a);
    fc (letterSort (string "sv") a);

    let collator = new%js Intl.collator_constr undefined undefined in
    fc (collator##.compare (string "a") (string "c"));
    fc (collator##.compare (string "c") (string "a"));
    fc (collator##.compare (string "a") (string "a"));

    let collator = new%js Intl.collator_constr (def (jas [| "de" |])) undefined in
    fc (collator##.compare (string "ä") (string "z"));
    let collator = new%js Intl.collator_constr (def (jas [| "sv" |])) undefined in
    fc (collator##.compare (string "ä") (string "z"));
    let options = Intl.Collator.options () in
    let () = options##.sensitivity := string "base" in
    let collator = new%js Intl.collator_constr (def (jas [| "de" |])) (def options) in
    fc (collator##.compare (string "ä") (string "a"));
    let collator = new%js Intl.collator_constr (def (jas [| "sv" |])) (def options) in
    fc (collator##.compare (string "ä") (string "a"));

    let firstAlphabetical locale letter1 letter2 =
      let collator = new%js Intl.collator_constr (def (array [| locale |])) undefined in
      if collator##.compare letter1 letter2 > 0 then letter1 else letter2
    in
    fc (firstAlphabetical (string "de") (string "z") (string "ä"));
    fc (firstAlphabetical (string "sv") (string "z") (string "ä"));

    let a = jas [| "Offenbach"; "Österreich"; "Odenwald" |] in
    let collator =
      new%js Intl.collator_constr (def (jas [| "de-u-co-phonebk" |])) undefined
    in
    let a =
      a##sort
        (wrap_callback (fun v1 v2 -> Js.float (float_of_int (collator##.compare v1 v2))))
    in
    fc (a##join (string ", "));

    let a = jas [| "Congrès"; "congres"; "Assemblée"; "poisson" |] in
    let options = Intl.Collator.options () in
    let () = options##.usage := string "search" in
    let () = options##.sensitivity := string "base" in
    let collator = new%js Intl.collator_constr (def (jas [| "fr" |])) (def options) in
    let s = string "congres" in
    let matches =
      a##filter (wrap_callback (fun v _ _ -> bool (collator##.compare v s = 0)))
    in
    fc (matches##join (string ", "));

    let options = Intl.Collator.options () in
    let () = options##.sensitivity := string "base" in
    let collator = new%js Intl.collator_constr (def (jas [| "de" |])) (def options) in
    let usedOptions = collator##resolvedOptions () in
    fc usedOptions##.locale;
    fc usedOptions##.usage;
    fc usedOptions##.sensitivity;
    fc usedOptions##.ignorePunctuation;
    fc usedOptions##.collation;
    fc usedOptions##.numeric;

    let date = new%js date_sec 2012 11 20 3 0 0 in
    (* Results below assume UTC timezone - your results may vary *)
    let dtf = new%js Intl.dateTimeFormat_constr (def (jas [| "en-US" |])) undefined in
    fc (dtf##.format date);
    let dtf = new%js Intl.dateTimeFormat_constr (def (jas [| "ban"; "id" |])) undefined in
    fc (dtf##.format date);
    let dtf =
      new%js Intl.dateTimeFormat_constr (def (jas [| "ja-JP-u-ca-japanese" |])) undefined
    in
    fc (dtf##.format date);
    let options = Intl.DateTimeFormat.options () in
    let () = options##.weekday := def (string "long") in
    let () = options##.year := def (string "numeric") in
    let () = options##.month := def (string "long") in
    let () = options##.day := def (string "numeric") in
    let dtf = new%js Intl.dateTimeFormat_constr (def (jas [| "de-DE" |])) (def options) in
    fc (dtf##.format date);
    let () = options##.timeZone := def (string "UTC") in
    let () = options##.timeZoneName := def (string "short") in
    let dtf = new%js Intl.dateTimeFormat_constr (def (jas [| "en-US" |])) (def options) in
    fc (dtf##.format date);
    let options = Intl.DateTimeFormat.options () in
    let () = options##.hour := def (string "numeric") in
    let () = options##.minute := def (string "numeric") in
    let () = options##.second := def (string "numeric") in
    let () = options##.timeZone := def (string "Australia/Sydney") in
    let () = options##.timeZoneName := def (string "short") in
    let dtf = new%js Intl.dateTimeFormat_constr (def (jas [| "en-AU" |])) (def options) in
    fc (dtf##.format date);
    let options = Intl.DateTimeFormat.options () in
    let () = options##.year := def (string "numeric") in
    let () = options##.month := def (string "numeric") in
    let () = options##.day := def (string "numeric") in
    let () = options##.hour := def (string "numeric") in
    let () = options##.minute := def (string "numeric") in
    let () = options##.second := def (string "numeric") in
    let () = options##.hour12 := def _false in
    let () = options##.timeZone := def (string "America/Los_Angeles") in
    let dtf = new%js Intl.dateTimeFormat_constr (def (jas [| "en-US" |])) (def options) in
    fc (dtf##.format date);

    let date = new%js date_month 2012 05 in
    let options = Intl.DateTimeFormat.options () in
    let () = options##.weekday := def (string "long") in
    let () = options##.year := def (string "numeric") in
    let () = options##.month := def (string "long") in
    let () = options##.day := def (string "numeric") in
    let dtf = new%js Intl.dateTimeFormat_constr (def (jas [| "sr-RS" |])) (def options) in
    fc (dtf##.format date);
    let dtf = new%js Intl.dateTimeFormat_constr (def (jas [| "en-GB" |])) (def options) in
    fc (dtf##.format date);

    let a =
      array
        [| new%js date_month 2012 08
         ; new%js date_month 2012 11
         ; new%js date_month 2012 03
        |]
    in
    let options = Intl.DateTimeFormat.options () in
    let () = options##.year := def (string "numeric") in
    let () = options##.month := def (string "long") in
    let dtf = new%js Intl.dateTimeFormat_constr (def (jas [| "pt-BR" |])) (def options) in
    let formatted = array_map dtf##.format a in
    fc (formatted##join (string "; "));

    let date = new%js date_sec 2012 11 17 3 0 42 in
    let options = Intl.DateTimeFormat.options () in
    let () = options##.weekday := def (string "long") in
    let () = options##.year := def (string "numeric") in
    let () = options##.month := def (string "numeric") in
    let () = options##.day := def (string "numeric") in
    let () = options##.hour := def (string "numeric") in
    let () = options##.minute := def (string "numeric") in
    let () = options##.second := def (string "numeric") in
    let () = options##.hour12 := def _true in
    let () = options##.timeZone := def (string "UTC") in
    let dtf = new%js Intl.dateTimeFormat_constr (def (jas [| "en-us" |])) (def options) in
    fc (dtf##.format date);
    let parts = dtf##formatToParts (def date) in
    fc parts;
    let dateString =
      (array_map
         (fun (v : Intl.DateTimeFormat.format_part Js.t) ->
           match to_string v##._type with
           | "dayPeriod" -> (string "<b>")##concat_2 v##._value (string "</b>")
           | _ -> v##._value)
         (dtf##formatToParts (def date)))##reduce
        (wrap_callback (fun s part _ _ -> s##concat part))
    in
    fc dateString;

    let options = Intl.DateTimeFormat.options () in
    let () = options##.timeZone := def (string "UTC") in
    let dtf = new%js Intl.dateTimeFormat_constr (def (jas [| "zh-CN" |])) (def options) in
    let ropt = dtf##resolvedOptions () in
    fc ropt##.locale;
    fc ropt##.calendar;
    fc ropt##.numberingSystem;

    let options = Intl.DateTimeFormat.options () in
    let () = options##.timeZone := def (string "UTC") in
    let dtf = new%js Intl.dateTimeFormat_constr (def (jas [| "de-XX" |])) (def options) in
    let ropt = dtf##resolvedOptions () in
    fc ropt##.locale;
    fc ropt##.calendar;
    fc ropt##.numberingSystem;
    fc ropt##.timeZone;
    fc ropt##.month;

    let options = Intl.DateTimeFormat.object_options () in
    options##.localeMatcher := string "lookup";
    fc
      (intl##._DateTimeFormat##supportedLocalesOf
         (jas [| "ban"; "id-u-co-pinyin"; "de-ID" |])
         (def options));

    let number = number_of_float 123456.789 in
    let options = Intl.NumberFormat.options () in
    options##.style := string "currency";
    options##.currency := def (string "EUR");
    let nf = new%js Intl.numberFormat_constr (def (jas [| "de-DE" |])) (def options) in
    fc (nf##.format number);
    options##.currency := def (string "JPY");
    let nf = new%js Intl.numberFormat_constr (def (jas [| "ja-JP" |])) (def options) in
    fc (nf##.format number);
    let options = Intl.NumberFormat.options () in
    options##.maximumSignificantDigits := def 3;
    let nf = new%js Intl.numberFormat_constr (def (jas [| "en-IN" |])) (def options) in
    fc (nf##.format number);

    let nf = new%js Intl.numberFormat_constr (def (jas [| "de-DE" |])) undefined in
    fc (nf##.format number);
    let nf = new%js Intl.numberFormat_constr (def (jas [| "ar-EG" |])) undefined in
    fc (nf##.format number);
    let nf =
      new%js Intl.numberFormat_constr
        (def (jas [| "zh-Hans-CN-u-nu-hanidec" |]))
        undefined
    in
    fc (nf##.format number);
    let nf = new%js Intl.numberFormat_constr (def (jas [| "ban"; "id" |])) undefined in
    fc (nf##.format number);

    let amount = number_of_float 654321.987 in
    let options = Intl.NumberFormat.options () in
    options##.style := string "currency";
    options##.currency := def (string "RUB");
    let nf = new%js Intl.numberFormat_constr (def (jas [| "ru-RU" |])) (def options) in
    fc (nf##.format amount);
    options##.currency := def (string "USD");
    let nf = new%js Intl.numberFormat_constr (def (jas [| "en-US" |])) (def options) in
    fc (nf##.format amount);

    let a =
      array
        [| number_of_float 123456.789
         ; number_of_float 987654.321
         ; number_of_float 456789.123
        |]
    in
    let nf = new%js Intl.numberFormat_constr (def (jas [| "es-ES" |])) undefined in
    let formatted = array_map nf##.format a in
    fc (formatted##join (string "; "));

    let number = number_of_float 3500. in
    let options = Intl.NumberFormat.options () in
    options##.style := string "currency";
    options##.currency := def (string "EUR");
    let nf = new%js Intl.numberFormat_constr (def (jas [| "de-DE" |])) (def options) in
    fc (nf##.format number);
    let parts = nf##formatToParts (def number) in
    fc parts;
    let numberString =
      (array_map
         (fun (v : Intl.NumberFormat.format_part Js.t) ->
           match to_string v##._type with
           | "currency" -> (string "<strong>")##concat_2 v##._value (string "</strong>")
           | _ -> v##._value)
         (nf##formatToParts (def number)))##reduce
        (wrap_callback (fun s part _ _ -> s##concat part))
    in
    fc numberString;

    let nf = new%js Intl.numberFormat_constr (def (jas [| "de-DE" |])) undefined in
    let options = nf##resolvedOptions () in
    fc options##.locale;
    fc options##.numberingSystem;
    fc options##.style;
    fc options##.minimumIntegerDigits;
    fc options##.minimumFractionDigits;
    fc options##.maximumFractionDigits;
    fc options##.useGrouping;

    let options = Intl.NumberFormat.object_options () in
    options##.localeMatcher := string "lookup";
    fc
      (intl##._NumberFormat##supportedLocalesOf
         (jas [| "ban"; "id-u-co-pinyin"; "de-ID" |])
         (def options));

    let pr = new%js Intl.pluralRules_constr undefined undefined in
    fc (pr##select (number_of_float 0.));
    fc (pr##select (number_of_float 1.));
    fc (pr##select (number_of_float 2.));

    let pr = new%js Intl.pluralRules_constr (def (jas [| "ar-EG" |])) undefined in
    fc (pr##select (number_of_float 0.));
    fc (pr##select (number_of_float 1.));
    fc (pr##select (number_of_float 2.));
    fc (pr##select (number_of_float 6.));
    fc (pr##select (number_of_float 18.));

    let options = Intl.PluralRules.options () in
    options##._type := string "ordinal";
    let pr = new%js Intl.pluralRules_constr (def (jas [| "en-US" |])) (def options) in
    fc (pr##select (number_of_float 0.));
    fc (pr##select (number_of_float 1.));
    fc (pr##select (number_of_float 3.));
    fc (pr##select (number_of_float 4.));
    fc (pr##select (number_of_float 42.));

    let de = new%js Intl.pluralRules_constr (def (jas [| "de-DE" |])) undefined in
    let usedOptions = de##resolvedOptions () in
    fc usedOptions##.locale;
    fc usedOptions##.maximumFractionDigits;
    fc usedOptions##.minimumFractionDigits;
    fc usedOptions##.minimumIntegerDigits;
    fc usedOptions##.pluralCategories;
    fc usedOptions##._type;

    let options = Intl.PluralRules.object_options () in
    options##.localeMatcher := string "lookup";
    fc
      (intl##._PluralRules##supportedLocalesOf
         (jas [| "ban"; "id-u-co-pinyin"; "de-ID" |])
         (def options));

    let options = Intl.RelativeTimeFormat.options () in
    let () = options##.numeric := def (string "auto") in
    let () = options##.style := def (string "short") in
    let th_rtf = new%js Intl.relativeTimeFormat_constr (def (jas [| "th-TH" |])) (def options) in
    fc (th_rtf##format (number_of_float (-1.)) (string "day"));
  with Error err -> Console.console##debug (string (string_of_error err)))
else Console.console##debug (string "Intl is not supported!")
```
see [https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global\_Objects/Intl](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Intl) for API documentation.
see [https://www.ecma-international.org/ecma-402/1.0/](https://www.ecma-international.org/ecma-402/1.0/) for the ECMAScript specification.
```ocaml
module type Shared = sig ... end
```
```ocaml
module Collator : sig ... end
```
```ocaml
module DateTimeFormat : sig ... end
```
```ocaml
module NumberFormat : sig ... end
```
```ocaml
module PluralRules : sig ... end
```
```ocaml
module RelativeTimeFormat : sig ... end
```
```ocaml
class type  intl = object ... end
```
```ocaml
val intl : intl Js.t
```
```ocaml
val collator_constr : 
  (Js.js_string Js.t Js.js_array Js.t Js.optdef ->
    Collator.options Js.t Js.optdef ->
    Collator.t Js.t)
    Js.constr
```
```ocaml
val dateTimeFormat_constr : 
  (Js.js_string Js.t Js.js_array Js.t Js.optdef ->
    DateTimeFormat.options Js.t Js.optdef ->
    DateTimeFormat.t Js.t)
    Js.constr
```
```ocaml
val numberFormat_constr : 
  (Js.js_string Js.t Js.js_array Js.t Js.optdef ->
    NumberFormat.options Js.t Js.optdef ->
    NumberFormat.t Js.t)
    Js.constr
```
```ocaml
val pluralRules_constr : 
  (Js.js_string Js.t Js.js_array Js.t Js.optdef ->
    PluralRules.options Js.t Js.optdef ->
    PluralRules.t Js.t)
    Js.constr
```
```ocaml
val relativeTimeFormat_constr : 
  (Js.js_string Js.t Js.js_array Js.t Js.optdef ->
    RelativeTimeFormat.options Js.t Js.optdef ->
    RelativeTimeFormat.t Js.t)
    Js.constr
```
```ocaml
val is_supported : unit -> bool
```