(* Js_of_ocaml library
 * http://www.ocsigen.org/js_of_ocaml/
 * Copyright (C) 2018 Stéphane Legrand
 *
 * This program is free software; you can redistribute it and/or modify
 * it under the terms of the GNU Lesser General Public License as published by
 * the Free Software Foundation, with linking exception;
 * either version 2.1 of the License, or (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU Lesser General Public License for more details.
 *
 * You should have received a copy of the GNU Lesser General Public License
 * along with this program; if not, write to the Free Software
 * Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.
 *)

(** Internationalization API

    A code example:
    {[
      open Js;;

      let fc v = Firebug.console##debug v in

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
          let collator =
            new%js Intl.collator_constr (def (jas [| "de" |])) (def options)
          in
          fc (collator##.compare (string "ä") (string "a"));
          let collator =
            new%js Intl.collator_constr (def (jas [| "sv" |])) (def options)
          in
          fc (collator##.compare (string "ä") (string "a"));

          let firstAlphabetical locale letter1 letter2 =
            let collator =
              new%js Intl.collator_constr (def (array [| locale |])) undefined
            in
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
              (wrap_callback (fun v1 v2 ->
                   Js.float (float_of_int (collator##.compare v1 v2))))
          in
          fc (a##join (string ", "));

          let a = jas [| "Congrès"; "congres"; "Assemblée"; "poisson" |] in
          let options = Intl.Collator.options () in
          let () = options##.usage := string "search" in
          let () = options##.sensitivity := string "base" in
          let collator =
            new%js Intl.collator_constr (def (jas [| "fr" |])) (def options)
          in
          let s = string "congres" in
          let matches =
            a##filter (wrap_callback (fun v _ _ -> bool (collator##.compare v s = 0)))
          in
          fc (matches##join (string ", "));

          let options = Intl.Collator.options () in
          let () = options##.sensitivity := string "base" in
          let collator =
            new%js Intl.collator_constr (def (jas [| "de" |])) (def options)
          in
          let usedOptions = collator##resolvedOptions () in
          fc usedOptions##.locale;
          fc usedOptions##.usage;
          fc usedOptions##.sensitivity;
          fc usedOptions##.ignorePunctuation;
          fc usedOptions##.collation;
          fc usedOptions##.numeric;

          let date = new%js date_sec 2012 11 20 3 0 0 in
          (* Results below assume UTC timezone - your results may vary *)
          let dtf =
            new%js Intl.dateTimeFormat_constr (def (jas [| "en-US" |])) undefined
          in
          fc (dtf##.format date);
          let dtf =
            new%js Intl.dateTimeFormat_constr (def (jas [| "ban"; "id" |])) undefined
          in
          fc (dtf##.format date);
          let dtf =
            new%js Intl.dateTimeFormat_constr
              (def (jas [| "ja-JP-u-ca-japanese" |]))
              undefined
          in
          fc (dtf##.format date);
          let options = Intl.DateTimeFormat.options () in
          let () = options##.weekday := def (string "long") in
          let () = options##.year := def (string "numeric") in
          let () = options##.month := def (string "long") in
          let () = options##.day := def (string "numeric") in
          let dtf =
            new%js Intl.dateTimeFormat_constr (def (jas [| "de-DE" |])) (def options)
          in
          fc (dtf##.format date);
          let () = options##.timeZone := def (string "UTC") in
          let () = options##.timeZoneName := def (string "short") in
          let dtf =
            new%js Intl.dateTimeFormat_constr (def (jas [| "en-US" |])) (def options)
          in
          fc (dtf##.format date);
          let options = Intl.DateTimeFormat.options () in
          let () = options##.hour := def (string "numeric") in
          let () = options##.minute := def (string "numeric") in
          let () = options##.second := def (string "numeric") in
          let () = options##.timeZone := def (string "Australia/Sydney") in
          let () = options##.timeZoneName := def (string "short") in
          let dtf =
            new%js Intl.dateTimeFormat_constr (def (jas [| "en-AU" |])) (def options)
          in
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
          let dtf =
            new%js Intl.dateTimeFormat_constr (def (jas [| "en-US" |])) (def options)
          in
          fc (dtf##.format date);

          let date = new%js date_month 2012 05 in
          let options = Intl.DateTimeFormat.options () in
          let () = options##.weekday := def (string "long") in
          let () = options##.year := def (string "numeric") in
          let () = options##.month := def (string "long") in
          let () = options##.day := def (string "numeric") in
          let dtf =
            new%js Intl.dateTimeFormat_constr (def (jas [| "sr-RS" |])) (def options)
          in
          fc (dtf##.format date);
          let dtf =
            new%js Intl.dateTimeFormat_constr (def (jas [| "en-GB" |])) (def options)
          in
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
          let dtf =
            new%js Intl.dateTimeFormat_constr (def (jas [| "pt-BR" |])) (def options)
          in
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
          let dtf =
            new%js Intl.dateTimeFormat_constr (def (jas [| "en-us" |])) (def options)
          in
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
          let dtf =
            new%js Intl.dateTimeFormat_constr (def (jas [| "zh-CN" |])) (def options)
          in
          let ropt = dtf##resolvedOptions () in
          fc ropt##.locale;
          fc ropt##.calendar;
          fc ropt##.numberingSystem;

          let options = Intl.DateTimeFormat.options () in
          let () = options##.timeZone := def (string "UTC") in
          let dtf =
            new%js Intl.dateTimeFormat_constr (def (jas [| "de-XX" |])) (def options)
          in
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
          let nf =
            new%js Intl.numberFormat_constr (def (jas [| "de-DE" |])) (def options)
          in
          fc (nf##.format number);
          options##.currency := def (string "JPY");
          let nf =
            new%js Intl.numberFormat_constr (def (jas [| "ja-JP" |])) (def options)
          in
          fc (nf##.format number);
          let options = Intl.NumberFormat.options () in
          options##.maximumSignificantDigits := def 3;
          let nf =
            new%js Intl.numberFormat_constr (def (jas [| "en-IN" |])) (def options)
          in
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
          let nf =
            new%js Intl.numberFormat_constr (def (jas [| "ban"; "id" |])) undefined
          in
          fc (nf##.format number);

          let amount = number_of_float 654321.987 in
          let options = Intl.NumberFormat.options () in
          options##.style := string "currency";
          options##.currency := def (string "RUB");
          let nf =
            new%js Intl.numberFormat_constr (def (jas [| "ru-RU" |])) (def options)
          in
          fc (nf##.format amount);
          options##.currency := def (string "USD");
          let nf =
            new%js Intl.numberFormat_constr (def (jas [| "en-US" |])) (def options)
          in
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
          let nf =
            new%js Intl.numberFormat_constr (def (jas [| "de-DE" |])) (def options)
          in
          fc (nf##.format number);
          let parts = nf##formatToParts (def number) in
          fc parts;
          let numberString =
            (array_map
               (fun (v : Intl.NumberFormat.format_part Js.t) ->
                 match to_string v##._type with
                 | "currency" ->
                     (string "<strong>")##concat_2 v##._value (string "</strong>")
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
          let pr =
            new%js Intl.pluralRules_constr (def (jas [| "en-US" |])) (def options)
          in
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
               (def options))
        with Error err -> Firebug.console##debug (string (string_of_error err)))
      else Firebug.console##debug (string "Intl is not supported!")
    ]}
    @see <https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Intl>
      for API documentation.
    @see <https://www.ecma-international.org/ecma-402/1.0/>
      for the ECMAScript specification. *)

module type Shared = sig
  class type object_options = object
    method localeMatcher : Js.js_string Js.t Js.prop
  end

  val object_options : unit -> object_options Js.t

  class type _object = object
    method supportedLocalesOf :
         Js.js_string Js.t Js.js_array Js.t
      -> object_options Js.t Js.optdef
      -> Js.js_string Js.t Js.js_array Js.t Js.meth
  end
end

module Collator : sig
  include Shared

  class type resolved_options = object
    method locale : Js.js_string Js.t Js.readonly_prop

    method usage : Js.js_string Js.t Js.readonly_prop

    method sensitivity : Js.js_string Js.t Js.readonly_prop

    method ignorePunctuation : bool Js.t Js.readonly_prop

    method collation : Js.js_string Js.t Js.readonly_prop

    method numeric : bool Js.t Js.readonly_prop

    method caseFirst : Js.js_string Js.t Js.readonly_prop
  end

  class type options = object
    method localeMatcher : Js.js_string Js.t Js.prop

    method usage : Js.js_string Js.t Js.prop

    method sensitivity : Js.js_string Js.t Js.prop

    method ignorePunctuation : bool Js.t Js.prop

    method numeric : bool Js.t Js.prop

    method caseFirst : Js.js_string Js.t Js.prop
  end

  val options : unit -> options Js.t

  class type t = object
    method compare : (Js.js_string Js.t -> Js.js_string Js.t -> int) Js.readonly_prop

    method resolvedOptions : unit -> resolved_options Js.t Js.meth
  end
end

module DateTimeFormat : sig
  include Shared

  class type resolved_options = object
    method locale : Js.js_string Js.t Js.readonly_prop

    method calendar : Js.js_string Js.t Js.readonly_prop

    method numberingSystem : Js.js_string Js.t Js.readonly_prop

    method timeZone : Js.js_string Js.t Js.readonly_prop

    method hour12 : bool Js.t Js.readonly_prop

    method weekday : Js.js_string Js.t Js.optdef_prop

    method era : Js.js_string Js.t Js.optdef_prop

    method year : Js.js_string Js.t Js.optdef_prop

    method month : Js.js_string Js.t Js.optdef_prop

    method day : Js.js_string Js.t Js.optdef_prop

    method hour : Js.js_string Js.t Js.optdef_prop

    method minute : Js.js_string Js.t Js.optdef_prop

    method second : Js.js_string Js.t Js.optdef_prop

    method timeZoneName : Js.js_string Js.t Js.optdef_prop
  end

  class type options = object
    method dateStyle : Js.js_string Js.t Js.optdef Js.prop

    method timeStyle : Js.js_string Js.t Js.optdef Js.prop

    method calendar : Js.js_string Js.t Js.optdef Js.prop

    method dayPeriod : Js.js_string Js.t Js.optdef Js.prop

    method numberingSystem : Js.js_string Js.t Js.optdef Js.prop

    method localeMatcher : Js.js_string Js.t Js.prop

    method timeZone : Js.js_string Js.t Js.optdef Js.prop

    method hour12 : bool Js.t Js.optdef Js.prop

    method hourCycle : Js.js_string Js.t Js.optdef Js.prop

    method formatMatcher : Js.js_string Js.t Js.prop

    method weekday : Js.js_string Js.t Js.optdef Js.prop

    method era : Js.js_string Js.t Js.optdef Js.prop

    method year : Js.js_string Js.t Js.optdef Js.prop

    method month : Js.js_string Js.t Js.optdef Js.prop

    method day : Js.js_string Js.t Js.optdef Js.prop

    method hour : Js.js_string Js.t Js.optdef Js.prop

    method minute : Js.js_string Js.t Js.optdef Js.prop

    method second : Js.js_string Js.t Js.optdef Js.prop

    method fractionalSecondDigits : int Js.optdef Js.prop

    method timeZoneName : Js.js_string Js.t Js.optdef Js.prop
  end

  val options : unit -> options Js.t

  class type format_part = object
    method _type : Js.js_string Js.t Js.readonly_prop

    method _value : Js.js_string Js.t Js.readonly_prop
  end

  class type t = object
    method format : (Js.date Js.t -> Js.js_string Js.t) Js.readonly_prop

    method formatToParts :
      Js.date Js.t Js.optdef -> format_part Js.t Js.js_array Js.t Js.meth

    method resolvedOptions : unit -> resolved_options Js.t Js.meth
  end
end

module NumberFormat : sig
  include Shared

  class type resolved_options = object
    method locale : Js.js_string Js.t Js.readonly_prop

    method numberingSystem : Js.js_string Js.t Js.readonly_prop

    method style : Js.js_string Js.t Js.readonly_prop

    method currency : Js.js_string Js.t Js.optdef_prop

    method currencyDisplay : Js.js_string Js.t Js.optdef_prop

    method useGrouping : bool Js.t Js.readonly_prop

    method minimumIntegerDigits : int Js.optdef_prop

    method minimumFractionDigits : int Js.optdef_prop

    method maximumFractionDigits : int Js.optdef_prop

    method minimumSignificantDigits : int Js.optdef_prop

    method maximumSignificantDigits : int Js.optdef_prop
  end

  class type options = object
    method compactDisplay : Js.js_string Js.t Js.optdef Js.prop

    method currency : Js.js_string Js.t Js.optdef Js.prop

    method currencyDisplay : Js.js_string Js.t Js.optdef Js.prop

    method currencySign : Js.js_string Js.t Js.optdef Js.prop

    method localeMatcher : Js.js_string Js.t Js.prop

    method notation : Js.js_string Js.t Js.optdef Js.prop

    method numberingSystem : Js.js_string Js.t Js.optdef Js.prop

    method signDisplay : Js.js_string Js.t Js.optdef Js.prop

    method style : Js.js_string Js.t Js.prop

    method unit : Js.js_string Js.t Js.optdef Js.prop

    method unitDisplay : Js.js_string Js.t Js.optdef Js.prop

    method useGrouping : bool Js.t Js.prop

    method roundingMode : Js.js_string Js.t Js.optdef Js.prop

    method roundingPriority : Js.js_string Js.t Js.optdef Js.prop

    method roundingIncrement : Js.js_string Js.t Js.optdef Js.prop

    method trailingZeroDisplay : Js.js_string Js.t Js.optdef Js.prop

    method minimumIntegerDigits : int Js.optdef Js.prop

    method minimumFractionDigits : int Js.optdef Js.prop

    method maximumFractionDigits : int Js.optdef Js.prop

    method minimumSignificantDigits : int Js.optdef Js.prop

    method maximumSignificantDigits : int Js.optdef Js.prop
  end

  val options : unit -> options Js.t

  class type format_part = object
    method _type : Js.js_string Js.t Js.readonly_prop

    method _value : Js.js_string Js.t Js.readonly_prop
  end

  class type t = object
    method format : (Js.number Js.t -> Js.js_string Js.t) Js.readonly_prop

    method formatToParts :
      Js.number Js.t Js.optdef -> format_part Js.t Js.js_array Js.t Js.meth

    method resolvedOptions : unit -> resolved_options Js.t Js.meth
  end
end

module PluralRules : sig
  include Shared

  class type resolved_options = object
    method locale : Js.js_string Js.t Js.readonly_prop

    method pluralCategories : Js.js_string Js.t Js.js_array Js.t Js.readonly_prop

    method _type : Js.js_string Js.t Js.readonly_prop

    method minimumIntegerDigits : int Js.optdef_prop

    method minimumFractionDigits : int Js.optdef_prop

    method maximumFractionDigits : int Js.optdef_prop

    method minimumSignificantDigits : int Js.optdef_prop

    method maximumSignificantDigits : int Js.optdef_prop
  end

  class type options = object
    method localeMatcher : Js.js_string Js.t Js.prop

    method _type : Js.js_string Js.t Js.prop
  end

  val options : unit -> options Js.t

  class type t = object
    method select : Js.number Js.t -> Js.js_string Js.t Js.meth

    method resolvedOptions : unit -> resolved_options Js.t Js.meth
  end
end

class type intl = object
  method _Collator : Collator._object Js.t Js.readonly_prop

  method _DateTimeFormat : DateTimeFormat._object Js.t Js.readonly_prop

  method _NumberFormat : NumberFormat._object Js.t Js.readonly_prop

  method _PluralRules : PluralRules._object Js.t Js.readonly_prop

  method getCanonicalLocales :
    Js.js_string Js.t Js.js_array Js.t -> Js.js_string Js.t Js.js_array Js.t Js.meth
end

val intl : intl Js.t

val collator_constr :
  (   Js.js_string Js.t Js.js_array Js.t Js.optdef
   -> Collator.options Js.t Js.optdef
   -> Collator.t Js.t)
  Js.constr

val dateTimeFormat_constr :
  (   Js.js_string Js.t Js.js_array Js.t Js.optdef
   -> DateTimeFormat.options Js.t Js.optdef
   -> DateTimeFormat.t Js.t)
  Js.constr

val numberFormat_constr :
  (   Js.js_string Js.t Js.js_array Js.t Js.optdef
   -> NumberFormat.options Js.t Js.optdef
   -> NumberFormat.t Js.t)
  Js.constr

val pluralRules_constr :
  (   Js.js_string Js.t Js.js_array Js.t Js.optdef
   -> PluralRules.options Js.t Js.optdef
   -> PluralRules.t Js.t)
  Js.constr

val is_supported : unit -> bool
