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
open! Import

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

module Shared : Shared = struct
  class type object_options = object
    method localeMatcher : Js.js_string Js.t Js.prop
  end

  let object_options () =
    object%js
      val mutable localeMatcher = Js.string "best fit"
    end

  class type _object = object
    method supportedLocalesOf :
         Js.js_string Js.t Js.js_array Js.t
      -> object_options Js.t Js.optdef
      -> Js.js_string Js.t Js.js_array Js.t Js.meth
  end
end

module Collator = struct
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

  let options () =
    object%js
      val mutable localeMatcher = Js.string "best fit"

      val mutable usage = Js.string "sort"

      val mutable sensitivity = Js.string "variant"

      val mutable ignorePunctuation = Js._false

      val mutable numeric = Js._false

      val mutable caseFirst = Js.string "false"
    end

  class type t = object
    method compare : (Js.js_string Js.t -> Js.js_string Js.t -> int) Js.readonly_prop

    method resolvedOptions : unit -> resolved_options Js.t Js.meth
  end
end

module DateTimeFormat = struct
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

  let options () : options Js.t =
    object%js
      val mutable dateStyle = Js.undefined

      val mutable timeStyle = Js.undefined

      val mutable calendar = Js.undefined

      val mutable dayPeriod = Js.undefined

      val mutable numberingSystem = Js.undefined

      val mutable localeMatcher = Js.string "best fit"

      val mutable timeZone = Js.undefined

      val mutable hour12 = Js.undefined

      val mutable hourCycle = Js.undefined

      val mutable formatMatcher = Js.string "best fit"

      val mutable weekday = Js.undefined

      val mutable era = Js.undefined

      val mutable year = Js.undefined

      val mutable month = Js.undefined

      val mutable day = Js.undefined

      val mutable hour = Js.undefined

      val mutable minute = Js.undefined

      val mutable second = Js.undefined

      val mutable fractionalSecondDigits = Js.undefined

      val mutable timeZoneName = Js.undefined
    end

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

module NumberFormat = struct
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

  let options () : options Js.t =
    object%js
      val mutable compactDisplay = Js.undefined

      val mutable currency = Js.undefined

      val mutable currencyDisplay = Js.undefined

      val mutable currencySign = Js.undefined

      val mutable localeMatcher = Js.string "best fit"

      val mutable notation = Js.undefined

      val mutable numberingSystem = Js.undefined

      val mutable signDisplay = Js.undefined

      val mutable style = Js.string "decimal"

      val mutable unit = Js.undefined

      val mutable unitDisplay = Js.undefined

      val mutable useGrouping = Js._true

      val mutable roundingMode = Js.undefined

      val mutable roundingPriority = Js.undefined

      val mutable roundingIncrement = Js.undefined

      val mutable trailingZeroDisplay = Js.undefined

      val mutable minimumIntegerDigits = Js.undefined

      val mutable minimumFractionDigits = Js.undefined

      val mutable maximumFractionDigits = Js.undefined

      val mutable minimumSignificantDigits = Js.undefined

      val mutable maximumSignificantDigits = Js.undefined
    end

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

module PluralRules = struct
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

  let options () : options Js.t =
    object%js
      val mutable localeMatcher = Js.string "best fit"

      val mutable _type = Js.string "cardinal"
    end

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

let intl = Js.Unsafe.global##._Intl

let collator_constr = Js.Unsafe.global##._Intl##._Collator

let dateTimeFormat_constr = Js.Unsafe.global##._Intl##._DateTimeFormat

let numberFormat_constr = Js.Unsafe.global##._Intl##._NumberFormat

let pluralRules_constr = Js.Unsafe.global##._Intl##._PluralRules

let is_supported () = Js.Optdef.test intl
