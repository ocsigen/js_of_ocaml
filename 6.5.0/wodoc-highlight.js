// wodoc default syntax-highlight starter: start odoc's bundled highlight.js and
// teach it the OCaml extensions used across Ocsigen, so any doc can colour them:
//   - eliom:       let%client / %server / %shared / %rpc (per-side colour), ~%x injection
//   - lwt:         let%lwt / match%lwt / … and the let* / and* / let+ operators
//   - js_of_ocaml: object%js / [%js] / … and the obj##meth / obj##.prop operators
//   - the name bound by let / and (function names)
(function () {
  var oc = window.hljs && hljs.getLanguage && hljs.getLanguage("ocaml");
  if (oc && oc.contains) {
    var rules = [];
    var lead = "(?:let|and|val|module|open|include|method|class|type|exception|fun)";
    // eliom: whole `let%client` (etc.) and bare `%client` -> per-side colour.
    // %rpc gets its own class; the stylesheet gives it the same green as %shared
    // (a server-defined call exposed to the client) instead of the generic %xxx keyword.
    ["client", "server", "shared", "rpc"].forEach(function (s) {
      rules.push({ className: "eliom-" + s, begin: new RegExp("\\b" + lead + "%" + s + "\\b") });
    });
    ["client", "server", "shared", "rpc"].forEach(function (s) {
      rules.push({ className: "eliom-" + s, begin: new RegExp("%" + s + "\\b") });
    });
    // lwt: let*, and*, let+, and+ binding operators
    rules.push({ className: "keyword", begin: /\b(let|and)[*+]/ });
    // js_of_ocaml: obj##meth, obj##.prop
    rules.push({ className: "operator", begin: /##\.?/ });
    // eliom: ~%x client-value injection
    rules.push({ className: "subst", begin: /~%[A-Za-z_][\w']*/ });
    // labelled / optional arguments: ~name(:) and ?name(:)  (after ~%x, so it
    // never swallows a client-value injection; label names are lowercase idents)
    rules.push({ className: "label", begin: /[~?][a-z_][\w']*:?/ });
    // any other ppx extension (%lwt, %js, %rpc, …) -- LAST so the specific rules win
    rules.push({ className: "keyword", begin: /%[a-z]+/ });
    oc.contains.unshift.apply(oc.contains, rules);
    // the name bound by let / let%x / let* / and -> a function/title (lookbehind
    // may be unsupported on old browsers; guard so the rest still applies)
    try {
      // The bound name(s) after let/and -> title (orange). Covers:
      //   let f x y = ...        -> f only (x y are parameters)
      //   let p, r, s = ...      -> p, r, s (top-level tuple binding)
      //   let (p, r) = ...       -> p, r   (parenthesised tuple binding)
      // but NOT a tuple PARAMETER: in `let f (p, r) = ...` a name precedes the
      // paren, so the `\(?` anchored right after let/and cannot reach p/r, and
      // f alone stays coloured.
      // `(?:rec\s+)?` lets the name be found after `let rec`; the negative
      // lookahead then keeps the `rec`/`module` keywords themselves uncoloured.
      var lead = "\\b(?:let|and)(?:%[a-z]+|[*+])?\\s+(?:rec\\s+)?";
      // first name, possibly the first one inside a parenthesised tuple; never a
      // bare `rec`/`module` keyword (e.g. `let rec f`, `let (module M)`).
      oc.contains.unshift({
        className: "title",
        begin: new RegExp("(?<=" + lead + "\\(?\\s*)(?!(?:module|rec)\\b)[a-z_][\\w']*"),
      });
      // the following tuple names, reached through identifiers/commas/spaces only
      // (no further parens, and never across `=`), so only genuine tuple bindings
      // get every name coloured.
      oc.contains.unshift({
        className: "title",
        begin: new RegExp("(?<=" + lead + "\\(?[\\w\\s,']*,\\s*)[a-z_][\\w']*"),
      });
    } catch (e) { /* lookbehind unsupported: skip function-name highlighting */ }
    hljs.registerLanguage("ocaml", function () { return oc; });
  }
  if (window.hljs) {
    if (document.readyState === "loading")
      document.addEventListener("DOMContentLoaded", function () { hljs.highlightAll(); });
    else hljs.highlightAll();
  }
})();
