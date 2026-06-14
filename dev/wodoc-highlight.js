// wodoc default syntax-highlight starter: start odoc's bundled highlight.js and
// teach it the OCaml extensions used across Ocsigen, so any doc can colour them:
//   - eliom:       let%client / %server / %shared (per-side colour), ~%x injection
//   - lwt:         let%lwt / match%lwt / … and the let* / and* / let+ operators
//   - js_of_ocaml: object%js / [%js] / … and the obj##meth / obj##.prop operators
//   - the name bound by let / and (function names)
(function () {
  var oc = window.hljs && hljs.getLanguage && hljs.getLanguage("ocaml");
  if (oc && oc.contains) {
    var rules = [];
    var lead = "(?:let|and|val|module|open|include|method|class|type|exception|fun)";
    // eliom: whole `let%client` (etc.) and bare `%client` -> per-side colour
    ["client", "server", "shared"].forEach(function (s) {
      rules.push({ className: "eliom-" + s, begin: new RegExp("\\b" + lead + "%" + s + "\\b") });
    });
    ["client", "server", "shared"].forEach(function (s) {
      rules.push({ className: "eliom-" + s, begin: new RegExp("%" + s + "\\b") });
    });
    // lwt: let*, and*, let+, and+ binding operators
    rules.push({ className: "keyword", begin: /\b(let|and)[*+]/ });
    // js_of_ocaml: obj##meth, obj##.prop
    rules.push({ className: "operator", begin: /##\.?/ });
    // eliom: ~%x client-value injection
    rules.push({ className: "subst", begin: /~%[A-Za-z_][\w']*/ });
    // any other ppx extension (%lwt, %js, %rpc, …) -- LAST so the specific rules win
    rules.push({ className: "keyword", begin: /%[a-z]+/ });
    oc.contains.unshift.apply(oc.contains, rules);
    // the name bound by let / let%x / let* / and -> a function/title (lookbehind
    // may be unsupported on old browsers; guard so the rest still applies)
    try {
      oc.contains.unshift({
        className: "title",
        begin: new RegExp("(?<=\\b(?:let|and)(?:%[a-z]+|[*+])?\\s+)[a-z_][\\w']*"),
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
