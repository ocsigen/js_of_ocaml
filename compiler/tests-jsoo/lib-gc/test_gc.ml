(* TEST *)

type stat =
  { minor_words : float
  ; promoted_words : float
  ; major_words : float
  ; minor_collections : int
  ; major_collections : int
  ; heap_words : int
  ; heap_chunks : int
  ; live_words : int
  ; live_blocks : int
  ; free_words : int
  ; free_blocks : int
  ; largest_free : int
  ; fragments : int
  ; compactions : int
  ; top_heap_words : int
  ; stack_size : int
  ; forced_major_collections : int
  }

type control =
  { minor_heap_size : int
  ; major_heap_increment : int
  ; space_overhead : int
  ; verbose : int
  ; max_overhead : int
  ; stack_limit : int
  ; allocation_policy : int
  ; window_size : int
  ; custom_major_ratio : int
  ; custom_minor_ratio : int
  ; custom_minor_max_size : int
  }

let () =
  assert ((Gc.get ()).custom_minor_max_size = 0);
  assert ((Gc.stat ()).forced_major_collections = 0);
  assert ((Gc.quick_stat ()).forced_major_collections = 0)
