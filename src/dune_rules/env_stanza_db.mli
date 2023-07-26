open Import

val value
  :  default:'a
  -> dir:Path.Build.t
  -> f:(Dune_env.Stanza.config -> 'a option)
  -> 'a Memo.t

val bin_annot : dir:Path.Build.t -> bool Memo.t
