open Import
open Memo.O

module Node = struct
  type t =
    { value : Dune_env.Stanza.t
    ; parent : t option Memo.t
    }

  let by_context dir =
    let open Memo.O in
    let+ context = Context.DB.by_dir dir in
    let { Context.Env_nodes.context; workspace } = context.env_nodes in
    let context = Option.some_if (not (context = Dune_env.Stanza.empty)) context in
    let workspace = Option.some_if (not (workspace = Dune_env.Stanza.empty)) workspace in
    match context, workspace with
    | None, None -> None
    | Some value, None | None, Some value -> Some { value; parent = Memo.return None }
    | Some context, Some workspace ->
      Some
        { value = context
        ; parent = Memo.return (Some { value = workspace; parent = Memo.return None })
        }
  ;;

  let in_dir ~dir =
    let+ stanzas = Only_packages.stanzas_in_dir dir in
    match stanzas with
    | None -> None
    | Some stanzas ->
      List.find_map stanzas.stanzas ~f:(function
        | Dune_env.T config -> Some config
        | _ -> None)
  ;;

  let rec by_dir dir =
    let parent =
      let* scope = Scope.DB.find_by_dir dir in
      if Path.Build.equal dir (Scope.root scope)
      then by_context dir
      else (
        match Path.Build.parent dir with
        | None -> by_context dir
        | Some parent -> by_dir parent)
    in
    in_dir ~dir
    >>= function
    | Some value -> Memo.return (Some { value; parent })
    | None -> parent
  ;;
end

let value ~default ~f =
  let rec loop = function
    | None -> Memo.return default
    | Some { Node.value; parent } ->
      let* next =
        f value
        >>| function
        | Some x -> `Ok x
        | None -> `Parent
      in
      (match next with
       | `Ok x -> Memo.return x
       | `Parent -> parent >>= loop)
  in
  fun ~dir -> Node.by_dir dir >>= loop
;;

let profile ~dir =
  let name, _ = Path.Build.extract_build_context_exn dir in
  let context = Context_name.of_string name in
  Per_context.profile context
;;

let value ~default ~dir ~f =
  let profile = lazy (profile ~dir) in
  value ~default ~dir ~f:(fun stanza ->
    let+ profile = Lazy.force profile in
    Dune_env.Stanza.find_opt stanza profile |> Option.bind ~f)
;;

let bin_annot ~dir =
  value ~default:true ~dir ~f:(fun (t : Dune_env.Stanza.config) -> t.bin_annot)
;;
