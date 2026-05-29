open Provenance

(* ===== Prompt builder ===== *)

let prompt_of_context ~(prov_report : string) ~(source_code : string) : string =
  {|You are an expert in embedded systems and static analysis.
Below are analysis results for an interrupt-driven embedded program in a toy language.
The analyzer uses abstract interpretation and provenance tracing.

The toy language has:
- `init { ... }` for global initialization and interrupt registration
- `main { ... }` for the main program
- `handler N { ... }` for interrupt handler N
- `enable` / `disable` for interrupt delivery
- `malloc(n, v)` for heap allocation
- `*base[offset]` for heap array read/write

Explain the bug report for a developer.
For each bug:
1. State the main reason in plain English
2. Trace how the bad value or pointer reaches the bug, using line numbers and expressions
3. Name the interrupt handler if it is part of the cause
4. Suggest a concrete fix

Be concise and easy to understand. Do not dump raw interval notation unless it is needed to explain the trace.

=== Provenance Report (-prov output) ===
|}
  ^ prov_report
  ^ {|

=== Example Code ===
```si
|}
  ^ source_code
  ^ {|
```
|}

(* ===== Codex exec call ===== *)

let read_all (ic : in_channel) : string =
  let buf = Buffer.create 4096 in
  (try
     while true do
       Buffer.add_string buf (input_line ic);
       Buffer.add_char buf '\n'
     done
   with End_of_file -> ());
  Buffer.contents buf

let status_to_string = function
  | Unix.WEXITED n -> Printf.sprintf "exit %d" n
  | Unix.WSIGNALED n -> Printf.sprintf "signal %d" n
  | Unix.WSTOPPED n -> Printf.sprintf "stopped %d" n

let is_executable (path : string) : bool =
  try
    Unix.access path [ Unix.X_OK ];
    true
  with Unix.Unix_error _ -> false

let find_in_path (name : string) : string option =
  let path = Option.value (Sys.getenv_opt "PATH") ~default:"" in
  let dirs = String.split_on_char ':' path in
  List.find_map
    (fun dir ->
      let dir = if dir = "" then "." else dir in
      let candidate = Filename.concat dir name in
      if is_executable candidate then Some candidate else None)
    dirs

let is_directory (path : string) : bool =
  try Sys.is_directory path with Sys_error _ -> false

let find_vscode_codex () : string option =
  let platform_dirs = [ "linux-x86_64"; "linux-arm64" ] in
  match Sys.getenv_opt "HOME" with
  | None -> None
  | Some home ->
      let extension_roots =
        [
          Filename.concat home ".vscode-server/extensions";
          Filename.concat home ".vscode/extensions";
        ]
      in
      extension_roots
      |> List.filter is_directory
      |> List.concat_map (fun root ->
             Sys.readdir root |> Array.to_list
             |> List.filter (String.starts_with ~prefix:"openai.chatgpt-")
             |> List.concat_map (fun extension_dir ->
                    List.map
                      (fun platform_dir ->
                        Filename.concat root
                          (Filename.concat extension_dir
                             (Filename.concat "bin"
                                (Filename.concat platform_dir "codex"))))
                      platform_dirs))
      |> List.find_opt is_executable

let codex_command () : (string, string) result =
  match Sys.getenv_opt "CODEX_EXECUTABLE" with
  | Some path when path <> "" ->
      if is_executable path then Ok path
      else
        Error
          (Printf.sprintf
             "CODEX_EXECUTABLE is set to %s, but it is not executable" path)
  | _ -> (
      match find_in_path "codex" with
      | Some path -> Ok path
      | None -> (
          match find_vscode_codex () with
          | Some path -> Ok path
          | None ->
              Error
                "could not find `codex` in PATH or the VS Code extension \
                 directory. Set CODEX_EXECUTABLE to the full codex binary \
                 path, or add codex to PATH."))

let call_codex_exec (prompt : string) : string =
  try
    match codex_command () with
    | Error msg -> "failed to run codex exec: " ^ msg
    | Ok cmd ->
        let args =
          [|
            "codex";
            "exec";
            "--color";
            "never";
            "--sandbox";
            "read-only";
            "-";
          |]
        in
        let env = Unix.environment () in
        let stdout_ic, stdin_oc, stderr_ic =
          Unix.open_process_args_full cmd args env
        in
        output_string stdin_oc prompt;
        close_out stdin_oc;
        let stdout = read_all stdout_ic in
        let stderr = read_all stderr_ic in
        match Unix.close_process_full (stdout_ic, stdin_oc, stderr_ic) with
        | Unix.WEXITED 0 -> stdout
        | status ->
            Printf.sprintf
              "codex exec failed (%s).\n\nstderr:\n%s\nstdout:\n%s"
              (status_to_string status) stderr stdout
  with
  | Unix.Unix_error (err, fn, arg) ->
      Printf.sprintf "failed to run codex exec: %s(%s): %s" fn arg
        (Unix.error_message err)
  | Sys_error msg -> Printf.sprintf "failed to run codex exec: %s" msg

(* ===== Public entry point ===== *)

let explain ~(source_code : string) (chains : chain list) : string =
  if chains = [] then "# Codex Bug Report\n\nNo bugs detected - nothing to explain.\n"
  else
    let prov_report = string_of_report chains in
    let prompt = prompt_of_context ~prov_report ~source_code in
    let response = call_codex_exec prompt in
    Printf.sprintf "# Codex Bug Report\n\n%s\n" response
