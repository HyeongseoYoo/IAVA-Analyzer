open Provenance

let api_url = "https://api.anthropic.com/v1/messages"
let model = "claude-sonnet-4-6"

(* ===== Prompt builder ===== *)

let prompt_of_chains (chains : chain list) : string =
  let chain_strs =
    List.mapi
      (fun i c ->
        Printf.sprintf "### Bug #%d\n%s" (i + 1) (string_of_chain c))
      chains
  in
  let bugs_section = String.concat "\n\n" chain_strs in
  {|You are an expert in embedded systems and static analysis.
Below are bugs detected by an abstract-interpretation-based analyzer for an interrupt-driven embedded program.
The analyzer works on a toy language where:
- `init { ... }` initializes global state and registers interrupt handlers
- `main { ... }` is the main program
- `handler N { ... }` is the body of interrupt handler N
- `enable` / `disable` control whether interrupts are delivered
- `malloc(n, v)` allocates n cells initialized to v and returns a pointer
- `*base[offset]` dereferences a heap array (read or write)

For each detected bug:
1. Explain in plain English what went wrong (what value reached an out-of-bounds index, where it came from)
2. Identify the root cause based on the provenance chain
3. Suggest a concrete fix

Be concise. Use the line numbers and expressions from the provenance chain. Avoid restating raw interval notation unless it clarifies the explanation.

---

|}
  ^ bugs_section

(* ===== API call ===== *)

let build_request_body (prompt : string) : string =
  let msg =
    `Assoc
      [
        ("role", `String "user");
        ("content", `String prompt);
      ]
  in
  `Assoc
    [
      ("model", `String model);
      ("max_tokens", `Int 4096);
      ("messages", `List [ msg ]);
    ]
  |> Yojson.Basic.to_string

let extract_text (json_str : string) : string =
  try
    match Yojson.Basic.from_string json_str with
    | `Assoc fields -> (
        match List.assoc_opt "content" fields with
        | Some (`List ((`Assoc block_fields :: _))) -> (
            match List.assoc_opt "text" block_fields with
            | Some (`String t) -> t
            | _ -> "(no text in response)")
        | _ -> "(unexpected response shape)")
    | _ -> "(unexpected response shape)"
  with _ -> "(failed to parse API response)"

let call_api (prompt : string) : string =
  let api_key =
    match Sys.getenv_opt "ANTHROPIC_API_KEY" with
    | Some k -> k
    | None ->
        failwith
          "ANTHROPIC_API_KEY environment variable is not set"
  in
  let body_str = build_request_body prompt in
  let result = ref "" in
  Lwt_main.run
    (let open Lwt.Syntax in
     let headers =
       Cohttp.Header.of_list
         [
           ("x-api-key", api_key);
           ("anthropic-version", "2023-06-01");
           ("content-type", "application/json");
         ]
     in
     let body = Cohttp_lwt.Body.of_string body_str in
     let uri = Uri.of_string api_url in
     let* _resp, body =
       Cohttp_lwt_unix.Client.post ~headers ~body uri
     in
     let* body_str = Cohttp_lwt.Body.to_string body in
     result := body_str;
     Lwt.return_unit);
  extract_text !result

(* ===== Public entry point ===== *)

let explain (chains : chain list) : string =
  if chains = [] then "No bugs detected — nothing to explain."
  else
    let prompt = prompt_of_chains chains in
    let response = call_api prompt in
    Printf.sprintf "=== LLM Bug Report ===\n\n%s" response
