import "libs/dict.ml";
import "libs/heap.ml";
import "libs/int.ml";
import "libs/io.ml";
import "libs/iter.ml";
import "libs/curry/iter.ml";
import "libs/set.ml";
import "libs/str.ml";
import "libs/option.ml";
import "libs/vec.ml";
import "libs/parsing.ml";
import "libs/pbar.ml";   


let parse_device = 
  parsing.map_result(
    (fun abc -> vec.foldl((fun (l,r)->l^r), "", abc)), 
    parsing.repeat(parsing.any_char, 3, 3));

let parse_devices = parsing.seplist(parsing.ws, parse_device, 1);

let parse_outputs = parsing.sequence(
    parsing.suffixed(parse_device, parsing.text(": ")),
    parse_devices);

let edges = io.lines 
  |> (iter.map(fun line -> parsing.parse(line, parse_outputs)))
  |> (iter.map(fun x -> option.unwrap x))
  |> (iter.map(fun (up, dn) -> (up, set.collect vec.iter dn)))
  |> dict.collect;


let vars = {mut known=dict.empty};
vars.known <- dict.insert(vars.known, "out", {n=1; ndac=0; nfft=0; nboth=0});

let rec count_paths = fun(from) ->
  match dict.get(vars.known, from) with
    | `Some n -> n
    | `None _ -> begin
        let {n; ndac; nfft; nboth} = dict.get(edges, from) 
        |> option.unwrap 
        |> set.iter 
        |> (iter.map count_paths)
        |> ((iter.fold (fun 
              ({n=n1; ndac=ndac1; nfft=nfft1; nboth=nboth1}, 
               {n=n2; ndac=ndac2; nfft=nfft2; nboth=nboth2}) ->
              {n=n1+n2; ndac=ndac1+ndac2; nfft=nfft1+nfft2; nboth=nboth1+nboth2}))
            {n=0; ndac=0; nfft=0; nboth=0});

        let res = {mut n; mut ndac; mut nfft; mut nboth};

        if from == "dac" then
          res.nboth <- res.nboth + res.nfft;
          res.nfft <- 0;
          res.ndac <- res.n;
          res.n <- 0
        else {};

        if from == "fft" then
          res.nboth <- res.nboth + res.ndac;
          res.ndac <- 0;
          res.nfft <- res.n;
          res.n <- 0
        else {};

        vars.known <- dict.insert(vars.known, from, res);

        res
      end;


let result = (count_paths "svr").nboth;


io.write_line ("Day 11, Part 2: " ^ int.to_str(result));
