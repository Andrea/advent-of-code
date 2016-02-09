(*
--- Day 5: Doesn't He Have Intern-Elves For This? ---

Santa needs help figuring out which strings in his text file are naughty 
or nice.

A nice string is one with all of the following properties:

It contains at least three vowels (aeiou only), like aei, xazegov, or 
aeiouaeiouaeiou.
It contains at least one letter that appears twice in a row, like xx, 
abcdde (dd), or aabbccdd (aa, bb, cc, or dd).
It does not contain the strings ab, cd, pq, or xy, even if they are 
part of one of the other requirements.
For example:

ugknbfddgicrmopn is nice because it has at least three vowels 
(u...i...o...), a double letter (...dd...), and none of the disallowed 
substrings.
aaa is nice because it has at least three vowels and a double letter, 
even though the letters used by different rules overlap.
jchzalrnumimnmhp is naughty because it has no double letter.
haegwjzuvuyypxyu is naughty because it contains the string xy.
dvszwmarrgswjxmb is naughty because it contains only one vowel.
How many strings are nice*)
open System 

type NaughtyOrNice =
    |Naughty
    |Nice
let sumBools (op:bool -> bool -> bool)  xs  =   
  Seq.fold( fun acc x-> op x  acc ) (Seq.head xs) xs
let ContainsForbdiden (s:string)=
    ["ab";"cd";"pq";"xy"]
    |> Seq.map(fun x -> (s.Contains( x)  ))
    |> sumBools (&&)

let vowels (s:string)=
  let biggerThan2 n = n > 2
  s |> Seq.map(fun x -> 
      if(x = 'a' || x = 'e'|| x= 'i' || x= 'o' || x= 'u' ) then 
          1 else 0)
    |> Seq.sum
    |> biggerThan2 

let twiceInRow (s:string) =
  s |> Seq.pairwise
    |> Seq.map(fun m  -> match m with
                        | (x,y) when x= y -> true
                        | _ -> false)
    |> sumBools (||)

let (|Nice|) (s:string) =
  printfn "s %s vowels %A twice %A not %A"  s (vowels s) (twiceInRow s) (not ContainsForbdiden s)
  vowels s && twiceInRow s && (ContainsForbdiden s |>not)
let stringSelector (s: string) =
    match s.Trim().ToLower() with
    | Nice true -> Nice
    | _ -> Naughty

[ ("ugknbfddgicrmopn" ,  Nice );
  ("jchzalrnumimnmhp", Naughty);
  ("haegwjzuvuyypxyu", Naughty);
  ("dvszwmarrgswjxmb", Naughty) ]
|> Seq.map(fun x -> match x with 
                    | (t, r) -> stringSelector t = r, r)
|> Seq.iter(fun x -> printfn "%A "x)
