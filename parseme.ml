let rec append (l1 : list) (l2 : list) : list =
  match l1 with
  | Nil -> l2
  | Cons ((h : int), (t : list)) -> Cons (h, append t l2)
  
let rec reverse (l : list) : list =
  match l with
  | Nil -> Nil
  | Cons ((h : int), (t : list)) -> append (reverse t) (Cons (h, Nil))
  