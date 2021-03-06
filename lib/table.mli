type 'a code = 
  {
    length : int;
    code : int;
    data : 'a;
  }

type 'a table = (int * 'a) option array

val lookup : 'a code list -> 'a table * int
