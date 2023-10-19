// Lily Eap
// PPM image configurations program

// Do not change the API (module/namespace/function signatures)

// Refer to the PDF for more instructions on how to implement
// each function

// As of now, each function simply takes an image as input
// and returns it as it is

namespace ImageLibrary

module Operations =
 
  let rec Grayscale (width:int) 
                    (height:int) 
                    (depth:int) 
                    (image:(int*int*int) list list) = 
    
    image


  
  let rec Threshold (width:int) 
                    (height:int)
                    (depth:int)
                    (image:(int*int*int) list list)
                    (threshold:int) = 
    
    image


  
  let rec FlipHorizontal (width:int)
                         (height:int)
                         (depth:int)
                         (image:(int*int*int) list list) = 
    let rec reverseRows image =
      match image with
      | [] -> []
      | row :: rest -> List.rev row :: reverseRows rest

    reverseRows image


  
  let rec EdgeDetect (width:int)
               (height:int)
               (depth:int)
               (image:(int*int*int) list list)
               (threshold:int) = 
    image

 
  let rec RotateRight90 (width:int)
                        (height:int)
                        (depth:int)
                        (image:(int*int*int) list list) = 
    let rec transpose image =
      match image with
      | [] -> []
      | [] :: _ -> []
      | _ ->
          let heads = List.map (function [] -> None | x :: _ -> Some x) image |> List.choose id
          let tails = List.map (function [] -> None | _ :: xs -> Some xs) image |> List.choose id
          heads :: transpose tails
    let rec reverseRows image =
      match image with
      | [] -> []
      | row :: rest -> List.rev row :: reverseRows rest

    transpose image |> reverseRows


