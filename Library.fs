//
// Project Name Image processing in F#
// Project Description F# function library to perform image operations
// Name Lily Eap
// Netid 676977984
// Date 10/28/2023

//
// Do not change the API (module/namespace/function signatures)

// Refer to the PDF for more instructions on how to implement
// each function

// As of now, each function simply takes an image as input
// and returns it as it is

namespace ImageLibrary

module Operations =
  // description
  // changes each pixel's rgb so that each value is the calculated grayscale value

  // breakdown:
  // image passed to higher order function list.map 
  // each row will be passed to a lambda function
  // where each 3-int tuple in the row will be passed to higher order function list.map
  // each value in the tuple will have a new value that is the calculoated grayscale value
  let rec Grayscale (width:int) 
                    (height:int) 
                    (depth:int) 
                    (image:(int*int*int) list list) = 
    image |> List.map (fun row -> row |> List.map (fun (r, g, b) ->  (int (float r * 0.299 + float g * 0.587 + float b * 0.114), int (float r * 0.299 + float g * 0.587 + float b * 0.114), int (float r * 0.299 + float g * 0.587 + float b * 0.114))))

  // description:
  // changes each pixel's rgb so that if the existing r,g,b value exceeds a threshold, it will be the max rgb value
  // whereas if the existing r,g,b value is below a threshold, it will completely be the min rgb value

  // breakdown:
  // image passed to higher order function list.map 
  // each row will be passed to a lambda function
  // where each 3-int tuple in the row will be passed to higher order function list.map
  // each value in the tuple will be checked in a helper function (checkPixel) 
  // checks where it is relative to the threshold and will be changed accordingly
  let rec Threshold (width:int) 
                    (height:int)
                    (depth:int)
                    (image:(int*int*int) list list)
                    (threshold:int) = 
    let checkPixel (pix: int) =
      if pix > threshold then 255 else 0
    image |> List.map (fun row -> row |> List.map (fun (r, g, b) ->  (checkPixel r, checkPixel g, checkPixel b)))

  // description:
  // flips an image horizontally

  // breakdown:
  // image is passed to a recursive helper function (flipHorizontal)
  // the image is iterated through row by row
  // each head row will be reversed using a higher order function
  // then the rest of the image will be passed through tail recursion to continue until the end
  let rec FlipHorizontal (width:int)
                         (height:int)
                         (depth:int)
                         (image:(int*int*int) list list) = 
    let rec reverseRows img =
      match img with
      | [] -> []
      | row :: next -> List.rev row :: reverseRows next

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
    let rec transpose lst =
        match lst with
        | [] -> []
        | []::_ -> []
        | _ -> (List.map List.head lst) :: transpose (List.map List.tail lst)
    image |> transpose |> List.map List.rev
    

