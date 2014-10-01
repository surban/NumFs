

module NDArray =    

    type Order =
        | C
        | Fortran

    type Slice =
        | Element of int
        | Range of int * int
        | All
        | NewAxis
        | Fill

    let fortranStride shape =
        shape |> Seq.fold (fun strds elems -> (List.head strds * elems) :: strds) [1] 
              |> List.tail
    let cStride shape = shape |> List.rev |> fortranStride
    let mkStride order =
        match order with
            | C -> cStride 
            | Fortran -> fortranStride

    let size shape = shape |> Seq.reduce (*)
    let ndims shape = Seq.length shape

    let rec allIndices shape = 
        seq {
            if List.isEmpty shape then
                yield []
            else
                let mySize = List.head shape
                for i in 0 .. mySize - 1 do
                    for s in allIndices (List.tail shape) do
                        yield i :: s
        }
    
    let rec sliceData (slice: Slice list) (stride: int list) (shape: int list) (offset: int) =
        if List.isEmpty slice then 
            if List.isEmpty shape then
                ([], [], offset)
            else
                failwith "slice does not match number of dimensions"
        else
            let myStride() = List.head stride
            let myShape() = List.head shape              
            let checkRange idx =
                if idx < 0 then failwith "element index may not be negative"
                if idx >= myShape() then failwithf "element index %d out of range <%d" idx (myShape())

            match List.head slice with 
                | Element(i) -> 
                    checkRange i
                    let (tailShape, tailStride, tailOffset) = sliceData (List.tail slice) (List.tail stride) (List.tail shape) offset
                    (tailShape, tailStride, tailOffset + myStride() * i)
                | Range(start, stop) ->
                    checkRange start; checkRange stop
                    if start > stop then failwith "range start %d must not be larger than range end %d" start stop
                    let (tailShape, tailStride, tailOffset) = sliceData (List.tail slice) (List.tail stride) (List.tail shape) offset
                    ((stop - start)::tailShape, myStride()::tailStride, tailOffset + myStride() * start)
                | All ->
                    let (tailShape, tailStride, tailOffset) = sliceData (List.tail slice) (List.tail stride) (List.tail shape) offset
                    (myShape()::tailShape, myStride()::tailStride, tailOffset)
                | NewAxis ->
                    let (tailShape, tailStride, tailOffset) = sliceData (List.tail slice) stride shape offset
                    (1::tailShape, 1::tailStride, tailOffset)
                | Fill ->
                    let restSlice = 
                        List.tail slice 
                        |> List.filter (fun s -> match s with
                                                    | Element(_) -> true
                                                    | Range(_, _) -> true
                                                    | All -> true
                                                    | NewAxis -> false
                                                    | Fill -> failwithf "only one Fill is allowed per slice")
                        |> List.length
                    let restShape = List.length shape
                    let filler = List.init (restShape - restSlice) (fun _ -> All) 
                    sliceData (List.concat [filler; List.tail slice]) stride shape offset

    let rec broadcastData bcShape shape stride =
        if List.isEmpty bcShape then
            if List.isEmpty shape then
                ([], [])
            else
                failwith "new shape must have same number of dimensions"
        else
            let (tailShape, tailStride) = broadcastData (List.tail bcShape) (List.tail shape) (List.tail stride)

            let s = List.head stride
            match (List.head bcShape, List.head shape) with
                | (n, m) when n=m -> (n::tailShape, s::tailStride)
                | (n, 1) -> (n::tailShape, 0::tailStride)
                | _ -> failwith "can only broadcast dimensions that have exactly one element" 


    type View(dataIn: float array, offsetIn: int, strideIn: int list, shapeIn: int list, readOnlyIn: bool, orderIn: Order) =
        
        let elem pos = 
            if ndims pos <> ndims shapeIn then
                failwithf "index %A has different dimensionality than shape %A" pos shapeIn
            if not (Seq.forall2 (<) pos shapeIn) then 
                failwithf "index %A out of range %A" pos shapeIn
            if Seq.isEmpty pos then
                offsetIn
            else
                offsetIn + (Seq.map2 (*) pos strideIn |> Seq.reduce (+))

        let broadcastedIn =
            List.exists (fun s -> s=0) strideIn
        let singletonIn =
            ndims shapeIn = 0 || size shapeIn = 1

        let checkWriteable() =
            if readOnlyIn then failwith "view is read-only"
            if broadcastedIn then failwith "view is broadcasted"
        let checkSingleton() =
            if not singletonIn then failwith "view is not a singleton"

            

        member this.shape = shapeIn
        member this.offset = offsetIn
        member this.stride = strideIn
        member this.size = size shapeIn
        member this.ndims = ndims shapeIn
        member this.readOnly = readOnlyIn
        member this.order = orderIn
        member this.broadcasted = broadcastedIn
        member this.singleton = singletonIn
        member internal this.data = dataIn

        member this.readOnlyView() =
            View(dataIn, offsetIn, strideIn, shapeIn, true, orderIn)

        // single element access ------------------------------------------------------------
        member this.getElement pos = dataIn.[elem pos]
        member this.setElement pos value = 
            checkWriteable()
            dataIn.[elem pos] <- value

        static member (.@) (view: View, pos: int list) =
            view.getElement pos
        // ----------------------------------------------------------------------------------

        // singleton value access -----------------------------------------------------------
        member this.getValue =
            checkSingleton()
            dataIn.[offsetIn]
        member this.setValue value =
            checkSingleton(); checkWriteable()
            dataIn.[offsetIn] <- value

//        static member (~%%) (view: View) =
//            view.getValue 
        static member (<--) (view: View, v: float) =
            view.setValue v
        // ----------------------------------------------------------------------------------

        // slicing operators ----------------------------------------------------------------
        static member (.|) (view: View, s: Slice list) =
            let (sShape, sStride, sOffset) = sliceData s view.stride view.shape view.offset
            View(view.data, sOffset, sStride, sShape, view.readOnly, view.order)         

        static member (.|) (view: View, pos: int list) =
            let s = List.map (fun i -> Element(i)) pos
            view.|s
        // ----------------------------------------------------------------------------------

        override this.ToString() = 
            let rec asString (v: View) =
                if v.singleton then
                     sprintf "%3.3f" (v.getValue)
                else
                    let mutable contents = "["
                    for i in 0 .. v.shape.Head-1 do
                        contents <- contents + asString (v.|[Element(i); Fill])
                        if i < v.shape.Head-1 then 
                            contents <- contents + "; "
                    contents + "]"
            asString this         


    let slice (s: Slice list) (view: View)  =
        view.|s

    // shape handling -------------------------------------------------------------------
    let reshape shape (view: View) =
        if view.broadcasted then failwith "cannot reshape broadcasted view"
        if (size shape) <> view.size then failwithf "new size %A is different from current size %A"  (size shape) view.size
        new View(view.data, view.offset, (mkStride view.order shape), shape, view.readOnly, view.order)

    let padShape n (view: View) =
        let fill = List.init n (fun _ -> 1)
        new View(view.data, view.offset, List.concat [view.stride; fill], 
                 List.concat [view.shape; fill], view.readOnly, view.order)                

    let checkShapeMatch (v1: View) (v2: View) =
        if v1.shape <> v2.shape then failwithf "array shape %A is different from %A" v1.shape v2.shape
    // ----------------------------------------------------------------------------------

    // broadcasting ---------------------------------------------------------------------
    let broadcastToShape shape (view: View) =
        let (bShape, bStride) = broadcastData shape view.shape view.stride
        new View(view.data, view.offset, bStride, bShape, view.readOnly, view.order)

    let broadcastSecond (v1: View) (v2: View) =
        if v1.shape = v2.shape then 
            v2
        else
            let mutable nv2 = v2

            // pad shape of v2 with 1s if necessary
            if v1.ndims <> v2.ndims then
                if v2.shape < v1.shape then
                    nv2 <- padShape (v1.ndims - nv2.ndims) nv2
                else
                    failwithf "shape %A has more dimensions than target %A" v2.shape v1.shape

            // broadcast 
            let bShape = List.map2 (fun d1 d2 -> 
                                        match (d1, d2) with
                                            | (d1, d2) when d1 = d2 -> d1
                                            | (d1, 1) -> d1
                                            | _ -> failwithf "cannot broadcast view of shape %A to shape %A" v2.shape v1.shape)
                            v1.shape nv2.shape
            if bShape <> nv2.shape then nv2 <- broadcastToShape bShape nv2
            nv2          

    let broadcastBoth (v1: View) (v2: View) =
        if v1.shape = v2.shape then 
            (v1, v2)
        else
            let mutable nv1 = v1
            let mutable nv2 = v2

            // pad shape of array with less dimensions with 1s
            if nv1.ndims <> nv2.ndims then
                if nv1.ndims > nv2.ndims then 
                    nv2 <- padShape (nv1.ndims - nv2.ndims) nv2
                else
                    nv1 <- padShape (nv2.ndims - nv1.ndims) nv1

            // broadcast 
            let bShape = List.map2 (fun d1 d2 -> 
                                        match (d1, d2) with
                                            | (d1, d2) when d1 = d2 -> d1
                                            | (d1, 1) -> d1
                                            | (1, d2) -> d2
                                            | _ -> failwithf "cannot broadcast views of shape %A and %A to same shape" v1.shape v2.shape)
                            nv1.shape nv2.shape
            if bShape <> nv1.shape then nv1 <- broadcastToShape bShape nv1
            if bShape <> nv2.shape then nv2 <- broadcastToShape bShape nv2

            (nv1, nv2)
                    
    // ----------------------------------------------------------------------------------                    
                                                                  
    let zeros shape =
        View(Array.zeroCreate (size shape), 0, cStride shape, shape, false, C)

    let ones shape =
        View(Array.init (size shape) (fun _ -> 1.0), 0, cStride shape, shape, false, C)
      
    let map2 f v1 v2 =
        checkShapeMatch v1 v2
        let vo = zeros v1.shape
        for idx in allIndices v1.shape do
            vo.setElement idx (f (v1.getElement idx) (v2.getElement idx))
        vo
        
    let mapb2 f v1 v2 =
        let cv1, cv2 = broadcastBoth v1 v2
        map2 f cv1 cv2

    let iter2 f v1 v2 =
        checkShapeMatch v1 v2
        for idx in allIndices v1.shape do
            f (v1.getElement idx) (v2.getElement idx)

    let iterb2 f v1 v2 =
        let cv1, cv2 = broadcastBoth v1 v2
        iter2 f cv1 cv2 
         
    (*
    let inline (+) (v1: View) (v2: View) =
        mapb2 (+) v1 v2
    *)

    type View with
        static member (<--) (view: View, view2: View) =
            let bView2 = broadcastSecond view view2
            for idx in allIndices view.shape do
                view.setElement idx (bView2.@idx)

            
            
                     

open NDArray

let a1 = NDArray.zeros [2; 3; 4]
printfn "A1 shape: %A" a1.shape
printfn "A1 stride: %A" a1.stride
printfn "A1 offset: %A" a1.offset

//let a2 = a1.slice [NDArray.NewAxis; NDArray.Element 0; NDArray.Range(1, 2); NDArray.NewAxis; NDArray.Range(1,3)]
//let a2 = NDArray.slice [NDArray.All; NDArray.Fill; NDArray.Element(1)] a1
let a2 = a1.|[All; Fill; Element(1)] 
printfn "A2 shape: %A" a2.shape
printfn "A2 stride: %A" a2.stride
printfn "A2 offset: %A" a2.offset

let (a1b, a2b) = NDArray.broadcastBoth a1 a2

a1.|[0;0;1] <-- 123.0

printfn "a1.ToString = %s" (a1.ToString())
printfn "a2.ToString = %s" (a2.ToString())


