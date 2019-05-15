module Heap
    open Microsoft.FSharp.Collections

    type MutableHeap<'T> = { array: array<'T option>; compare: ('T -> 'T -> bool); size: int }

    let private NoneArrayOfSize capacity =
        Array.create capacity None;

    let EmptyHeap<'T> compare =
        { array = NoneArrayOfSize  4; compare = compare; size = 0}

    let MinHeap<'T> =
        EmptyHeap (fun left right -> left < right)

    let MaxHeap<'T> =
        EmptyHeap (fun left right -> left > right)

    let private Swap (array: array<'T option>) x y =
        let temp = array.[x]
        Array.set array x array.[y]
        Array.set array y temp
        array

    let private Parent index =
        (index - 1) / 2

    let private SwapWithParent heap index =
        let parentIndex = Parent index
        let _ = Swap heap.array index parentIndex
        heap

    let private LeftIsHeapier heap index1 index2 =
        heap.compare heap.array.[index1].Value heap.array.[index2].Value

    let private SwapOrDont heap parentIndex childIndex =
        if LeftIsHeapier heap parentIndex childIndex then
            (heap, None)
        else (SwapWithParent heap childIndex, Some childIndex)

    let private SwapOrSwapOrDont heap parentIndex =
        let child1 = parentIndex * 2
        let child2 = child1 + 1
        if child1 >= heap.size then
            (heap, None)
        elif child2 >= heap.size || LeftIsHeapier heap child1 child2 then
            SwapOrDont heap parentIndex child1
        else SwapOrDont heap parentIndex child2

    let rec private HeapifyDown heap index =
        let heap1, newIndex = SwapOrSwapOrDont heap index
        if newIndex.IsNone then
            heap1
        else HeapifyDown heap newIndex.Value

    let rec private HeapifyUp heap index =
        let parent = Parent index
        if LeftIsHeapier heap parent index then
            heap
        else SwapWithParent heap index |> HeapifyUp <| Parent index

    let Capacity heap =
        heap.array.Length

    let private GrowCapacity heap =
        { array = Array.concat [heap.array ; NoneArrayOfSize heap.array.Length]; compare = heap.compare; size = heap.size }

    let private ShrinkCapacity heap =
        let newCapacity = heap.array.Length / 2;
        { array = heap.array.[..newCapacity]; compare = heap.compare; size = heap.size }

    let private ResizeIfNeeded heap =
        if heap.size >= heap.array.Length * 3 / 2 then GrowCapacity heap
        elif heap.size < heap.array.Length / 4 then ShrinkCapacity heap
        else heap

    let Push heap item = 
        Array.set heap.array heap.size (Some item)
        {
            array = heap.array;
            compare = heap.compare;
            size = heap.size + 1;
        } |> ResizeIfNeeded |> HeapifyUp <| heap.size

    let Peak heap =
        heap.array.[0].Value

    let IsEmpty heap =
        heap.size = 0

    let Pop heap =
        let returnValue = heap.array.[0].Value
        let array = Swap heap.array 0 (heap.size - 1)
        Array.set array (heap.size - 1) None
        let newHeap = 
            { array = array;
              compare = heap.compare
              size = heap.size - 1 } |> HeapifyDown <| 0

        (returnValue, newHeap)
