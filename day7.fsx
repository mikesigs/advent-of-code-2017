type Tree<'LeafData, 'INodeData> =
    | Empty
    | Leaf of 'LeafData
    | Node of 'INodeData * Tree<'LeafData, 'INodeData> seq

let rec cata fLeaf fNode (tree:Tree<'LeafData, 'INodeData>) =
    let recurse = cata fLeaf fNode
    match tree with
    | Leaf leafInfo -> fLeaf leafInfo
    | Node (nodeInfo, subTrees) ->
        fNode nodeInfo (subTrees |> Seq.map recurse)

let rec fold fLeaf fNode acc (tree:Tree<'LeafData, 'INodeData>) =
    let recurse = fold fLeaf fNode
    match tree with
    | Leaf leafInfo ->
        fLeaf acc leafInfo
    | Node (nodeInfo, subTrees) ->
        let localAcc = fNode acc nodeInfo
        let finalAcc = subTrees |> Seq.fold recurse localAcc
        finalAcc

let node (nodeInfo: 'INodeData) subItems = 
    Node (nodeInfo, subItems)

let find predicate tree =
    let fLeaf (acc: int option) (leafInfo: int) =
        match acc with
        | Some _ -> acc
        | None -> if predicate leafInfo
                  then Some leafInfo
                  else None

    let fNode acc nodeInfo =
        match acc with
        | Some _ -> acc
        | None -> if predicate nodeInfo
                  then Some nodeInfo
                  else None

    fold fLeaf fNode None tree

let rec insertBy predicate value (tree: Tree<int, int>) =
    match tree with
    | Empty -> 
        printfn "Empty"
        Leaf value
    | Node (nodeInfo, subItems) when predicate nodeInfo -> 
        printfn "Node %A -> %A" nodeInfo value
        node nodeInfo (Seq.append [Leaf value] subItems)
    | Node (nodeInfo, subItems) ->
        printfn "Not Node"
        node nodeInfo (Seq.map (fun subItem -> insertBy predicate value subItem) subItems)
    | Leaf leafInfo when predicate leafInfo ->
        printfn "Leaf %A -> %A" leafInfo value
        node leafInfo [Leaf value]
    | _ -> 
        printfn "Else"
        tree

let print (tree: Tree<int,int>) =

    let thd (_,_,x) = x

    let getBranchChar = function
    | true ->  "└── "
    | false -> "├── "

    let getIndentChar = function
    | true ->  "    "
    | false -> "│   "

    let rec recurse state node =
        let isTail, indent, acc = state
        let bc = getBranchChar isTail
        let indentStr = acc + indent + bc

        match node with
        | Leaf value ->
            let str = indentStr + (value |> string)
            isTail, indent, str
        | Node (value, subTrees) ->
            let str = indentStr + (value |> string)
            let childIndent = indent + getIndentChar isTail

            let left, right =
                subTrees
                |> Seq.toList
                |> List.splitAt ((subTrees |> Seq.length) - 1)

            let leftAcc =
                left
                |> Seq.fold recurse (false, childIndent, str)
                |> thd

            let finalAcc =
                right
                |> Seq.fold recurse (true, childIndent, leftAcc)
                |> thd

            isTail, indent, finalAcc

    recurse (true, "\n", "") tree |> thd

let data1 =
    (Node (1,
        [Leaf 10
         Leaf 11
         Leaf 12
         (Node (2,
            [Leaf 20
             (Node (3,
                [Leaf 30]
             ))
             (Node (4,
                [Leaf 40
                 Leaf 41]
             ))
             Leaf 21
             Leaf 22
             Leaf 23
            ]))
         (Node (5,
            [Leaf 50]))
        ]))

let data2 =
    (Node (1,
        [(Node (2,
            [Leaf 4 ; Leaf 5]))
         (Node (3,
            [(Node (6, [Leaf 8]))
             Leaf 7
            ]))
        ]))

data1 |> find ((=) 1)

let isParent p x = p = x

insertBy ((=) 1) 1 Empty 
|> insertBy (isParent 1) 10
|> insertBy (isParent 1) 11
|> insertBy (isParent 1) 2
|> insertBy (isParent 1) 3
|> insertBy (isParent 2) 20
|> insertBy (isParent 2) 21
|> insertBy (isParent 3) 30
|> insertBy (isParent 3) 31
|> insertBy (isParent 11) 110
|> insertBy (isParent 3) 32
|> print

data1 |> print
