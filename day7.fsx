type Tree<'LeafData, 'INodeData> =
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

let insert parent child tree =
    let fLeaf (acc: Tree<int,int> option) (leafInfo: int) =
        match acc with
        | None -> Node (leafInfo, [])
        | Some _ ->
            if leafInfo <> parent then
                Leaf leafInfo
            else
                Node (leafInfo, [Leaf child])
        |> Some

    let fNode acc nodeInfo =
        match acc with
        | None -> Node (nodeInfo, [])
        | Some x -> x
        |> Some

    fold fLeaf fNode None tree

let thd (_,_,x) = x

let getBranchChar = function
| true ->  "└── "
| false -> "├── "

let getIndentChar = function
| true ->  "    "
| false -> "│   "

let print (tree: Tree<int,int>) =
    let rec recurse state node =
        let isTail, indent, acc = state
        let bc = getBranchChar isTail
        let indentStr = acc + indent + bc

        match node with
        | Leaf x ->
            let str = indentStr + (x |> string)
            isTail, indent, str
        | Node (x, subTrees) ->
            let str = indentStr + (x |> string)
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

let data =
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

let data1 =
    (Node (1,
        [(Node (2,
            [Leaf 4 ; Leaf 5]))
         (Node (3,
            [(Node (6, [Leaf 8]))
             Leaf 7
            ]))
        ]))

data |> find ((=) 1)
data |> insert 13 99
data1 |> print
data |> print