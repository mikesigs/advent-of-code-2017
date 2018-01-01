type Tree<'LeafData, 'INodeData> =
    | Leaf of 'LeafData
    | Node of 'INodeData * Tree<'LeafData, 'INodeData> seq

type Day7Tree = Tree<int, int>

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

let data = (Node (13, [Leaf 12; Leaf 11; Leaf 10; (Node (9, [Leaf 7; (Node (1, [Leaf 3]))]))]))
let predicate x y = x = y

data |> find ((=) 1)

insert 13 99 data
