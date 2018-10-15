data Tree elem = Empty
               | Node (Tree elem) elem (Tree elem)

Functor Tree where
    map func Empty = Empty
    map func (Node treel ele treer) =
        Node (map func treel) (func ele) (map func treer)

Foldable Tree where
  foldr func acc Empty = acc
  foldr func acc (Node left e right) =
      let leftfold = foldr func acc left
          rightfold = foldr func leftfold right in
          func e rightfold

testTree : Tree Integer
testTree = (Node (Node (Node Empty 1 Empty) 2 (Node Empty 3 Empty))
                 4
                 (Node (Node Empty 5 Empty) 6 (Node Empty 7 Empty)))
