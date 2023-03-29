data Arvore = Folha Int | Nodo Int Arvore Arvore deriving (Eq,Show)

arv1 :: Arvore
arv1 = Nodo 3 (Folha 1) (Nodo 2 (Folha 4) (Folha 5))

multArvore:: Int -> Arvore -> Arvore

multArvore n (Folha x) = Folha (n*x)
multArvore n (Nodo x a1 a2) = Nodo (n*x) (multArvore n a1) (multArvore n a2)

contaFolhas :: Arvore -> Int

contaFolhas (Folha x) = 1
contaFolhas (Nodo x a1 a2) = contaFolhas a1 + contaFolhas a2

contaNodos :: Arvore -> Int

contaNodos (Folha x) = 0
contaNodos (Nodo x a1 a2) = 1 + contaNodos a1 + contaNodos a2

quantasVezes :: Int -> Arvore -> Int

quantasVezes n (Folha x) = if n == x then 1 else 0
quantasVezes n (Nodo x a1 a2) = if n == x then 1 else quantasVezes n a1 + quantasVezes n a2

maxArvore :: Arvore -> Int

maxArvore (Folha x) = x
maxArvore (Nodo x a1 a2) = max x (max (maxArvore a1) (maxArvore a2))

refleteArvore :: Arvore -> Arvore

refleteArvore (Folha x) = Folha x
refleteArvore (Nodo x a1 a2) = Nodo x (refleteArvore a2) (refleteArvore a1)

