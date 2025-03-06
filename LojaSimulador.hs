module LojaSimulador where

import Data.List( find)
import Cliente

--Definição de loja
data Produto = Produto 
    {   nomeProduto :: String,
        valor :: Float
    } 
    deriving(Show, Eq)


data Loja = Loja
    {
        nomeLoja :: String,
        produtos :: [Produto]

    }deriving(Show, Eq)

--Retorna o numero de produtos da loja
numeroProdutos :: [Produto] -> Int
numeroProdutos [] = 0
numeroProdutos (_:xs) = 1 + numeroProdutos xs

--Função de exibição da loja
exibeLoja:: Loja -> IO()
exibeLoja loja = do
    putStrLn $ "Loja: " ++ nomeLoja loja
    putStrLn "Produtos disponíveis:"
    exibeProdutos (produtos loja)
    let totalProdutos = numeroProdutos (produtos loja)
    putStrLn $ "Número total de produtos: " ++ show totalProdutos

--Exibe produtos
exibeProdutos :: [Produto] -> IO()
exibeProdutos [] = return ()  -- Caso base, lista vazia
exibeProdutos (p:ps) = do
    putStrLn $ " - " ++ nomeProduto p ++ " - R$ " ++ show (valor p)
    exibeProdutos ps

--Funcao de compra de produto
compra :: Cliente -> Maybe Produto -> Sistema -> IO Sistema
compra cliente Nothing sistema = do
    putStrLn "Produto nao existe"
    return sistema
compra cliente (Just produto) sistema = 
    case cartao cliente of
        Just cartaoAtual -> do
            if limite cartaoAtual >= valor produto
                then do 
                    -- Atualiza o cartão reduzindo o limite
                    let novoCartao = cartaoAtual { limite = limite cartaoAtual - valor produto }
                    let clienteAtualizado = cliente { cartao = Just novoCartao }
                    -- Atualiza o sistema removendo o cliente antigo e adicionando o atualizado
                    let sistemaNovo = clienteAtualizado : filter (\c -> nome c /= nome cliente) sistema
                    putStrLn "Compra realizada com sucesso!"
                    putStrLn $ "Valor da compra: R$ " ++ show (valor produto)
                    putStrLn $ "Novo limite do cartao: R$ " ++ show (limite novoCartao)
                    return sistemaNovo
                else do
                    putStrLn "Limite insuficiente no cartao!"
                    putStrLn $ "O produto custa R$ " ++ show (valor produto) 
                    putStrLn $ "Seu limite atual eh R$ " ++ show (limite cartaoAtual)
                    return sistema
        Nothing -> do
            putStrLn "Cliente nao possui cartao cadastrado!"
            return sistema

--Retornar produto
retornaProduto :: Loja -> String -> Maybe Produto
retornaProduto loja nome = 
    buscarProduto(produtos loja) nome
    where 
        buscarProduto :: [Produto] -> String -> Maybe Produto
        buscarProduto [] _ = Nothing
        buscarProduto (p:ps) nome   
            | nomeProduto p == nome = Just p
            | otherwise = buscarProduto ps nome

--Loja exemplo
exemploLojaProdutos :: Loja 
exemploLojaProdutos = Loja 
    {   nomeLoja = "Loja de Roupa", 
        produtos = [produto1, produto2, produto3]
    }
    where
        produto1 = Produto { nomeProduto = "Camiseta", valor = 39.90}
        produto2 = Produto { nomeProduto = "Cueca", valor = 60}
        produto3 = Produto { nomeProduto = "Meia", valor = 2.50}