-- Main.hs
module Main where

import Cliente
import LojaSimulador 

-- Menu principal (acessível apenas após login)
menuPrincipal :: Cliente -> Sistema -> IO Sistema
menuPrincipal cliente sistema = do
  putStrLn "\n--- Menu Principal ---"
  putStrLn "1. Listar Clientes"
  putStrLn "2. Atualizar Dados Cadastrais"
  putStrLn "3. Logout"
  putStr "Escolha uma opção: "
  opcao <- getLine
  case opcao of
    "1" -> do
      listarClientes sistema
      menuPrincipal cliente sistema
    "2" -> do
      novoSistema <- atualizarDadosCliente cliente sistema
      menuPrincipal cliente novoSistema
    "3" -> do
      putStrLn $ "Logout realizado. Até logo, " ++ nome cliente ++ "!"
      return sistema
    _ -> do
      putStrLn "Opção inválida! Tente novamente."
      menuPrincipal cliente sistema

-- Menu inicial (login ou cadastro)
menuInicial :: Sistema -> IO ()
menuInicial sistema = do
  putStrLn "\n--- Menu Inicial ---"
  putStrLn "1. Login"
  putStrLn "2. Cadastrar Cliente"
  putStrLn "3. Sair"
  putStr "Escolha uma opção: "
  opcao <- getLine
  case opcao of
    "1" -> do
      maybeCliente <- autenticarCliente sistema
      case maybeCliente of
        Just cliente -> do
          putStrLn "\n--- Opções ---"
          putStrLn "1. Dados Cadastrais"
          putStrLn "2. Acessar Loja"
          putStrLn "Escolha uma opção: "
          
          opcao2 <- getLine

          case opcao2 of
            "1" -> do
              novoSistema <- menuPrincipal cliente sistema
              menuInicial novoSistema
            "2" -> do
              let lojaEx = exemploLojaProdutos --Exemplo de loja que é carregado automaticamente
              novoSistema <- menuCompras cliente sistema lojaEx
              menuInicial novoSistema
        Nothing -> menuInicial sistema
    "2" -> do
      novoSistema <- cadastrarCliente sistema
      menuInicial novoSistema
    "3" -> putStrLn "Saindo..."
    _ -> do
      putStrLn "Opção inválida! Tente novamente."
      menuInicial sistema


menuCompras :: Cliente -> Sistema -> Loja -> IO Sistema
menuCompras cliente sistema loja = do
  putStrLn "\n--- Lojas ---"
  putStrLn "1. Visualizar Loja e Produtos"
  putStrLn "2. Comprar"
  putStrLn "3. Voltar"
  
  opcao <- getLine
  case opcao of
    "1" -> do
      exibeLoja loja
      menuCompras cliente sistema loja
    "2" -> do
        putStrLn "Escreva o nome do Produto que você quer comprar"
        produto <- getLine
        compra cliente (retornaProduto loja produto) sistema
        menuCompras cliente sistema loja
    "3" -> do
      return sistema


-- Função principal
main :: IO ()
main = do
  putStrLn "Bem-vindo ao Sistema de Gerenciamento de Clientes!"
  menuInicial []