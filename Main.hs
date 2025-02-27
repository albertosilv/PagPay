-- Main.hs
module Main where

import Cliente

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
          novoSistema <- menuPrincipal cliente sistema
          menuInicial novoSistema
        Nothing -> menuInicial sistema
    "2" -> do
      novoSistema <- cadastrarCliente sistema
      menuInicial novoSistema
    "3" -> putStrLn "Saindo..."
    _ -> do
      putStrLn "Opção inválida! Tente novamente."
      menuInicial sistema

-- Função principal
main :: IO ()
main = do
  putStrLn "Bem-vindo ao Sistema de Gerenciamento de Clientes!"
  menuInicial []