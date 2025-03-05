-- Main.hs
module Main where

import Cliente
import LojaSimulador ( exemploLojaProdutos, exibeLoja, Loja ) 

-- Menu principal (acessivel apenas apos login)
menuPrincipal :: Cliente -> Sistema -> IO Sistema
menuPrincipal cliente sistema = do
  putStrLn "\n--- Menu Principal ---"
  putStrLn "1. Listar Clientes"
  putStrLn "2. Atualizar Dados Cadastrais"
  putStrLn "3. Voltar"
  putStr "Escolha uma opcao: "
  opcao <- getLine
  case opcao of
    "1" -> do
      listarClientes sistema
      menuPrincipal cliente sistema
    "2" -> do
      novoSistema <- atualizarDadosCliente cliente sistema
      menuPrincipal cliente novoSistema
    "3" -> return sistema
    _ -> do
      putStrLn "Opcao invalida! Tente novamente."
      menuPrincipal cliente sistema

-- Menu inicial (login ou cadastro)
menuInicial :: Sistema -> IO ()
menuInicial sistema = do
  putStrLn "\n--- Menu Inicial ---"
  putStrLn "1. Login"
  putStrLn "2. Cadastrar Cliente"
  putStrLn "3. Sair"
  putStr "Escolha uma opcao: "
  opcao <- getLine
  case opcao of
    "1" -> do
      maybeCliente <- autenticarCliente sistema
      case maybeCliente of
        Just cliente -> exibirMenuAposLogin cliente sistema
        Nothing -> do
          putStrLn "Falha na autenticacao! Tente novamente."
          menuInicial sistema
    "2" -> do
      novoSistema <- cadastrarCliente sistema
      menuInicial novoSistema
    "3" -> putStrLn "Saindo..."
    _ -> do
      putStrLn "Opcao invalida! Tente novamente."
      menuInicial sistema

-- Função auxiliar para exibir o menu após login
exibirMenuAposLogin :: Cliente -> Sistema -> IO ()
exibirMenuAposLogin cliente sistema = do
  putStrLn "\n--- Opcoes ---"
  putStrLn "1. Dados Cadastrais"
  putStrLn "2. Acessar Loja"
  putStrLn "3. Cadastrar Cartao de Credito"
  putStrLn "4. Contratar Seguro para o Cartao"
  putStrLn "5. Fazer Pix usando seu Cartao de Credito"
  putStrLn "6. Pagar contas usando seu Cartao de Credito"
  putStr "Escolha uma opcao: "
  
  opcao2 <- getLine

  case opcao2 of
    "1" -> do
      novoSistema <- menuPrincipal cliente sistema
      menuInicial novoSistema
    "2" -> do
      let lojaEx = exemploLojaProdutos
      novoSistema <- menuCompras cliente sistema lojaEx
      menuInicial novoSistema
    "3" -> do
      putStrLn "Cadastro de Cartao de Credito"
      putStrLn "Numero do Cartao: "
      numero <- getLine
      putStrLn "Titular: "
      titular <- getLine
      putStrLn "Validade (MM/AA): "
      validade <- getLine
      putStrLn "CVV: "
      cvv <- getLine
      if validarDadosCartao numero titular validade cvv
        then do
          let cartaoNovo = CartaoCredito numero titular validade cvv 1000.0 False
          let clienteComCartao = cadastrarCartao cliente cartaoNovo
          let sistemaNovo = clienteComCartao : filter (/= cliente) sistema
          putStrLn "Cartao de Credito cadastrado com sucesso!"
          putStrLn $ "Limite inicial do cartao: R$ " ++ show (limite cartaoNovo)
          exibirMenuAposLogin clienteComCartao sistemaNovo
        else do
          putStrLn "Dados do Cartao invalidos! Tente novamente."
          exibirMenuAposLogin cliente sistema
    "4" -> do
      case cartao cliente of
        Just cartaoAtual -> do
          let novoCartao = cartaoAtual { seguro = True }
          let clienteComSeguro = cliente { cartao = Just novoCartao }
          let sistemaNovo = clienteComSeguro : filter (/= cliente) sistema
          putStrLn "Seguro contratado com sucesso!"
          -- Volta para o menu após login
          exibirMenuAposLogin clienteComSeguro sistemaNovo
        Nothing -> do
          putStrLn "Cliente nao possui cartao cadastrado!"
          exibirMenuAposLogin cliente sistema
    "5" -> do
      case cartao cliente of
        Just cartaoAtual -> do
          putStrLn "Digite o valor do PIX: "
          valorStr <- getLine
          let valor = read valorStr :: Float
          if valor <= limite cartaoAtual
            then do
              let novoCartao = cartaoAtual { limite = limite cartaoAtual - valor }
              let clienteAtualizado = cliente { cartao = Just novoCartao }
              let sistemaNovo = clienteAtualizado : filter (/= cliente) sistema
              putStrLn "PIX realizado com sucesso!"
              putStrLn $ "Novo limite do cartao: R$ " ++ show (limite novoCartao)
              exibirMenuAposLogin clienteAtualizado sistemaNovo
            else do
              putStrLn "Limite insuficiente no cartao!"
              exibirMenuAposLogin cliente sistema
        Nothing -> do
          putStrLn "Cliente nao possui cartao cadastrado!"
          exibirMenuAposLogin cliente sistema
    "6" -> do
      case cartao cliente of
        Just cartaoAtual -> do
          putStrLn "Insira o codigo de barras do boleto: "
          codigoBarras <- getLine
          putStrLn "Insira o valor do boleto: "
          valorStr <- getLine
          let valor = read valorStr :: Float
          if valor <= limite cartaoAtual
            then do
              let novoCartao = cartaoAtual { limite = limite cartaoAtual - valor }
              let clienteAtualizado = cliente { cartao = Just novoCartao }
              let sistemaNovo = clienteAtualizado : filter (/= cliente) sistema
              putStrLn "Conta paga com sucesso!"
              putStrLn $ "Novo limite do cartao: R$ " ++ show (limite novoCartao)
              exibirMenuAposLogin clienteAtualizado sistemaNovo
            else do
              putStrLn "Limite insuficiente no cartao!"
              exibirMenuAposLogin cliente sistema
        Nothing -> do
          putStrLn "Cliente nao possui cartao cadastrado!"
          exibirMenuAposLogin cliente sistema
    _ -> do
      putStrLn "Opcao invalida!"
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
        menuCompras cliente sistema loja
    "3" -> do
      return sistema

main :: IO ()
main = do
  putStrLn "Bem-vindo ao Sistema de Gerenciamento de Clientes!"
  menuInicial []