-- Main.hs
module Main where

import Cliente
import LojaSimulador ( exemploLojaProdutos, exibeLoja, Loja )
import Data.List (find)

-- Menu inicial (login ou cadastro).
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
        Just cliente -> menuAposLogin cliente sistema
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

-- Menu principal do cliente (apos o login).
menuAposLogin :: Cliente -> Sistema -> IO ()
menuAposLogin cliente sistema = do
  putStrLn "\n--- Menu Principal ---"
  putStrLn "1. Opcoes"
  putStrLn "2. Acessar Loja"
  putStrLn "3. Cadastrar Cartao de Credito"
  putStrLn "4. Contratar Seguro para o Cartao"
  putStrLn "5. Fazer Pix usando seu Cartao de Credito"
  putStrLn "6. Pagar contas usando seu Cartao de Credito"
  putStrLn "7. Sair"
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
    "3" -> do -- Cadastra um  novo cartão de credito para o cliente.
      putStrLn "Cadastro de Cartao de Credito"
      putStrLn "Numero do Cartao: "
      numero <- getLine -- Deve obrigatoriamente ter 16 carateres.
      putStrLn "Titular: "
      titular <- getLine -- Não pode ser vazio.
      putStrLn "Validade (MM/AA): "
      validade <- getLine -- Deve obrigatoriamente ter 5 caracteres (formato MM/AA).
      putStrLn "CVV: "
      cvv <- getLine -- Deve obrigatoriamente ter 3 caracteres (formato de um codigo de seguranca).
      putStrLn "Limite: "
      limite <- getLine
      if validarDadosCartao numero titular validade cvv
        then do
          let limiteFloat = (read limite :: Float)
          let cartaoNovo = CartaoCredito numero titular validade cvv limiteFloat False -- Cria um cartao novo com todos os dados.
          let clienteComCartao = cadastrarCartao cliente cartaoNovo -- Atribui o cartao ao cliente.
          let sistemaNovo = clienteComCartao : filter (/= cliente) sistema
          putStrLn "Cartao de Credito cadastrado com sucesso!"
          putStrLn $ "Limite inicial do cartao: R$ " ++ show limiteFloat
          menuAposLogin clienteComCartao sistemaNovo
        else do
          putStrLn "Dados do Cartao invalidos! Tente novamente."
          menuAposLogin cliente sistema
    "4" -> do
      case cartao cliente of
        Just cartaoAtual -> do
          if limite cartaoAtual >= 50.0 -- Verifica se o cliente tem limite suficiente para contratar o seguro.
            then do
              let novoCartao = cartaoAtual { seguro = True, limite = limite cartaoAtual - 50.0 } -- Atualiza o cartao com o seguro contratado.
              let clienteComSeguro = cliente { cartao = Just novoCartao } -- Atribui o novo cartao ao cliente.
              let sistemaNovo = clienteComSeguro : filter (/= cliente) sistema
              putStrLn "Seguro contratado com sucesso!"
              putStrLn "Valor do seguro: R$ 50.00"
              putStrLn $ "Novo limite do cartao: R$ " ++ show (limite novoCartao)
              menuAposLogin clienteComSeguro sistemaNovo
            else do -- Caso o cliente não tenha limite suficiente para contratar o seguro.
              putStrLn "Limite insuficiente para contratar o seguro!"
              putStrLn $ "O seguro custa R$ 50,00 e seu limite atual eh R$ " ++ show (limite cartaoAtual)
              menuAposLogin cliente sistema
        Nothing -> do
          putStrLn "Cliente nao possui cartao cadastrado!"
          menuAposLogin cliente sistema
    "5" -> do 
      case cartao cliente of
        Just cartaoAtual -> do
          putStrLn "Digite o nome do usuario que recebera o PIX:"
          nomeRecebedor <- getLine -- Procura o usuário recebedor no sistema
          case find (\c -> nome c == nomeRecebedor) sistema of
            Just recebedor -> do
              putStrLn "Digite o valor do PIX: "
              valorStr <- getLine
              let valor = read valorStr :: Float
              if valor <= limite cartaoAtual
                then do 
                  let novoCartao = cartaoAtual { limite = limite cartaoAtual - valor } -- Atualiza o cartão do pagador (reduz o limite)
                  let clienteAtualizado = cliente { cartao = Just novoCartao }
                  let recebedorAtualizado = recebedor { saldoCliente = saldoCliente recebedor + valor } -- Atualiza o saldo do recebedor
                  let sistemaNovo = recebedorAtualizado : clienteAtualizado : filter (\c -> nome c /= nome cliente && nome c /= nome recebedor) sistema -- Atualiza o sistema com ambos os clientes modificados
                  putStrLn "PIX realizado com sucesso!"
                  putStrLn $ "Novo limite do cartao: R$ " ++ show (limite novoCartao)
                  putStrLn $ "Valor transferido para " ++ nomeRecebedor ++ ": R$ " ++ show valor
                  menuAposLogin clienteAtualizado sistemaNovo
                else do
                  putStrLn "Limite insuficiente no cartao!"
                  menuAposLogin cliente sistema
            Nothing -> do
              putStrLn "Usuario nao encontrado no sistema!"
              menuAposLogin cliente sistema
        Nothing -> do
          putStrLn "Cliente nao possui cartao cadastrado!"
          menuAposLogin cliente sistema
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
              menuAposLogin clienteAtualizado sistemaNovo
            else do
              putStrLn "Limite insuficiente no cartao!"
              menuAposLogin cliente sistema
        Nothing -> do
          putStrLn "Cliente nao possui cartao cadastrado!"
          menuAposLogin cliente sistema

    "7" -> menuInicial sistema
    _ -> do
      putStrLn "Opcao invalida!"
      menuInicial sistema

-- Menu de opcoes do cliente.
menuPrincipal :: Cliente -> Sistema -> IO Sistema
menuPrincipal cliente sistema = do
  putStrLn "\n--- Opcoes ---"
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


-- Menu de compras.
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