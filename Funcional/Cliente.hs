-- Cliente.hs
module Cliente where

import Data.List (find)
import Text.XHtml (input)
import Text.Read (Lexeme(String))

-- Definição do tipo de dados para representar um cliente
data Cliente = Cliente
  { nome :: String,
    email :: String,
    senha :: String,
    dataNascimento :: String,
    cpf :: String,
    endereco :: String,
    telefone :: String,
    saldoCliente :: Float,
    cartao :: Maybe CartaoCredito
  }
  deriving (Show, Eq)

-- Definição do tipo de dados para representar um cartão de crédito de um cliente
data CartaoCredito = CartaoCredito 
  { numeroCartao :: String,
    nomeTitular :: String,
    dataValidade :: String,
    cvv :: String,
    limite :: Float,
    seguro :: Bool
  } deriving (Show, Eq)

-- Tipo de dados para representar o sistema de gerenciamento de clientes
type Sistema = [Cliente]

-- Função para cadastrar um novo cliente.
cadastrarCliente :: Sistema -> IO Sistema
cadastrarCliente sistema = do
  putStrLn "Cadastro de Cliente"
  putStrLn "Nome: "
  nome <- getLine
  putStrLn "Email: "
  email <- getLine
  putStrLn "Senha: "
  senha <- getLine
  putStrLn "Data de Nascimento (DD/MM/AAAA): "
  dataNascimento <- getLine
  putStrLn "CPF: "
  cpf <- getLine
  putStrLn "Endereço: "
  endereco <- getLine
  putStrLn "Telefone: "
  telefone <- getLine
  putStrLn "Faça uma recarga de saldo:"
  recarga <- getLine 
  let novoCliente = Cliente {nome = nome, email = email, senha = senha, dataNascimento = dataNascimento, cpf = cpf, endereco = endereco, telefone = telefone, saldoCliente = read recarga :: Float, cartao = Nothing}
  putStrLn "Cliente cadastrado com sucesso!"
  return (novoCliente : sistema)

-- Função para cadastrar um cartão.
cadastrarCartao :: Cliente -> CartaoCredito -> Cliente
cadastrarCartao cliente cartao = cliente {cartao = Just cartao}

-- Função para validar dados do cartão.
validarDadosCartao :: String -> String -> String -> String -> Bool 
validarDadosCartao numero titular validade cvv = 
    length numero == 16 && -- verifica se tem 16 dígitos
    length cvv == 3 && -- verifica se CVV tem 3 dígitos
    not (null titular) && -- verifica se titular não está vazio
    length validade == 5 -- formato MM/AA

-- Função para listar todos os clientes cadastrados.
listarClientes :: Sistema -> IO ()
listarClientes sistema = do
  putStrLn "Clientes cadastrados:"
  mapM_ print sistema

-- Função para autenticar um cliente.
autenticarCliente :: Sistema -> IO (Maybe Cliente)
autenticarCliente sistema = do
  putStrLn "Login"
  putStrLn "Email: "
  emailinput <- getLine
  putStrLn "Senha: "
  senhainput <- getLine
  let clienteEncontrado = find (\cliente -> emailinput == email cliente && senhainput == senha cliente) sistema
  case clienteEncontrado of
    Just cliente -> do
      putStrLn $ "Bem-vindo, " ++ nome cliente ++ "!"
      return (Just cliente)
    Nothing -> do
      putStrLn "Email ou senha incorretos."
      return Nothing

-- Função para atualizar os dados cadastrais do cliente.
atualizarDadosCliente :: Cliente -> Sistema -> IO Sistema
atualizarDadosCliente cliente sistema = do
  putStrLn "\n--- Atualizar Dados Cadastrais ---"
  putStrLn "Deixe em branco os campos que nao deseja alterar."
  putStrLn $ "Nome atual: " ++ nome cliente ++ "\nNovo Nome: "
  novoNome <- getLine
  putStrLn $ "Data de Nascimento atual: " ++ dataNascimento cliente ++ "\nNova Data de Nascimento (DD/MM/AAAA): "
  novaDataNascimento <- getLine
  putStrLn $ "CPF atual: " ++ cpf cliente ++ "\nNovo CPF: "
  novoCpf <- getLine
  putStrLn $ "Endereco atual: " ++ endereco cliente ++ "\nNovo Endereco: "
  novoEndereco <- getLine
  putStrLn $ "Telefone atual: " ++ telefone cliente ++ "\nNovo Telefone: "
  novoTelefone <- getLine
  let exemplo = show (saldoCliente cliente)
  putStrLn $ "Saldo atual: " ++  exemplo ++ "\n Novo Saldo: "
  novoSaldo <- getLine
  -- Atualiza apenas os campos que foram modificados
  let nomeAtualizado = if null novoNome then nome cliente else novoNome
  let dataNascimentoAtualizada = if null novaDataNascimento then dataNascimento cliente else novaDataNascimento
  let cpfAtualizado = if null novoCpf then cpf cliente else novoCpf
  let enderecoAtualizado = if null novoEndereco then endereco cliente else novoEndereco
  let telefoneAtualizado = if null novoTelefone then telefone cliente else novoTelefone
  let saldoClienteAtualizado = if null novoSaldo then saldoCliente cliente else read novoSaldo ::Float
  let clienteAtualizado = 
        Cliente
          { nome = nomeAtualizado,
            email = email cliente, -- Email não pode ser alterado
            senha = senha cliente, -- Senha deve ser alterada em uma função separada
            dataNascimento = dataNascimentoAtualizada,
            cpf = cpfAtualizado,
            endereco = enderecoAtualizado,
            telefone = telefoneAtualizado,  
            saldoCliente = saldoClienteAtualizado,
            cartao = cartao cliente
          }

  -- Remove o cliente antigo e adiciona o cliente atualizado
  let sistemaAtualizado = clienteAtualizado : filter (/= cliente) sistema
  putStrLn "Dados atualizados com sucesso!"
  return sistemaAtualizado

-- Função para fazer uma recarga de saldo no cliente, recebe o cliente, um valor para recarga, o sistema e atualiza ele
fazerRecarga :: Cliente -> Float -> Sistema -> IO Sistema
fazerRecarga cliente recarga sistema = do
  -- Faz a recarga
  let novoSaldo = recarga + saldoCliente cliente
  -- Atualiza a lista de clientes no sistema, adicionando o cliente com a recarga nova
  let novoCliente = cliente { saldoCliente = novoSaldo }

  let sistemaAtualizado = novoCliente : filter (/= cliente) sistema
  putStrLn "Dados atualizados com sucesso!"
  return sistemaAtualizado