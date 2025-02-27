-- Cliente.hs
module Cliente where

import Data.List (find)

-- Definição do tipo de dados para representar um cliente
data Cliente = Cliente
  { nome :: String,
    email :: String,
    senha :: String,
    dataNascimento :: String,
    cpf :: String,
    endereco :: String,
    telefone :: String
  }
  deriving (Show, Eq)

-- Tipo de dados para representar o sistema de gerenciamento de clientes
type Sistema = [Cliente]

-- Função para cadastrar um novo cliente
cadastrarCliente :: Sistema -> IO Sistema
cadastrarCliente sistema = do
  putStrLn "Cadastro de Cliente"
  putStr "Nome: "
  nome <- getLine
  putStr "Email: "
  email <- getLine
  putStr "Senha: "
  senha <- getLine
  putStr "Data de Nascimento (DD/MM/AAAA): "
  dataNascimento <- getLine
  putStr "CPF: "
  cpf <- getLine
  putStr "Endereço: "
  endereco <- getLine
  putStr "Telefone: "
  telefone <- getLine
  let novoCliente = Cliente {nome = nome, email = email, senha = senha, dataNascimento = dataNascimento, cpf = cpf, endereco = endereco, telefone = telefone}
  putStrLn "Cliente cadastrado com sucesso!"
  return (novoCliente : sistema)

-- Função para listar todos os clientes cadastrados
listarClientes :: Sistema -> IO ()
listarClientes sistema = do
  putStrLn "Clientes cadastrados:"
  mapM_ print sistema

-- Função para autenticar um cliente
autenticarCliente :: Sistema -> IO (Maybe Cliente)
autenticarCliente sistema = do
  putStrLn "Login"
  putStr "Email: "
  email <- getLine
  putStr "Senha: "
  senha <- getLine
  let clienteEncontrado = find (\cliente -> email == email cliente && senha == senha cliente) sistema
  case clienteEncontrado of
    Just cliente -> do
      putStrLn $ "Bem-vindo, " ++ nome cliente ++ "!"
      return (Just cliente)
    Nothing -> do
      putStrLn "Email ou senha incorretos."
      return Nothing

-- Função para atualizar os dados cadastrais do cliente
atualizarDadosCliente :: Cliente -> Sistema -> IO Sistema
atualizarDadosCliente cliente sistema = do
  putStrLn "\n--- Atualizar Dados Cadastrais ---"
  putStrLn "Deixe em branco os campos que não deseja alterar."
  putStr $ "Nome atual: " ++ nome cliente ++ "\nNovo Nome: "
  novoNome <- getLine
  putStr $ "Data de Nascimento atual: " ++ dataNascimento cliente ++ "\nNova Data de Nascimento (DD/MM/AAAA): "
  novaDataNascimento <- getLine
  putStr $ "CPF atual: " ++ cpf cliente ++ "\nNovo CPF: "
  novoCpf <- getLine
  putStr $ "Endereço atual: " ++ endereco cliente ++ "\nNovo Endereço: "
  novoEndereco <- getLine
  putStr $ "Telefone atual: " ++ telefone cliente ++ "\nNovo Telefone: "
  novoTelefone <- getLine

  -- Atualiza apenas os campos que foram modificados
  let nomeAtualizado = if null novoNome then nome cliente else novoNome
  let dataNascimentoAtualizada = if null novaDataNascimento then dataNascimento cliente else novaDataNascimento
  let cpfAtualizado = if null novoCpf then cpf cliente else novoCpf
  let enderecoAtualizado = if null novoEndereco then endereco cliente else novoEndereco
  let telefoneAtualizado = if null novoTelefone then telefone cliente else novoTelefone

  let clienteAtualizado =
        Cliente
          { nome = nomeAtualizado,
            email = email cliente, -- Email não pode ser alterado
            senha = senha cliente, -- Senha deve ser alterada em uma função separada
            dataNascimento = dataNascimentoAtualizada,
            cpf = cpfAtualizado,
            endereco = enderecoAtualizado,
            telefone = telefoneAtualizado
          }

  -- Remove o cliente antigo e adiciona o cliente atualizado
  let sistemaAtualizado = clienteAtualizado : filter (/= cliente) sistema
  putStrLn "Dados atualizados com sucesso!"
  return sistemaAtualizado