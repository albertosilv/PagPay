% Módulo de gerenciamento de clientes e suas operações.

% Estrutura de dados:
% cliente(Nome, Email, Senha, DataNasc, CPF, Endereco, Telefone, Saldo, Cartao)
% cartao(Numero, Titular, Validade, CVV, Limite, Seguro)
:- dynamic cliente/9.
:- dynamic cartao/6.
:- dynamic sistema/1.
:- [utils].


% Inicializa o sistema.
:- initialization(init_sistema).

init_sistema :-
    retractall(sistema(_)),
    assert(sistema([])).

% Valida dados básicos do cartão (número, titular, validade, CVV).
% O número deve ter 16 dígitos, a validade deve ter 5 caracteres (MM/AA) e o CVV deve ter 3 dígitos (padrão).
validar_cartao(Numero, Titular, Validade, CVV) :-
    string_length(Numero, 16),
    Titular \= '',
    string_length(Validade, 5),
    string_length(CVV, 3).

% Busca cliente no sistema.
buscar_cliente(Email, Senha, Cliente) :-
    sistema(Clientes),
    member(Cliente, Clientes),
    Cliente = cliente(_, Email, Senha, _, _, _, _, _, _).

% Interface para cadastro de um novo cliente.
menu_cadastro :-
    writeln('\n--- Cadastro de Cliente ---'),
    write('Nome: '), 
    read_line_to_string(user_input, Nome),
    write('Email: '), 
    read_line_to_string(user_input, Email),
    write('Senha: '), 
    read_line_to_string(user_input, Senha),
    write('Data de Nascimento (DD/MM/AAAA): '), 
    read_line_to_string(user_input, DataNasc),
    write('CPF: '), 
    read_line_to_string(user_input, CPF),
    write('Endereco: '), 
    read_line_to_string(user_input, End),
    write('Telefone: '), 
    read_line_to_string(user_input, Tel),
    write('Saldo inicial: '), 
    read_line_to_string(user_input, SaldoStr),
    atom_number(SaldoStr, Saldo),
    cadastrar_cliente(Nome, Email, Senha, DataNasc, CPF, End, Tel, Saldo),
    menu_inicial.

% Cadastra o novo cliente no sistema.
cadastrar_cliente(Nome, Email, Senha, DataNasc, CPF, End, Tel, Saldo) :-
    sistema(ClientesAtuais),
    NovoCliente = cliente(Nome,Email,Senha,DataNasc,CPF,End,Tel,Saldo,null),
    append(ClientesAtuais, [NovoCliente], NovosClientes),
    retract(sistema(ClientesAtuais)),
    assert(sistema(NovosClientes)),
    salvar_dados,
    writeln('Cliente cadastrado com sucesso!').

% Interface para cadastro de cartão.
% Se já tiver um cartão cadastrado, sobrescreve o existente.
menu_cadastro_cartao(Cliente) :-
    carregar_dados,
    sistema(Clientes),
    cliente(Nome,Email,Senha,_,_,_,_,_,Cartao) = Cliente,
    member(ClienteAtual, Clientes),
    ClienteAtual = cliente(Nome,Email,Senha,_,_,_,_,_,Cartao),
    
    (Cartao \= null ->
        writeln('Voce ja possui um cartao cadastrado. O novo cartao substituira o atual.')
    ;
        true),
    
    writeln('\n--- Cadastro de Cartao ---'),
    write('Numero do cartao: '),
    read_line_to_string(user_input, Numero),
    write('Titular: '),
    read_line_to_string(user_input, Titular),
    write('Validade (MM/AA): '),
    read_line_to_string(user_input, Validade),
    write('CVV: '),
    read_line_to_string(user_input, CVV),
    write('Limite: '),
    read_line_to_string(user_input, LimiteStr),
    atom_number(LimiteStr, Limite),
    
    (validar_cartao(Numero, Titular, Validade, CVV) ->
        NovoCartao = cartao(Numero, Titular, Validade, CVV, Limite, false),
        atualizar_cliente_cartao(ClienteAtual, NovoCartao),
        writeln('Cartao cadastrado com sucesso!')
    ;
        writeln('Dados do cartao invalidos!')).

% Realiza contratação de seguro do cartão se o limite for maior do que 50.
contratar_seguro(Cliente) :-
    carregar_dados,
    sistema(Clientes),
    cliente(Nome,Email,Senha,_,_,_,_,_,_) = Cliente,
    member(ClienteAtual, Clientes),
    ClienteAtual = cliente(Nome,Email,Senha,_,_,_,_,_,Cartao),
    (Cartao \= null ->
        Cartao = cartao(Num,Tit,Val,Cvv,Limite,Seguro),
        (Seguro == true ->
            writeln('Cliente ja possui seguro!')
        ;
            (Limite >= 50 ->
                NovoLimite is Limite - 50,
                NovoCartao = cartao(Num,Tit,Val,Cvv,NovoLimite,true),
                atualizar_cartao_arquivo(ClienteAtual, NovoCartao),
                writeln('Seguro contratado com sucesso!')
            ;
                writeln('Limite insuficiente para contratar seguro!'))
        )
    ;
        writeln('Cliente nao possui cartao cadastrado!')).

% Realiza transferência PIX entre usuários.
% Também permite que o destinatário faça um PIX para sí mesmo, usando o limite do seu cartão para aumentar seu saldo.
fazer_pix(Cliente) :-
    carregar_dados,
    sistema(Clientes),
    write('Digite o email do destinatario: '),
    read_line_to_string(user_input, EmailDestino),
    
    Cliente = cliente(Nome,Email,Senha,_,_,_,_,_,_),
    member(ClienteAtual, Clientes),
    ClienteAtual = cliente(Nome,Email,Senha,_,_,_,_,_,_),
    
    (member(Destinatario, Clientes),
     Destinatario = cliente(_,EmailDestino,_,_,_,_,_,SaldoDestino,_) ->
        write('Digite o valor do PIX: '),
        read_line_to_string(user_input, ValorStr),
        atom_number(ValorStr, Valor),
        
        (escolher_metodo_pagamento(ClienteAtual, Valor) ->
            carregar_dados,
            sistema(ClientesAtualizados),
            member(ClienteRemetente, ClientesAtualizados),
            ClienteRemetente = cliente(Nome,Email,Senha,_,_,_,_,SaldoAtual,_),
            
            (Email = EmailDestino ->
                NovoSaldo is SaldoAtual + Valor,
                atualizar_saldo(ClienteRemetente, NovoSaldo)
            ;
                NovoSaldoDestino is SaldoDestino + Valor,
                atualizar_saldo(Destinatario, NovoSaldoDestino)
            ),
            writeln('PIX realizado com sucesso!')
        ;
            writeln('Nao foi possivel realizar o PIX.'))
    ;
        writeln('Destinatario nao encontrado!')).

% Realiza pagamento de contas.
pagar_conta(Cliente) :-
    carregar_dados,
    sistema(Clientes),
    cliente(Nome,Email,Senha,_,_,_,_,_,_) = Cliente,
    member(ClienteAtual, Clientes),
    ClienteAtual = cliente(Nome,Email,Senha,_,_,_,_,_,_),
    
    write('Digite o valor da conta: '),
    read_line_to_string(user_input, ValorStr),
    atom_number(ValorStr, Valor),
    
    (escolher_metodo_pagamento(ClienteAtual, Valor) ->
        writeln('Conta paga com sucesso!')
    ;
        writeln('Nao foi possivel pagar a conta.')).

% Exibe dados do cliente.
exibir_opcoes(Cliente) :-
    sistema(Clientes),
    cliente(Nome,Email,Senha,_,_,_,_,_,_) = Cliente,
    member(ClienteAtual, Clientes),
    ClienteAtual = cliente(Nome,Email,Senha,DataNasc,CPF,End,Tel,Saldo,Cartao),
    format('Nome: ~w~n', [Nome]),
    format('Email: ~w~n', [Email]),
    format('Data de Nascimento: ~w~n', [DataNasc]),
    format('CPF: ~w~n', [CPF]),
    format('Endereco: ~w~n', [End]),
    format('Telefone: ~w~n', [Tel]),
    format('Saldo: R$ ~2f~n', [Saldo]),
    (Cartao \= null ->
        cartao(Num,_,Val,_,Limite,Seguro) = Cartao,
        format('Cartao: ~w~n', [Num]),
        format('Validade: ~w~n', [Val]),
        format('Limite: R$ ~2f~n', [Limite]),
        format('Seguro: ~w~n', [Seguro])
    ;
        writeln('Sem cartao cadastrado.')).