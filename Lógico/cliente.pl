:- dynamic cliente/9.
:- dynamic cartao/6.
:- dynamic sistema/1.
:- [utils].

:- initialization(init_sistema).

init_sistema :-
    retractall(sistema(_)),
    assert(sistema([])).

% Estrutura cliente: cliente(Nome, Email, Senha, DataNasc, CPF, Endereco, Telefone, Saldo, Cartao)
% Estrutura cartao: cartao(Numero, Titular, Validade, CVV, Limite, Seguro)

% Validações básicas do cartão
validar_cartao(Numero, Titular, Validade, CVV) :-
    string_length(Numero, 16),
    Titular \= '',
    string_length(Validade, 5),
    string_length(CVV, 3).

% Buscar cliente
buscar_cliente(Email, Senha, Cliente) :-
    sistema(Clientes),
    member(Cliente, Clientes),
    Cliente = cliente(_, Email, Senha, _, _, _, _, _, _).

% Menu de cadastro
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

% Cadastrar cliente
cadastrar_cliente(Nome, Email, Senha, DataNasc, CPF, End, Tel, Saldo) :-
    sistema(ClientesAtuais),
    NovoCliente = cliente(Nome,Email,Senha,DataNasc,CPF,End,Tel,Saldo,null),
    append(ClientesAtuais, [NovoCliente], NovosClientes),
    retract(sistema(ClientesAtuais)),
    assert(sistema(NovosClientes)),
    salvar_dados,
    writeln('Cliente cadastrado com sucesso!').

% Menu de cadastro de cartão
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

% Contratar seguro
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

% Fazer PIX
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
        
        (escolher_metodo_pagamento(ClienteAtual, Valor, _) ->
            carregar_dados,
            sistema(ClientesAtualizados),
            member(ClienteRemetente, ClientesAtualizados),
            ClienteRemetente = cliente(Nome,Email,Senha,_,_,_,_,SaldoAtual,_),
            
            % Se o destinatário é o próprio remetente
            (Email = EmailDestino ->
                NovoSaldo is SaldoAtual + Valor,
                atualizar_saldo(ClienteRemetente, NovoSaldo)
            ;
                % Se não, atualiza apenas o saldo do destinatário
                NovoSaldoDestino is SaldoDestino + Valor,
                atualizar_saldo(Destinatario, NovoSaldoDestino)
            ),
            writeln('PIX realizado com sucesso!')
        ;
            writeln('Nao foi possivel realizar o PIX.'))
    ;
        writeln('Destinatario nao encontrado!')).

% Pagar conta
pagar_conta(Cliente) :-
    carregar_dados,
    sistema(Clientes),
    cliente(Nome,Email,Senha,_,_,_,_,_,_) = Cliente,
    member(ClienteAtual, Clientes),
    ClienteAtual = cliente(Nome,Email,Senha,_,_,_,_,_,_),
    
    write('Digite o valor da conta: '),
    read_line_to_string(user_input, ValorStr),
    atom_number(ValorStr, Valor),
    
    (escolher_metodo_pagamento(ClienteAtual, Valor, _) ->
        writeln('Conta paga com sucesso!')
    ;
        writeln('Nao foi possivel pagar a conta.')).

% Exibir opções do cliente
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
