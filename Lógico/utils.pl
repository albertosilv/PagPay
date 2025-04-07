% Funções auxiliares para manipulação de clientes e cartões

% Atualizar limite do cartão
atualizar_limite_cartao(Cliente, CartaoAntigo, NovoLimite) :-
    CartaoAntigo = cartao(Num,Tit,Val,Cvv,_,Seg),  % Padroniza extração
    NovoCartao = cartao(Num,Tit,Val,Cvv,NovoLimite,Seg),
    atualizar_cliente_cartao(Cliente, NovoCartao).

% Atualizar cartão do cliente
atualizar_cliente_cartao(ClienteAntigo, NovoCartao) :-
    ClienteAntigo = cliente(Nome,Email,Senha,Data,CPF,End,Tel,Saldo,_),
    NovoCliente = cliente(Nome,Email,Senha,Data,CPF,End,Tel,Saldo,NovoCartao),
    sistema(Clientes),
    delete(Clientes, ClienteAntigo, ClientesSemAntigo),
    append(ClientesSemAntigo, [NovoCliente], NovosClientes),
    retract(sistema(Clientes)),
    assert(sistema(NovosClientes)),
    salvar_dados.  % Salva após atualizar

% Atualizar saldo do cliente
atualizar_saldo(ClienteAntigo, NovoSaldo) :-
    % Remove o cliente antigo do sistema
    sistema(Clientes),
    ClienteAntigo = cliente(Nome,Email,Senha,Data,CPF,End,Tel,_,Cartao),
    delete(Clientes, ClienteAntigo, ClientesSemAntigo),
    
    % Cria e adiciona o novo cliente
    NovoCliente = cliente(Nome,Email,Senha,Data,CPF,End,Tel,NovoSaldo,Cartao),
    append(ClientesSemAntigo, [NovoCliente], NovosClientes),
    
    % Atualiza o sistema
    retract(sistema(Clientes)),
    assert(sistema(NovosClientes)),
    
    % Salva no arquivo
    salvar_dados.

% Validar formato de email
validar_email(Email) :-
    sub_string(Email, _, _, _, '@'),
    sub_string(Email, _, _, _, '.').

% Validar formato de CPF
validar_cpf(CPF) :-
    string_length(CPF, 11),
    string_chars(CPF, Chars),
    maplist(char_type(_, digit), Chars).

% Validar data de nascimento
validar_data(Data) :-
    sub_string(Data, 2, 1, _, '/'),
    sub_string(Data, 5, 1, _, '/'),
    string_length(Data, 10).

% Funções de atualização do sistema

% Remove duplicatas baseadas no email
remover_duplicatas([], Acc, Acc).
remover_duplicatas([Cliente|Resto], Acc, Resultado) :-
    Cliente = cliente(_,Email,_,_,_,_,_,_,_),
    (member(ClienteExistente, Acc),
     ClienteExistente = cliente(_,Email,_,_,_,_,_,_,_) ->
        remover_duplicatas(Resto, Acc, Resultado)
    ;
        remover_duplicatas(Resto, [Cliente|Acc], Resultado)
    ).


% Funções para persistência de dados
% Salvar dados do sistema em arquivo
salvar_dados :-
    sistema(ClientesComDuplicata),
    remover_duplicatas(ClientesComDuplicata, [], ClientesSemDuplicata),
    open('clientes.txt', write, Stream),
    forall(member(Cliente, ClientesSemDuplicata),
           (write_canonical(Stream, Cliente),
            write(Stream, '.\n'))),
    close(Stream).

% Carregar dados do sistema do arquivo
carregar_dados :-
    retractall(sistema(_)),
    assert(sistema([])),
    (exists_file('clientes.txt') ->
        setup_call_cleanup(
            open('clientes.txt', read, Stream),
            ler_clientes(Stream),
            close(Stream)
        )
    ;
        true).

% Ler clientes do arquivo
ler_clientes(Stream) :-
    read(Stream, Cliente),
    (Cliente == end_of_file ->
        true
    ;
        sistema(ClientesAtuais),
        append(ClientesAtuais, [Cliente], NovosClientes),
        retract(sistema(ClientesAtuais)),
        assert(sistema(NovosClientes)),
        ler_clientes(Stream)
    ).

% Atualizar cartão no arquivo
atualizar_cartao_arquivo(ClienteAntigo, NovoCartao) :-
    sistema(Clientes),
    ClienteAntigo = cliente(Nome,Email,Senha,Data,CPF,End,Tel,Saldo,_),
    delete(Clientes, ClienteAntigo, ClientesSemAntigo),
    NovoCliente = cliente(Nome,Email,Senha,Data,CPF,End,Tel,Saldo,NovoCartao),
    append(ClientesSemAntigo, [NovoCliente], NovosClientes),
    retract(sistema(Clientes)),
    assert(sistema(NovosClientes)),
    salvar_dados.

% Escolher método de pagamento
escolher_metodo_pagamento(Cliente, Valor, MetodoEscolhido) :-
    carregar_dados,
    sistema(Clientes),
    cliente(Nome,Email,Senha,_,_,_,_,_,_) = Cliente,
    member(ClienteAtual, Clientes),
    ClienteAtual = cliente(Nome,Email,Senha,_,_,_,_,Saldo,Cartao),
    writeln('\nEscolha o metodo de pagamento:'),
    writeln('1. Saldo'),
    writeln('2. Cartao'),
    write('Opcao: '),
    read_line_to_string(user_input, OpcaoStr),
    atom_number(OpcaoStr, Opcao),
    (Opcao = 1 ->
        (Saldo >= Valor ->
            MetodoEscolhido = saldo,
            NovoSaldo is Saldo - Valor,
            atualizar_saldo(ClienteAtual, NovoSaldo)
        ;
            writeln('Saldo insuficiente!'),
            fail)
    ; Opcao = 2 ->
        (Cartao \= null ->
            cartao(Num,Tit,Val,Cvv,Limite,Seguro) = Cartao,
            (Limite >= Valor ->
                MetodoEscolhido = cartao,
                NovoLimite is Limite - Valor,
                NovoCartao = cartao(Num,Tit,Val,Cvv,NovoLimite,Seguro),
                atualizar_cartao_arquivo(ClienteAtual, NovoCartao)
            ;
                writeln('Limite insuficiente!'),
                fail)
        ;
            writeln('Cliente nao possui cartao!'),
            fail)
    ;
        writeln('Opcao invalida!'),
        fail).