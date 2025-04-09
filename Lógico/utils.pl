% Módulo de funções utilitárias e persistência de dados.
% Aqui são definidas funções auxiliares para manipulação de dados e persistência em arquivo txt.

% Atualiza o saldo do cliente.
atualizar_saldo(ClienteAntigo, NovoSaldo) :-
    sistema(Clientes),
    ClienteAntigo = cliente(Nome,Email,Senha,Data,CPF,End,Tel,_,Cartao),
    delete(Clientes, ClienteAntigo, ClientesSemAntigo),
    NovoCliente = cliente(Nome,Email,Senha,Data,CPF,End,Tel,NovoSaldo,Cartao),
    append(ClientesSemAntigo, [NovoCliente], NovosClientes),
    retract(sistema(Clientes)),
    assert(sistema(NovosClientes)),
    salvar_dados.

% Remove clientes duplicados do sistema.
remover_duplicatas([], Acc, Acc).
remover_duplicatas([Cliente|Resto], Acc, Resultado) :-
    Cliente = cliente(_,Email,_,_,_,_,_,_,_),
    (member(ClienteExistente, Acc),
     ClienteExistente = cliente(_,Email,_,_,_,_,_,_,_) ->
        remover_duplicatas(Resto, Acc, Resultado)
    ;
        remover_duplicatas(Resto, [Cliente|Acc], Resultado)
    ).

% Salva os dados do sistema em um arquivo txt (persistência dos dados).
salvar_dados :-
    sistema(ClientesComDuplicata),
    remover_duplicatas(ClientesComDuplicata, [], ClientesSemDuplicata),
    open('clientes.txt', write, Stream),
    forall(member(Cliente, ClientesSemDuplicata),
           (write_canonical(Stream, Cliente),
            write(Stream, '.\n'))),
    close(Stream).

% Carrega os dados do sistema do arquivo txt.
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

% Lê os dados dos clientes do arquivo txt.
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

% Atualiza cartão no arquivo.
atualizar_cartao_arquivo(ClienteAntigo, NovoCartao) :-
    sistema(Clientes),
    ClienteAntigo = cliente(Nome,Email,Senha,Data,CPF,End,Tel,Saldo,_),
    delete(Clientes, ClienteAntigo, ClientesSemAntigo),
    NovoCliente = cliente(Nome,Email,Senha,Data,CPF,End,Tel,Saldo,NovoCartao),
    append(ClientesSemAntigo, [NovoCliente], NovosClientes),
    retract(sistema(Clientes)),
    assert(sistema(NovosClientes)),
    salvar_dados.

% Realiza a escolha do método de pagamento (saldo ou cartão) e atualiza os dados do cliente.
% Se o saldo for suficiente, atualiza o saldo. Se o cartão for suficiente, atualiza o limite do cartão.
escolher_metodo_pagamento(Cliente, Valor) :-
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
            NovoSaldo is Saldo - Valor,
            atualizar_saldo(ClienteAtual, NovoSaldo)
        ;
            writeln('Saldo insuficiente!'),
            fail)
    ; Opcao = 2 ->
        (Cartao \= null ->
            cartao(Num,Tit,Val,Cvv,Limite,Seguro) = Cartao,
            (Limite >= Valor ->
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