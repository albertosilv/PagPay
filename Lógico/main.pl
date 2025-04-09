% Módulo principal que controla o fluxo do programa.
:- [cliente].
:- [loja].
:- [utils].

:- initialization(main).

% Inicializa o sistema e apresenta o menu inicial.
main :-
    carregar_dados,
    writeln('Bem-vindo ao Sistema de Gerenciamento de Clientes!'),
    menu_inicial.

menu_inicial :-
    writeln('\n--- Menu Inicial ---'),
    writeln('1. Login'),
    writeln('2. Cadastrar Cliente'),
    writeln('3. Sair'),
    write('Escolha uma opcao: '),
    read_line_to_string(user_input, OpcaoStr),
    atom_number(OpcaoStr, Opcao),
    processar_opcao_inicial(Opcao).

% Processa as opções do menu inicial.
processar_opcao_inicial(1) :- menu_login.
processar_opcao_inicial(2) :- menu_cadastro.
processar_opcao_inicial(3) :- 
    salvar_dados,
    writeln('Saindo...').
processar_opcao_inicial(_) :-
    writeln('Opcao invalida!'),
    menu_inicial.

% Faz autenticação do usuário.
menu_login :-
    write('Email: '),
    read_line_to_string(user_input, Email),
    write('Senha: '),
    read_line_to_string(user_input, Senha),
    (buscar_cliente(Email, Senha, Cliente) ->
        menu_principal(Cliente)
    ;
        writeln('Email ou senha incorretos!'),
        menu_inicial).

% Menu principal do sistema aonde o cliente, já logado, pode acessar diversas funcionalidades.
menu_principal(Cliente) :-
    writeln('\n--- Menu Principal ---'),
    writeln('1. Exibir Dados'),
    writeln('2. Cadastrar Cartao'),
    writeln('3. Acessar Loja'),
    writeln('4. Contratar Seguro'),
    writeln('5. Fazer PIX'),
    writeln('6. Pagar Contas'),
    writeln('7. Sair'),
    write('Escolha uma opcao: '),
    read_line_to_string(user_input, OpcaoStr),
    atom_number(OpcaoStr, Opcao),
    processar_opcao_principal(Opcao, Cliente).

% Processa as opções do menu principal:
processar_opcao_principal(1, Cliente) :- 
    exibir_opcoes(Cliente),
    menu_principal(Cliente).
processar_opcao_principal(2, Cliente) :- 
    menu_cadastro_cartao(Cliente),
    menu_principal(Cliente).
processar_opcao_principal(3, Cliente) :- 
    exibir_produtos,
    write('Digite o codigo do produto: '),
    read_line_to_string(user_input, CodigoStr),
    (realizar_compra(Cliente, CodigoStr) -> 
        writeln('Compra realizada com sucesso!')
    ;
        writeln('Nao foi possivel realizar a compra.')),
    menu_principal(Cliente).
processar_opcao_principal(4, Cliente) :- 
    contratar_seguro(Cliente),
    menu_principal(Cliente).
processar_opcao_principal(5, Cliente) :- 
    fazer_pix(Cliente),
    menu_principal(Cliente).
processar_opcao_principal(6, Cliente) :- 
    pagar_conta(Cliente),
    menu_principal(Cliente).
processar_opcao_principal(7, _) :- 
    salvar_dados,
    writeln('Voltando ao menu inicial...'),
    menu_inicial.
processar_opcao_principal(_, Cliente) :- 
    writeln('Opcao invalida!'),
    menu_principal(Cliente).