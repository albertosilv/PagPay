% Módulo da loja virtual.

% Estrutura de dados: 
% produto(Codigo, Nome, Valor)
:- dynamic produto/3.
:- [utils].


% Catálogo de produtos disponíveis.
produto(1, 'Camiseta', 39.90).
produto(2, 'Cueca', 60.0).
produto(3, 'Meia', 2.50).

% Exibe todos produtos disponíveis.
exibir_produtos :-
    forall(produto(Codigo, Nome, Valor),
           format('~w. ~w - R$ ~2f~n', [Codigo, Nome, Valor])).

% Realiza a compra de produtos.
realizar_compra(Cliente, CodigoStr) :-
    carregar_dados,
    sistema(Clientes),
    cliente(Nome,Email,Senha,_,_,_,_,_,_) = Cliente,
    member(ClienteAtual, Clientes),
    ClienteAtual = cliente(Nome,Email,Senha,_,_,_,_,_,_),
    
    atom_number(CodigoStr, Codigo),
    produto(Codigo, _, Valor),
    
    escolher_metodo_pagamento(ClienteAtual, Valor).