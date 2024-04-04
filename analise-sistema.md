# Análise de sistema resumida para uma DLL de notificações

Este documento apresenta uma análise de sistema resumida para o desenvolvimento de uma DLL de notificações genérica e extensível em Delphi, utilizando padrões de projetos, SOLID e clean code. 

> A DLL deve atender às necessidades de diferentes aplicações em termos de gerenciamento e entrega de notificações aos usuários.

# 1. Requisitos Funcionais

## 1.1. Envio de Notificações

- Criar um método "notificar" que será exposto pela DLL
- Criar uma interface onde o usuario possa assinar uma notificação e escolher a periodicidade dos envios.
- - Utilizar a DLL para os envios de notificações
- A interface deve ser simples e já listar pelo menos duas notificações
- - **Notificação de backup**
- - **Notificação de tarefa vencida**
- Permitir que o usário possa acionar o envio da notificação de forma manual para realização dos testes

# 2. Arquitetura do Sistema

## 2.1. Camadas e Padrões de Design (MVC)

###  Modelo (Model)
- Entidades: Notificação

### Repositórios
- Acesso e manipulação de dados

### Visão (View)
- Interface para configuração de notificações.

### Controle (Controller)
- Lógica da aplicação
- Assinatura e cancelamento de notificações.
- Disparo de notificações de acordo com a frequência configurada.
- Integração com diferente serviços de comunicação como(SMS, e-mail, etc.).

# 3. Desenvolvimento e Testes

- Implementação do protótipo da API em Delphi.
- Foco na estrutura, código e organização do projeto, seguindo boas práticas de desenvolvimento.
- Simulação do envio de notificações:
- Testes unitários e de integração para garantir a qualidade do código.
