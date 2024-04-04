Seja bem-vindo a mais uma etapa do processo de seleção da XXXX. Neste etapa você precisa desenvolver este pequeno projeto. Leia com atenção e **em caso de dúvidas crie issues neste repositório**.

# Prazo
- 14 dias corridos.

# Projeto
- Criar um protótipo de um framework genérico de notificações ao usuário, que possa ser usado em diferentes aplicativos.
- Criar um aplicativo VCL que demonstre o uso deste framework, permitindo configurar quais notificações receber e testar os agendamentos dessas notificações.
- Criar uma documentação de como usar seu framework em outros projetos e de como adicionar novas formas de notificação.

## Requisitos

- Implementar um protótipo de framework/biblioteca que gerencie notificações dentro do sistema.
- Novos métodos de envio das notificações devem ser fáceis de serem plugados no framework.
- Deverá ser possível configurar a frequência das notificações, ex: Diária, semanal ou mensal.
- O framework deverá ser independente do projeto VCL e deverá ser facilmente plugável em outros aplicativos.

## Formas de envio das notificações

Alguns exemplos são SMS, push, e-mail, etc: O framework deverá suportar diversos tipos de notificações que poderão ser plugadas.

**Não é necessário implementar e fazer funcionar as formas de envio**, ou seja, o protótipo não precisa realmente enviar um e-mail, iremos avaliar a estrutura, código e organização do projeto, apenas mostrar uma mensagem por exemplo "Enviando e-mail" é suficiente.

## Casos de uso

Abaixo alguns exemplos de casos de uso para facilitar o entendimento do problema:

- **Notificação de backup**: Quando não for efetuado um backup após um determinado período, os usuários que assinaram essa notificação deverão ser notificados.
- **Notificação de tarefa vencida**: Caso o usuário tenha uma tarefa vencida o sistema deverá enviar um e-mail, sms, etc diário para ele com as tarefas vencidas.

## Como codificar

- Implemente o protótipo usando testes unitários
- Entregue um aplicativo VCL simples (pré compilado) que permita configurar e executar as notificações e demonstre o framework funcionando.
- Envie Pull Requests para o repositório conforme for implementando os requisitos

# O que será avaliado?

- Simplicidade, clareza e estilo do código
- Arquitetura do framework
- Facilidade de plugar novas formas de notificação
- Facilidade de utilizar o framework em outros projetos
- UI e UX do aplicativo de exemplo
- Testes

# Isso é apenas um teste
- Apesar de ser um problema aparentemente real de um sistema de notificações ao usuário, ele foi escrito apenas para teste. Seu código não será usado depois que o teste for finalizado.
