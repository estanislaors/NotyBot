# NotyBot

## Description

Slack application to manage reminders with commands.

The goal is build a simple mechanism that keeps reminders using Slack service to notify when they happen in our devices like PC or mobile phone.

## Design decisions
1) I decided to save the state of the reminders into a NoSQL database because it's good for resume after shutdown or energy fault.

2) We have some level of abstraction in the database interface.

| ReminderDB |
| :--: |
| BasicDB |
| MongoDB |

3) Code modularity was keep in mind to give an easy way to add new commands and operations.

Starting with the types defined in NotyTypes.hs
* NotyReq: Represent requests by the user.
* NotyResp: Represent reply for an user request.

The same for the parsing module (Parsing.lhs) and communication settings for Slack (SlackConfig.hs) behind Linklater library.

<table>
    <thead>
        <tr>
            <th colspan=4>Reminder API </th>
        </tr>
    </thead>
    <tbody>
        <tr>
            <td colspan=2 style="text-align:center;">DailyReminder</td>
            <td colspan=2 style="text-align:center;">EventualReminder</td>
        </tr>
        <tr>
            <td style="text-align:center;">ReminderTypes</td>
            <td style="text-align:center;color:blue">NotyTypes</td>
            <td style="text-align:center;color:blue">SlackConfig</td>
            <td style="text-align:center;color:blue">Parsing</td>
        </tr>
    </tbody>
</table>


## Architecture

We have a listener thread running in port 3000 that uses Reminder Manager's interface. After a command is received, processing functions will be executed, so can be happen the next:

1) Generate a direct reply.
2) Change the state of the reminders.
3) Launch a thread for new reminder.

When the waiting time of a reminder is reached, the system wakes up its thread that will send a message to the Slack channel and then check for a new iteration.

## Install

#### Running server application:

Inside __scripts__, you will find:

1) `deps.sh` to install needed dependencies.
2) `config.sh` to configure the project with stack and download needed libs.
3) `build.sh` to build NotyBot application.
4) `run.sh` to run NotyBot application in a shell.
5) `launch.sh` to run NotyBot application with screen and record log file.


#### Register NotyBot in Slack:

We need to do some steps more to use NotyBot app in Slack:

1) Create an account and workspace in [Slack](https://slack.com/)
2) Create a channel where user NotyBot app
3) Crear una app en [Slack API](https://api.slack.com/apps)
4) Add features and functionalities:
    1) Incoming webhooks: Habilitarlo y crear un webhook en el canal donde se va a utilizar la app. Guardar la URL en un archivo llamado "hook" en el directorio raíz de este repo.
    2) Slash commands: Agregar dos comandos "eventual" y "daily", completar el campo "Request URL" con http://TU_IP_PUBLICA:3000 
    **NOTA:** Si no tienes IP pública puedes usar algún servicio como [ngrok](https://ngrok.com/) y asignar esa.
    3) Permissions: Entiendo que los que vienen por defecto alcanza, se podría agregar alguno si hiciera falta.
5) Instalar la app en el workspace

## Modo de uso

Una vez que se haya instalado la aplicación en un canal de Slack y se encuentre en ejecución el servidor. Es posible utilizar los comandos para empezar a usarla:

### Commands

* `daily`: To manage recurring reminders. Ex: Run in the park, water the plants, change toothbrush, etc
* `eventual` To manage reminders about defined datetimes. Ex: A meeting, anniversary, etc.

We defined 3 operations for each command.

### Operations
* `add`  Add new reminder.
* `del`  Delete reminder.
* `list` List all reminders.

### Example uses

#### Create reminder:

The next command creates a reminders with id "iD_rec" that will notify you at 7:30 pm during 8 days.

`/daily add iD_rec at 19:30 repeat 7`

The next command creates a reminders with id "clase2" that will notify you every 22 minutes once time.

`/daily add clase2 every 22 minutes`

he next command creates a reminders with id "cumple_juan" that will notify you at 20 of December of 2019 at 7:30 pm.

`/eventual add cumple_juan on 2019-12-20 at 19:30`

**NOTE:** The identifiers must be start with lowercase letter follow by any combination of alphanumeric chars, middle hyphen or underscore.

#### List pending reminders:

`/eventual list`

`/daily list`

Get a list with all pending reminders with some information associated.

#### Delete pending reminder:

`/eventual del cumple_juan`

`/daily del iD_rec`

## TODOs:
* Distribute NotyBot into Slack App Directory:
    * Configure SSL certificate to use HTTPS.
    * Enable OAuth 2 y URL redirection.
