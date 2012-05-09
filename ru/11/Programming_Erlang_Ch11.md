plug-in — дополнение

middle man — посредник

**Глава 11**

**Лёгкий IRC**

Вот и пришло время для приложения. До сих пор мы видели только
разрозненные части. Мы видели как писать последовательный код, как
порождать процессы, как регистрировать процессы и т.д. Теперь мы соберём
все эти части в одно работающее целое.

В этой главе мы создадим простую IRC-подобную программу. Мы не будем
придерживаться настоящего IRC протокола. Вместо этого мы придумаем наш
собственный совершенно другой и не совместимый ни с чем
протоколHYPERLINK \\l "id.4c3e30fd4f52"[1]HYPERLINK \\l
"id.4c3e30fd4f52". С точки зрения пользователя наша программа *является*
реализацией IRC, хотя нижележащая реализация гораздо проще, чем это
могло бы ожидаться, т. к. мы используем сообщения Эрланга как основу для
межпроцессовых сообщений. Это полностью устраняет разбор сообщений и
значительно упрощает дизайн.

Наша программа является программой на *чистом* Эрланге, которая
совершенно не использует библиотеки OTP и минимально использует
стандартные библиотеки. Так что, к примеру, у неё полностью
самодостаточная клиент-серверная архитектура и форма восстановления
после ошибок, основанная на явном манипулировании связей. Причина
неиспользования библиотек в том, что я хочу вносить вам на рассмотрение
по одной концепции за раз и показывать, что мы можем достичь с одним
языком и минимальным использованием библиотек. Мы будем писать код как
набор компонентов. Каждый компонент прост, но вместе они работают
достаточно сложно. Мы можем заставить большую часть сложностей убраться,
используя библиотеки OTP, так что позднее в этой книге мы покажем более
правильные способы организации кода, основанном на общий библиотеках OTP
для построения деревьев клиент-серверов и наблюдения (супервизоров).



*Рис.11. 1: Структура процесса*

**

Наше приложение построено из пяти компонентов. Структура этих
компонентов показана на Рис.11. 1. Рисунок показывает три клиентских
узла (предполагается, что они на других машинах) и один серверный узел
(тоже на другой машине). Эти компоненты выполняют следующие функции:

*Интерфейс с пользователем* — это графическое приложение, которое
используется для отправки сообщений и отображения полученных сообщений .
Сообщения отправляются чат-клиенту.

*Чат-клиент* («C» на рисунке) — разбирается с сообщениями от
пользовательского приложения и отправляет их к контроллеру группы для
текущей группы. Принимает сообщения от контроллера группы и отправляет
их к пользовательскому приложению.

*Контроллер группы* («G» на рисунке) — управляет одной чат-группой. Если
контроллеру посылается сообщение, то он рассылает это сообщение всем
участникам в данной группе. Он отслеживает новых участников, которые
присоединились к группе и участников, которые покинули группу.
Контроллер завершается, если в группе не осталось участников.

*Чат-сервер* («S» на рисунке) — отслеживает контроллеров группы.
Чат-сервер нужен только когда новый участник пытается присоединиться к
группе. Чат-сервер существует в единственном экземпляре, в то время как
контроллеры групп создаются для каждой активной группы.

*Посредник* («M» на рисунке) — обеспечивает транспортировку данных в
системе. Если процесс C посылает сообщение к M, оно попадёт к G (см.
Рис.11. 1). Процесс M скрывает низкоуровневый интерфейс сокетов между
двумя машинами. Главным образом процесс M прячет физическую границу
между машинами за какой-то абстракцией. Это значит, что на основе
передачи сообщений Эрланга можно построить целое приложение и не
заботиться о подробностях нижележащей инфраструктуры связи.

**11.1 Диаграммы последовательности сообщений**

Если у нас много параллельных процессов, то очень легко потерять нить
происходящего. Чтобы помочь нам понять, что происходит, мы можем
нарисовать диаграмму последовательности сообщений (MSD), которая
показывает взаимодействие между различными процессами.



*Рис.11. 2: Прохождение сообщений, участвующих в передаче текстового
сообщения*

**

Диаграмма последовательности сообщений на Рис.11. 2 показывает
последовательность сообщений, которые пересылаются, когда пользователь
напечатает строку в поле ввода. Это приводит к отправке сообщения к
чат-контроллеру (C), за которым следует сообщение к одному из
посредников (M1), затем через М2 к контроллеру группы (G). На этапе
между посредниками происходит двоичное кодирование сообщений Эрланга.

MSD даёт хорошее представление того, что происходит. Если вы будете
глазеть на MSD и на код программы достаточно долго, то вы сможете
убедить себя в том, что этот код реализует именно ту последовательность
передачи сообщений, которая изображена на диаграмме.

Когда я проектирую программу наподобие чата, я часто рисую множество MSD
диаграмм — это помогает мне думать о том, что происходит. Я не большой
любитель графических методов проектирования, но MDS диаграммы полезны
для отображения того, что происходит в ряде параллельных процессов,
которые обмениваются сообщениями для решения определённой проблемы.

А сейчас посмотрим на индивидуальные компоненты.

**11.2 Пользовательский интерфейс**





*Рис.11. 3: Виджет ввода-вывода*

**

Пользовательский интерфейс построен на базе простого виджета
ввода-вывода. Этот виджет показан на Рис.11. 3. Код этого виджета
достаточно длинный и в основном касается доступа к оконной системе
посредством стандартной библиотеки gs. Т. к. мы пока не хотим прыгать в
эту кроличью нору, то мы не покажем здесь соответствующий код (хотя вы
найдёте этот код, начиная со страницы \_17\_). Интерфейс у виджета
ввода-вывода следующий:

@spec io\_widget:start(Pid) -\> Widget

Создаёт новый виджет ввода-вывода. Возвращает Widget, который является
PID, который может использоваться для общения с виджетом. Когда
пользователь печатает что-либо в поле ввода виджета процессу, который
вызвал эту функцию посылаются сообщения вида {Widget, State, Parse}.
State — это переменная состояния, сохранённая в виджете, которая может
устанавливаться пользователем. Parse — это результат разбора строки
ввода пользовательским парсером.

@spec io\_widget:set\_title(Widget, Str)

Устанавливает заголовок в виджете.



@spec io\_widget:set\_state(Widget, State)

Устанавливает состояние виджета.



@spec io\_widget:insert\_str(Widget, Str)

Вставляет строку в основную область виджета.



@spec io\_widget:set\_handler(Widget, Fun)

Устанавливает парсер виджета в Fun (см. далее).

Виджет ввода-вывода может генерировать следующие сообщения:



{Widget, State, Parse}

Это сообщение отправляется, когда пользователь вводит строку в нижней
области команд виджета. Parse — это результат разбора этой строки
парсером, связанным с данным виджетом.



{Widget, destroyed}

Это сообщение отправляется, когда пользователь разрушает виджет
посредством закрытия окна.

В общем, виджет ввода-вывода — это программируемая штучка. С ним можно
связать парсер, который будет использоваться для разбора всех сообщений,
которые вводятся в поле ввода виджета. Разбор делается вызовом функции
Parse(Str). Эта функция может быть установлена вызовом
set\_handler(Widget, Parse).

Парсер по-умолчанию — это такая функция:

Parse(Str) -\> Str end.

**11.3 Клиентская часть**

Клиентская часть программы чата состоит из трёх процессов: виджет
ввода-вывода (о котором мы уже говорили), клиент чата (который
организует взаимодействие между виджетом и посредником) и процесс
посредника. В этой части мы сосредоточимся на клиенте чата.

**Клиент чата**

Мы запускаем чат-клиент вызовом start/0:

HYPERLINK
"http://media.pragprog.com/titles/jaerlang/code/socket\_dist/chat\_client.erl"DownloadHYPERLINK
"http://media.pragprog.com/titles/jaerlang/code/socket\_dist/chat\_client.erl"
HYPERLINK
"http://media.pragprog.com/titles/jaerlang/code/socket\_dist/chat\_client.erl"socketHYPERLINK
"http://media.pragprog.com/titles/jaerlang/code/socket\_dist/chat\_client.erl"\_HYPERLINK
"http://media.pragprog.com/titles/jaerlang/code/socket\_dist/chat\_client.erl"distHYPERLINK
"http://media.pragprog.com/titles/jaerlang/code/socket\_dist/chat\_client.erl"/HYPERLINK
"http://media.pragprog.com/titles/jaerlang/code/socket\_dist/chat\_client.erl"chatHYPERLINK
"http://media.pragprog.com/titles/jaerlang/code/socket\_dist/chat\_client.erl"\_HYPERLINK
"http://media.pragprog.com/titles/jaerlang/code/socket\_dist/chat\_client.erl"clientHYPERLINK
"http://media.pragprog.com/titles/jaerlang/code/socket\_dist/chat\_client.erl".HYPERLINK
"http://media.pragprog.com/titles/jaerlang/code/socket\_dist/chat\_client.erl"erl

start() -\>

connect("localhost" , 2223, "AsDT67aQ" , "general" , "joe" ).

Он пытается подсоединиться к localhost на порт 2223 (это жестко зашито в
код для тестовых целей). Функция connect/5 просто создаёт параллельный
процесс, вызывая hander/5. А вот обработчику приходится выполнять
несколько задач:

● он делает себя системным процессом, так что теперь он может
перехватывать сигналы выхода

● он создаёт виджет ввода-вывода и устанавливает подсказку и заголовок
этого виджета

● он порождает процесс соединения (который пытается соединиться с
сервером)

● в конце он ждёт события соединения в disconnected/2 (прим. перев.:
«Синее, а не бурое! А по описанию -- бурое, а не синее!..» (С) АБС)

Код для него:

HYPERLINK
"http://media.pragprog.com/titles/jaerlang/code/socket\_dist/chat\_client.erl"DownloadHYPERLINK
"http://media.pragprog.com/titles/jaerlang/code/socket\_dist/chat\_client.erl"
HYPERLINK
"http://media.pragprog.com/titles/jaerlang/code/socket\_dist/chat\_client.erl"socketHYPERLINK
"http://media.pragprog.com/titles/jaerlang/code/socket\_dist/chat\_client.erl"\_HYPERLINK
"http://media.pragprog.com/titles/jaerlang/code/socket\_dist/chat\_client.erl"distHYPERLINK
"http://media.pragprog.com/titles/jaerlang/code/socket\_dist/chat\_client.erl"/HYPERLINK
"http://media.pragprog.com/titles/jaerlang/code/socket\_dist/chat\_client.erl"chatHYPERLINK
"http://media.pragprog.com/titles/jaerlang/code/socket\_dist/chat\_client.erl"\_HYPERLINK
"http://media.pragprog.com/titles/jaerlang/code/socket\_dist/chat\_client.erl"clientHYPERLINK
"http://media.pragprog.com/titles/jaerlang/code/socket\_dist/chat\_client.erl".HYPERLINK
"http://media.pragprog.com/titles/jaerlang/code/socket\_dist/chat\_client.erl"erl

connect(Host, Port, HostPsw, Group, Nick) -\>

spawn(fun() -\> handler(Host, Port, HostPsw, Group, Nick) end).

handler(Host, Port, HostPsw, Group, Nick) -\>

process\_flag(trap\_exit, true),

Widget = io\_widget:start(self()),

set\_title(Widget, Nick),

set\_state(Widget, Nick),

set\_prompt(Widget, [Nick, " \> " ]),

set\_handler(Widget, fun parse\_command/1),

start\_connector(Host, Port, HostPsw),

disconnected(Widget, Group, Nick).

В отключенном состоянии процесс либо получит сообщение {connected,
MM}HYPERLINK \\l "id.826360296f2c"[2]HYPERLINK \\l "id.826360296f2c",
после чего он посылает сообщение login к серверу и ждёт ответа на логин,
либо виджет может быть разрушен, что приводит к всеобщему завершению.
Соединяющийся процесс периодически шлёт сообщения о состоянии к
чат-клиенту. Эти сообщения сразу же пересылаются к виджету ввода-вывода
для показа.

HYPERLINK
"http://media.pragprog.com/titles/jaerlang/code/socket\_dist/chat\_client.erl"DownloadHYPERLINK
"http://media.pragprog.com/titles/jaerlang/code/socket\_dist/chat\_client.erl"
HYPERLINK
"http://media.pragprog.com/titles/jaerlang/code/socket\_dist/chat\_client.erl"socketHYPERLINK
"http://media.pragprog.com/titles/jaerlang/code/socket\_dist/chat\_client.erl"\_HYPERLINK
"http://media.pragprog.com/titles/jaerlang/code/socket\_dist/chat\_client.erl"distHYPERLINK
"http://media.pragprog.com/titles/jaerlang/code/socket\_dist/chat\_client.erl"/HYPERLINK
"http://media.pragprog.com/titles/jaerlang/code/socket\_dist/chat\_client.erl"chatHYPERLINK
"http://media.pragprog.com/titles/jaerlang/code/socket\_dist/chat\_client.erl"\_HYPERLINK
"http://media.pragprog.com/titles/jaerlang/code/socket\_dist/chat\_client.erl"clientHYPERLINK
"http://media.pragprog.com/titles/jaerlang/code/socket\_dist/chat\_client.erl".HYPERLINK
"http://media.pragprog.com/titles/jaerlang/code/socket\_dist/chat\_client.erl"erl

disconnected(Widget, Group, Nick) -\>

receive

{connected, MM} -\>

insert\_str(Widget, "connected to server\\nsending data\\n" ),

MM ! {login, Group, Nick},

wait\_login\_response(Widget, MM);

{Widget, destroyed} -\>

exit(died);

{status, S} -\>

insert\_str(Widget, to\_str(S)),

disconnected(Widget, Group, Nick);

Other -\>

io:format("chat\_client disconnected unexpected:\~p\~n" ,[Other]),

disconnected(Widget, Group, Nick)

end.

Сообщение {connected, MM} очевидно должно придти от соединяющегося
процесса, который был создан вызовом start\_connection(Host, Port,
HostPsw). Этот вызов создаёт параллельный процесс, который в свою
очередь периодически пытается соединиться с IRC сервером.

HYPERLINK
"http://media.pragprog.com/titles/jaerlang/code/socket\_dist/chat\_client.erl"DownloadHYPERLINK
"http://media.pragprog.com/titles/jaerlang/code/socket\_dist/chat\_client.erl"
HYPERLINK
"http://media.pragprog.com/titles/jaerlang/code/socket\_dist/chat\_client.erl"socketHYPERLINK
"http://media.pragprog.com/titles/jaerlang/code/socket\_dist/chat\_client.erl"\_HYPERLINK
"http://media.pragprog.com/titles/jaerlang/code/socket\_dist/chat\_client.erl"distHYPERLINK
"http://media.pragprog.com/titles/jaerlang/code/socket\_dist/chat\_client.erl"/HYPERLINK
"http://media.pragprog.com/titles/jaerlang/code/socket\_dist/chat\_client.erl"chatHYPERLINK
"http://media.pragprog.com/titles/jaerlang/code/socket\_dist/chat\_client.erl"\_HYPERLINK
"http://media.pragprog.com/titles/jaerlang/code/socket\_dist/chat\_client.erl"clientHYPERLINK
"http://media.pragprog.com/titles/jaerlang/code/socket\_dist/chat\_client.erl".HYPERLINK
"http://media.pragprog.com/titles/jaerlang/code/socket\_dist/chat\_client.erl"erl

start\_connector(Host, Port, Pwd) -\>

S = self(),

spawn\_link(fun() -\> try\_to\_connect(S, Host, Port, Pwd) end).

try\_to\_connect(Parent, Host, Port, Pwd) -\>

%% Parent is the Pid of the process that spawned this process

case lib\_chan:connect(Host, Port, chat, Pwd, []) of

{error, \_Why} -\>

Parent ! {status, {cannot, connect, Host, Port}},

sleep(2000),

try\_to\_connect(Parent, Host, Port, Pwd);

{ok, MM} -\>

lib\_chan\_mm:controller(MM, Parent),

Parent ! {connected, MM},

exit(connectorFinished)

end.

try\_to\_connect зацикливается навечно, пытаясь каждые две секунды
подключиться к серверу. Если подключиться не удаётся, то он посылает
сообщение о состоянии к чат-клиенту.

*Замечание:* в start\_connection мы написали следующее:

S = self(),

spawn\_link(fun() -\> try\_to\_connect(S, ...) end)

*Это не то же самое, что и здесь:*

**

spawn\_link(fun() -\> try\_to\_connect(self(), ...) end)

Причина в том, что в первом фрагменте кода self() выполняется внутри
родительского процесса. Во втором куске кода self() выполняется внутри
порождённой функции, так что он возвращает идентификатор порождённого
процесса, а не PID текущего процесса, как вы могли бы подумать. Это
довольно распространённая причина для ошибок (и непонимания).

Если соединение установлено, то он посылает сообщение {connected, MM} к
чат-клиенту. По прибытии этого сообщения чат-клиент посылает сообщение
для логина к серверу (оба этих события происходят в disconnected/2) и
ждёт ответа в wait\_login\_response/2:

HYPERLINK
"http://media.pragprog.com/titles/jaerlang/code/socket\_dist/chat\_client.erl"DownloadHYPERLINK
"http://media.pragprog.com/titles/jaerlang/code/socket\_dist/chat\_client.erl"
HYPERLINK
"http://media.pragprog.com/titles/jaerlang/code/socket\_dist/chat\_client.erl"socketHYPERLINK
"http://media.pragprog.com/titles/jaerlang/code/socket\_dist/chat\_client.erl"\_HYPERLINK
"http://media.pragprog.com/titles/jaerlang/code/socket\_dist/chat\_client.erl"distHYPERLINK
"http://media.pragprog.com/titles/jaerlang/code/socket\_dist/chat\_client.erl"/HYPERLINK
"http://media.pragprog.com/titles/jaerlang/code/socket\_dist/chat\_client.erl"chatHYPERLINK
"http://media.pragprog.com/titles/jaerlang/code/socket\_dist/chat\_client.erl"\_HYPERLINK
"http://media.pragprog.com/titles/jaerlang/code/socket\_dist/chat\_client.erl"clientHYPERLINK
"http://media.pragprog.com/titles/jaerlang/code/socket\_dist/chat\_client.erl".HYPERLINK
"http://media.pragprog.com/titles/jaerlang/code/socket\_dist/chat\_client.erl"erl

wait\_login\_response(Widget, MM) -\>

receive

{MM, ack} -\>

active(Widget, MM);

Other -\>

io:format("chat\_client login unexpected:\~p\~n" ,[Other]),

wait\_login\_response(Widget, MM)

end.

Если всё идёт по плану, то процесс должен получить подтверждающее
сообщение (ack). (В нашем случае это единственно возможный ответ, т. к.
пароль точно был правильным). После получения подтверждения эта функция
вызывает active/2:

HYPERLINK
"http://media.pragprog.com/titles/jaerlang/code/socket\_dist/chat\_client.erl"DownloadHYPERLINK
"http://media.pragprog.com/titles/jaerlang/code/socket\_dist/chat\_client.erl"
HYPERLINK
"http://media.pragprog.com/titles/jaerlang/code/socket\_dist/chat\_client.erl"socketHYPERLINK
"http://media.pragprog.com/titles/jaerlang/code/socket\_dist/chat\_client.erl"\_HYPERLINK
"http://media.pragprog.com/titles/jaerlang/code/socket\_dist/chat\_client.erl"distHYPERLINK
"http://media.pragprog.com/titles/jaerlang/code/socket\_dist/chat\_client.erl"/HYPERLINK
"http://media.pragprog.com/titles/jaerlang/code/socket\_dist/chat\_client.erl"chatHYPERLINK
"http://media.pragprog.com/titles/jaerlang/code/socket\_dist/chat\_client.erl"\_HYPERLINK
"http://media.pragprog.com/titles/jaerlang/code/socket\_dist/chat\_client.erl"clientHYPERLINK
"http://media.pragprog.com/titles/jaerlang/code/socket\_dist/chat\_client.erl".HYPERLINK
"http://media.pragprog.com/titles/jaerlang/code/socket\_dist/chat\_client.erl"erl

active(Widget, MM) -\>

receive

{Widget, Nick, Str} -\>

MM ! {relay, Nick, Str},

active(Widget, MM);

{MM,{msg,From,Pid,Str}} -\>

insert\_str(Widget, [From,"@" ,pid\_to\_list(Pid)," " , Str, "\\n" ]),

active(Widget, MM);

{'EXIT',Widget,windowDestroyed} -\>

MM ! close;

{close, MM} -\>

exit(serverDied);

Other -\>

io:format("chat\_client active unexpected:\~p\~n" ,[Other]),

active(Widget, MM)

end.

active/2 просто шлёт сообщения от виджета к группе (и наоборот) и
отслеживает соединение с группой.

За исключением некоторых объявлений модулей и простейших процедур
форматирования и разбора это завершает чат-клиент.

Полный код чат-клиента приведён на стр. \_\_\_\_

**11.4 Серверная часть**

Серверная часть программы сложнее, чем клиентская. Для каждого клиента
чата есть соответствующий чат-контроллер, который организует
взаимодействие чат-клиента с чат-сервером. Есть единственный чат-сервер,
который знает обо всех сеансах чата в данный момент и ещё есть некоторое
количество менеджеров групп (по одному на чат-группу), которые управляют
отдельными чат-группами.

**Чат-контроллер**

Чат-контроллер — это дополнение (plug-in) для lib\_chan, дистрибутивному
набору, основанному на сокетах. Мы встречали его в главе 10.5,
lib\_chan, на стр. \_\_\_\_. lib\_chan нуждается в конфигурационном
файле и модуле дополнении.

Конфигурационный файл для системы чата следующий:

HYPERLINK
"http://media.pragprog.com/titles/jaerlang/code/socket\_dist/chat.conf"DownloadHYPERLINK
"http://media.pragprog.com/titles/jaerlang/code/socket\_dist/chat.conf"
HYPERLINK
"http://media.pragprog.com/titles/jaerlang/code/socket\_dist/chat.conf"socketHYPERLINK
"http://media.pragprog.com/titles/jaerlang/code/socket\_dist/chat.conf"\_HYPERLINK
"http://media.pragprog.com/titles/jaerlang/code/socket\_dist/chat.conf"distHYPERLINK
"http://media.pragprog.com/titles/jaerlang/code/socket\_dist/chat.conf"/HYPERLINK
"http://media.pragprog.com/titles/jaerlang/code/socket\_dist/chat.conf"chatHYPERLINK
"http://media.pragprog.com/titles/jaerlang/code/socket\_dist/chat.conf".HYPERLINK
"http://media.pragprog.com/titles/jaerlang/code/socket\_dist/chat.conf"conf

{port, 2223}.

{service, chat, password,"AsDT67aQ"
,mfa,mod\_chat\_controller,start,[]}.

Если вы посмотрите назад на код chat\_client.erl, вы увидите, что номер
порта, имя сервиса и пароль согласуются с информацией из
конфигурационного файла.

Модуль чат-контроллера очень прост:

HYPERLINK
"http://media.pragprog.com/titles/jaerlang/code/socket\_dist/mod\_chat\_controller.erl"DownloadHYPERLINK
"http://media.pragprog.com/titles/jaerlang/code/socket\_dist/mod\_chat\_controller.erl"
HYPERLINK
"http://media.pragprog.com/titles/jaerlang/code/socket\_dist/mod\_chat\_controller.erl"socketHYPERLINK
"http://media.pragprog.com/titles/jaerlang/code/socket\_dist/mod\_chat\_controller.erl"\_HYPERLINK
"http://media.pragprog.com/titles/jaerlang/code/socket\_dist/mod\_chat\_controller.erl"distHYPERLINK
"http://media.pragprog.com/titles/jaerlang/code/socket\_dist/mod\_chat\_controller.erl"/HYPERLINK
"http://media.pragprog.com/titles/jaerlang/code/socket\_dist/mod\_chat\_controller.erl"modHYPERLINK
"http://media.pragprog.com/titles/jaerlang/code/socket\_dist/mod\_chat\_controller.erl"\_HYPERLINK
"http://media.pragprog.com/titles/jaerlang/code/socket\_dist/mod\_chat\_controller.erl"chatHYPERLINK
"http://media.pragprog.com/titles/jaerlang/code/socket\_dist/mod\_chat\_controller.erl"\_HYPERLINK
"http://media.pragprog.com/titles/jaerlang/code/socket\_dist/mod\_chat\_controller.erl"controllerHYPERLINK
"http://media.pragprog.com/titles/jaerlang/code/socket\_dist/mod\_chat\_controller.erl".HYPERLINK
"http://media.pragprog.com/titles/jaerlang/code/socket\_dist/mod\_chat\_controller.erl"erl

-module(mod\_chat\_controller).

-export([start/3]).

-import(lib\_chan\_mm, [send/2]).

start(MM, \_, \_) -\>

process\_flag(trap\_exit, true),

io:format("mod\_chat\_controller off we go ...\~p\~n" ,[MM]),

loop(MM).

loop(MM) -\>

receive

{chan, MM, Msg} -\>

chat\_server ! {mm, MM, Msg},

loop(MM);

{'EXIT', MM, \_Why} -\>

chat\_server ! {mm\_closed, MM};

Other -\>

io:format("mod\_chat\_controller unexpected message =\~p (MM=\~p)\~n" ,

[Other, MM]),

loop(MM)

end.

Этот код будет принимать только два сообщения. Когда клиент соединяется
он получит произвольное сообщение и просто отправит его к чат-серверу. С
другой стороны, если сеанс завершается по какой-либо причине, он получит
сообщение о выходе и затем скажет чат-серверу, что клиент умер.

**Чат-сервер**

Чат-сервер — это зарегистрированный процесс, называемый (что
неудивительно) chat\_server. Вызов chat\_server:start/0 запускает и
регистрирует сервер, а он запускает lib\_chan.

HYPERLINK
"http://media.pragprog.com/titles/jaerlang/code/socket\_dist/chat\_server.erl"DownloadHYPERLINK
"http://media.pragprog.com/titles/jaerlang/code/socket\_dist/chat\_server.erl"
HYPERLINK
"http://media.pragprog.com/titles/jaerlang/code/socket\_dist/chat\_server.erl"socketHYPERLINK
"http://media.pragprog.com/titles/jaerlang/code/socket\_dist/chat\_server.erl"\_HYPERLINK
"http://media.pragprog.com/titles/jaerlang/code/socket\_dist/chat\_server.erl"distHYPERLINK
"http://media.pragprog.com/titles/jaerlang/code/socket\_dist/chat\_server.erl"/HYPERLINK
"http://media.pragprog.com/titles/jaerlang/code/socket\_dist/chat\_server.erl"chatHYPERLINK
"http://media.pragprog.com/titles/jaerlang/code/socket\_dist/chat\_server.erl"\_HYPERLINK
"http://media.pragprog.com/titles/jaerlang/code/socket\_dist/chat\_server.erl"serverHYPERLINK
"http://media.pragprog.com/titles/jaerlang/code/socket\_dist/chat\_server.erl".HYPERLINK
"http://media.pragprog.com/titles/jaerlang/code/socket\_dist/chat\_server.erl"erl

start() -\>

start\_server(),

lib\_chan:start\_server("chat.conf" ).

start\_server() -\>

register(chat\_server,

spawn(fun() -\>

process\_flag(trap\_exit, true),

Val= (catch server\_loop([])),

io:format("Server terminated with:\~p\~n" ,[Val])

end)).

Серверный цикл прост. Он ждёт сообщения {login, Group, Nick}HYPERLINK
\\l "id.6415a23dbd50"[3]HYPERLINK \\l "id.6415a23dbd50" от посредника с
PID, равным Channel. Если есть контроллер чат-группы для этой группы, то
он просто посылает сообщение о логине к контроллеру группы, а иначе он
запускает нового контроллера группы.

Чат-сервер — это единственный процесс, который знает PID-ы всех
контроллеров групп, так что, когда делается новое соединение к системе,
к чат-серверу приходит запрос на поиск идентификатора процесса
контроллера группы.

Сам по себе сервер прост:

HYPERLINK
"http://media.pragprog.com/titles/jaerlang/code/socket\_dist/chat\_server.erl"DownloadHYPERLINK
"http://media.pragprog.com/titles/jaerlang/code/socket\_dist/chat\_server.erl"
HYPERLINK
"http://media.pragprog.com/titles/jaerlang/code/socket\_dist/chat\_server.erl"socketHYPERLINK
"http://media.pragprog.com/titles/jaerlang/code/socket\_dist/chat\_server.erl"\_HYPERLINK
"http://media.pragprog.com/titles/jaerlang/code/socket\_dist/chat\_server.erl"distHYPERLINK
"http://media.pragprog.com/titles/jaerlang/code/socket\_dist/chat\_server.erl"/HYPERLINK
"http://media.pragprog.com/titles/jaerlang/code/socket\_dist/chat\_server.erl"chatHYPERLINK
"http://media.pragprog.com/titles/jaerlang/code/socket\_dist/chat\_server.erl"\_HYPERLINK
"http://media.pragprog.com/titles/jaerlang/code/socket\_dist/chat\_server.erl"serverHYPERLINK
"http://media.pragprog.com/titles/jaerlang/code/socket\_dist/chat\_server.erl".HYPERLINK
"http://media.pragprog.com/titles/jaerlang/code/socket\_dist/chat\_server.erl"erl

server\_loop(L) -\>

receive

{mm, Channel, {login, Group, Nick}} -\>

case lookup(Group, L) of

{ok, Pid} -\>

Pid ! {login, Channel, Nick},

server\_loop(L);

error -\>

Pid = spawn\_link(fun() -\>

chat\_group:start(Channel, Nick)

end),

server\_loop([{Group,Pid}|L])

end;

{mm\_closed, \_} -\>

server\_loop(L);

{'EXIT', Pid, allGone} -\>

L1 = remove\_group(Pid, L),

server\_loop(L1);

Msg -\>

io:format("Server received Msg=\~p\~n" ,

[Msg]),

server\_loop(L)

end.

Код для манипуляций списком групп включает в себя несколько простых
подпрограмм для обработки списков:

HYPERLINK
"http://media.pragprog.com/titles/jaerlang/code/socket\_dist/chat\_server.erl"DownloadHYPERLINK
"http://media.pragprog.com/titles/jaerlang/code/socket\_dist/chat\_server.erl"
HYPERLINK
"http://media.pragprog.com/titles/jaerlang/code/socket\_dist/chat\_server.erl"socketHYPERLINK
"http://media.pragprog.com/titles/jaerlang/code/socket\_dist/chat\_server.erl"\_HYPERLINK
"http://media.pragprog.com/titles/jaerlang/code/socket\_dist/chat\_server.erl"distHYPERLINK
"http://media.pragprog.com/titles/jaerlang/code/socket\_dist/chat\_server.erl"/HYPERLINK
"http://media.pragprog.com/titles/jaerlang/code/socket\_dist/chat\_server.erl"chatHYPERLINK
"http://media.pragprog.com/titles/jaerlang/code/socket\_dist/chat\_server.erl"\_HYPERLINK
"http://media.pragprog.com/titles/jaerlang/code/socket\_dist/chat\_server.erl"serverHYPERLINK
"http://media.pragprog.com/titles/jaerlang/code/socket\_dist/chat\_server.erl".HYPERLINK
"http://media.pragprog.com/titles/jaerlang/code/socket\_dist/chat\_server.erl"erl

lookup(G, [{G,Pid}|\_]) -\> {ok, Pid};

lookup(G, [\_|T])

-\> lookup(G, T);

lookup(\_,[])

-\> error.

remove\_group(Pid, [{G,Pid}|T]) -\> io:format("\~p removed\~n" ,[G]), T;

remove\_group(Pid, [H|T])

-\> [H|remove\_group(Pid, T)];

remove\_group(\_, [])

-\> [].

**Менеджер группы**

К текущему моменту всё, что осталось — это менеджер группы. Важнейшая
часть этого — диспетчер.

HYPERLINK
"http://media.pragprog.com/titles/jaerlang/code/socket\_dist/chat\_group.erl"DownloadHYPERLINK
"http://media.pragprog.com/titles/jaerlang/code/socket\_dist/chat\_group.erl"
HYPERLINK
"http://media.pragprog.com/titles/jaerlang/code/socket\_dist/chat\_group.erl"socketHYPERLINK
"http://media.pragprog.com/titles/jaerlang/code/socket\_dist/chat\_group.erl"\_HYPERLINK
"http://media.pragprog.com/titles/jaerlang/code/socket\_dist/chat\_group.erl"distHYPERLINK
"http://media.pragprog.com/titles/jaerlang/code/socket\_dist/chat\_group.erl"/HYPERLINK
"http://media.pragprog.com/titles/jaerlang/code/socket\_dist/chat\_group.erl"chatHYPERLINK
"http://media.pragprog.com/titles/jaerlang/code/socket\_dist/chat\_group.erl"\_HYPERLINK
"http://media.pragprog.com/titles/jaerlang/code/socket\_dist/chat\_group.erl"groupHYPERLINK
"http://media.pragprog.com/titles/jaerlang/code/socket\_dist/chat\_group.erl".HYPERLINK
"http://media.pragprog.com/titles/jaerlang/code/socket\_dist/chat\_group.erl"erl

group\_controller([]) -\>

exit(allGone);

group\_controller(L) -\>

receive

{C, {relay, Nick, Str}} -\>

foreach(fun({Pid,\_}) -\> Pid ! {msg, Nick, C, Str} end, L),

group\_controller(L);

{login, C, Nick} -\>

controller(C, self()),

C ! ack,

self() ! {C, {relay, Nick, "I'm joining the group" }},

group\_controller([{C,Nick}|L]);

{close,C} -\>

{Nick, L1} = delete(C, L, []),

self() ! {C, {relay, Nick, "I'm leaving the group" }},

group\_controller(L1);

Any -\>

io:format("group controller received Msg=\~p\~n" , [Any]),

group\_controller(L)

end.

Аргумент L в group\_controller(L) — это список имён и идентификаторов
процессов посредников {Pid, Nick}.

Когда менеджер группы получает сообщение {relay, Nick, Str}, он просто
рассылает его всем процессам в группе. Если приходит сообщение {login,
C, Nick}, он добавляет кортеж {C, Nick} в список рассылки. *Важно*
упомянуть вызов lib\_chan\_mm:controller/2. Этот вызов устанавливает
управляющий процесс посредника в контроллер группы, что означает, что
*все сообщения к сокету, управляемому посредником, будут посланы к
контроллеру группы* — это, вероятно, главная часть для понимания — как
работает весь этот код.

Всё, что остаётся — это код, который запускает сервер группы:

HYPERLINK
"http://media.pragprog.com/titles/jaerlang/code/socket\_dist/chat\_group.erl"DownloadHYPERLINK
"http://media.pragprog.com/titles/jaerlang/code/socket\_dist/chat\_group.erl"
HYPERLINK
"http://media.pragprog.com/titles/jaerlang/code/socket\_dist/chat\_group.erl"socketHYPERLINK
"http://media.pragprog.com/titles/jaerlang/code/socket\_dist/chat\_group.erl"\_HYPERLINK
"http://media.pragprog.com/titles/jaerlang/code/socket\_dist/chat\_group.erl"distHYPERLINK
"http://media.pragprog.com/titles/jaerlang/code/socket\_dist/chat\_group.erl"/HYPERLINK
"http://media.pragprog.com/titles/jaerlang/code/socket\_dist/chat\_group.erl"chatHYPERLINK
"http://media.pragprog.com/titles/jaerlang/code/socket\_dist/chat\_group.erl"\_HYPERLINK
"http://media.pragprog.com/titles/jaerlang/code/socket\_dist/chat\_group.erl"groupHYPERLINK
"http://media.pragprog.com/titles/jaerlang/code/socket\_dist/chat\_group.erl".HYPERLINK
"http://media.pragprog.com/titles/jaerlang/code/socket\_dist/chat\_group.erl"erl

-module(chat\_group).

-import(lib\_chan\_mm, [send/2, controller/2]).

-import(lists, [foreach/2, reverse/2]).

-export([start/2]).

start(C, Nick) -\>

process\_flag(trap\_exit, true),

controller(C, self()),

C ! ack,

self() ! {C, {relay, Nick, "I'm starting the group" }},

group\_controller([{C,Nick}]).

и функция delete/3, вызываемая из цикла диспетчера процесса

HYPERLINK
"http://media.pragprog.com/titles/jaerlang/code/socket\_dist/chat\_group.erl"DownloadHYPERLINK
"http://media.pragprog.com/titles/jaerlang/code/socket\_dist/chat\_group.erl"
HYPERLINK
"http://media.pragprog.com/titles/jaerlang/code/socket\_dist/chat\_group.erl"socketHYPERLINK
"http://media.pragprog.com/titles/jaerlang/code/socket\_dist/chat\_group.erl"\_HYPERLINK
"http://media.pragprog.com/titles/jaerlang/code/socket\_dist/chat\_group.erl"distHYPERLINK
"http://media.pragprog.com/titles/jaerlang/code/socket\_dist/chat\_group.erl"/HYPERLINK
"http://media.pragprog.com/titles/jaerlang/code/socket\_dist/chat\_group.erl"chatHYPERLINK
"http://media.pragprog.com/titles/jaerlang/code/socket\_dist/chat\_group.erl"\_HYPERLINK
"http://media.pragprog.com/titles/jaerlang/code/socket\_dist/chat\_group.erl"groupHYPERLINK
"http://media.pragprog.com/titles/jaerlang/code/socket\_dist/chat\_group.erl".HYPERLINK
"http://media.pragprog.com/titles/jaerlang/code/socket\_dist/chat\_group.erl"erl

delete(Pid, [{Pid,Nick}|T], L) -\> {Nick, reverse(T, L)};

delete(Pid, [H|T], L)

-\> delete(Pid, T, [H|L]);

delete(\_, [], L)

-\> {"????" , L}.

**11.5 Запуск приложения**

Приложение целиком располагается в каталоге pathto/code/socket\_dist.
Оно также использует некоторые библиотечные модули из каталога
pathto/code.

Для запуска приложения получите исходные коды веб-сайта этой книги и
распакуйте их в какой-нибудь каталог. (Здесь мы предполагаем, что это
каталог /home/joe/erlbook). Откройте окно терминала и выполните
следующие команды:

$ cd /home/joe/erlbook/code

/home/joe/erlbook/code $ make

...

/home/joe/erlbook/code $ cd socket\_dist

/home/joe/erlbook/code/socket\_dist $ make chat\_server

...

Это запустит чат-сервер. А теперь нам надо открыть другое терминальное
окно и запустить тест клиента:

$ cd /home/joe/erlbook/code/socket\_dist

/home/joe/erlbook/code/socket\_dist $ make chat\_client

...

Запуск make chat\_client выполняет функцию chat\_client:test(). Это на
самом деле создаёт четыре окна, которые подключаются к тестовой группе,
названной «general». На Рис.11. 4 мы можем увидеть снимок экрана,
показывающий как выглядит система после выдачи этих команд.



*Рис.11. 4: Снимок экрана, показывающий четыре окна, подключенные к
одной группе*

**

Для развёртывания системы в Интернете всё, что нам надо сделать — это
поменять пароль и порт на что-нибудь подходящее и разрешить входящие
соединения на порт, который мы выбрали.

**11.6 Исходные коды программы чата**

Итак, мы завершили описание программы чата. При описании программы мы
разбили её на несколько маленьких фрагментов и опустили некоторую часть
кода. Этот раздел содержит весь код в одном месте, что облегчает его
чтение. Если у вас есть трудности с пониманием, что делает та или иная
часть кода, обратитесь к описанию, изложенному ранее в этой главе.

**Чат-клиент**

HYPERLINK
"http://media.pragprog.com/titles/jaerlang/code/socket\_dist/chat\_client.erl"DownloadHYPERLINK
"http://media.pragprog.com/titles/jaerlang/code/socket\_dist/chat\_client.erl"
HYPERLINK
"http://media.pragprog.com/titles/jaerlang/code/socket\_dist/chat\_client.erl"socketHYPERLINK
"http://media.pragprog.com/titles/jaerlang/code/socket\_dist/chat\_client.erl"\_HYPERLINK
"http://media.pragprog.com/titles/jaerlang/code/socket\_dist/chat\_client.erl"distHYPERLINK
"http://media.pragprog.com/titles/jaerlang/code/socket\_dist/chat\_client.erl"/HYPERLINK
"http://media.pragprog.com/titles/jaerlang/code/socket\_dist/chat\_client.erl"chatHYPERLINK
"http://media.pragprog.com/titles/jaerlang/code/socket\_dist/chat\_client.erl"\_HYPERLINK
"http://media.pragprog.com/titles/jaerlang/code/socket\_dist/chat\_client.erl"clientHYPERLINK
"http://media.pragprog.com/titles/jaerlang/code/socket\_dist/chat\_client.erl".HYPERLINK
"http://media.pragprog.com/titles/jaerlang/code/socket\_dist/chat\_client.erl"erl

-module(chat\_client).

-import(io\_widget,

[get\_state/1, insert\_str/2, set\_prompt/2, set\_state/2,

set\_title/2, set\_handler/2, update\_state/3]).

-export([start/0, test/0, connect/5]).

start() -\>

connect("localhost" , 2223, "AsDT67aQ" , "general" , "joe" ).

test() -\>

connect("localhost" , 2223, "AsDT67aQ" , "general" , "joe" ),

connect("localhost" , 2223, "AsDT67aQ" , "general" , "jane" ),

connect("localhost" , 2223, "AsDT67aQ" , "general" , "jim" ),

connect("localhost" , 2223, "AsDT67aQ" , "general" , "sue" ).

connect(Host, Port, HostPsw, Group, Nick) -\>

spawn(fun() -\> handler(Host, Port, HostPsw, Group, Nick) end).

handler(Host, Port, HostPsw, Group, Nick) -\>

process\_flag(trap\_exit, true),

Widget = io\_widget:start(self()),

set\_title(Widget, Nick),

set\_state(Widget, Nick),

set\_prompt(Widget, [Nick, " \> " ]),

set\_handler(Widget, fun parse\_command/1),

start\_connector(Host, Port, HostPsw),

disconnected(Widget, Group, Nick).

disconnected(Widget, Group, Nick) -\>

receive

{connected, MM} -\>

insert\_str(Widget, "connected to server\\nsending data\\n" ),

MM ! {login, Group, Nick},

wait\_login\_response(Widget, MM);

{Widget, destroyed} -\>

exit(died);

{status, S} -\>

insert\_str(Widget, to\_str(S)),

disconnected(Widget, Group, Nick);

Other -\>

io:format("chat\_client disconnected unexpected:\~p\~n" ,[Other]),

disconnected(Widget, Group, Nick)

end.

wait\_login\_response(Widget, MM) -\>

receive

{MM, ack} -\>

active(Widget, MM);

Other -\>

io:format("chat\_client login unexpected:\~p\~n" ,[Other]),

wait\_login\_response(Widget, MM)

end.

active(Widget, MM) -\>

receive

{Widget, Nick, Str} -\>

MM ! {relay, Nick, Str},

active(Widget, MM);

{MM,{msg,From,Pid,Str}} -\>

insert\_str(Widget, [From,"@" ,pid\_to\_list(Pid)," " , Str, "\\n" ]),

active(Widget, MM);

{'EXIT',Widget,windowDestroyed} -\>

MM ! close;

{close, MM} -\>

exit(serverDied);

Other -\>

io:format("chat\_client active unexpected:\~p\~n" ,[Other]),

active(Widget, MM)

end.

start\_connector(Host, Port, Pwd) -\>

S = self(),

spawn\_link(fun() -\> try\_to\_connect(S, Host, Port, Pwd) end).

try\_to\_connect(Parent, Host, Port, Pwd) -\>

%% Parent is the Pid of the process that spawned this process

case lib\_chan:connect(Host, Port, chat, Pwd, []) of

{error, \_Why} -\>

Parent ! {status, {cannot, connect, Host, Port}},

sleep(2000),

try\_to\_connect(Parent, Host, Port, Pwd);

{ok, MM} -\>

lib\_chan\_mm:controller(MM, Parent),

Parent ! {connected, MM},

exit(connectorFinished)

end.

sleep(T) -\>

receive

after T -\> true

end.

to\_str(Term) -\>

io\_lib:format("\~p\~n" ,[Term]).

parse\_command(Str) -\> skip\_to\_gt(Str).

skip\_to\_gt("\>" ++ T) -\> T;

skip\_to\_gt([\_|T])

-\> skip\_to\_gt(T);

skip\_to\_gt([])

-\> exit("no \>" ).

**Конфигурация lib\_chan**

HYPERLINK
"http://media.pragprog.com/titles/jaerlang/code/socket\_dist/chat.conf"DownloadHYPERLINK
"http://media.pragprog.com/titles/jaerlang/code/socket\_dist/chat.conf"
HYPERLINK
"http://media.pragprog.com/titles/jaerlang/code/socket\_dist/chat.conf"socketHYPERLINK
"http://media.pragprog.com/titles/jaerlang/code/socket\_dist/chat.conf"\_HYPERLINK
"http://media.pragprog.com/titles/jaerlang/code/socket\_dist/chat.conf"distHYPERLINK
"http://media.pragprog.com/titles/jaerlang/code/socket\_dist/chat.conf"/HYPERLINK
"http://media.pragprog.com/titles/jaerlang/code/socket\_dist/chat.conf"chatHYPERLINK
"http://media.pragprog.com/titles/jaerlang/code/socket\_dist/chat.conf".HYPERLINK
"http://media.pragprog.com/titles/jaerlang/code/socket\_dist/chat.conf"conf

{port, 2223}.

{service, chat, password,"AsDT67aQ"
,mfa,mod\_chat\_controller,start,[]}.

**Чат-контроллер**

HYPERLINK
"http://media.pragprog.com/titles/jaerlang/code/socket\_dist/mod\_chat\_controller.erl"DownloadHYPERLINK
"http://media.pragprog.com/titles/jaerlang/code/socket\_dist/mod\_chat\_controller.erl"
HYPERLINK
"http://media.pragprog.com/titles/jaerlang/code/socket\_dist/mod\_chat\_controller.erl"socketHYPERLINK
"http://media.pragprog.com/titles/jaerlang/code/socket\_dist/mod\_chat\_controller.erl"\_HYPERLINK
"http://media.pragprog.com/titles/jaerlang/code/socket\_dist/mod\_chat\_controller.erl"distHYPERLINK
"http://media.pragprog.com/titles/jaerlang/code/socket\_dist/mod\_chat\_controller.erl"/HYPERLINK
"http://media.pragprog.com/titles/jaerlang/code/socket\_dist/mod\_chat\_controller.erl"modHYPERLINK
"http://media.pragprog.com/titles/jaerlang/code/socket\_dist/mod\_chat\_controller.erl"\_HYPERLINK
"http://media.pragprog.com/titles/jaerlang/code/socket\_dist/mod\_chat\_controller.erl"chatHYPERLINK
"http://media.pragprog.com/titles/jaerlang/code/socket\_dist/mod\_chat\_controller.erl"\_HYPERLINK
"http://media.pragprog.com/titles/jaerlang/code/socket\_dist/mod\_chat\_controller.erl"controllerHYPERLINK
"http://media.pragprog.com/titles/jaerlang/code/socket\_dist/mod\_chat\_controller.erl".HYPERLINK
"http://media.pragprog.com/titles/jaerlang/code/socket\_dist/mod\_chat\_controller.erl"erl

-module(mod\_chat\_controller).

-export([start/3]).

-import(lib\_chan\_mm, [send/2]).

start(MM, \_, \_) -\>

process\_flag(trap\_exit, true),

io:format("mod\_chat\_controller off we go ...\~p\~n" ,[MM]),

loop(MM).

loop(MM) -\>

receive

{chan, MM, Msg} -\>

chat\_server ! {mm, MM, Msg},

loop(MM);

{'EXIT', MM, \_Why} -\>

chat\_server ! {mm\_closed, MM};

Other -\>

io:format("mod\_chat\_controller unexpected message =\~p (MM=\~p)\~n" ,

[Other, MM]),

loop(MM)

end.

**Чат-сервер**

HYPERLINK
"http://media.pragprog.com/titles/jaerlang/code/socket\_dist/chat\_server.erl"DownloadHYPERLINK
"http://media.pragprog.com/titles/jaerlang/code/socket\_dist/chat\_server.erl"
HYPERLINK
"http://media.pragprog.com/titles/jaerlang/code/socket\_dist/chat\_server.erl"socketHYPERLINK
"http://media.pragprog.com/titles/jaerlang/code/socket\_dist/chat\_server.erl"\_HYPERLINK
"http://media.pragprog.com/titles/jaerlang/code/socket\_dist/chat\_server.erl"distHYPERLINK
"http://media.pragprog.com/titles/jaerlang/code/socket\_dist/chat\_server.erl"/HYPERLINK
"http://media.pragprog.com/titles/jaerlang/code/socket\_dist/chat\_server.erl"chatHYPERLINK
"http://media.pragprog.com/titles/jaerlang/code/socket\_dist/chat\_server.erl"\_HYPERLINK
"http://media.pragprog.com/titles/jaerlang/code/socket\_dist/chat\_server.erl"serverHYPERLINK
"http://media.pragprog.com/titles/jaerlang/code/socket\_dist/chat\_server.erl".HYPERLINK
"http://media.pragprog.com/titles/jaerlang/code/socket\_dist/chat\_server.erl"erl

-module(chat\_server).

-import(lib\_chan\_mm, [send/2, controller/2]).

-import(lists, [delete/2, foreach/2, map/2, member/2,reverse/2]).

-compile(export\_all).

start() -\>

start\_server(),

lib\_chan:start\_server("chat.conf" ).

start\_server() -\>

register(chat\_server,

spawn(fun() -\>

process\_flag(trap\_exit, true),

Val= (catch server\_loop([])),

io:format("Server terminated with:\~p\~n" ,[Val])

end)).

server\_loop(L) -\>

receive

{mm, Channel, {login, Group, Nick}} -\>

case lookup(Group, L) of

{ok, Pid} -\>

Pid ! {login, Channel, Nick},

server\_loop(L);

error -\>

Pid = spawn\_link(fun() -\>

chat\_group:start(Channel, Nick)

end),

server\_loop([{Group,Pid}|L])

end;

{mm\_closed, \_} -\>

server\_loop(L);

{'EXIT', Pid, allGone} -\>

L1 = remove\_group(Pid, L),

server\_loop(L1);

Msg -\>

io:format("Server received Msg=\~p\~n" ,

[Msg]),

server\_loop(L)

end.

lookup(G, [{G,Pid}|\_]) -\> {ok, Pid};

lookup(G, [\_|T])

-\> lookup(G, T);

lookup(\_,[])

-\> error.

remove\_group(Pid, [{G,Pid}|T]) -\> io:format("\~p removed\~n" ,[G]), T;

remove\_group(Pid, [H|T])

-\> [H|remove\_group(Pid, T)];

remove\_group(\_, [])

-\> [].

**Чат-группы**

HYPERLINK
"http://media.pragprog.com/titles/jaerlang/code/socket\_dist/chat\_group.erl"DownloadHYPERLINK
"http://media.pragprog.com/titles/jaerlang/code/socket\_dist/chat\_group.erl"
HYPERLINK
"http://media.pragprog.com/titles/jaerlang/code/socket\_dist/chat\_group.erl"socketHYPERLINK
"http://media.pragprog.com/titles/jaerlang/code/socket\_dist/chat\_group.erl"\_HYPERLINK
"http://media.pragprog.com/titles/jaerlang/code/socket\_dist/chat\_group.erl"distHYPERLINK
"http://media.pragprog.com/titles/jaerlang/code/socket\_dist/chat\_group.erl"/HYPERLINK
"http://media.pragprog.com/titles/jaerlang/code/socket\_dist/chat\_group.erl"chatHYPERLINK
"http://media.pragprog.com/titles/jaerlang/code/socket\_dist/chat\_group.erl"\_HYPERLINK
"http://media.pragprog.com/titles/jaerlang/code/socket\_dist/chat\_group.erl"groupHYPERLINK
"http://media.pragprog.com/titles/jaerlang/code/socket\_dist/chat\_group.erl".HYPERLINK
"http://media.pragprog.com/titles/jaerlang/code/socket\_dist/chat\_group.erl"erl

-module(chat\_group).

-import(lib\_chan\_mm, [send/2, controller/2]).

-import(lists, [foreach/2, reverse/2]).

-export([start/2]).

start(C, Nick) -\>

process\_flag(trap\_exit, true),

controller(C, self()),

C ! ack,

self() ! {C, {relay, Nick, "I'm starting the group" }},

group\_controller([{C,Nick}]).

delete(Pid, [{Pid,Nick}|T], L) -\> {Nick, reverse(T, L)};

delete(Pid, [H|T], L)

-\> delete(Pid, T, [H|L]);

delete(\_, [], L)

-\> {"????" , L}.

group\_controller([]) -\>

exit(allGone);

group\_controller(L) -\>

receive

{C, {relay, Nick, Str}} -\>

foreach(fun({Pid,\_}) -\> Pid ! {msg, Nick, C, Str} end, L),

group\_controller(L);

{login, C, Nick} -\>

controller(C, self()),

C ! ack,

self() ! {C, {relay, Nick, "I'm joining the group" }},

group\_controller([{C,Nick}|L]);

{close,C} -\>

{Nick, L1} = delete(C, L, []),

self() ! {C, {relay, Nick, "I'm leaving the group" }},

group\_controller(L1);

Any -\>

io:format("group controller received Msg=\~p\~n" , [Any]),

group\_controller(L)

end.

**Виджет ввода-вывода**

HYPERLINK
"http://media.pragprog.com/titles/jaerlang/code/socket\_dist/io\_widget.erl"DownloadHYPERLINK
"http://media.pragprog.com/titles/jaerlang/code/socket\_dist/io\_widget.erl"
HYPERLINK
"http://media.pragprog.com/titles/jaerlang/code/socket\_dist/io\_widget.erl"socketHYPERLINK
"http://media.pragprog.com/titles/jaerlang/code/socket\_dist/io\_widget.erl"\_HYPERLINK
"http://media.pragprog.com/titles/jaerlang/code/socket\_dist/io\_widget.erl"distHYPERLINK
"http://media.pragprog.com/titles/jaerlang/code/socket\_dist/io\_widget.erl"/HYPERLINK
"http://media.pragprog.com/titles/jaerlang/code/socket\_dist/io\_widget.erl"ioHYPERLINK
"http://media.pragprog.com/titles/jaerlang/code/socket\_dist/io\_widget.erl"\_HYPERLINK
"http://media.pragprog.com/titles/jaerlang/code/socket\_dist/io\_widget.erl"widgetHYPERLINK
"http://media.pragprog.com/titles/jaerlang/code/socket\_dist/io\_widget.erl".HYPERLINK
"http://media.pragprog.com/titles/jaerlang/code/socket\_dist/io\_widget.erl"erl

-module(io\_widget).

-export([get\_state/1,

start/1, test/0,

set\_handler/2,

set\_prompt/2,

set\_state/2,

set\_title/2, insert\_str/2, update\_state/3]).

start(Pid) -\>

gs:start(),

spawn\_link(fun() -\> widget(Pid) end).

get\_state(Pid)

-\> rpc(Pid, get\_state).

set\_title(Pid, Str)

-\> Pid ! {title, Str}.

set\_handler(Pid, Fun)

-\> Pid ! {handler, Fun}.

set\_prompt(Pid, Str)

-\> Pid ! {prompt, Str}.

set\_state(Pid, State)

-\> Pid ! {state, State}.

insert\_str(Pid, Str)

-\> Pid ! {insert, Str}.

update\_state(Pid, N, X) -\> Pid ! {updateState, N, X}.

rpc(Pid, Q) -\>

Pid ! {self(), Q},

receive

{Pid, R} -\>

R

end.

widget(Pid) -\>

Size = [{width,500},{height,200}],

Win = gs:window(gs:start(),

[{map,true},{configure,true},{title,"window" }|Size]),

gs:frame(packer, Win,[{packer\_x, [{stretch,1,500}]},

{packer\_y, [{stretch,10,120,100},

{stretch,1,15,15}]}]),

gs:create(editor,editor,packer,
[{pack\_x,1},{pack\_y,1},{vscroll,right}]),

gs:create(entry, entry, packer,
[{pack\_x,1},{pack\_y,2},{keypress,true}]),

gs:config(packer, Size),

Prompt = " \> " ,

State = nil,

gs:config(entry, {insert,{0,Prompt}}),

loop(Win, Pid, Prompt, State, fun parse/1).

loop(Win, Pid, Prompt, State, Parse) -\>

receive

{From, get\_state} -\>

From ! {self(), State},

loop(Win, Pid, Prompt, State, Parse);

{handler, Fun} -\>

loop(Win, Pid, Prompt, State, Fun);

{prompt, Str} -\>

%% this clobbers the line being input ...

%% this could be fixed - hint

gs:config(entry, {delete,{0,last}}),

gs:config(entry, {insert,{0,Str}}),

loop(Win, Pid, Str, State, Parse);

{state, S} -\>

loop(Win, Pid, Prompt, S, Parse);

{title, Str} -\>

gs:config(Win, [{title, Str}]),

loop(Win, Pid, Prompt, State, Parse);

{insert, Str} -\>

gs:config(editor, {insert,{'end',Str}}),

scroll\_to\_show\_last\_line(),

loop(Win, Pid, Prompt, State, Parse);

{updateState, N, X} -\>

io:format("setelemtn N=\~p X=\~p Satte=\~p\~n" ,[N,X,State]),

State1 = setelement(N, State, X),

loop(Win, Pid, Prompt, State1, Parse);

{gs,\_,destroy,\_,\_} -\>

io:format("Destroyed\~n" ,[]),

exit(windowDestroyed);

{gs, entry,keypress,\_,['Return'|\_]} -\>

Text = gs:read(entry, text),

%% io:format("Read:\~p\~n",[Text]),

gs:config(entry, {delete,{0,last}}),

gs:config(entry, {insert,{0,Prompt}}),

try Parse(Text) of

Term -\>

Pid ! {self(), State, Term}

catch

\_:\_ -\>

self() ! {insert, "\*\* bad input\*\*\\n\*\* /h for help\\n" }

end,

loop(Win, Pid, Prompt, State, Parse);

{gs,\_,configure,[],[W,H,\_,\_]} -\>

gs:config(packer, [{width,W},{height,H}]),

loop(Win, Pid, Prompt, State, Parse);

{gs, entry,keypress,\_,\_} -\>

loop(Win, Pid, Prompt, State, Parse);

Any -\>

io:format("Discarded:\~p\~n" ,[Any]),

loop(Win, Pid, Prompt, State, Parse)

end.

scroll\_to\_show\_last\_line() -\>

Size

= gs:read(editor, size),

Height

= gs:read(editor, height),

CharHeight = gs:read(editor, char\_height),

TopRow

= Size - Height/CharHeight,

if

TopRow \> 0 -\> gs:config(editor, {vscrollpos, TopRow});

true

-\> gs:config(editor, {vscrollpos, 0})

end.

test() -\>

spawn(fun() -\> test1() end).

test1() -\>

W = io\_widget:start(self()),

io\_widget:set\_title(W, "Test window" ),

loop(W).

loop(W) -\>

receive

{W, {str, Str}} -\>

Str1 = Str ++ "\\n" ,

io\_widget:insert\_str(W, Str1),

loop(W)

end.

parse(Str) -\>

{str, Str}.

**11.7 Упражнения**

● улучшите графический виджет, добавив боковую панель для перечисления
имён участников группы

● добавьте код для показа имён всех участников группы

● добавьте код для перечисления всех групп

● добавьте личные сообщения

● добавьте такой код, чтобы контроллер группы работал не на серверной
машине, а на машине первого пользователя, который подключился к данной
группе

● Посмотрите внимательно на диаграмму последовательности сообщений
(Рис.11. 2), чтобы убедиться, что вы понимаете её и проверьте, что вы
можете указать все сообщения из диаграммы в программном коде

● нарисуйте свою собственную диаграмму последовательности сообщений,
чтобы показать, как решается проблема в фазе логина (в оригинале «фаза
логина проблемы»)

HYPERLINK \\l "id.0b9a32f8a799"[1]HYPERLINK \\l "id.0b9a32f8a799" Это
облегчает нам жизнь и позволяет сосредоточиться на приложении вместо
низкоуровневых деталей протокола.

HYPERLINK \\l "id.64ae7d66b7a1"[2]HYPERLINK \\l "id.64ae7d66b7a1" MM
означает middle man — посредник. Это процесс, который используется для
связи с сервером.

HYPERLINK \\l "id.3da754ac9620"[3]HYPERLINK \\l "id.3da754ac9620" Nick —
это прозвище пользователя
