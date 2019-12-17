#Описание
Сервис предназначен для разделения одной (или нескольких) ККТ Атол между несколькими кассовыми местами
При подключении к нему нескольких ККТ выполнение команды на одном из них, блокирует выполнение команды для другом (команды даже при наличии нескольких ККТ не могут выполнятся одновременно на разных устройствах)
Для работы требует установленного драйвера АТОЛ версии 8.16.05 или выше (более высокая версия не тестировалась)
Межпроцессное взаимодействие идет через именованный канал с использованием собственного протокола, основанного на формате json
Возможно сетевое взаимодействие через этот канал
Сервис легко модифицируется для работы по HTTP
Настройки сервиса лежат в файле ini.ini
Тестирование работы сервиса можно и нужно производить на нефискализированных ККТ Атол, поддерживаемым драйвером

#Формат передаваемых данных

##Запросы

###Информация о состоянии устройства
```json
{
    "atol_device_data":{
        "atol_device_number":1,
        "atol_operator_password":"30"
    },
    "comand":"CASH_REGISTER_INF",
    "data":""
}
```
atol_device_number - номер устройства (из ini.ini) на котором требуется выполнить команду, указанную в поле comand
atol_operator_password - пароль из этого поля будет использован для выполнения команды

Пример ответа:
```json
{
  "response": {
    "KKMDateTime": "15.06.2018 00:04:22",
    "KKMEarnings": 0,
    "KKMEarningSession": 0,
    "KKMSum": 0,
    "Mode": 4,
    "SerialNumber": "00106201273705",
    "CheckFontSizeString": "7x5",
    "CheckLineSpacing": 3,
    "SessionNumber": 0,
    "isFiskal": true,
    "isSessionExceedLimit24": false,
    "isSessionOpened": false,
    "CashRegisterNum": 1
  },
  "error": "",
  "error_code": 0
}
```

error_code=0 означает отсутствие ошибки, т.е. команда была успешно выполнена на ККТ

###Печать тестового чека (не фискального)
```json
{
    "atol_device_data":{
        "atol_device_number":1,
        "atol_operator_password":"3"
    },
    "comand":"PRINT_TEXT_CHEQUE",
    "data":""
}
```
Пример ответа:
```json
{
  "response": "готово",
  "error": "",
  "error_code": 0
}
```

###Печать текста
Можно печатать и несколько строк, разделяя их символами \r\n
Всё будет напечатано одним шрифтом по-умолчанию
```json
{
  "atol_device_data": {
    "atol_device_number": 1,
    "atol_operator_password": "3"
  },
  "comand": "PRINT_STRING_LIST",
  "data": {
    "text": "         Магазин \"Детский Мир\"          \r\n        Москва,ул.Вавилова,д.19,        \r\n             тел. 123-4567              \r\n                                        \r\n24.12.14                           10:41\r\n                  ЧЕК                   \r\n                 Оплата                 \r\nНомер операции:                     0007\r\nТерминал:                       00859916\r\nПункт обслуживания:         854444445555\r\n                   Visa                 \r\nКарта:(D)               ************0002\r\nКлиент:                                 \r\n                                        \r\nСумма (Руб):                            \r\n                4.28                    \r\nКомиссия за операцию - 0 руб.           \r\nОДОБРЕНО   Код авторизации:       12Y561\r\n                                        \r\n                                        \r\n                                        \r\n   ________________________________     \r\n            подпись клиента             \r\n                                        \r\n                                        \r\n   _________________________________    \r\n      подпись кассира(контролера)       \r\n2FEBABF549641069E2C9DCAC831DD04373B24A83\r\n========================================",
    "setmode_one": true,
    "use_print_string": true
  }
}
```

###Открытие смены
```json
{
  "atol_device_data": {
    "atol_device_number": 1,
    "atol_operator_password": "3"
  },
  "comand": "OPEN_SESSION",
  "data": ""
}
```

###Печать строк текста
Каждая строка может иметь собственный формат текста и шрифта
```json
{
  "atol_device_data": {
    "atol_device_number": 1,
    "atol_operator_password": "3"
  },
  "comand": "PRINT_TSTRING_LIST",
  "data": {
    "text": [
      {
        "text": "По центру",
        "font": {
          "b": false,
          "i": false,
          "n": false,
          "u": false,
          "w": 0,
          "a": 1
        }
      },
      {
        "text": "Слева",
        "font": {
          "b": false,
          "i": false,
          "n": false,
          "u": false,
          "w": 0,
          "a": 0
        }
      },
      {
        "text": "Справа",
        "font": {
          "b": false,
          "i": false,
          "n": false,
          "u": false,
          "w": 0,
          "a": 2
        }
      },
      {
        "text": "Жирно И Наклонно",
        "font": {
          "b": false,
          "i": true,
          "n": false,
          "u": false,
          "w": 0,
          "a": 0
        }
      },
      {
        "text": "Инверсия",
        "font": {
          "b": false,
          "i": false,
          "n": true,
          "u": false,
          "w": 0,
          "a": 0
        }
      }
    ],
    "setmode_one": false,
    "use_print_string": false
  }
}
```
Кодирование параметров шрифта такое:
* b - жирно (true|false)
* i - курсив (true|false)
* n - белый текст на темном фоне (true|false)
* u - подчеркнуто (true|false)
* w - ширина символов (0 - обычная)
* a - выравнивание (0 - слева, 1 - по центру, 2 - справа)

use_print_string - определяет, использовать ли параметры шрифта, заданные для каждой строки в поле font (true|false)

###Снятие X-отчёта (без гашения)
```json
{
  "atol_device_data": {
    "atol_device_number": 1,
    "atol_operator_password": "3"
  },
  "comand": "XREPORT",
  "data": ""
}
```

###Снятие Z-отчёта (с гашением)
```json
{
  "atol_device_data": {
    "atol_device_number": 1,
    "atol_operator_password": "3"
  },
  "comand": "ZREPORT",
  "data": ""
}
```

###Сброс основных параметров ККТ (обнуление итогов и т.п.)
```json
{
  "atol_device_data": {
    "atol_device_number": 1,
    "atol_operator_password": "3"
  },
  "comand": "RESET_SUMMARY",
  "data": ""
}
```

###Заводской сброс ККТ
```json
{
  "atol_device_data": {
    "atol_device_number": 1,
    "atol_operator_password": "3"
  },
  "comand": "TECH_RESET",
  "data": ""
}
```

###Положить наличные в кассу
```json
{
  "atol_device_data": {
    "atol_device_number": 1,
    "atol_operator_password": "3"
  },
  "comand": "CASH_INCOME",
  "data": 100
}
```

###Взять наличные из кассы
```json
{
  "atol_device_data": {
    "atol_device_number": 1,
    "atol_operator_password": "3"
  },
  "comand": "CASH_OUTCOME",
  "data": 100.00
}
```
data - сумма, взятая из кассы, может иметь копейки в виде дробной части

###Фискализировать продажу и напечатать чек
```json
{
  "atol_device_data": {
    "atol_device_number": 1,
    "atol_operator_password": "3"
  },
  "comand": "INCOME",
  "data": {
    "header_text": [
      {
        "text": "ЗАО Рога и Копыта",
        "font": {
          "b": false,
          "i": false,
          "n": false,
          "u": false,
          "dh": false,
          "dw": false,
          "w": 0,
          "a": 1
        }
      },
      {
        "text": "ИНН 6600023456",
        "font": {
          "b": false,
          "i": false,
          "n": false,
          "u": false,
          "dh": false,
          "dw": false,
          "w": 0,
          "a": 2
        }
      }
    ],
    "goods": [
      {
        "name": "Аллергодил \\глазные капли\\ (флак; №1; 6мл; 0,05%)",
        "price": 367,
        "kol": 1,
        "nds": "nds10",
        "barcode": "",
        "dep": 0,
        "discount": 0
      },
      {
        "name": "Алерана (пак; №6; 15мл) {маска для всех типов волос}",
        "price": 389,
        "kol": 1,
        "nds": "nds18",
        "barcode": "",
        "dep": 0,
        "discount": 0
      },
      {
        "name": "Алерана (флак; 250мл) {шампунь для сухих и нормальных волос}",
        "price": 293,
        "kol": 1,
        "nds": "nds18",
        "barcode": "",
        "dep": 0,
        "discount": 0
      },
      {
        "name": "Актовегин* (амп; №5; 2мл; 40мг/мл) [оригинал №25] {р-р д/ин.}",
        "price": 250,
        "kol": 1,
        "nds": "nds10",
        "barcode": "",
        "dep": 0,
        "discount": 0
      },
      {
        "name": "Аллохол (таб; №10)",
        "price": 6,
        "kol": 10,
        "nds": "nds10",
        "barcode": "",
        "dep": 0,
        "discount": 0
      },
      {
        "name": "Алерана \\спрей\\ (флак; 60мл; 5%) ",
        "price": 653,
        "kol": 1,
        "nds": "nds10",
        "barcode": "",
        "dep": 0,
        "discount": 0
      }
    ],
    "footer_text": [
      {
        "text": "Спасибо!",
        "font": {
          "b": false,
          "i": false,
          "n": false,
          "u": false,
          "dh": false,
          "dw": false,
          "w": 0,
          "a": 0
        }
      }
    ],
    "check_sm": 2012.00,
    "check_discount_sm": 0.00,
    "type_close": "tcCash",
    "cash_sm": 2012.00,
    "tr_id": 1097333
  }
}
```
type_close - тип оплаты (может иметь следующие значения: tcCash, tc1, tc2, tc3 .. tc6), при этом значения tc1..tc6 задаются в самой ККТ, где например, для tc1 можно задать тип Безнал
tr_id - идентификатор транзакции (можно не заполнять - отсылать 0)

###Печать последнего чека в нефискальном режиме(на случай окончания бумаги при фискализации продажи)
```json
{
  "atol_device_data": {
    "atol_device_number": 1,
    "atol_operator_password": "3"
  },
  "comand": "PRINT_LAST_CHEQUE_COPY",
  "data": ""
}
```

###Отправка длинной команды(если json не умещается по длине в 65536 байт)
В этом случае надо весь json этой длинной команды упаковать в base64 и поместить в виде текста в поле text
```json
{
  "atol_device_data": {
    "atol_device_number": 1,
    "atol_operator_password": "3"
  },
  "comand": "SEGMENT",
  "data": {
    "segment_guid": "{F0A21076-1A6B-42BA-B8FE-6C0D5C122F42}",
    "segment_num": 1,
    "segment_count": 88,
    "text": "eyJhdG9sX2RldmljZV9k"
  }
}
...
{
  "atol_device_data": {
    "atol_device_number": 1,
    "atol_operator_password": "3"
  },
  "comand": "SEGMENT",
  "data": {
    "segment_guid": "{F0A21076-1A6B-42BA-B8FE-6C0D5C122F42}",
    "segment_num": 88,
    "segment_count": 88,
    "text": "NDV9fQ=="
  }
}
```
Многоточием отмечены пропущенные команды
segment_guid - должен быть одинаков для всех сегментов одной команды и уникально идентифицировать команду
Сервис будет дожидаться получения последнего сегмента для "склеивания" сегментов и декодирования их из base64 в строку json

##Ответ

###Ответ с ошибкой

При ошибке поле response содержит пустой текст вместо соответствующего объекта или текстового ответа, а error_code <> 0
Поле error содержит текстовое описание ошибки
Примеры:
```json
{
  "response": "",
  "error": "Тестовое сообщение об окончании бумаги!",
  "error_code": -3807
}

{
  "response": "",
  "error": "Устройство с номером 1 не найдено",
  "error_code": -1
}

{
  "response": "",
  "error": "Неизвестная команда",
  "error_code": -1
}
```
