# IP System Unified Logging Guide

## Overview

Система IP теперь использует единую систему логирования через опциональный модуль `ip-debug`. Все модули (`ip-core`, `ip-forgejo`, `ip-invoice`) поддерживают как расширенное логирование (при наличии `ip-debug`), так и минимальное через стандартный `message` (без `ip-debug`).

## Архитектура логирования

### Модуль ip-debug (опциональный)

- **Основной буфер**: `*IP Debug Log*` - все сообщения от всех модулей
- **Модульные буферы**: `*IP Debug Core*`, `*IP Debug Forgejo*`, `*IP Debug Invoice*` - отдельные буферы для каждого модуля
- **Отчеты**: каждый модуль сохраняет свои специальные отчеты (например, `*Forgejo Sync Report*`)

### Fallback логирование

Если `ip-debug` не загружен, все модули автоматически используют функции fallback, которые логируют в стандартный `*Messages*` буфер.

## Основные функции

### ip-debug-log(level, module, message, &rest args)

Основная функция логирования:

- `level`: 'info, 'success, 'warning, 'error
- `module`: 'core, 'forgejo, 'invoice
- `message`: строка формата для `format`
- `args`: аргументы для форматирования

### ip-debug(module, message, &rest args)

Макрос для быстрого логирования на уровне 'info.

## Примеры использования

```elisp
;; В коде модулей
(ip-debug-log 'info 'core "Loading configuration file: %s" filename)
(ip-debug-log 'success 'forgejo "Imported %d issues successfully" count)
(ip-debug-log 'warning 'invoice "Client %s not found, using default rate" client-id)
(ip-debug-log 'error 'core "Failed to parse file: %s" error-message)

;; Быстрое логирование info
(ip-debug 'forgejo "Processing issue #%d" issue-number)
```

## Интерактивные команды

### ip-debug-show (&optional module)

- Без аргумента: показать основной буфер `*IP Debug Log*`
- С аргументом: показать буфер конкретного модуля
- `C-u ip-debug-show` - выбрать модуль из списка

### ip-debug-clear (&optional module)

- Очистить основной буфер или буфер конкретного модуля
- `C-u ip-debug-clear` - выбрать модуль для очистки

### ip-debug-clear-all ()

- Очистить все debug буферы

### ip-debug-list-buffers ()

- Показать список всех доступных debug буферов

## Специальные отчеты модулей

Каждый модуль сохраняет свои детальные отчеты в отдельных буферах:

### Forgejo модуль

- **Буфер отчета**: `*Forgejo Sync Report*`
- **Команда**: `ip-forgejo-show-sync-report`
- **Содержимое**: детальная информация о синхронизации с API, список импортированных задач, ошибки API

### Core модуль

- **Буфер отчета**: `*IP Overview*`
- **Команда**: `ip-show-overview`
- **Содержимое**: информация о компании, клиентах, настройках системы

### Invoice модуль

- **Буфер отчета**: `*Invoice Preview*`
- **Команда**: `ip-preview-invoice-text`
- **Содержимое**: текстовый предпросмотр счета

## Настройка

### Основные переменные

```elisp
;; Включить/выключить debug логирование
(setq ip-debug-enabled t)

;; Основной буфер для unified логов
(setq ip-debug-main-buffer "*IP Debug Log*")

;; Создавать ли отдельные буферы для модулей
(setq ip-debug-module-buffers t)
```

### Пример конфигурации

```elisp
;; Загрузка модулей в правильном порядке
(require 'ip-debug)    ; опционально
(require 'ip-core)     ; обязательно
(require 'ip-forgejo)  ; опционально
(require 'ip-invoice)  ; опционально

;; Настройки логирования
(setq ip-debug-enabled t
      ip-debug-module-buffers t)

;; Клавиши для быстрого доступа к логам
(global-set-key (kbd "C-c i d") 'ip-debug-show)
(global-set-key (kbd "C-c i D") 'ip-debug-clear)
```

## Уровни логирования

### 'info (ℹ️)

- Общая информация о ходе выполнения
- Загрузка файлов, начало операций
- Нормальные события в системе

### 'success (✅)

- Успешное завершение операций
- Создание файлов, импорт данных
- Подтверждение выполненных действий

### 'warning (⚠️)

- Потенциальные проблемы
- Отсутствующие файлы или данные
- Ситуации, требующие внимания

### 'error (❌)

- Критические ошибки
- Невозможность выполнить операцию
- Исключения и сбои

## Fallback поведение

Если модуль `ip-debug` не загружен:

1. Все вызовы `ip-debug-log` перенаправляются в `message`
2. Формат: `[MODULE/LEVEL] message`
3. Все сообщения попадают в стандартный буфер `*Messages*`
4. Специальные отчеты модулей работают как обычно

Пример fallback вывода:

```
[CORE/INFO] Loading org file: ~/org/ip/clients.org
[FORGEJO/SUCCESS] Imported 5 issues successfully
[INVOICE/WARNING] Client test not found, using default rate
```

## Практические рекомендации

### Для разработчиков

1. **Используйте подходящие уровни**: не логируйте все как 'info
2. **Указывайте контекст**: включайте имена файлов, ID клиентов, счетчики
3. **Логируйте начало и конец операций**: помогает отслеживать прогресс
4. **Не логируйте в циклах**: избегайте спама в логах

### Для пользователей

1. **Включите debug заранее**: `(setq ip-debug-enabled t)` до загрузки модулей
2. **Используйте модульные буферы**: проще находить проблемы конкретного модуля
3. **Очищайте логи периодически**: `ip-debug-clear-all`
4. **Сохраняйте логи для отчетов**: копируйте важные сессии в файлы

## Интеграция с существующим кодом

Все модули уже обновлены для использования новой системы:

- **ip-core**: логирование загрузки файлов, кеширования, валидации
- **ip-forgejo**: логирование API запросов, синхронизации, ошибок
- **ip-invoice**: логирование генерации счетов, обработки задач

Старые функции логирования (например, `ip-forgejo--log`) сохранены для совместимости и теперь используют unified систему внутри.

## Отладка и диагностика

### Проверка статуса системы

```elisp
;; Проверить, загружен ли ip-debug
(featurep 'ip-debug)

;; Посмотреть все debug буферы
(ip-debug-list-buffers)

;; Показать основной лог
(ip-debug-show)

;; Показать лог конкретного модуля
(ip-debug-show 'forgejo)
```

### Типичные проблемы

1. **Нет логов**: убедитесь, что `ip-debug-enabled` установлен в `t`
2. **Fallback режим**: проверьте, загружен ли `ip-debug` через `(featurep 'ip-debug)`
3. **Переполнение буферов**: используйте `ip-debug-clear-all` для очистки

## Миграция со старой системы

Если у вас есть старый код с прямыми вызовами `message`:

```elisp
;; Старый код
(message "Processing client: %s" client-id)

;; Новый код
(ip-debug-log 'info 'mymodule "Processing client: %s" client-id)
;; или
(ip-debug 'mymodule "Processing client: %s" client-id)
```

Все изменения обратно совместимы - старый код будет работать, но не получит преимуществ unified логирования.
