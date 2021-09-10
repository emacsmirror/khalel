;;; khalel.el --- description -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2021 Hanno Perrey
;;
;; Author: Hanno Perrey <http://gitlab.com/hperrey>
;; Maintainer: Hanno Perrey <hanno@hoowl.se>
;; Created: september 10, 2021
;; Modified: september 10, 2021
;; Version: 0.0.1
;; Keywords: event, calendar, ics, khal
;; Homepage: https://gitlab.com/hperrey/khalel
;; Package-Requires: ((emacs 27.1) (cl-lib "0.5") (org 9.5) (ox-icalendar))
;;
;; This file is not part of GNU Emacs.
;;
;;    This program is free software: you can redistribute it and/or modify
;;    it under the terms of the GNU General Public License as published by
;;    the Free Software Foundation, either version 3 of the License, or
;;    (at your option) any later version.
;;
;;    This program is distributed in the hope that it will be useful,
;;    but WITHOUT ANY WARRANTY; without even the implied warranty of
;;    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;;    GNU General Public License for more details.
;;
;;    You should have received a copy of the GNU General Public License
;;    along with this program.  If not, see <https://www.gnu.org/licenses/>.
;;
;;; Commentary:
;;
;;  khalel provides helper routines to import upcoming events from a local
;;  calendar through khal and to capture new events that are exported to
;;  khal.
;;
;;  The local calendar can be synced with other external tools such as vdirsyncer.
;;
;;  First steps/quick start:
;;  - install and configure khal
;;  - customize the values for default calendar, capture file and import file for khalel
;;  - call `khalel-add-capture-template' to set up a capture template
;;  - consider adding the import org file to your org agenda to show upcoming events there
;;  - import upcoming events through `khalel-import-upcoming-events' or create new ones through `org-capture'
;;
;;; Code:

;;;; Requirements

(require 'org)
(require 'ox-icalendar)

;;;; Customization

(defgroup khalel nil
  "Calendar import functions using khal."
  :group 'Calendar)

(defcustom khalel-khal-command "~/.local/bin/khal"
  "The command to run when executing khal. When set to nil then it will be guessed."
  :group 'khalel
  :type 'string)

(defcustom khalel-default-calendar "privat"
  "The khal calendar to import into by default. The calendar for a new event can be modified during the capture process."
  :group 'khalel
  :type 'string)

(defcustom khalel-capture-key "e"
  "The key to use when registering an `org-capture' template via `khalel-add-capture-template'."
  :group 'khalel
  :type 'string)

(defcustom khalel-import-org-file (concat org-directory "calendar.org")
  "The file to import khal calendar entries into.

CAUTION: the file will be overwritten with each import! The
prompt for overwriting the file can be disabled by setting
`khalel-import-org-file-confirm-overwrite' to nil."
  :group 'khalel
  :type 'string)

(defcustom khalel-import-org-file-confirm-overwrite 't
  "When nil, always overwrite the org file into which events are imported.
Otherwise, ask for confirmation."
  :group 'khalel
  :type 'string)

(defcustom khalel-capture-org-file (concat org-directory "new_events.org")
  "The file to capture new calendar entries into and from which to export to ics."
  :group 'khalel
  :type 'string)

(defcustom khalel-import-time-delta "30d"
  "How many hours, days, or months in the future to consider when import.
Used as DELTA argument to the khal date range."
  :group 'khalel
  :type 'string)


;;;; Commands

(defun khalel-import-upcoming-events ()
  "Imports future calendar entries by calling khal externally.

The time delta which determines how far into the future events
are imported is configured through `khalel-import-time-delta'.
CAUTION: The results are imported into the file
`khalel-import-org-file' which is overwritten to avoid adding
duplicate entires already imported previously."
  (interactive)
  (let
    ( ;; call khal directly.
      (khal-bin (or khalel-khal-command
        (executable-find "khal")))
      (dst (generate-new-buffer "khalel-output")))
          (call-process khal-bin nil dst nil "list" "--format"
          "* {title} {cancelled}\n<{start-date} \
{start-time}-{end-time}>\nlocation: {location}\n\
description: {description}\ncalendar: {calendar}\n"
          "--day-format" "" "today" khalel-import-time-delta)
          (save-excursion
            (with-current-buffer dst
              (write-file khalel-import-org-file khalel-import-org-file-confirm-overwrite)
              (message (format "Imported %d future events from khal into %s"
                               (length (org-map-entries nil nil nil))
                               khalel-import-org-file))))
          (kill-buffer dst)
          ))


(defun khalel-export-org-subtree-to-calendar ()
  "Exports current subtree as ics file and into an external khal calendar.
A UID will be automatically be created and stored as property of
the subtree. See documentation of `org-icalendar-export-to-ics'
for details of the supported fields."
  (interactive)
  (save-excursion
    ;; org-icalendar-export-to-ics doesn't reliably export the full event when
    ;; operating on a subtree only; narrowing the buffer, however, works fine
    (org-narrow-to-subtree)
    (let*
        ;; store IDs right away to avoid duplicates
        ((org-icalendar-store-UID 't)
         ;; create events from non-TODO entries with scheduled time
         (org-icalendar-use-scheduled '(event-if-not-todo))
         (khal-bin (or khalel-khal-command
                       (executable-find "khal")))
         (path (file-name-directory
                (buffer-file-name
                 (buffer-base-buffer))))
         (calendar (or (org-entry-get nil "calendar")
                       khalel-default-calendar))
         ;; export to ics
         (ics
          (org-icalendar-export-to-ics nil nil 't)
          )
         ;; call khal import
         (import
          (with-temp-buffer
            (list
             :exit-status
             (if calendar
                 (call-process khal-bin nil t nil
                               "import" "-a" calendar
                               "--batch"
                               (concat path ics))
               (call-process khal-bin nil t nil
                             "import"
                             "--batch"
                             (concat path ics)))
             :output
             (buffer-string)))))
      (widen)
      (when
          (/= 0 (plist-get import :exit-status))
        (message
         (format
          "%s failed importing %s and exited with status %d: %s"
          khal-bin
          ics
          (plist-get import :exit-status)
          (plist-get import :output)))))))

(defun khalel-add-capture-template (&optional file key)
  "Add an `org-capture' template with KEY for creating new events in FILE.
If arguments are nil then `khalel-capture-key' and
`khalel-capture-org-file' will be used instead. New events will
be immediately exported to khal. The key used for the capture
template can be configured via `khalel-capture-key'."
  (eval-after-load 'org
    (add-to-list 'org-capture-templates
        (quote (
                ((or key khalel-capture-key) "calendar event"
                 entry
                 (file (or file khalel-capture-org-file))
                 "* %?\nSCHEDULED: %^T\n:PROPERTIES:\n:CREATED:
%U\n:CALENDAR: \n:CATEGORIES: event\n:LOCATION:
unknown\n:APPT_WARNTIME: 10\n:END:\n" )))))
  (add-hook 'org-capture-before-finalize-hook
            'khalel--capture-finalize-calendar-export))

;;;; Functions
(defun khalel--capture-finalize-calendar-export ()
  "Exports current event capture.
To be added as hook to `org-capture-before-finalize-hook'."
  (let ((key  (plist-get org-capture-plist :key))
        (desc (plist-get org-capture-plist :description)))
    (when (and (not org-note-abort) (equal key khalel-capture-key))
      (khalel-export-org-subtree-to-calendar))))

;;;; Footer
(provide 'khalel)
;;; khalel.el ends here
