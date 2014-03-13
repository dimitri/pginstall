;;;
;;; The build animal is responsible for building extensions against a set of
;;; PostgreSQL installations.
;;;
;;; This file containst server side implementation of management routines
;;; for buildfarm animals.

(in-package #:pginstall.repo)

(defvar *animal-name-registry*
  '(("Abraxas"             . "abraxas_14282_mth.gif")
    ("Basilisk"            . "basilisk_23923_mth.gif")
    ("Centaur"             . "73344_man-centaur_mth.gif")
    ("Roman"               . "59426_roman_c_mth.gif")
    ("Cockatrice"          . "cockatrice_7198_mth.gif")
    ("Dagon"               . "dagon_23884_mth.gif")
    ("Dolphin"             . "dolphin_7196_mth.gif")
    ("Dragon"              . "dragon_7197_mth.gif")
    ("Baby Dragon"         . "72953_dragon_mth.gif")
    ("Griffin"             . "87150_griffin_mth.gif")
    ("Greek Griffin"       . "59421_griffin_g_mth.gif")
    ("Griffin Renaissance" . "59423_ren_griffin_mth.gif")
    ("Roman Griffin"       . "59422_rome_griffin_mth.gif")
    ("Sitting Griffin"     . "59427_sit_griffin_mth.gif")
    ("Griffon"             . "griffon_7193_mth.gif")
    ("Harpy"               . "harpy_3_mth.gif")
    ("Hercules"            . "hercules_1_mth.gif")
    ("Idol"                . "87162_idol_mth.gif")
    ("Maenads"             . "73020_maenads_mth.gif")
    ("Monsters"            . "monster_29663_mth.gif")
    ("Oannes"              . "oannes_25900_mth.gif")
    ("Ondine"              . "72989_ondine_mth.gif")
    ("Pan"                 . "72856_pan_mth.gif")
    ("Rosmarine"           . "rosmarine_27681_mth.gif")
    ("Sphinx"              . "sphinx_1_mth.gif")
    ("Unicorn"             . "unicorn_16821_mth.gif")
    ("Wyvern"              . "wyvern_1_mth.gif")))

(defun pick-animal-name ()
  "Return any unused animal name."
  (with-pgsql-connection (*dburi*)
    (query "with freenames as (
              select name from registry
              except
              select name from animal
            )
            select name from freenames order by random() limit 1"
           :single)))

(defun register-animal (name os version arch)
  "Register a new animal, with its plaftorm details."
  ;; conflict avoidance and resolution?
  (with-pgsql-connection (*dburi*)
    (let ((platform (or
                     (car (select-dao 'platform (:and (:= 'os_name os)
                                                      (:= 'os_version version)
                                                      (:= 'arch arch))))
                     (make-dao 'platform
                               :os-name os
                               :os-version version
                               :arch arch))))
      (make-dao 'animal :name name :platform (platform-id platform)))))

(defun list-pg-configs (&key ((:animal-name *animal-name*) *animal-name*))
  "List the pgconfigs associated to given :ANIMAL-NAME, which defaults to
   the special variable *ANIMAL-NAME*, as initialized by reading the local
   configuration file."
  (with-pgsql-connection (*dburi*)
    (query-dao 'pgconfig "select pgc.*
                            from pgconfig pgc
                                 join animal a on a.id = pgc.animal
                           where a.name = $1"
               *animal-name*)))


