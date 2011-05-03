(in-package :spcr)

(defparameter *groups-table*
  '((1 . "[OH]")
    (2 . "C[OH]")
    (3 . "[CH2,CH3][OH]")
    (4 . "C[CH2][OH]")
    (5 . "c[CH2][OH]")
    (6 . "[#6][CH]([#6])[OH]")
    (7 . "[CH2,CH3][CH]([CH2,CH3])[OH]")
    (8 . "[C;r6][OH]")
    (9 . "[#6]C([#6])([#6])[OH]")
    (10 . "C([OH])C([OH])")
    (11 . "[c;r6][OH]")

    (12 . "[Nh]")
    (13 . "[NH2]")
    (14 . "C[NH2]")
    (15 . "[CH2][NH2]")
    (16 . "[CH](C)[NH2]")
    (17 . "[c][NH2]")
    (18 . "[NH]")
    (19 . "C[NH](C)")
    (20 . "C[NH][CH3]")

    (21 . "[#6]N")
    (22 . "[#6]N([#6])[#6]")
    (23 . "CN(C)C")
    (24 . "CN([CH3])C")

    (27 . "[Sh]")
    (28 . "[#6][SH]")
    (29 . "[C,N]#")
    (30 . "C#C")
    (31 . "[#6]C#[CH]")
    (32 . "C#N")
    (33 . "CC#N")
    (34 . "cC#N")
    (35 . "*(=C)(=*)")
    (36 . "N(=C)(=O)")))

(defparameter *OH-groups-tree*
  '(1
    (2
     (3 4 5)
     (6 7 8)
     9
     10)
    11))

(defparameter *NH-groups-tree*
  '(12
    (13
     (14 15 16)
     17)
    (18
     (19 20))))

(defparameter *CN-groups-tree*
  '(21
    (22
     (23 24))))

(defparameter *SH-groups-tree*
  '(27 (28)))

(defparameter *C/Nsp-groups-tree*
  '(29
    (30 (31))
    (32 (33 34))
    (35 (36))))

(defparameter *groups-tree*
  (list *OH-groups-tree*
	*NH-groups-tree*
	*CN-groups-tree*
	*SH-groups-tree*
	*C/Nsp-groups-tree*))

(defun tree-smarts-idx (idx)
  (cdr (assoc idx *groups-table*)))
