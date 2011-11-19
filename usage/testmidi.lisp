#| See also following websites:
http://midi.mathewvp.com/midiSequencer.php
http://midi.mathewvp.com/midiEditor.php
http://www.sonicspot.com/guide/midifiles.html
http://en.wikipedia.org/wiki/Chord_names_and_symbols_%28jazz_and_pop_music%29
http://chordfind.com/
|#
(let* ((+acoustic-bass-drum+ 35)
       (+conga-high+ 62)
       (+high-hat-top+ 46)
       (+low-mid-tom+ 47)
       (measure-count 4)
       (ticks/beat 60)
       (beats/min 60)
       (*midi-channel* 9)
       (timing-track
	(list (make-instance 'time-signature-message
			     :time 0)
	      (make-instance 'tempo-message
			     :time 0
			     :tempo (round 60000000 beats/min))))
       (drum-track
	(flet ((make-sound (key vel time duration)
                 (list (make-instance 'note-on-message
                                      :time time
                                      :key key
                                      :velocity vel)
                       (make-instance 'note-off-message
                                      :time (+ time duration)
                                      :key key
                                      :velocity vel))))
	      (cons (make-instance 'program-change-message
				   :program 0
				   :time 0)
		    (loop with now = (- ticks/beat)
			  with duration = (/ ticks/beat 5)
			  repeat measure-count nconc
			  (nconc (make-sound +low-mid-tom+
                                             127
                                             (incf now ticks/beat)
                                             (round (* 1.5 duration)))
                                 (make-sound +acoustic-bass-drum+
					     80
					     (incf now (/ ticks/beat 2))
					     duration)
                                 (make-sound +conga-high+
                                             80
                                             (incf now (/ ticks/beat 2))
                                             duration)
                                 (make-sound +high-hat-top+
                                             100
                                             (incf now ticks/beat)
                                             duration)
                                 (make-sound +conga-high+
                                             80
                                             (incf now ticks/beat)
                                             duration))))))
       (*midi-channel* 5)
       (guitar-track
	(flet ((make-sound (key vel time duration)
                 (unless (listp key) (setf key (list key)))
                 (append (mapcar (lambda (k)
                                   (make-instance 'note-on-message
                                                  :time time
                                                  :key (+ k 36)
                                                  :velocity vel))
                                 key)
                         (mapcar (lambda (k)
                                   (make-instance 'note-off-message
                                                  :time (+ time duration)
                                                  :key (+ k 36)
                                                  :velocity vel))
                                 key))))
	      (cons (make-instance 'program-change-message
				   :program 25
				   :time 0)
		    (loop with now = (- ticks/beat)
			  with duration = (/ ticks/beat 5)
			  repeat measure-count nconc
			  (nconc (make-sound '(19 23 26 29)
					     60
					     (incf now ticks/beat)
					     (* 2 duration))
				 (make-sound '(19 23)
					     60
					     (incf now ticks/beat)
					     (* 2 duration))
				 (make-sound '(19 23 26 29 31)
					     60
					     (incf now ticks/beat)
					     (* 2 duration))
				 (make-sound '(19 26)
					     60
					     (incf now ticks/beat)
					     (* 2 duration)))))))
       (midi-file
	(make-instance 'midifile
		       :format 1
		       :division ticks/beat
		       :tracks (list timing-track drum-track guitar-track))))
  (setf (slot-value (first timing-track) 'dd) 2
	(slot-value (first timing-track) 'nn) 4
	(slot-value (first timing-track) 'cc) 24
	(slot-value (first timing-track) 'bb) 8)
  (defparameter *timing* timing-track)
  (write-midi-file midi-file "/tmp/t.mid"))
