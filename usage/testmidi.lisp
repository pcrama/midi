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

(defun make-metronome-track (name beats/measure beats/min duration)
  "Create a Midi file NAME usable as a metronome: BEATS/MEASURE, BEATS/MIN and
DURATION \(in seconds\)"
  #|
  (loop for speed from 140 to 188 by 8 do
           (make-metronome-track (format nil "/tmp/valse~A.mid" speed)
                                 3
                                 speed
                                 (* 60 10)))
  (loop for speed from 60 to 140 by 4 do
           (make-metronome-track (format nil "/tmp/c~A.mid" speed)
                                 4
                                 speed
                                 (* 60 10)))
for file in /tmp/*mid
do
	echo $file ${file%mid}wav ${file%mid}mp3
	timidity -Ow -A500a -o ${file%mid}wav $file
	lame --scale 2 -b 16 --resample 16 /${file%mid}wav /${file%mid}mp3
	rm -- ${file%mid}wav
done

  |#
  (let* ((+acoustic-bass-drum+ 35)
         (+low-mid-tom+ 47)
         (measure-count (ceiling (/ duration 60)
                                 (/ beats/measure beats/min)))
         (ticks/beat 60)
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
                              (when (> beats/measure 1)
                                (loop repeat (1- beats/measure) nconc
                                     (make-sound +acoustic-bass-drum+
                                                 80
                                                 (incf now ticks/beat)
                                                 duration))))))))
         (midi-file
          (make-instance 'midifile
                         :format 1
                         :division ticks/beat
                         :tracks (list timing-track drum-track))))
    (setf (slot-value (first timing-track) 'dd) 2
          (slot-value (first timing-track) 'nn) 4
          (slot-value (first timing-track) 'cc) 24
          (slot-value (first timing-track) 'bb) 8)
    (write-midi-file midi-file name)))
