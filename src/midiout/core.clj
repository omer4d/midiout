(ns midiout.core)

(defn get-device-infos []
  (into [] (javax.sound.midi.MidiSystem/getMidiDeviceInfo)))

(defn get-device [info]
  (javax.sound.midi.MidiSystem/getMidiDevice info))

(defn get-nth-device [n]
  (get-device (nth (get-device-infos) n)))

(defn get-sequencer
  ([] (javax.sound.midi.MidiSystem/getSequencer))
  ([connected] (javax.sound.midi.MidiSystem/getSequencer connected)))

(defn make-midi-sequence [ticks-per-beat]
  (new javax.sound.midi.Sequence javax.sound.midi.Sequence/PPQ ticks-per-beat))

(defn make-short-msg [cmd chan data1 data2]
  (let [msg (new javax.sound.midi.ShortMessage)]
    (.setMessage msg cmd chan data1 data2)
    msg))

(defn make-midi-event [msg tick]
  (new javax.sound.midi.MidiEvent msg tick))

(def midi-state (atom nil))

(defn init-midi []
  (let [synth (get-nth-device 0)
        sequencer (get-sequencer false)]
    (. synth open)
    (. sequencer open)
    (.. sequencer getTransmitter (setReceiver (.getReceiver synth)))
    (reset! midi-state {:synth synth :sequencer sequencer})))

(defn deinit-midi []
  (let [{:keys [synth sequencer]} @midi-state]
    (. sequencer close)
    (. synth close)
    (reset! midi-state nil)))

(defn play [data bpm ticks-per-beat]
  (let [{:keys [synth sequencer]} @midi-state
        seq (make-midi-sequence ticks-per-beat)
        track (. seq createTrack)]
    (run! #(. track add %) data)
    (doto sequencer
      (.stop)
      (.setSequence seq)
      (.setTempoInBPM bpm)
      (.setTickPosition 0)
      (.start))))

(defn make-track-aspect [coll key]
  (map #(assoc {} key %) coll))

(defn merge-track-aspects [& aspects]
  (apply map (fn [& args] (apply merge args)) aspects))

(defn legato [durs]
  (map #(assoc {} :start %1 :dur %2) (reductions + 0 durs) durs))

(defn midify-track [track]
  (for [in (filter #(:key %) track)
        out [(make-midi-event (make-short-msg 0x90 0 (:key in) 64) (:start in))
             (make-midi-event (make-short-msg 0x80 0 (:key in) 0) (+ (:start in) (:dur in)))]]
    out))

(defn indexed [seq]
  (map vector (range) seq))

(def name-key-lut (into {} (for [[deg-idx deg-name] (map vector [0 2 4 5 7 9 11] ["c" "d" "e" "f" "g" "a" "b"])
                                 oct [0 1 2 3 4 5 6 7 8 9]
                                 [mod-shift mod-name] [[0 ""] [-1 "b"] [1 "#"]]]
                             (vector (symbol (str deg-name mod-name oct)) (+ (* oct 12) deg-idx mod-shift)))))

(def interval-lut (into {} (for [[ival ival-semitones] (map vector [1 2 3 4 5 6 7 8] [0 2 4 5 7 9 11 12])
                                 [mod-name mod-shift] [["m" -1] ["" 0]]
                                 [dir-name dir] [["-" -1] ["" 1]]
                                 :let [shift (* (+ ival-semitones mod-shift) dir)]]
                             (if (empty? mod-name)
                               [(* ival dir) shift]
                               [(symbol (str dir-name mod-name ival)) shift]))))

(defn symbol-to-key [accum curr]
  (cond
    (= curr '-) accum
    (contains? name-key-lut curr) (get name-key-lut curr)
    (contains? interval-lut curr) (+ accum (get interval-lut curr))
    :else (throw (Exception. "Unrecognized symbol"))))

(defn symbols-to-keys [syms]
  (if (contains? name-key-lut (first syms))
    (make-track-aspect (map #(if (= %2 '-) nil %1)
                            (reductions symbol-to-key (get name-key-lut (first syms)) (rest syms))
                            syms)
                       :key)
    (throw (Exception. "First can't be interval"))))

(defn cycle* [coll n]
  (take (* (count coll) n) (cycle coll)))

(play (midify-track (merge-track-aspects (symbols-to-keys (cycle* '(c3 8 -4) 8))
                                         (legato (cycle [32 16 16]))))
      120 64)
