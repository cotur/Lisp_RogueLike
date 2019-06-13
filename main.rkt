#lang racket
(require 2htdp/image)
(require 2htdp/universe)



; functions

(define (replace_list list n elem)
  (cond
    ((null? list) list)
    ((eq? n 0) (cons elem (cdr list)))
    (#t (cons (car list) (replace_list (cdr list) (- n 1) elem)))))


; ----------
; settings
(define SIZE 50)
(define WORLD_SIZE (* SIZE 9))


(define (_truePosition factor)(+ (/ SIZE 2) (* factor SIZE)))

; ---------

(define WORLD_SCENE (empty-scene WORLD_SIZE WORLD_SIZE))

(define _soil_images (list
                      (bitmap "Assets/soil.png")
                      (bitmap "Assets/soil2.png")
                      ))

(define IMAGES (list
                (list-ref _soil_images (random 2))
                (bitmap "Assets/Rogue.png")
                (bitmap "Assets/zombie.png")
                (bitmap "Assets/zombie2.png")
                ))

(define OBJECTS (list
               (list "Player" (list 4 4) (list-ref IMAGES 1) 10);last val is start life, other obj don't have it
               (list "Zombie" (list 0 0) (list-ref IMAGES 2) 5)
               (list "Zombie" (list 8 8) (list-ref IMAGES 3) 5)
               ))

(define _PLAYER_ID 0)

(define (_obj_name obj)(list-ref obj 0))
(define (_obj_pos obj)(list-ref obj 1))
(define (_obj_pos_x obj)(list-ref (_obj_pos obj) 1))
(define (_obj_pos_y obj)(list-ref (_obj_pos obj) 0))

(define (_get_obj id)(list-ref OBJECTS id))

(define (_get_obj_by_name name lst)(cond
                                 [(or (null? lst)(null? (car lst))) #f]
                                 [(equal? (list-ref (car lst) 0) name) (list-ref lst 0)]
                                 [else (_get_obj_by_name name (cdr lst))]
                                 ))

(define (_get_obj_by_pos pos lst)(cond
                                 [(or (null? lst)(null? (car lst))) #f]
                                 [(equal? (list-ref (car lst) 1) pos) (list-ref lst 0)]
                                 [else (_get_obj_by_pos pos (cdr lst))]
                                 ))


(define (_obj_replace obj_id new_obj)
  (set! OBJECTS (replace_list OBJECTS obj_id new_obj))
  )

(define (_obj_pos_change obj_id new_pos)
  (_obj_replace obj_id
   (replace_list (list-ref OBJECTS obj_id) 1 new_pos)
   )
  )
; scene

(define (_put_image_to_scene row col img)
  (set! WORLD_SCENE (place-image img (_truePosition col) (_truePosition row) WORLD_SCENE))
  )

(define (_put_image_to_scene_by_pos pos img)(_put_image_to_scene (list-ref pos 0) (list-ref pos 1) img))

(define (_put_empty_img row col)
  (_put_image_to_scene row col (list-ref IMAGES 0))
  )

(define (objs-to-scene lst)(cond
                            [(null? lst) WORLD_SCENE]
                            [else
                             (_put_image_to_scene_by_pos (list-ref (car lst) 1)(list-ref (car lst) 2))
                             (objs-to-scene (cdr lst))
                             ]
                            )
  )

(define (_fill_scene row col)(cond
                              [
                               (and (< col 9)(< row 9))
                               
                                (_put_empty_img row col)
                                (_fill_scene row (+ col 1))
                                
                               ]
                              [(equal? col 9)(_fill_scene (+ row 1) 0)]
                              [(equal? row 9) (objs-to-scene OBJECTS) (_fill_scene (+ 1 row) col)]
                              [(equal? row 10) WORLD_SCENE]
                              ))

(define (DRAW_WORLD frame)
  (set! WORLD_SCENE (empty-scene WORLD_SIZE WORLD_SIZE))(_fill_scene 0 0)
  )
; ----------

; movement
(define (check-border obj to)(cond
                            [(equal? to "right")(if (equal? (_obj_pos_x obj) 8) #f #t)]
                            [(equal? to "left")(if (equal? (_obj_pos_x obj) 0) #f #t)]
                            [(equal? to "up")(if (equal? (_obj_pos_y obj) 0) #f #t)]
                            [(equal? to "down")(if (equal? (_obj_pos_y obj) 8) #f #t)]
                            ))

(define (go_right id) (_obj_pos_change id (list (_obj_pos_y (list-ref OBJECTS id)) (+ 1 (_obj_pos_x (list-ref OBJECTS id))))))
(define (go_left id) (_obj_pos_change id (list (_obj_pos_y (list-ref OBJECTS id)) (- (_obj_pos_x (list-ref OBJECTS id)) 1))))
(define (go_up id) (_obj_pos_change id (list (- (_obj_pos_y (list-ref OBJECTS id)) 1) (_obj_pos_x (list-ref OBJECTS id)))))
(define (go_down id) (_obj_pos_change id (list (+ (_obj_pos_y (list-ref OBJECTS id)) 1) (_obj_pos_x (list-ref OBJECTS id)))))


(define (vector-diff v_o v_t pos)
  (- (list-ref v_o pos) (list-ref v_t pos))
  )

(define (_find_my_path my_pos target_pos)
  (cond
    [(>= (abs (vector-diff my_pos target_pos 0)) (abs (vector-diff my_pos target_pos 1)) )
     (cond
       [(< (vector-diff my_pos target_pos 0) 0) "down"]
       [else "up"]
       )
     ]; Y axis
    [else
     (cond
       [(< (vector-diff my_pos target_pos 1) 0) "right"]
       [else "left"]
       )
     ]; X axis
    )
  )

(define (_move my_pos target_pos id)
  (cond
    [(equal? (_find_my_path my_pos target_pos) "right") (go_right id)]
    [(equal? (_find_my_path my_pos target_pos) "left") (go_left id)]
    [(equal? (_find_my_path my_pos target_pos) "up") (go_up id)]
    [(equal? (_find_my_path my_pos target_pos) "down") (go_down id)]
    )
  )

(define (_zombies_turn lst id)(cond
                                 [(null? lst) #t]
                                 [(equal? (_obj_name (car lst)) "Zombie")
                                  (_move (_obj_pos (car lst)) (_obj_pos (list-ref OBJECTS 0)) id)(_zombies_turn (cdr lst) (+ id 1))
                                  ]
                                 [else (_zombies_turn (cdr lst) (+ id 1))]
                                 ))










(define (move_obj obj to)(cond
                           [(key=? to "right") (go_right 0)(_zombies_turn OBJECTS 0)]
                           [(key=? to "left") (go_left 0)(_zombies_turn OBJECTS 0)]
                           [(key=? to "up") (go_up 0)(_zombies_turn OBJECTS 0)]
                           [(key=? to "down") (go_down 0)(_zombies_turn OBJECTS 0)]
                           ))



; --------
(big-bang 0
          [on-draw DRAW_WORLD]
          [on-release move_obj]
)
(list-ref OBJECTS 0)