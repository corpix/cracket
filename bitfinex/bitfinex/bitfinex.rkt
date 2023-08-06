#lang racket
(require racket/async-channel
         racket/generator
         corpix/json
         corpix/url
         corpix/websocket
         corpix/prometheus
         (for-syntax corpix/syntax))
(provide current-bitfinex-connection
         make-bitfinex-connection
         bitfinex-subscribe
         bitfinex-subscribe/trades
         bitfinex-subscribe/ticker
         bitfinex-subscribe/book
         bitfinex-unsubscribe
         bitfinex-metric-subscriptions
         bitfinex-metric-events
         bitfinex-metric-messages
         bitfinex-metrics
         bitfinex-metrics-register!
         bitfinex-metrics-unregister!
         (struct-out exn:fail:user:bitfinex:eof)
         (struct-out exn:fail:user:bitfinex:api)
         (struct-out exn:fail:user:bitfinex:api:info)
         (struct-out exn:fail:user:bitfinex:message)
         (struct-out exn:fail:user:bitfinex:event)
         (struct-out exn:fail:user:bitfinex:market-type)
         (struct-out exn:fail:user:bitfinex:timeout)
         (struct-out exn:fail:user:bitfinex:closed)
         (struct-out exn:fail:user:bitfinex:version)
         (struct-out bitfinex-subscribed)
         (struct-out bitfinex-unsubscribed)
         (struct-out bitfinex-description)
         (except-out (struct-out bitfinex-connection) -make-bitfinex-connection)
         (struct-out bitfinex-stream)
         (struct-out bitfinex-subscriptions)
         (struct-out bitfinex-trading-ticker-message)
         (struct-out bitfinex-funding-ticker-message)
         (struct-out bitfinex-trading-book-message)
         (struct-out bitfinex-funding-book-message)
         (struct-out bitfinex-trading-trades-message)
         (struct-out bitfinex-funding-trades-message))
(module+ test
  (require rackunit))

(define current-bitfinex-connection (make-parameter #f))

;; Bitfinex abbreviations:
;; bu             balance update
;; ps             position snapshot
;; pn             new position
;; pu             position update
;; pc             position close
;; ws             wallet snapshot
;; wu             wallet update
;; os             order snapshot
;; on             new order
;; on-req         new order request
;; ou             order update
;; oc             order cancel
;; oc-req         order cancel request
;; oc_multi-req   multiple orders cancel request
;; te             trade executed
;; tu             trade execution update
;; fte            funding trade execution
;; ftu            funding trade update
;; hos            historical order snapshot
;; mis            margin information snapshot
;; miu            margin information update
;; n              notification
;; fos            funding offer snapshot
;; fon            funding offer new
;; fou            funding offer update
;; foc            funding offer cancel
;; hfos           historical funding offer snapshot
;; fcs            funding credits snapshot
;; fcn            funding credits new
;; fcu            funding credits update
;; fcc            funding credits close
;; hfcs           historical funding credits snapshot
;; fls            funding loan snapshot
;; fln            funding loan new
;; flu            funding loan update
;; flc            funding loan close
;; hfls           historical funding loan snapshot
;; hfts           historical funding trade snapshot
;; uac            user custom price alert

(define endpoint (string->url "wss://api-pub.bitfinex.com/ws/2"))
(define version 2)

(define errors
  (hasheq
   10000 "unknown error"
   10001 "generic error"
   10008 "concurrency error"
   10020 "request parameters error"
   10050 "configuration setup failed"
   10100 "failed authentication"
   10111 "error in authentication request payload"
   10112 "error in authentication request signature"
   10113 "error in authentication request encryption"
   10114 "error in authentication request nonce"
   10200 "error in un-authentication request"
   10300 "failed channel subscription"
   10301 "failed channel subscription: already subscribed"
   10400 "failed channel un-subscription: channel not found"
   11000 "not ready, try again later"))

(define events
  (hasheq
   20051 "websocket server stopping, please reconnect later"
   20060 "websocket server resyncing, please reconnect later"
   20061 "websocket server resync complete, please reconnect"
   5000  "info message"))

;;

(define-struct (exn:fail:user:bitfinex:eof exn:fail:user)
  ())
(define-struct (exn:fail:user:bitfinex:api exn:fail:user)
  (code))
(define-struct (exn:fail:user:bitfinex:api:info exn:fail:user:bitfinex:api)
  ())
(define-struct (exn:fail:user:bitfinex:message exn:fail:user)
  (message))
(define-struct (exn:fail:user:bitfinex:event exn:fail:user)
  (message))
(define-struct (exn:fail:user:bitfinex:market-type exn:fail:user)
  ())
(define-struct (exn:fail:user:bitfinex:timeout exn:fail:user)
  ())
(define-struct (exn:fail:user:bitfinex:closed exn:fail:user)
  ())
(define-struct (exn:fail:user:bitfinex:version exn:fail:user)
  (version))
(define-struct (exn:fail:user:bitfinex:transport:opened exn:fail:user)
  ())
(define-struct (exn:fail:user:bitfinex:transport:closed exn:fail:user)
  ())

;; FIXME: this is what it looks like in 2017
;; do we still need this in 2023?
(define (exn:input-port-closed? x)
  (and (exn:fail? x)
       (regexp-match #rx"input port is closed" (exn-message x))))

;;

(define-struct bitfinex-subscribed
  (id)
  #:transparent)
(define-struct bitfinex-unsubscribed
  (id)
  #:transparent)

(define-struct bitfinex-description
  (server-id version status)
  #:transparent)
(define-struct bitfinex-connection
  (transport subscriptions description)
  #:transparent
  #:constructor-name -make-bitfinex-connection)
(define-struct bitfinex-stream
  (channel
   (last-message #:mutable)
   (last-heartbeat #:mutable))
  #:transparent)
(define-struct bitfinex-subscriptions
  (streams pending semaphore))

;; Response Fields (trading pairs, ex. tBTCUSD)
;; Index Field                 Type   Description
;; [0]   BID                   float  Price of last highest bid
;; [1]   BID_SIZE              float  Sum of the 25 highest bid sizes
;; [2]   ASK                   float  Price of last lowest ask
;; [3]   ASK_SIZE              float  Sum of the 25 lowest ask sizes
;; [4]   DAILY_CHANGE          float  Amount that the last price has changed since yesterday
;; [5]   DAILY_CHANGE_RELATIVE float  Relative price change since yesterday (*100 for percentage change)
;; [6]   LAST_PRICE            float  Price of the last trade
;; [7]   VOLUME                float  Daily volume
;; [8]   HIGH                  float  Daily high
;; [9]   LOW                   float  Daily low
(define-struct bitfinex-trading-ticker-message
  (
   bid bid-size
   ask ask-size
   daily-change daily-change-percent
   last-price
   volume high low)
  #:transparent)

;; Response Fields (funding currencies, ex. fUSD)
;; Index  Field                Type    Description
;; [0]    FRR                  float   Flash Return Rate - average of all fixed rate funding over the last hour
;; [1]    BID                  float   Price of last highest bid
;; [2]    BID_PERIOD           int     Bid period covered in days
;; [3]    BID_SIZE             float   Sum of the 25 highest bid sizes
;; [4]    ASK                  float   Price of last lowest ask
;; [5]    ASK_PERIOD           int     Ask period covered in days
;; [6]    ASK_SIZE             float   Sum of the 25 lowest ask sizes
;; [7]    DAILY_CHANGE         float   Amount that the last price has changed since yesterday
;; [8]    DAILY_CHANGE_PERC    float   Relative price change since yesterday (*100 for percentage change)
;; [9]    LAST_PRICE           float   Price of the last trade
;; [10]   VOLUME               float   Daily volume
;; [11]   HIGH                 float   Daily high
;; [12]   LOW                  float   Daily low
;; [ . . . ]
;; [15]   FRR_AMOUNT_AVAILABLE float   The amount of funding that is available at the Flash Return Rate
(define-struct bitfinex-funding-ticker-message
  (frr
   bid bid-period bid-size
   ask ask-period ask-size
   daily-change daily-change-percent
   last-price
   volume high low
   frr-amount-available)
  #:transparent)

;; For trading pair symbols (ex. tBTCUSD)
;; Index  Field    Type    Description
;; [0]    PRICE    float   Price level
;; [1]    COUNT    int     Number of orders at that price level
;; [2]    AMOUNT   float   Total amount available at that price level (if AMOUNT > 0 then bid else ask)
(define-struct bitfinex-trading-book-message
  (price count amount)
  #:transparent)

;; For funding currency symbols (ex. fUSD)
;; Index  Field    Type    Description
;; [0]    RATE     float   Rate level
;; [1]    PERIOD   int     Period level
;; [2]    COUNT    int     Number of orders at that price level
;; [3]    AMOUNT   float   Total amount available at that price level (if AMOUNT > 0 then ask else bid)
(define-struct bitfinex-funding-book-message
  (rate period count amount)
  #:transparent)

;; For trading pair symbols (ex. tBTCUSD)
;; Index  Field    Type     Description
;; [0]    ID       int      ID of the trade
;; [1]    MTS      int      Millisecond epoch timestamp
;; [2]    AMOUNT   float    How much was bought (positive) or sold (negative)
;; [3]    PRICE    float    Price at which the trade was executed
(define-struct bitfinex-trading-trades-message
  (id timestamp amount price)
  #:transparent)

;; For funding currency symbols (ex. fUSD)
;; Index  Field    Type    Description
;; [0]    ID       int     ID of the trade
;; [1]    MTS      int     Millisecond epoch timestamp
;; [2]    AMOUNT   float   How much was bought (positive) or sold (negative)
;; [3]    RATE     float   Rate at which funding transaction occurred
;; [4]    PERIOD   int     Amount of time the funding transaction was for
(define-struct bitfinex-funding-trades-message
  (id timestamp amount rate period)
  #:transparent)

;;

(define bitfinex-metric-subscriptions
  (make-prometheus-metric-gauge 'bitfinex-subscriptions
                                #:doc "Amount of active subscriptions"))
(define bitfinex-metric-events
  (make-prometheus-metric-counter 'bitfinex-events
                                  #:doc "Amount of events observed"))
(define bitfinex-metric-messages
  (make-prometheus-metric-counter 'bitfinex-messages
                                  #:doc "Amount of messages observed"))
(define bitfinex-metrics
  (list bitfinex-metric-subscriptions
        bitfinex-metric-events
        bitfinex-metric-messages))

(define (bitfinex-metrics-register! #:registry (registry (current-prometheus-registry)))
  (for ((metric (in-list bitfinex-metrics)))
    (prometheus-register! metric #:registry registry)))

(define (bitfinex-metrics-unregister! #:registry (registry (current-prometheus-registry)))
  (for ((metric (in-list bitfinex-metrics)))
    (prometheus-unregister! metric #:registry registry)))

;;

(define bitfinex-transport<%>
  (interface () open close read write))

(define bitfinex-websocket%
  (class* object% (bitfinex-transport<%>)
    (field (socket (void)))
    (super-new)

    (define-syntax-rule (with-exn-handler result body ...)
      (with-handlers
        ((exn:fail:network?
          (lambda (e) (log-error (exn-message e)) result))
         (exn:input-port-closed?
          (lambda (e) (log-error (exn-message e)) result)))
        body ...))

    (define/public (open target)
      (set! socket
        (with-exn-handler (void)
          (websocket-connect target))))

    (define/public (close)
      (when (not (void? socket))
        (websocket-close socket)
        (set! socket (void))))

    (define/public (read)
      (if (void? socket)
          eof
          (with-exn-handler eof
            (websocket-receive socket #:payload-type websocket-payload-text))))

    (define/public (write payload)
      (when (not (void? socket))
        (with-exn-handler eof
          (websocket-send socket payload #:payload-type websocket-payload-text))))))

(define (make-bitfinex-websocket-transport)
  (new bitfinex-websocket%))

(define (make-bitfinex-transport type)
  (case type
    ((websocket) (make-bitfinex-websocket-transport))))

(module+ test
  (check-true (is-a? (make-bitfinex-transport 'websocket) bitfinex-transport<%>))
  (check-true (is-a? (make-bitfinex-transport 'websocket) bitfinex-websocket%)))

;;

(define-syntax (select-by-market-type stx)
  (syntax-case stx ()
    ((_ market-type t f)
     #'(cond
         ((eq? market-type 'trading) t)
         ((eq? market-type 'funding) f)
         (else (raise (exn:fail:user:bitfinex:market-type
                       (format "unknown market type: ~a" market-type)
                       (current-continuation-marks))))))))

(define-syntax (bitfinex-entity-by-market-type stx)
  (syntax-case stx ()
    ((_ market-type entity)
     (with-syntax ((t (format-id #'entity "bitfinex-trading-~a-message" #'entity))
                   (f (format-id #'entity "bitfinex-funding-~a-message" #'entity)))
       #'(select-by-market-type market-type t f)))
    ((_ market-type entity field)
     (with-syntax ((t (format-id #'entity "bitfinex-trading-~a-message-~a" #'entity #'field))
                   (f (format-id #'entity "bitfinex-funding-~a-message-~a" #'entity #'field)))
       #'(select-by-market-type market-type t f)))))

(define-syntax (make-bitfinex-request stx)
  (syntax-case stx ()
    ((make-bitfinex-request type v ...)
     #'(hasheq 'event type v ...))))

(define (make-symbol market-type pair)
  (string-append
   (select-by-market-type market-type "t" "f")
   (car pair)
   (cdr pair)))

(module+ test
  (test-case "select-by-market-type"
    (check-equal? (select-by-market-type 'trading 't 'f) 't)
    (check-equal? (select-by-market-type 'funding 't 'f) 'f))

  (test-case "bitfinex-entity-by-market-type"
    (let ((bitfinex-trading-entity-name-message 1)
          (bitfinex-trading-entity-name-message-field-name 2)
          (bitfinex-funding-entity-name-message 3)
          (bitfinex-funding-entity-name-message-field-name 4))
      (check-equal? (bitfinex-entity-by-market-type 'trading entity-name)
                    bitfinex-trading-entity-name-message)
      (check-equal? (bitfinex-entity-by-market-type 'trading entity-name field-name)
                    bitfinex-trading-entity-name-message-field-name)
      (check-equal? (bitfinex-entity-by-market-type 'funding entity-name)
                    bitfinex-funding-entity-name-message)
      (check-equal? (bitfinex-entity-by-market-type 'funding entity-name field-name)
                    bitfinex-funding-entity-name-message-field-name)))

  (test-case "make-bitfinex-request"
    (check-equal? (make-bitfinex-request 'test) (hasheq 'event 'test))
    (check-equal? (make-bitfinex-request 'test 'foo 'bar) (hasheq 'event 'test 'foo 'bar)))

  (test-case "make-symbol"
    (check-equal? (make-symbol 'trading '("BTC" . "USD")) "tBTCUSD")
    (check-equal? (make-symbol 'funding '("BTC" . "USD")) "fBTCUSD")))

;;

(define (bitfinex-subscribe/trades market-type pair #:connection (conn (current-bitfinex-connection)))
  (let ((constructor (bitfinex-entity-by-market-type market-type trades)))
    (bitfinex-subscribe
     (make-bitfinex-request "subscribe"
                            'channel "trades"
                            'symbol (make-symbol market-type pair))
     (lambda (message)
       (cond
         ((list? (cadr message))
          (map (lambda (v) (apply constructor v))
               (cadr message)))
         (else (apply constructor (caddr message)))))
     #:connection conn
     #:filter
     (lambda (message _)
       (define type (cadr message))
       (cond
         ((list? type) #f)
         ((eq? (string->symbol type) 'te)  #t)
         ((eq? (string->symbol type) 'fte) #t)
         (else #f))))))

(define (bitfinex-subscribe/ticker market-type pair #:connection (conn (current-bitfinex-connection)))
  (let ((constructor (bitfinex-entity-by-market-type market-type ticker)))
    (bitfinex-subscribe
     (make-bitfinex-request "subscribe"
                            'channel "ticker"
                            'symbol (make-symbol market-type pair))
     (lambda (message)
       (apply constructor (cadr message)))
     #:connection conn)))

(define (bitfinex-subscribe/book market-type pair #:connection (conn (current-bitfinex-connection)))
  (let ((constructor (bitfinex-entity-by-market-type market-type book)))
    (bitfinex-subscribe
     (make-bitfinex-request "subscribe"
                            'channel "book"
                            'symbol (make-symbol market-type pair))
     (lambda (message)
       (cond
         ((list? (caadr message))
          (map (lambda (v) (apply constructor v))
               (cadr message)))
         (else (apply constructor (cadr message)))))
     #:connection conn)))

;;

(define (bitfinex-handshake transport)
  (let* ((buf (send transport read))
         (raw-description (bytes->json (begin0 buf
                                         (when (eof-object? buf)
                                           (raise (exn:fail:user:bitfinex:eof
                                                   "unexpected eof"
                                                   (current-continuation-marks)))))))
         (remote-version (hash-ref raw-description "version")))
    (when (not (= remote-version version))
      (raise (exn:fail:user:bitfinex:version
              (format "invalid api version: ~a, while expected >= ~a" remote-version version)
              (current-continuation-marks)
              remote-version)))
    (make-bitfinex-description
     (hash-ref raw-description "serverId")
     remote-version
     (hash-ref (hash-ref raw-description "platform") "status"))))

(define (bitfinex-handle-event subscriptions message)
  (match (hash-ref message "event")
    ("subscribed"   (bitfinex-handle-event/subscribed   subscriptions message))
    ("unsubscribed" (bitfinex-handle-event/unsubscribed subscriptions message))
    ("error"        (bitfinex-handle-event/error        subscriptions message))
    ("info"         (bitfinex-handle-event/info         subscriptions message))
    (event (raise (exn:fail:user:bitfinex:event
                   (format "unsupported event: ~a" event)
                   (current-continuation-marks)
                   message)))))

;; XXX: bitfinex-handle-event/* should not perform any non thread-safe operations all
;; such operations should be performed inside imperative functions like
;; subscribe/unsubscribe with semaphore.
(define (bitfinex-handle-event/error subscriptions message)
  (let* ((code (hash-ref message "code" #f))
         (err (exn:fail:user:bitfinex:api
               (format "api error: ~a" (hash-ref message "msg"))
               (current-continuation-marks)
               code))
         (semaphore (bitfinex-subscriptions-semaphore subscriptions)))
    (prometheus-increment! bitfinex-metric-events
                           #:labels `((type . error)
                                      (code . ,code)))
    (cond
      ;; locked semaphore indicates pending subscription
      ((semaphore-try-wait? semaphore)
       (begin
         (semaphore-post semaphore)
         (raise err)))
      (else (async-channel-put (bitfinex-subscriptions-pending subscriptions) err)))))

(define (bitfinex-handle-event/info _ message)
  (let* ((code (hash-ref message "code" #f))
         (msg  (or (hash-ref errors code #f)
                   (hash-ref events code #f)
                   "unknown")))
    (prometheus-increment! bitfinex-metric-events
                           #:labels `((type . info)
                                      (code . ,code)))
    (raise
     (exn:fail:user:bitfinex:api:info
      (format "server info: ~a (code: ~a)" msg code)
      (current-continuation-marks)
      code))))

(define (bitfinex-handle-event/subscribed subs message)
  (async-channel-put
   (bitfinex-subscriptions-pending subs)
   (make-bitfinex-subscribed (hash-ref message "chanId")))
  (prometheus-increment! bitfinex-metric-events
                         #:labels `((type . subscribed))))

(define (bitfinex-handle-event/unsubscribed subs message)
  (async-channel-put
   (bitfinex-subscriptions-pending subs)
   (make-bitfinex-unsubscribed (hash-ref message "chanId")))
  (prometheus-increment! bitfinex-metric-events
                         #:labels `((type . unsubscribed))))

(define (bitfinex-handle-message streams message)
  (let ((stream (hash-ref streams (car message) #f)))
    (cond
      ((eq? stream #f)
       (prometheus-increment! bitfinex-metric-events
                              #:labels `((type . message)
                                         (kind . orphan)))
       (log-warning "orphaned message: ~a, have no subscriptions for stream ~a" message (car message)))
      ((equal? (cadr message) "hb")
       (prometheus-increment! bitfinex-metric-events
                              #:labels `((type . message)
                                         (kind . heartbeat)))
       ;; TODO: detect dead connections
       (set-bitfinex-stream-last-heartbeat! stream (current-seconds)))
      (else
       (prometheus-increment! bitfinex-metric-events
                              #:labels `((type . message)
                                         (kind . data)))
       (set-bitfinex-stream-last-message! stream (current-seconds))
       (async-channel-put (bitfinex-stream-channel stream) message)))))

(define (make-bitfinex-message-generator channel transform
                                         #:filter (filter? #f))
  (generator
   ()
   (define (maybe-yield message result)
     (when (or (not filter?)
               (filter? message result))
       (yield result)))
   (let loop ()
     (define message (async-channel-get channel))
     (cond
       ((eof-object? message) (void))
       ((exn? message) (raise message))
       (else
        (define result (transform message))
        ;; XXX: we could have a stream of messages
        ;; in this case we have 1 message per channel-get
        ;; or we could have a bunch of messages as list
        ;; coming from single channel-get.
        ;; this shit comes from bitfinex api design so
        ;; that's why we have list? here, transform
        ;; should also deal with this because we could
        ;; want to handle list? messages differently.
        ;; for example see ticker and trades.
        (if (list? result)
            (for ((v result)) (maybe-yield message v))
            (maybe-yield message result))
        (loop))))))

(define (bitfinex-subscribe request transform
                            #:connection  (conn (current-bitfinex-connection))
                            #:filter      (filter?     #f)
                            #:timeout     (timeout     10)
                            #:buffer-size (buffer-size 32))
  (let* ((transport     (bitfinex-connection-transport     conn))
         (subscriptions (bitfinex-connection-subscriptions conn))
         (streams       (bitfinex-subscriptions-streams    subscriptions))
         (pending       (bitfinex-subscriptions-pending    subscriptions)))
    (call-with-semaphore
     (bitfinex-subscriptions-semaphore subscriptions)
     (thunk
      (send transport write (json->bytes request))
      (define evt (sync/timeout timeout pending))

      (cond
        ((eq? evt #f)
         (raise (exn:fail:user:bitfinex:timeout
                 (format "timed out: waiting for subscription for ~a seconds" timeout)
                 (current-continuation-marks))))
        ((eof-object? evt)
         (raise (exn:fail:user:bitfinex:closed
                 "closed: while waiting for channel id"
                 (current-continuation-marks))))
        ((exn? evt) (raise evt))
        (else
         (let ((id (bitfinex-subscribed-id evt))
               (channel (make-async-channel buffer-size)))
           (hash-set! streams id
                      (make-bitfinex-stream channel
                                            (current-seconds)
                                            (current-seconds)))
           (prometheus-set! bitfinex-metric-subscriptions (length (hash-keys streams)))
           (values id (make-bitfinex-message-generator channel transform #:filter filter?)))))))))

(define (bitfinex-unsubscribe id
                              #:connection (conn (current-bitfinex-connection))
                              #:timeout (timeout 10))
  (let* ((transport (bitfinex-connection-transport conn))
         (subscriptions (bitfinex-connection-subscriptions conn))
         (streams (bitfinex-subscriptions-streams subscriptions))
         (pending (bitfinex-subscriptions-pending subscriptions))
         (unsubscribe (json->bytes (make-bitfinex-request "unsubscribe" 'chanId id))))
    (call-with-semaphore
     (bitfinex-subscriptions-semaphore subscriptions)
     (thunk
      (send transport write unsubscribe)
      (define evt (sync/timeout timeout pending))

      (async-channel-put (bitfinex-stream-channel (hash-ref streams id)) eof)
      (cond
        ((eq? evt #f)
         (raise (exn:fail:user:bitfinex:timeout
                 (format "timed out: waiting for unsubscription for ~a seconds" timeout)
                 (current-continuation-marks))))
        ((eof-object? evt)
         (raise (exn:fail:user:bitfinex:closed
                 "closed: while waiting for channel id"
                 (current-continuation-marks))))
        ((exn? evt) (raise evt))
        (else
         (hash-remove! streams id)
         (prometheus-set! bitfinex-metric-subscriptions (length (hash-keys streams)))))))))

(module+ test
  (test-case "bitfinex-handshake"
    (define (make-dummy-transport #:version (version version)
                                  #:read-result (read-result (json->bytes
                                                              (hasheq
                                                               'event    "info"
                                                               'platform (hasheq 'status 1)
                                                               'serverId "fe6a38fb-7e2b-49ae-8416-f32dcf9a2149"
                                                               'version  version))))
      (new (class object%
             (super-new)
             (define/public (read) read-result))))

    (test-case "should return bitfinex-description constructed from bitfinex-handshake meta-data"
      (check-equal?
       (bitfinex-handshake (make-dummy-transport))
       (make-bitfinex-description "fe6a38fb-7e2b-49ae-8416-f32dcf9a2149" 2 1)))
    (test-case "should fail with unexpected eof"
      (check-exn exn:fail:user:bitfinex:eof?
                 (thunk (bitfinex-handshake (make-dummy-transport #:read-result eof)))))
    (test-case "should fail with unsupported server api version"
      (check-exn exn:fail:user:bitfinex:version?
                 (thunk (bitfinex-handshake (make-dummy-transport #:version 1))))
      (check-exn exn:fail:user:bitfinex:version?
                 (thunk (bitfinex-handshake (make-dummy-transport #:version 3))))))

  (test-case "bitfinex-handle-event/error"
    (test-case "api exception should be raised if error event was handled with no pending subscriptions"
      (let ((semaphore (make-semaphore 1)))
        (check-exn
         exn:fail:user:bitfinex:api?
         (thunk
          (bitfinex-handle-event/error
           (make-bitfinex-subscriptions (void) (void) semaphore)
           (hash "msg" "test"
                 "code" 666))))
        ;; semaphore should not be locked after exception was raised
        (check-not-false (sync/timeout 0 semaphore))))
    (test-case "api exception should be passed into a pending subscription channel if error event was handled"
      (let ((chan (make-async-channel)))
        (check-true
         (begin
           (bitfinex-handle-event/error
            (make-bitfinex-subscriptions (void) chan (make-semaphore 0))
            (hash "msg" "test"
                  "code" 666))
           (exn:fail:user:bitfinex:api? (async-channel-get chan)))))))

  (test-case "bitfinex-handle-event/info"
    (test-case "exception should be raised if info event was handled"
      (check-exn
       exn:fail:user:bitfinex:api:info?
       (thunk (bitfinex-handle-event/info (void) (hash "code" 0))))))

  (test-case "bitfinex-handle-event/subscribed"
    (test-case "subscribed struct containing channel id should be returned when handled subscribed event"
      (let ((chan (make-async-channel)))
        (bitfinex-handle-event/subscribed (make-bitfinex-subscriptions (void) chan (void))
                                          (hash "chanId" 666))
        (check-equal?
         (async-channel-get chan)
         (make-bitfinex-subscribed 666)))))

  (test-case "bitfinex-handle-event/unsubscribed"
    (test-case "unsubscribed struct containing channel id should be returned when handled unsubscribed event"
      (let ((chan (make-async-channel)))
        (bitfinex-handle-event/unsubscribed (make-bitfinex-subscriptions (void) chan (void))
                                            (hash "chanId" 666))
        (check-equal?
         (async-channel-get chan)
         (make-bitfinex-unsubscribed 666))))

    (test-case "bitfinex-handle-message"
      (test-case "ignore messages from channels we have no subscription for"
        (check-equal?
         (bitfinex-handle-message (hash) (list 13 "hb"))
         (void)))
      (test-case "heartbeat in channel we have subscription for should change the timestamp"
        (let ((streams (hasheq 13 (make-bitfinex-stream (void) 0 0))))
          (bitfinex-handle-message streams (list 13 "hb"))
          (check-equal? (bitfinex-stream-last-message (hash-ref streams 13)) 0)
          (check-true (> (bitfinex-stream-last-heartbeat (hash-ref streams 13)) 0))))
      (test-case "any message other then heartbeat in a channel we have subscription for should change mtime and put a message into stream channel"
        (let* ((chan    (make-async-channel))
               (streams (hasheq 13 (make-bitfinex-stream chan 0 0)))
               (message (list 13 "hello")))
          (bitfinex-handle-message streams message)
          (check-true (> (bitfinex-stream-last-message (hash-ref streams 13)) 0))
          (check-equal? (bitfinex-stream-last-heartbeat (hash-ref streams 13)) 0)
          (check-equal? (async-channel-get chan) message)))))

  (test-case "message generator"
    (test-case "message passed to channel should be produced by generator"
      (let* ((channel   (make-async-channel))
             (transform (lambda (message) message))
             (next      (make-bitfinex-message-generator channel transform)))
        (check-equal? (begin
                        (async-channel-put channel "hello")
                        (next))
                      "hello")))
    (test-case "errors from channel should be raised by generator"
      (let* ((channel   (make-async-channel))
             (transform (lambda (message) message))
             (next      (make-bitfinex-message-generator channel transform)))
        (check-exn exn:test?
                   (thunk
                    (begin
                      (async-channel-put channel (exn:test "hello" (current-continuation-marks)))
                      (next))))))
    (test-case "eof should stop generator"
      (let* ((channel   (make-async-channel))
             (transform (lambda (message) message))
             (next      (make-bitfinex-message-generator channel transform)))
        (check-equal? (begin
                        (async-channel-put channel "hey")
                        (next))
                      "hey")
        (check-equal? (begin
                        (async-channel-put channel eof)
                        (next))
                      (void))))
    (test-case "eof should stop generator even if there was no messages"
      (let* ((channel   (make-async-channel))
             (transform (lambda (message) message))
             (next      (make-bitfinex-message-generator channel transform)))
        (check-equal? (begin
                        (async-channel-put channel eof)
                        (next))
                      (void))))
    (test-case "stopped generator should stop forever"
      (let* ((channel   (make-async-channel))
             (transform (lambda (message) message))
             (next      (make-bitfinex-message-generator channel transform)))
        (check-equal? (begin
                        (async-channel-put channel "hey")
                        (next))
                      "hey")
        (check-equal? (begin
                        (async-channel-put channel eof)
                        (next))
                      (void))
        (check-equal? (begin
                        (async-channel-put channel "yo")
                        (next))
                      (void)))))

  (test-case "subscribe"
    (let ((make-dummy-transform (lambda (result) (lambda (message) result)))
          (make-subscribe-request
           (thunk (make-bitfinex-request "subscribe"
                                         'channel "some"
                                         'symbol (make-symbol 'trading '("BTC" . "USD")))))
          (make-subscriptions
           (thunk (make-bitfinex-subscriptions (make-hash)
                                               (make-async-channel)
                                               (make-semaphore 1))))
          (make-dummy-transport (lambda (#:on-write (on-write void))
                                  (new (class object%
                                         (super-new)
                                         (define/public (write buf)
                                           (on-write buf)))))))
      (test-case "channel id received from transport should be returned"
        (let*-values
            (((subscriptions) (make-subscriptions))
             ((request) (make-subscribe-request))
             ((message) "hello you")
             ((write-handler) (lambda (buf)
                                (check-equal? (bytes->json buf)
                                              '#hash(("channel" . "some")
                                                     ("event" . "subscribe")
                                                     ("symbol" . "tBTCUSD")))
                                (bitfinex-handle-event/subscribed
                                 subscriptions
                                 (hash "chanId" 666))))
             ((id next)
              (bitfinex-subscribe request (make-dummy-transform message)
                                  #:connection (-make-bitfinex-connection
                                                (make-dummy-transport #:on-write write-handler)
                                                subscriptions (void))))
             ((channel)
              (bitfinex-stream-channel (hash-ref (bitfinex-subscriptions-streams subscriptions) id))))
          (check-equal? id 666)
          (test-case "should create single subscription"
            (check-equal? (hash-keys (bitfinex-subscriptions-streams subscriptions))
                          (list 666)))))

      (test-case "absence of subscription acknowledge should produce a timeout exception"
        (let ((subscriptions (make-subscriptions)))
          (check-exn exn:fail:user:bitfinex:timeout?
                     (thunk (bitfinex-subscribe (make-subscribe-request)
                                                (make-dummy-transform (void))
                                                #:connection (-make-bitfinex-connection
                                                              (make-dummy-transport #:on-write void) subscriptions (void))
                                                #:timeout 1)))
          (test-case "should not create any stream"
            (check-true (hash-empty? (bitfinex-subscriptions-streams subscriptions))))))

      (test-case "exception passed into pending subscription channel should be raised"
        (let ((subscriptions (make-subscriptions)))
          (check-exn exn:test?
                     (thunk (bitfinex-subscribe (make-subscribe-request)
                                                (make-dummy-transform (void))
                                                #:connection (-make-bitfinex-connection
                                                              (make-dummy-transport
                                                               #:on-write (thunk*
                                                                           (async-channel-put (bitfinex-subscriptions-pending subscriptions)
                                                                                              (exn:test "test" (current-continuation-marks)))))
                                                              subscriptions (void)))))
          (test-case "should not create any stream"
            (check-true (hash-empty? (bitfinex-subscriptions-streams subscriptions))))))

      (test-case "eof passed into pending subscription channel should produce an exception"
        (let ((subscriptions (make-subscriptions)))
          (check-exn exn:fail:user:bitfinex:closed?
                     (thunk (bitfinex-subscribe (make-subscribe-request)
                                                (make-dummy-transform (void))
                                                #:connection (-make-bitfinex-connection
                                                              (make-dummy-transport
                                                               #:on-write
                                                               (thunk* (async-channel-put (bitfinex-subscriptions-pending subscriptions) eof)))
                                                              subscriptions (void)))))
          (test-case "should not create any stream"
            (check-true (hash-empty? (bitfinex-subscriptions-streams subscriptions))))))

      (test-case "exception raised in transport should be raised"
        (let ((subscriptions (make-subscriptions)))
          (check-exn exn:test?
                     (thunk (bitfinex-subscribe (make-subscribe-request)
                                                (make-dummy-transform (void))
                                                #:connection (-make-bitfinex-connection
                                                              (make-dummy-transport #:on-write (thunk* (raise (exn:test "test" (current-continuation-marks)))))
                                                              subscriptions (void)))))
          (test-case "should not create any stream"
            (check-true (hash-empty? (bitfinex-subscriptions-streams subscriptions)))))))))

;;

(define (make-bitfinex-connection #:url (url endpoint)
                                  #:timeout (timeout 10)
                                  #:transport (transport (make-bitfinex-transport 'websocket))
                                  #:when-eof (when-eof void))
  (let ((subscriptions
         (make-bitfinex-subscriptions (make-hash)
                                      (make-async-channel 1)
                                      (make-semaphore 1))))
    (send transport open (url->string url))
    (begin0 (-make-bitfinex-connection transport subscriptions (bitfinex-handshake transport))
      (thread (thunk (bitfinex-connection-worker transport subscriptions when-eof))))))

(define (bitfinex-connection-worker transport subscriptions when-eof)
  (define (log-worker-error e)
    (log-error
     "bitfinex connection worker error, leaving the loop: ~a"
     (exn-message e)))

  (define streams (bitfinex-subscriptions-streams subscriptions))
  (let loop ()
    (define buf
      (with-handlers
        ((exn? (lambda (e) (log-worker-error e) eof)))
        (send transport read)))
    (cond
      ((eof-object? buf)
       (when (not (semaphore-try-wait? (bitfinex-subscriptions-semaphore subscriptions)))
         (async-channel-put (bitfinex-subscriptions-pending subscriptions) eof))
       (for ((k (hash-keys streams)))
         (async-channel-put (bitfinex-stream-channel (hash-ref streams k)) eof))
       (when-eof transport))
      (else
       ;; XXX: be careful, it is very easy to get a memory leak here
       ;; because this loop should never exit you should never call (loop)
       ;; inside (with-handlers) s-exp, it always should be outside.
       (define continue?
         (with-handlers
           ((exn? (lambda (e) (log-worker-error e) #f)))
           (let ((message (bytes->json buf)))
             (cond
               ((hash? message) (bitfinex-handle-event subscriptions message))
               ((list? message) (bitfinex-handle-message streams message))
               (else (raise (exn:fail:user:bitfinex:message
                             (format "unsupported message: ~a" message)
                             (current-continuation-marks)
                             message)))))
           #t))
       (if continue?
           (loop)
           (begin
             (send transport close)
             (when-eof transport)))))))

(define (bitfinex-close (connection (current-bitfinex-connection)))
  (send (bitfinex-connection-transport connection) close))

(module+ test
  (test-case "make-bitfinex-connection"
    (let ((transport
           (new (class object%
                  (init-field (was-opened #f)
                              (was-closed #f)
                              (next
                               (generator ()
                                          (yield (json->bytes
                                                  (hasheq
                                                   'event    "info"
                                                   'platform (hasheq 'status 1)
                                                   'serverId "fe6a38fb-7e2b-49ae-8416-f32dcf9a2149"
                                                   'version  version)))
                                          (yield eof))))
                  (super-new)
                  (define/public (open endpoint)
                    (set! was-opened (not was-opened)))
                  (define/public (close)
                    (set! was-closed (not was-closed)))
                  (define/public (read)
                    (let ((buf (next)))
                      ;; XXX: transport must return either bytes or eof
                      ;; if it return something other, say void, then
                      ;; you will receive void which is usually not what
                      ;; you want.
                      (if (void? buf) eof buf)))))))
      (let* ((semaphore  (make-semaphore 0))
             (eof-closer (lambda (transport)
                           (send transport close)
                           (semaphore-post semaphore))))
        (test-case "connection constructor should return connection struct"
          (check-true (bitfinex-connection? (make-bitfinex-connection
                                             #:transport transport
                                             #:when-eof eof-closer))))
        (test-case "connection constructor should open transport"
          (check-true (get-field was-opened transport)))
        (test-case "transport should call when-eof handler when eof encountered"
          (check-true (semaphore? (sync/timeout 5 semaphore)))
          (check-true (get-field was-closed transport))))))

  (test-case "bitfinex-connection-worker"
    (test-case "immediate eof"
      (let ((channel   (make-async-channel 1))
            (pending   (make-async-channel 1))
            (semaphore (make-semaphore 1)))
        (bitfinex-connection-worker
         (new (class object% (super-new) (define/public (read) eof)))
         (make-bitfinex-subscriptions (hasheq 1 (make-bitfinex-stream channel 0 0))
                                      pending
                                      (make-semaphore 1))
         (lambda (transport) (semaphore-post semaphore)))
        (check-true   (semaphore? (sync/timeout 5 semaphore)))
        (check-equal? (sync/timeout 1 channel) eof)
        (check-false  (sync/timeout 1 pending))))))
