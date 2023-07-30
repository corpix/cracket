#lang racket
(require corpix/jsonrpc
         corpix/websocket)

(define current-aria-rpc-secret (make-parameter #f))

(define (make-aria-secret (secret (current-aria-rpc-secret)))
  (format "token:~a" secret))

(define-jsonrpc (aria-add-uri secret uris (options (void)) (position (void)))
  #:method aria2.addUri
  #:doc "This method adds a new download. uris is an array of HTTP/FTP/SFTP/BitTorrent URIs (strings) pointing to the same resource. If you mix URIs pointing to different resources, then the download may fail or be corrupted without aria2 complaining. When adding BitTorrent Magnet URIs, uris must have only one element and it should be BitTorrent Magnet URI. options is a struct and its members are pairs of option name and value. See Options below for more details. If position is given, it must be an integer starting from 0. The new download will be inserted at position in the waiting queue. If position is omitted or position is larger than the current size of the queue, the new download is appended to the end of the queue. This method returns the GID of the newly registered download.")

(define-jsonrpc (aria-add-torrent secret torrent (uris (void)) (options (void)) (position (void)))
  #:method aria2.addTorrent
  #:doc "This method adds a BitTorrent download by uploading a \".torrent\" file. If you want to add a BitTorrent Magnet URI, use the aria2.addUri() method instead. torrent must be a base64-encoded string containing the contents of the \".torrent\" file. uris is an array of URIs (string). uris is used for Web-seeding. For single file torrents, the URI can be a complete URI pointing to the resource; if URI ends with /, name in torrent file is added. For multi-file torrents, name and path in torrent are added to form a URI for each file. options is a struct and its members are pairs of option name and value. See Options below for more details. If position is given, it must be an integer starting from 0. The new download will be inserted at position in the waiting queue. If position is omitted or position is larger than the current size of the queue, the new download is appended to the end of the queue. This method returns the GID of the newly registered download. If --rpc-save-upload-metadata is true, the uploaded data is saved as a file named as the hex string of SHA-1 hash of data plus \".torrent\" in the directory specified by --dir option. E.g. a file name might be 0a3893293e27ac0490424c06de4d09242215f0a6.torrent. If a file with the same name already exists, it is overwritten! If the file cannot be saved successfully or --rpc-save-upload-metadata is false, the downloads added by this method are not saved by --save-session.")

(define-jsonrpc (aria-add-metalink secret metalink (options (void)) (position (void)))
  #:method aria2.addMetalink
  #:doc "This method adds a Metalink download by uploading a \".metalink\" file. metalink is a base64-encoded string which contains the contents of the \".metalink\" file. options is a struct and its members are pairs of option name and value. See Options below for more details. If position is given, it must be an integer starting from 0. The new download will be inserted at position in the waiting queue. If position is omitted or position is larger than the current size of the queue, the new download is appended to the end of the queue. This method returns an array of GIDs of newly registered downloads. If --rpc-save-upload-metadata is true, the uploaded data is saved as a file named hex string of SHA-1 hash of data plus \".metalink\" in the directory specified by --dir option. E.g. a file name might be 0a3893293e27ac0490424c06de4d09242215f0a6.metalink. If a file with the same name already exists, it is overwritten! If the file cannot be saved successfully or --rpc-save-upload-metadata is false, the downloads added by this method are not saved by --save-session.")

(define-jsonrpc (aria-remove secret gid)
  #:method aria2.remove
  #:doc "This method removes the download denoted by gid (string). If the specified download is in progress, it is first stopped. The status of the removed download becomes removed. This method returns GID of removed download.")

(define-jsonrpc (aria-force-remove secret gid)
  #:method aria2.forceRemove
  #:doc "This method removes the download denoted by gid. This method behaves just like aria2.remove() except that this method removes the download without performing any actions which take time, such as contacting BitTorrent trackers to unregister the download first.")

(define-jsonrpc (aria-pause secret gid)
  #:method aria2.pause
  #:doc "This method pauses the download denoted by gid (string). The status of paused download becomes paused. If the download was active, the download is placed in the front of waiting queue. While the status is paused, the download is not started. To change status to waiting, use the aria2.unpause() method. This method returns GID of paused download.")

(define-jsonrpc (aria-pause-all secret)
  #:method aria2.pauseAll
  #:doc "This method is equal to calling aria2.pause() for every active/waiting download. This methods returns OK.")

(define-jsonrpc (aria-force-pause secret gid)
  #:method aria2.forcePause
  #:doc "This method pauses the download denoted by gid. This method behaves just like aria2.pause() except that this method pauses downloads without performing any actions which take time, such as contacting BitTorrent trackers to unregister the download first.")

(define-jsonrpc (aria-force-pause-all secret)
  #:method aria2.forcePauseAll
  #:doc "This method is equal to calling aria2.forcePause() for every active/waiting download. This methods returns OK.")

(define-jsonrpc (aria-unpause secret gid)
  #:method aria2.unpause
  #:doc "This method changes the status of the download denoted by gid (string) from paused to waiting, making the download eligible to be restarted. This method returns the GID of the unpaused download.")

(define-jsonrpc (aria-unpause-all secret)
  #:method aria2.unpauseAll
  #:doc "This method is equal to calling aria2.unpause() for every paused download. This methods returns OK.")

(define-jsonrpc (aria-tell-status secret gid (keys (void)))
  #:method aria2.tellStatus
  #:doc "This method returns the progress of the download denoted by gid (string). keys is an array of strings. If specified, the response contains only keys in the keys array. If keys is empty or omitted, the response contains all keys. This is useful when you just want specific keys and avoid unnecessary transfers. For example, aria2.tellStatus(\"2089b05ecca3d829\", [\"gid\", \"status\"]) returns the gid and status keys only. The response is a struct and contains following keys. Values are strings.
        - gid
          GID of the download.
        - status
          active for currently downloading/seeding downloads. waiting for downloads in the queue; download is not started. paused for paused downloads. error for downloads that were stopped because of error. complete for stopped and completed downloads. removed for the downloads removed by user.
        - totalLength
          Total length of the download in bytes.
        - completedLength
          Completed length of the download in bytes.
        - uploadLength
          Uploaded length of the download in bytes.
        - bitfield
          Hexadecimal representation of the download progress. The highest bit corresponds to the piece at index 0. Any set bits indicate loaded pieces, while unset bits indicate not yet loaded and/or missing pieces. Any overflow bits at the end are set to zero. When the download was not started yet, this key will not be included in the response.
        - downloadSpeed
          Download speed of this download measured in bytes/sec.
        - uploadSpeed
          Upload speed of this download measured in bytes/sec.
        - infoHash
          InfoHash. BitTorrent only.
        - numSeeders
          The number of seeders aria2 has connected to. BitTorrent only.
        - seeder
          true if the local endpoint is a seeder. Otherwise false. BitTorrent only.
        - pieceLength
          Piece length in bytes.
        - numPieces
          The number of pieces.
        - connections
          The number of peers/servers aria2 has connected to.
        - errorCode
          The code of the last error for this item, if any. The value is a string. The error codes are defined in the EXIT STATUS section. This value is only available for stopped/completed downloads.
        - errorMessage
          The (hopefully) human readable error message associated to errorCode.
        - followedBy
          List of GIDs which are generated as the result of this download. For example, when aria2 downloads a Metalink file, it generates downloads described in the Metalink (see the --follow-metalink option). This value is useful to track auto-generated downloads. If there are no such downloads, this key will not be included in the response.
        - following
          The reverse link for followedBy. A download included in followedBy has this object's GID in its following value.
        - belongsTo
          GID of a parent download. Some downloads are a part of another download. For example, if a file in a Metalink has BitTorrent resources, the downloads of \".torrent\" files are parts of that parent. If this download has no parent, this key will not be included in the response.
        - dir
          Directory to save files.
        - files
          Returns the list of files. The elements of this list are the same structs used in aria2.getFiles() method.
        - bittorrent
          Struct which contains information retrieved from the .torrent (file). BitTorrent only. It contains following keys.
            - announceList
              List of lists of announce URIs. If the torrent contains announce and no announce-list, announce is converted to the announce-list format.
            - comment
              The comment of the torrent. comment.utf-8 is used if available.
            - creationDate
              The creation time of the torrent. The value is an integer since the epoch, measured in seconds.
            - mode
              File mode of the torrent. The value is either single or multi.
            - info
              Struct which contains data from Info dictionary. It contains following keys.
            - name
              name in info dictionary. name.utf-8 is used if available.
        - verifiedLength
          The number of verified number of bytes while the files are being hash checked. This key exists only when this download is being hash checked.
        - verifyIntegrityPending
          true if this download is waiting for the hash check in a queue. This key exists only when this download is in the queue.
")

(define-jsonrpc (aria-get-uris secret gid)
  #:method aria2.getUris
  #:doc "This method returns the URIs used in the download denoted by gid (string). The response is an array of structs and it contains following keys. Values are string.")

(define-jsonrpc (aria-get-files secret gid)
  #:method aria2.getFiles
  #:doc "This method returns the file list of the download denoted by gid (string). The response is an array of structs which contain following keys. Values are strings.
        - index
          Index of the file, starting at 1, in the same order as files appear in the multi-file torrent.
        - path
          File path.
        - length
          File size in bytes.
        - completedLength
          Completed length of this file in bytes. Please note that it is possible that sum of completedLength is less than the completedLength returned by the aria2.tellStatus() method. This is because completedLength in aria2.getFiles() only includes completed pieces. On the other hand, completedLength in aria2.tellStatus() also includes partially completed pieces.
        - selected
          true if this file is selected by --select-file option. If --select-file is not specified or this is single-file torrent or not a torrent download at all, this value is always true. Otherwise false.
        - uris
          Returns a list of URIs for this file. The element type is the same struct used in the aria2.getUris() method.")

(define-jsonrpc (aria-get-peers secret gid)
  #:method aria2.getPeers
  #:doc "This method returns a list peers of the download denoted by gid (string). This method is for BitTorrent only. The response is an array of structs and contains the following keys. Values are strings.
        - peerId
          Percent-encoded peer ID.
        - ip
          IP address of the peer.
        - port
          Port number of the peer.
        - bitfield
          Hexadecimal representation of the download progress of the peer. The highest bit corresponds to the piece at index 0. Set bits indicate the piece is available and unset bits indicate the piece is missing. Any spare bits at the end are set to zero.
        - amChoking
          true if aria2 is choking the peer. Otherwise false.
        - peerChoking
          true if the peer is choking aria2. Otherwise false.
        - downloadSpeed
          Download speed (byte/sec) that this client obtains from the peer.
        - uploadSpeed
          Upload speed(byte/sec) that this client uploads to the peer.
        - seeder
          true if this peer is a seeder. Otherwise false.")

(define-jsonrpc (aria-get-servers secret gid)
  #:method aria2.getServers
  #:doc "This method returns currently connected HTTP(S)/FTP/SFTP servers of the download denoted by gid (string). The response is an array of structs and contains the following keys. Values are strings.
        - index
          Index of the file, starting at 1, in the same order as files appear in the multi-file metalink.
        - servers
          A list of structs which contain the following keys.
          - uri
            Original URI.
          - currentUri
            This is the URI currently used for downloading. If redirection is involved, currentUri and uri may differ.
          - downloadSpeed
            Download speed (byte/sec)")

(define-jsonrpc (aria-tell-active secret (keys (void)))
  #:method aria2.tellActive
  #:doc "This method returns a list of active downloads. The response is an array of the same structs as returned by the aria2.tellStatus() method. For the keys parameter, please refer to the aria2.tellStatus() method.")

(define-jsonrpc (aria-tell-waiting secret offset num (keys (void)))
  #:method aria2.tellWaiting
  #:doc "This method returns a list of waiting downloads, including paused ones. offset is an integer and specifies the offset from the download waiting at the front. num is an integer and specifies the max. number of downloads to be returned. For the keys parameter, please refer to the aria2.tellStatus() method.")

(define-jsonrpc (aria-tell-stopped secret offset num (keys (void)))
  #:method aria2.tellStopped
  #:doc "This method returns a list of stopped downloads. offset is an integer and specifies the offset from the least recently stopped download. num is an integer and specifies the max. number of downloads to be returned. For the keys parameter, please refer to the aria2.tellStatus() method.")

(define-jsonrpc (aria-change-position secret gid pos how)
  #:method aria2.changePosition
  #:doc "This method changes the position of the download denoted by gid in the queue. pos is an integer. how is a string. If how is POS_SET, it moves the download to a position relative to the beginning of the queue. If how is POS_CUR, it moves the download to a position relative to the current position. If how is POS_END, it moves the download to a position relative to the end of the queue. If the destination position is less than 0 or beyond the end of the queue, it moves the download to the beginning or the end of the queue respectively. The response is an integer denoting the resulting position.")

(define-jsonrpc (aria-change-uri secret gid file-index del-uris add-uris (position (void)))
  #:method aria2.changeUri
  #:doc "This method removes the URIs in delUris from and appends the URIs in addUris to download denoted by gid. delUris and addUris are lists of strings. A download can contain multiple files and URIs are attached to each file. fileIndex is used to select which file to remove/attach given URIs. fileIndex is 1-based. position is used to specify where URIs are inserted in the existing waiting URI list. position is 0-based. When position is omitted, URIs are appended to the back of the list. This method first executes the removal and then the addition. position is the position after URIs are removed, not the position when this method is called. When removing an URI, if the same URIs exist in download, only one of them is removed for each URI in delUris. In other words, if there are three URIs http://example.org/aria2 and you want remove them all, you have to specify (at least) 3 http://example.org/aria2 in delUris. This method returns a list which contains two integers. The first integer is the number of URIs deleted. The second integer is the number of URIs added.")

(define-jsonrpc (aria-get-option secret gid)
  #:method aria2.getOption
  #:doc "This method returns options of the download denoted by gid. The response is a struct where keys are the names of options. The values are strings. Note that this method does not return options which have no default value and have not been set on the command-line, in configuration files or RPC methods.")

(define-jsonrpc (aria-change-option secret gid options)
  #:method aria2.changeOption
  #:doc "This method changes options of the download denoted by gid (string) dynamically. options is a struct. The options listed in Input File subsection are available, except for following options:
    - dry-run
    - metalink-base-uri
    - parameterized-uri
    - pause
    - piece-length
    - rpc-save-upload-metadata
Except for the following options, changing the other options of active download makes it restart (restart itself is managed by aria2, and no user intervention is required):
    - bt-max-peers
    - bt-request-peer-speed-limit
    - bt-remove-unselected-file
    - force-save
    - max-download-limit
    - max-upload-limit
This method returns OK for success.")

(define-jsonrpc (aria-get-global-option secret)
  #:method aria2.getGlobalOption
  #:doc "This method returns the global options. The response is a struct. Its keys are the names of options. Values are strings. Note that this method does not return options which have no default value and have not been set on the command-line, in configuration files or RPC methods. Because global options are used as a template for the options of newly added downloads, the response contains keys returned by the aria2.getOption() method.")

(define-jsonrpc (aria-change-global-option secret options)
  #:method aria2.changeGlobalOption
  #:doc "This method changes global options dynamically. options is a struct. The following options are available:
        - bt-max-open-files
        - download-result
        - keep-unfinished-download-result
        - log
        - log-level
        - max-concurrent-downloads
        - max-download-result
        - max-overall-download-limit
        - max-overall-upload-limit
        - optimize-concurrent-downloads
        - save-cookies
        - save-session
        - server-stat-of
    In addition, options listed in the Input File subsection are available, except for following options: checksum, index-out, out, pause and select-file.
    With the log option, you can dynamically start logging or change log file. To stop logging, specify an empty string(\"\") as the parameter value. Note that log file is always opened in append mode. This method returns OK for success.")

(define-jsonrpc (aria-get-global-stat secret)
  #:method aria2.getGlobalStat
  #:doc "This method returns global statistics such as the overall download and upload speeds. The response is a struct and contains the following keys. Values are strings.
        - downloadSpeed
          Overall download speed (byte/sec).
        - uploadSpeed
          Overall upload speed(byte/sec).
        - numActive
          The number of active downloads.
        - numWaiting
          The number of waiting downloads.
        - numStopped
          The number of stopped downloads in the current session. This value is capped by the --max-download-result option.
        - numStoppedTotal
          The number of stopped downloads in the current session and not capped by the --max-download-result option.")

(define-jsonrpc (aria-purge-download-result secret)
  #:method aria2.purgeDownloadResult
  #:doc "This method purges completed/error/removed downloads to free memory. This method returns OK.")

(define-jsonrpc (aria-remove-download-result secret gid)
  #:method aria2.removeDownloadResult
  #:doc "This method removes a completed/error/removed download denoted by gid from memory. This method returns OK for success.")

(define-jsonrpc (aria-get-version secret)
  #:method aria2.getVersion
  #:doc "This method returns the version of aria2 and the list of enabled features. The response is a struct and contains following keys.
        - version
          Version number of aria2 as a string.
        - enabledFeatures
          List of enabled features. Each feature is given as a string.")

(define-jsonrpc (aria-get-session-info secret)
  #:method aria2.getSessionInfo
  #:doc "This method returns session information. The response is a struct and contains following key.
        - sessionId
          Session ID, which is generated each time when aria2 is invoked.")

(define-jsonrpc (aria-shutdown secret)
  #:method aria2.shutdown
  #:doc "This method shuts down aria2. This method returns OK.")

(define-jsonrpc (aria-force-shutdown secret)
  #:method aria2.forceShutdown
  #:doc "This method shuts down aria2(). This method behaves like :func:'aria2.shutdown` without performing any actions which take time, such as contacting BitTorrent trackers to unregister downloads first. This method returns OK.")

(define-jsonrpc (aria-save-session secret)
  #:method aria2.saveSession
  #:doc "This method saves the current session to a file specified by the --save-session option. This method returns OK if it succeeds.")

;;

(define-jsonrpc (aria-on-download-start params)
  #:method aria2.onDownloadStart
  (displayln (list 'on-download-start params)))

(define-jsonrpc (aria-on-download-complete params)
  #:method aria2.onDownloadComplete
  (displayln (list 'on-download-complete params)))

(define-jsonrpc (aria-on-download-error params)
  #:method aria2.onDownloadError
  (displayln (list 'on-download-error params)))

(define-jsonrpc (aria-on-bt-download-complete params)
  #:method aria2.onBtDownloadComplete
  (displayln (list 'on-bt-download-complete params)))

;;

(current-websocket-idle-timeout (* 60 60 24))
(define client (jsonrpc-websocket-client "ws://127.0.0.1:6800/jsonrpc"))
(with-jsonrpc client
  (aria-add-uri (make-aria-secret "secret")
                (list "magnet:?xt=urn:btih:dd8255ecdc7ca55fb0bbf81323d87062db1f6d1c&dn=Big+Buck+Bunny&tr=udp%3A%2F%2Fexplodie.org%3A6969")))

;; (with-jsonrpc client
;;   (aria-tell-status (make-aria-secret "secret")
;;                     "f87dbb53b42ac06d"))

;;(close-jsonrpc-client client)

(with-jsonrpc client (aria-get-session-info (make-aria-secret "secret")))
(with-jsonrpc client (aria-get-global-stat (make-aria-secret "secret")))
(with-jsonrpc client (aria-get-files (make-aria-secret "secret") "0fde0ef81d620ec4"))
(with-jsonrpc client (aria-get-peers (make-aria-secret "secret") "0fde0ef81d620ec4"))
(with-jsonrpc client (aria-get-servers (make-aria-secret "secret") "0fde0ef81d620ec4"))
(with-jsonrpc client (aria-tell-active (make-aria-secret "secret")))
