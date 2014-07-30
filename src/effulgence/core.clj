(ns effulgence.core
	(:require [net.cgrand.enlive-html :as html]))

(def ^:dynamic *toc-url* "http://edgeofyourseat.dreamwidth.org/2121.html")

(defn in?
	"True if seq contains elm"
	[seq elm]
	(some #(= elm %) seq)
)

(defn fetch-url [url]
	(html/html-resource (java.net.URL. url))
)

(defn getUserURL [url1] 
	"
	Given a URL, returns the dreamwidth user associated with that URL.
	Example:
		(getUserURL 'http://edgeofyourseat.dreamwidth.com/2121.html')
		edgeofyourseat
	"
	(first 
		(clojure.string/split 
			(.getHost 
				(java.net.URL. url1)
			) 
			#"\."
		)
	)
)

(defn getNumberURL [url1] 
	"
	Given a URL, returns the page number in the first part of the path.
	Example:
		(getNumberURL 'http://edgeofyourseat.dreamwidth.com/2121.html')
		2121
	"
	(second 
		(clojure.string/split 
			(.getPath 
				(java.net.URI. url1)
			) 
			#"/|\."
		)
	)
)

(defn getWholeQueryURL [url1]
	"
	Given a URL, returns the queries as a hash-map.
	Example:
		(getWholeQueryURL 
			'http://belltower.dreamwidth.org/1898.html?thread=47722&style=site#cmt47722'
		)
		{:thread, 47722, :style, 'site'}
	"
	(clojure.walk/keywordize-keys 
		(apply 
			hash-map 
                        (if-let [queryResult (.getQuery (java.net.URI. url1))]
                          (clojure.string/split queryResult #"(&|=)"))
		)
	)
)

(defn getThreadURL [url1] 
	"
	Given a URL, returns either the thread query-value or else nil.
	Example:
    (getWholeQueryURL
	    'http://belltower.dreamwidth.org/1898.html?thread=47722&style=site#cmt47722' 
    )
    47722
	"
	(:thread 
		(getWholeQueryURL url1)
	)
)

(defn newPageName [url1]

	(str 
		(getUserURL url1) 
		"-" 
		(getNumberURL url1)
                (if-let [thread-url (getThreadURL url1)]
                  (str "-" thread-url))
		".html"
	)
)

(def chapterDir "chapters")

(def userPicDir "userpics")

(defn newPagePath [url1] 

	(str 
		chapterDir 
		"/" 
		(newPageName url1)
	)
)

(def TOCWithUpdatedTags 
	(html/at 
		(fetch-url *toc-url*) 
		[:div.entry-content :a] 
		(fn [mytag] 
			(assoc 
				mytag 
				:attrs 
				(assoc 
					(:attrs mytag) 
					:oldhref 
					(:href (:attrs mytag)) 
					:href 
					(newPagePath (:href (:attrs mytag)))
				)
			)
		)
	)
)

(def TOCURLList
	(rest (map (fn [map1] (:href (:attrs map1))) (html/select (fetch-url *toc-url*) [:div.entry-content :a])))
)

(defn flattenTheURL [url1]
	(str 
		"http://"
		(getUserURL url1)
		".dreamwidth.org/"
		(getNumberURL url1)
		".html?style=site&view=flat"
	)
)

(defn getNotFlatURL [url1]
	(str
		"http://"
		(getUserURL url1)
		".dreamwidth.org/"
		(getNumberURL url1)
		".html?style=site"
                (if-let [thread-url (getThreadURL url1)]
                  (str "&thread=" thread-url))
	)
)

(defn copyURIToFile [uri1 file1]
	(with-open
		[in (clojure.java.io/input-stream uri1)
		out (clojure.java.io/output-stream file1)]
		(clojure.java.io/copy in out)
	)
)

(defn determinePageCount [url1]
  (if-let [mystr
                (first
			(:content 
				(first 
					(html/select 
						(fetch-url 
							(flattenTheURL url1)
						) 
						[:div.comment-pages-wrapper :div :p]
					)
				)
			)
		)
          ]
    (Integer. (nth (clojure.string/split mystr #" ")
                   3))
    1))

(defn getCommentIDFromMap [map1]
	(:id
		(:attrs
			map1
		)
	)
)

(defn getCommentTextFromMap [map1]
	(:content 
		(first 
			(html/select 
				(html/at 
					map1 
					[:span.datetime] 
					nil 
					[:div.edittime] 
					nil
				) 
				[:div.comment-content]
			)
		)
	)
)

(defn getUserPicFromMap [map1]
	(html/select map1 [:div.userpic :img])
)

(defn getUserPicSrcFromMap [map1]
	(:src (:attrs (first (getUserPicFromMap map1))))
)

(defn getUserPicFileNameFromMap [map1] 
	"
	Given a comment map, returns the appropriate file name for the user pic.
	"
        (clojure.string/join "-"
		(rest 
			(clojure.string/split 
				(.getPath 
					(java.net.URI.
						(getUserPicSrcFromMap map1)
					)
				) 
				#"/|\."
			)
		)
	)
)

(defn getUserPicFilePathFromMap [map1]
	"
	Given a comment map, returns a relative file path to the user pic.
	"
	(str userPicDir "/" (getUserPicFileNameFromMap map1))
)

(defn getTransformedUserPic [map1]
	(html/select (html/at map1 [:div.userpic :img] (html/set-attr :src (getUserPicFilePathFromMap map1))) [:div.userpic :img])
)

(defn doesUserPicExistFromMap [map1]
	"
	Given a comment map, return whether or not the associated image has been downloaded as a resource.
	"
	(clojure.java.io/.exists (clojure.java.io/as-file (str chapterDir "/" (getUserPicFilePathFromMap map1))))
)

(defn hasUserPicInMap [map1]
	(not (nil? (getUserPicSrcFromMap map1)))
)

(defn downloadUserPicUsingMap [map1]
	(if
		(hasUserPicInMap map1)
		(if 
			(doesUserPicExistFromMap map1)
			nil
			(copyURIToFile 
				(getUserPicSrcFromMap map1)
				(str chapterDir "/" (getUserPicFilePathFromMap map1))
			)
		)
		nil
	)
)

(defn getAuthorNameFromMap [map1]
	(:lj:user 
		(:attrs 
			(first 
				(html/select 
					map1 
					[:span.ljuser]
				)
			)
		)
	)
)

(defn emitNewHTMLForComment [map1]
	(str 
		"<tr><td>" 
		(if
			(hasUserPicInMap map1)
			(apply 
				str 
				(html/emit* 
					(getTransformedUserPic map1)
				)
			)
			""
		)
		"<br />Author: " 
		(getAuthorNameFromMap map1) 
		"</td><td>" 
		(apply 
			str 
			(html/emit* 
				(getCommentTextFromMap map1)
			)
		)
		"</td></tr>"
	)
)

(defn getHeaderImageFromChapter [url1]
	"
		Gets the main image from chapter. Threading is irrelevant.
	"
	(getTransformedUserPic
		(html/select
			(fetch-url
				(getNotFlatURL url1)
			)
			[:div.entry]
		)
	)
)

(defn downloadHeaderImageFromChapter [url1]
	(downloadUserPicUsingMap 
		(html/select 
			(fetch-url 
				(getNotFlatURL url1)
			) 
			[:div.entry]
		)
	)
)

(defn getPrimaryAuthorNameChapter [url1]
	"
		Gets the author's name for the chapter. Threading is irrelevant.
	"
	(:lj:user 
		(:attrs 
			(first 
				(html/select 
					(fetch-url 
						(getNotFlatURL url1)
					) 
					[:div.poster-info :span.ljuser]
				)
			)
		)
	)
)

(defn getMainContentFromChapter [url1]
	"
		Gets the main submission from chapter. Threading is irrelevant.
	"
	(html/select
		(fetch-url
			(getNotFlatURL url1)
		)
		[:div.entry-content]
	)
)

(defn emitHeaderHTML [url1]
	(str 
		"<tr><td>" 
		(apply 
			str 
			(html/emit* 
				(getHeaderImageFromChapter url1)
			)
		) 
		"<br />Author: " 
		(getPrimaryAuthorNameChapter url1) 
		"</td><td>" 
		(apply 
			str 
			(html/emit* 
				(getMainContentFromChapter url1)
			)
		)
		"</td></tr>"
	)
)	

(defn getAllCommentIDsFromChapter [url1]
	"
		Gets all the comment ids from a chapter. Pays attention to threading.
	"
	(map
		getCommentIDFromMap
		(html/select 
			(fetch-url 
				(getNotFlatURL url1)
			)	
			[:div.comment]
		)
	)
)

(defn extractCommentsFromSinglePage [url1 pageNum1]
	(html/select
		(fetch-url 
			(str 
				(flattenTheURL url1) 
				"&page=" 
				pageNum1
			)
		)
		[:div.comment]
	)
)

(defn extractAllCommentsFromChapter [url1]
	"
		Gets all of the comments from the chapter in a hashmap.
		Ignores threading information!
	"
	(apply 
		concat
		(map 
			(fn [num1] 
				(extractCommentsFromSinglePage url1 num1)
			) 
			(map 
				inc 
				(range 
					0 
					(determinePageCount url1)
				)
			)
		)
	)
)

(defn getCommentsFromPotentiallyThreadedChapter [url1]
  (let [listOfCommentIDs (getAllCommentIDsFromChapter url1)
        mapOfComments (extractAllCommentsFromChapter url1)]
			(filter
				(fn [map1]
					(in? listOfCommentIDs (:id (:attrs map1)))
				)
				mapOfComments
			)
  ))

(defn emitNewHTMLForChapter [url1]
	(str
		"<table border=\"1\">"
		(emitHeaderHTML url1)
		(apply
			str
			(map
				emitNewHTMLForComment
				(getCommentsFromPotentiallyThreadedChapter url1)
			)
		)
		"</table>"
	)
)

(defn processChapterURL [url1]
	(do
		(downloadHeaderImageFromChapter url1)
		(dorun (map downloadUserPicUsingMap (getCommentsFromPotentiallyThreadedChapter url1)))
		(spit (newPagePath url1) (emitNewHTMLForChapter url1)) 
	)
)
