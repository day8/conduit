(ns conduit.events
  (:require
   [conduit.db :refer [default-db set-user-ls remove-user-ls]]
   [re-frame.core :refer [reg-event-db reg-event-fx reg-fx inject-cofx trim-v after path]]
   [conduit.router :as router]
   [day8.re-frame.http-fx] ;; even if we don't use this require its existence will cause the :http-xhrio effect handler to self-register with re-frame
   [ajax.core :refer [json-request-format json-response-format]]
   [clojure.string :as str]))

(defn comp-dbfx
  [fns]
  (fn [coeffects event-v]
    (reduce
      (fn [db-fx fn]
        (fn db-fx coeffects event-v))
      {:fx []
       :db (:db coeffects)}
      fns)))

;; -- Interceptors --------------------------------------------------------------
;; Every event handler can be "wrapped" in a chain of interceptors. Each of these
;; interceptors can do things "before" and/or "after" the event handler is executed.
;; They are like the "middleware" of web servers, wrapping around the "handler".
;; Interceptors are a useful way of factoring out commonality (across event
;; handlers) and looking after cross-cutting concerns like logging or validation.
;;
;; They are also used to "inject" values into the `coeffects` parameter of
;; an event handler, when that handler needs access to certain resources.
;;
;; Each event handler can have its own chain of interceptors. Below we create
;; the interceptor chain shared by all event handlers which manipulate user.
;; A chain of interceptors is a vector.
;; Explanation of `trim-v` is given further below.
;;
(def set-user-interceptor [(path :user)                     ;; `:user` path within `db`, rather than the full `db`.
                           (after set-user-ls)              ;; write user to localstore (after)
                           trim-v])                         ;; removes first (event id) element from the event vec

;; After logging out clean up local-storage so that when a users refreshes
;; the browser she/he is not automatically logged-in, and because it's a
;; good practice to clean-up after yourself.
;;
(def remove-user-interceptor [(after remove-user-ls)])

;; -- Helpers -----------------------------------------------------------------
;;
(def api-url "https://conduit.productionready.io/api")

(defn endpoint
  "Concat any params to api-url separated by /"
  [& params]
  (str/join "/" (concat [api-url] params)))

(defn auth-header
  "Get user token and format for API authorization"
  [db]
  (when-let [token (get-in db [:user :token])]
    [:Authorization (str "Token " token)]))

(defn add-epoch
  "Takes date identifier and adds :epoch (cljs-time.coerce/to-long) timestamp to coll"
  [date coll]
  (map (fn [item] (assoc item :epoch (.getTime (js/Date.) date))) coll))

(defn index-by
  "Transform a coll to a map with a given key as a lookup value"
  [key coll]
  (into {} (map (juxt key identity) (add-epoch :createdAt coll))))

(reg-fx
 :set-url
 (fn [{:keys [url]}]
   (router/set-token! url)))

;; -- Event Handlers ----------------------------------------------------------
;;
(reg-event-fx                                               ;; usage: (dispatch [:initialise-db])
 :initialise-db                                            ;; sets up initial application state

  ;; the interceptor chain (a vector of interceptors)
 [(inject-cofx :local-store-user)]                         ;; gets user from localstore, and puts into coeffects arg

  ;; the event handler (function) being registered
 (fn [{:keys [local-store-user]} _]                        ;; take 2 vals from coeffects. Ignore event vector itself.
   {:db (assoc default-db :user local-store-user)}))     ;; what it returns becomes the new application state

(reg-event-fx                                               ;; usage: (dispatch [:set-active-page {:page :home})
 :set-active-page                                          ;; triggered when the user clicks on a link that redirects to a another page
 (fn [{:keys [db]} [_ {:keys [page slug profile favorited]}]] ;; destructure 2nd parameter to obtain keys
   (let [set-page (assoc db :active-page page)]
     (case page
                 ;; -- URL @ "/" --------------------------------------------------------
       :home {:db         set-page
              :dispatch-n [(if (empty? (:user db)) ;; dispatch more than one event. When a user
                             [:get-articles {:limit 10}] ;; is NOT logged in we display all articles
                             [:get-feed-articles {:limit 10}]) ;; otherwiser we get her/his feed articles
                           [:get-tags]]}          ;; we also can't forget to get tags

                 ;; -- URL @ "/login" | "/register" | "/settings" -----------------------
       (:login :register :settings) {:db set-page} ;; when using case with multiple test constants that
                 ;; do the same thing we can group them together
                 ;; (:login :register :settings) {:db set-page} is the same as:
                 ;; :login {:db set-page}
                 ;; :register {:db set-page}
                 ;; :settings {:db set-page}
                 ;; -- URL @ "/editor" --------------------------------------------------
       :editor {:db       set-page
                :dispatch (if slug                ;; When we click article to edit we need
                            [:set-active-article slug] ;; to set it active or if we want to write
                            [:reset-active-article])} ;; a new article we reset

                 ;; -- URL @ "/article/:slug" -------------------------------------------
       :article {:db         (assoc set-page :active-article slug)
                 :dispatch-n [[:get-articles {:limit 10}]
                              [:get-article-comments {:slug slug}]
                              [:get-user-profile {:profile (get-in db [:articles slug :author :username])}]]}

                 ;; -- URL @ "/profile/:slug" -------------------------------------------
       :profile {:db         (assoc set-page :active-article slug)
                 :dispatch-n [[:get-user-profile {:profile profile}] ;; again for dispatching multiple
                              [:get-articles {:author profile}]]} ;; events we can use :dispatch-n
                 ;; -- URL @ "/profile/:slug/favorites" ---------------------------------
       :favorited {:db       (assoc db :active-page :profile) ;; even though we are at :favorited we still
                   :dispatch [:get-articles {:favorited favorited}]})))) ;; display :profile with :favorited articles

(reg-event-db                                               ;; usage: (dispatch [:reset-active-article])
 :reset-active-article                                     ;; triggered when the user enters new-article i.e. editor without slug
 (fn [db _]                                                ;; 1st paramter in -db events is db, 2nd paramter not important therefore _
   (dissoc db :active-article)))                         ;; compute and return the new state

(reg-event-fx                                               ;; usage: (dispatch [:set-active-article slug])
 :set-active-article
 (fn [{:keys [db]} [_ slug]]                               ;; 1st parameter in -fx events is no longer just db. It is a map which contains a :db key.
   {:db         (assoc db :active-article slug)          ;; The handler is returning a map which describes two side-effects:
    :dispatch-n [[:get-article-comments {:slug slug}]    ;; changne to app-state :db and future event in this case :dispatch-n
                 [:get-user-profile {:profile (get-in db [:articles slug :author :username])}]]}))

(defn http-xhrio
  [{:keys [method uri-parts on-success on-failure params]}]
  (fn [{:keys [db] :as db-fx} coeffects event]
    (let [uri-parts (if (fn? uri-parts)
                      (uri-parts db-fx coeffects event)
                      uri-parts)
          params    (if (fn? params) ;; TODO verify nil params is safe with :http-xhrio
                      (params db-fx coeffects event)
                      params)
          method    (if (fn? method)
                      (method db-fx coeffects event)
                      method)]
      (update db-fx :fx conj
              {:http-xhrio
               {:method          method
                :uri             (apply endpoint uri-parts)
                :headers         (auth-header db)             ;; get and pass user token obtained during login
                :params          params                       ;; include params in the request
                :format          (json-request-format)
                :response-format (json-response-format {:keywords? true}) ;; json response and all keys to keywords
                :on-success      on-success
                :on-failure      on-failure}}))))

(defn store-articles
  [db-fx coeffects [_ {articles :articles, articles-count :articlesCount}]]
  (-> db-fx
      (assoc-in [:db :articles-count] articles-count)         ;; change app-state by adding articles-count
      (assoc-in [:db :articles] (index-by :slug articles))))  ;; and articles, which we index-by slug

(defn start-loading-indicator
  [kw]
  (fn [db-fx _ _]
    (assoc-in db-fx [:db :loading kw] true)))

(defn stop-loading-indicator
  [kw]
  (fn [db-fx _ _]
    (assoc-in db-fx [:db :loading kw] false)))                ;; turn off loading flag for this event

(reg-event-fx
  :get-articles-success
  (comp-dbfx [(stop-loading-indicator :articles) store-articles]))

;; -- GET Articles @ /api/articles --------------------------------------------
;;
(defn update-filter
  [db-fx coeffects [_ params]] ;; params = {:limit 10 :tag "tag-name" ...}
  (-> db-fx
      (assoc-in [:db :filter :offset] (:offset params)) ;; base on paassed params set a filter
      (assoc-in [:db :filter :tag] (:tag params)) ;; so that we can easily show and hide
      (assoc-in [:db :filter :author] (:author params)) ;; appropriate ui components
      (assoc-in [:db :filter :favorites] (:favorited params))
      (assoc-in [:db :filter :feed] false))) ;; we need to disable filter by feed every time since it's not supported query param

(reg-event-fx ;; usage (dispatch [:get-articles {:limit 10 :tag "tag-name" ...}])
  :get-articles ;; triggered every time user request articles with differetn params
  (comp-dbfx [(start-loading-indicator :articles)
              update-filter
              (http-xhrio {:method     :get
                           :uri-parts  ["articles"]                             ;; evaluates to "api/articles/"
                           :params     (fn [_ _ [_ params]]
                                         params)
                           :on-success [:get-articles-success] ;; trigger get-articles-success event
                           :on-failure [:api-request-error :get-articles]})])) ;; trigger api-request-error with :get-articles

;; -- GET Article @ /api/articles/:slug ---------------------------------------
;;
(reg-event-fx        ;; usage (dispatch [:get-article {:slug "slug"}])
  :get-article        ;; triggered when a user upserts article i.e. is redirected to article page after saving an article
  (comp-dbfx [(start-loading-indicator :article)
              (http-xhrio {:method     :get
                           :uri-parts  (fn [_ _ [_ params]]    ;; params = {:slug "slug"} evaluates to "api/articles/:slug"
                                         ["articles" (:slug params)])
                           :on-success [:get-articles-success] ;; trigger get-article-success event
                           :on-failure [:api-request-error :get-article]})])) ;; trigger api-request-error with :get-articles

(defn store-article
  [db-fx _ [_ {article :article}]]
  (assoc-in db-fx [:db :articles] (index-by :slug [article])))

(reg-event-db
 :get-article-success
 (comp-dbfx [(stop-loading-indicator :article)
             store-article]))

;; -- POST/PUT Article @ /api/articles(/:slug) --------------------------------
;;
(reg-event-fx                                               ;; usage (dispatch [:upsert-article article])
 :upsert-article                                           ;; when we update or insert (upsert) we are sending the same shape of information
 (comp-dbfx [(start-loading-indicator :article)            ;; params = {:slug "article-slug" :article {:body "article body"}}
             (http-xhrio {:method     (fn [_ _ [_ params]]
                                        (if (:slug params) :put :post)) ;; when we get a slug we'll update (:put) otherwise insert (:post)
                          :uri        (fn [_ _ [_ params]]
                                        (if (:slug params) ;; Same logic as above but we go with different
                                          ["articles" (:slug params)] ;; endpoint - one with :slug to update
                                          ["articles"])) ;; and another to insert
                          :params     (fn [_ _ [_ params]]
                                        {:article (:article params)})
                          :on-success [:upsert-article-success] ;; trigger upsert-article-success event
                          :on-failure [:api-request-error :upsert-article]})])) ;; trigger api-request-error with :upsert-article

(defn dispatch-get-article
  [get-slug-fn]
  (fn [db-fx coeffects event]
    (update db-fx :fx conj
            [:dispatch [:get-article {:slug (get-slug-fn db-fx coeffects event)}]])))

(defn dispatch-get-article-comments
  [get-slug-fn]
  (fn [db-fx coeffects event]
    (update db-fx :fx conj
            [:dispatch [:get-article-comments {:slug (get-slug-fn db-fx coeffects event)}]])))

(reg-event-fx
 :upsert-article-success
 (comp-dbfx [(stop-loading-indicator :article)
             (fn [db-fx _ [_ {article :article}]]
               (-> db-fx
                   (dissoc-in [:db :comments])                   ;; clean up any comments that we might have in db
                   (dissoc-in [:db :errors])                     ;; clean up any erros that we might have in db
                   (assoc-in [:db :active-page ] :article)
                   (assoc-in [:db :active-article] (:slug article))
                   (update :fx conj [:set-url {:url (str "/article/" (:slug article))}])))
             (dispatch-get-article ;; when the users clicks save we fetch the new version
               (fn [_ _ [_ {article :article}]]
                 (:slug article)))
             (dispatch-get-article-comments ;; of the article and comments from the server
               (fn [_ _ [_ {article :article}]]
                 (:slug article)))]))

;; -- DELETE Article @ /api/articles/:slug ------------------------------------
;;
(reg-event-fx                                               ;; usage (dispatch [:delete-article slug])
 :delete-article                                           ;; triggered when a user deletes an article
 (comp-dbfx [(start-loading-indicator :article)
             (http-xhrio {:method          :delete
                          :uri             (fn [_ _ [_ slug]]
                                             ["articles" slug]) ;; slug = {:slug "article-slug"} evaluates to "api/articles/:slug"
                          :params          (fn [_ _ [_ slug]]
                                             slug)                   ;; pass the article slug to delete
                          :on-success      [:delete-article-success] ;; trigger get-articles-success
                          :on-failure      [:api-request-error :delete-article]})])) ;; trigger api-request-error with :delete-article

(reg-event-fx
 :delete-article-success
 (fn [{:keys [db]} _]
   {:db       (-> db
                  (update-in [:articles] dissoc (:active-article db))
                  (assoc-in [:loading :article] false))
    :dispatch [:set-active-page {:page :home}]}))

;; -- GET Feed Articles @ /api/articles/feed ----------------------------------
;;

(defn reset-filter
  [db-fx _ [_ params]]
  (-> db-fx
      (assoc-in [:db :filter :offset] (:offset params))
      (assoc-in [:db :filter :tag] nil)        ;; with feed-articles we turn off almost all
      (assoc-in [:db :filter :author] nil)     ;; filters to make sure everythinig on the
      (assoc-in [:db :filter :favorites] nil)  ;; client is displayed correctly.
      (assoc-in [:db :filter :feed] true)))  ;; This is the only one we need

(reg-event-fx                                               ;; usage (dispatch [:get-feed-articles {:limit 10 :offset 0 ...}])
 :get-feed-articles                                        ;; triggered when Your Feed tab is loaded
 (comp-dbfx [(start-loading-indicator :articles)           ;; params = {:offset 0 :limit 10}
             reset-filter
             (http-xhrio {:method          :get
                          :uri             ["articles" "feed"] ;; evaluates to "api/articles/feed"
                          :params          (fn [_ _ [_ params]]
                                             params)                 ;; include params in the request
                          :on-success      [:get-feed-articles-success] ;; trigger get-articles-success event
                          :on-failure      [:api-request-error :get-feed-articles]})])) ;; trigger api-request-error with :get-feed-articles

(reg-event-db
 :get-feed-articles-success
 (fn [db [_ {articles :articles, articles-count :articlesCount}]]
   (-> db
       (assoc-in [:loading :articles] false)
       (assoc :articles-count articles-count
              :articles (index-by :slug articles)))))

;; -- GET Tags @ /api/tags ----------------------------------------------------
;;
(reg-event-fx                                               ;; usage (dispatch [:get-tags])
 :get-tags                                                 ;; triggered when the home page is loaded
 (comp-dbfx [(start-loading-indicator :tags) ;; second parameter is not important, therefore _
             (http-xhrio {:method          :get
                          :uri             ["tags"]      ;; evaluates to "api/tags"
                          :on-success      [:get-tags-success]    ;; trigger get-tags-success event
                          :on-failure      [:api-request-error :get-tags]})])) ;; trigger api-request-error with :get-tags

(reg-event-db
 :get-tags-success
 (fn [db [_ {tags :tags}]]
   (-> db
       (assoc-in [:loading :tags] false)
       (assoc :tags tags))))

;; -- GET Comments @ /api/articles/:slug/comments -----------------------------
;;
(reg-event-fx                                               ;; usage (dispatch [:get-article-comments {:slug "article-slug"}])
 :get-article-comments                                     ;; triggered when the article page is loaded
 (comp-dbfx [(start-loading-indicator :comments) ;; params = {:slug "article-slug"}
             (http-xhrio {:method          :get
                          :uri             (fn [_ _ [_ params]]
                                             ["articles" (:slug params) "comments"]) ;; evaluates to "api/articles/:slug/comments"
                          :on-success      [:get-article-comments-success] ;; trigger get-article-comments-success
                          :on-failure      [:api-request-error :get-article-comments]})])) ;; trigger api-request-error with :get-article-comments

(reg-event-db
 :get-article-comments-success
 (fn [db [_ {comments :comments}]]
   (-> db
       (assoc-in [:loading :comments] false)
       (assoc :comments (index-by :id comments)))))      ;; another index-by, this time by id

;; -- POST Comments @ /api/articles/:slug/comments ----------------------------
;;
(reg-event-fx                                               ;; usage (dispatch [:post-comment comment])
 :post-comment                                             ;; triggered when a user submits a comment
 (comp-dbfx [(start-loading-indicator :comments) ;; body = {:body "body" }
             (http-xhrio {:method          :post
                          :uri-parts       (fn [{:keys [db]} _ _]
                                             ["articles" (:active-article db) "comments"]) ;; evaluates to "api/articles/:slug/comments"
                          :params          (fn [_ _ [_ body]]
                                             {:comment body})
                          :on-success      [:post-comment-success] ;; trigger get-articles-success
                          :on-failure      [:api-request-error :comments]})])) ;; trigger api-request-error with :comments

(reg-event-fx
 :post-comment-success
 (fn [{:keys [db]} [_ comment]]
   {:db       (-> db
                  (assoc-in [:loading :comments] false)
                  (assoc-in [:articles (:active-article db) :comments] comment)
                  (update-in [:errors] dissoc :comments)) ;; clean up errors, if any
    :dispatch [:get-article-comments {:slug (:active-article db)}]}))

;; -- DELETE Comments @ /api/articles/:slug/comments/:comment-id ----------------------
;;
(defn set-active-comment
  [db-fx _ [_ comment-id]] ;; comment-id = 1234
  (assoc-in db-fx [:db :active-comment] comment-id))

(reg-event-fx                                               ;; usage (dispatch [:delete-comment comment-id])
 :delete-comment                                           ;; triggered when a user deletes an article
 (comp-dbfx [(start-loading-indicator :comments)
             set-active-comment
             (http-xhrio {:method          :delete
                          :uri-parts       (fn [{:keys [db]} _ [_ comment-id]]
                                             ["articles" (:active-article db) "comments" comment-id]) ;; evaluates to "api/articles/:slug/comments/:comment-id"
                          :on-success      [:delete-comment-success] ;; trigger delete-comment-success
                          :on-failure      [:api-request-error :delete-comment]})])) ;; trigger api-request-error with :delete-comment

(reg-event-db
 :delete-comment-success
 (fn [db _]
   (-> db
       (update-in [:comments] dissoc (:active-comment db)) ;; we could do another fetch of comments
       (dissoc :active-comment)                          ;; but instead we just remove it from app-db
       (assoc-in [:loading :comment] false))))           ;; which gives us much snappier ui

;; -- GET Profile @ /api/profiles/:username -----------------------------------
;;
(reg-event-fx                                               ;; usage (dispatch [:get-user-profile {:profile "profile"}])
 :get-user-profile                                         ;; triggered when the profile page is loaded
 (comp-dbfx [(start-loading-indicator :profile) ;; params = {:profile "profile"}
             (http-xhrio {:method          :get
                          :uri             (fn [_ _ [_ params]]
                                             ["profiles" (:profile params)]) ;; evaluates to "api/profiles/:profile"
                          :on-success      [:get-user-profile-success] ;; trigger get-user-profile-success
                          :on-failure      [:api-request-error :get-user-profile]})])) ;; trigger api-request-error with :get-user-profile

(reg-event-db
 :get-user-profile-success
 (fn [db [_ {profile :profile}]]
   (-> db
       (assoc-in [:loading :profile] false)
       (assoc :profile profile))))

;; -- POST Login @ /api/users/login -------------------------------------------
;;
(reg-event-fx                                               ;; usage (dispatch [:login user])
 :login                                                    ;; triggered when a users submits login form
 (comp-dbfx [(start-loading-indicator :login) ;; credentials = {:email ... :password ...}
             (http-xhrio {:method          :post
                          :uri             ["users" "login"] ;; evaluates to "api/users/login"
                          :params          (fn [_ _ [_ credentials]]
                                             {:user credentials})    ;; {:user {:email ... :password ...}}
                          :on-success      [:login-success]       ;; trigger login-success
                          :on-failure      [:api-request-error :login]})])) ;; trigger api-request-error with :login

(reg-event-fx
 :login-success
  ;; The standard set of interceptors, defined above, which we
  ;; use for all user-modifying event handlers. Looks after
  ;; writing user to localStorage.
  ;; NOTE: this chain includes `path` and `trim-v`
 set-user-interceptor

  ;; The event handler function.
  ;; The "path" interceptor in `set-user-interceptor` means 1st parameter is the
  ;; value at `:user` path within `db`, rather than the full `db`.
  ;; And, further, it means the event handler returns just the value to be
  ;; put into `:user` path, and not the entire `db`.
  ;; So, a path interceptor makes the event handler act more like clojure's `update-in`
 (fn [{user :db} [{props :user}]]
   {:db         (merge user props)
    :dispatch-n [[:get-feed-articles {:tag nil :author nil :offset 0 :limit 10}]
                 [:set-active-page {:page :home}]]}))

;; -- POST Registration @ /api/users ------------------------------------------
;;
(reg-event-fx                                               ;; usage (dispatch [:register-user registration])
 :register-user                                            ;; triggered when a users submits registration form
 (comp-dbfx [(start-loading-indicator :register-user) ;; registration = {:username ... :email ... :password ...}
             (http-xhrio {:method          :post
                          :uri             (endpoint "users")     ;; evaluates to "api/users"
                          :params          (fn [_ _ [_ registration]]
                                             {:user registration})   ;; {:user {:username ... :email ... :password ...}}
                          :on-success      [:register-user-success] ;; trigger login-success
                          :on-failure      [:api-request-error :register-user]})])) ;; trigger api-request-error with :login-success

(reg-event-fx
 :register-user-success
  ;; The standard set of interceptors, defined above, which we
  ;; use for all user-modifying event handlers. Looks after
  ;; writing user to LocalStore.
  ;; NOTE: this chain includes `path` and `trim-v`
 set-user-interceptor

  ;; The event handler function.
  ;; The "path" interceptor in `set-user-interceptor` means 1st parameter is the
  ;; value at `:user` path within `db`, rather than the full `db`.
  ;; And, further, it means the event handler returns just the value to be
  ;; put into `:user` path, and not the entire `db`.
  ;; So, a path interceptor makes the event handler act more like clojure's `update-in`
 (fn [{user :db} [{props :user}]]
   {:db       (merge user props)
    :dispatch [:set-active-page {:page :home}]}))

;; -- PUT Update User @ /api/user ---------------------------------------------
;;
(reg-event-fx                                               ;; usage (dispatch [:update-user user])
 :update-user                                              ;; triggered when a users updates settings
 (comp-dbfx [(start-loading-indicator :update-user) ;; user = {:img ... :username ... :bio ... :email ... :password ...}
             (http-xhrio {:method          :put
                          :uri             ["user"]      ;; evaluates to "api/user"
                          :params          (fn [_ _ [_ user]]
                                             {:user user})           ;; {:user {:img ... :username ... :bio ... :email ... :password ...}}
                          :on-success      [:update-user-success] ;; trigger update-user-success
                          :on-failure      [:api-request-error :update-user]})])) ;; trigger api-request-error with :update-user

(reg-event-fx
 :update-user-success
  ;; The standard set of interceptors, defined above, which we
  ;; use for all user-modifying event handlers. Looks after
  ;; writing user to LocalStore.
  ;; NOTE: this chain includes `path` and `trim-v`
 set-user-interceptor

  ;; The event handler function.
  ;; The "path" interceptor in `set-user-interceptor` means 1st parameter is the
  ;; value at `:user` path within `db`, rather than the full `db`.
  ;; And, further, it means the event handler returns just the value to be
  ;; put into `:user` path, and not the entire `db`.
  ;; So, a path interceptor makes the event handler act more like clojure's `update-in`
 (fn [{user :db} [{props :user}]]
   {:db (merge user props)}))

;; -- Toggle follow user @ /api/profiles/:username/follow ---------------------
;;
(reg-event-fx                                               ;; usage (dispatch [:toggle-follow-user username])
 :toggle-follow-user                                       ;; triggered when user clicks follow/unfollow button on profile page
 (comp-dbfx [(start-loading-indicator :toggle-follow-user) ;; username = :username
             (http-xhrio {:method          (fn [{:keys [db]} _ _]
                                             (if (get-in db [:profile :following]) :delete :post)) ;; check if we follow if yes DELETE, no POST
                          :uri             (fn [_ _ [_ username]]
                                             ["profiles" username "follow"]) ;; evaluates to "api/profiles/:username/follow"
                          :on-success      [:toggle-follow-user-success] ;; trigger toggle-follow-user-success
                          :on-failure      [:api-request-error :login]})])) ;; trigger api-request-error with :update-user-success

(reg-event-db                                               ;; usage: (dispatch [:toggle-follow-user-success])
 :toggle-follow-user-success
 (fn [db [_ {profile :profile}]]
   (-> db
       (assoc-in [:loading :toggle-follow-user] false)
       (assoc-in [:profile :following] (:following profile)))))

;; -- Toggle favorite article @ /api/articles/:slug/favorite ------------------
;;
(reg-event-fx                                               ;; usage (dispatch [:toggle-favorite-article slug])
 :toggle-favorite-article                                  ;; triggered when user clicks favorite/unfavorite button on profile page
 (comp-dbfx [(start-loading-indicator :toggle-favorite-article) ;; slug = :slug
             (http-xhrio {:method          (fn [{:keys [db]} _ [_ slug]]
                                             (if (get-in db [:articles slug :favorited]) :delete :post)) ;; check if article is already favorite: yes DELETE, no POST
                          :uri             (fn [_ _ [_ slug]]
                                             ["articles" slug "favorite"]) ;; evaluates to "api/articles/:slug/favorite"
                          :on-success      [:toggle-favorite-article-success] ;; trigger toggle-favorite-article-success
                          :on-failure      [:api-request-error :login]})])) ;; trigger api-request-error with :toggle-favorite-article

(reg-event-db                                               ;; usage: (dispatch [:toggle-favorite-article-success])
 :toggle-favorite-article-success
 (fn [db [_ {article :article}]]
   (let [slug (:slug article)
         favorited (:favorited article)]
     (-> db
         (assoc-in [:loading :toggle-favorite-article] false)
         (assoc-in [:articles slug :favorited] favorited)
         (assoc-in [:articles slug :favoritesCount] (if favorited
                                                      (:favoritesCount article inc)
                                                      (:favoritesCount article dec)))))))

;; -- Logout ------------------------------------------------------------------
;;
(reg-event-fx                                               ;; usage (dispatch [:logout])
 :logout
  ;; This interceptor, defined above, makes sure
  ;; that we clean up localStorage after logging-out
  ;; the user.
 remove-user-interceptor
  ;; The event handler function removes the user from
  ;; app-state = :db and sets the url to "/".
 (fn [{:keys [db]} _]
   {:db       (dissoc db :user)                          ;; remove user from db
    :dispatch [:set-active-page {:page :home}]}))

;; -- Request Handlers -----------------------------------------------------------
;;

(reg-event-db
 :api-request-error                 ; triggered when we get request-error from the server
 (fn [db [_ request-type response]] ;; destructure to obtain request-type and response
   (-> db                           ;; when we complete a request we need to clean so that our ui is nice and tidy
       (assoc-in [:errors request-type] (get-in response [:response :errors]))
       (assoc-in [:loading request-type] false))))
