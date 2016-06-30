// The CoqManager (& CoqPanel) class.
// (c) 2015-2016 Mines ParisTech/ARMINES
//
// CoqManager manages a document composed of several coq snippets,
// allowing the user to send/retract indivual coq sentences throu
// them. The Coq snippets can be provided by several sources, we just
// require them to be able to list parts and implement marks.
//

// XXX: use RequireJS or something like that.
"use strict";

// Extra stuff:

Array.prototype.last    = function() { return this[this.length-1]; };
Array.prototype.flatten = function() { return [].concat.apply([], this); };

/***********************************************************************/
/* A Provider Container aggregates several containers, the main deal   */
/* here is keeping track of focus, as the focused container can be     */
/* different from the "active" one                                     */
/***********************************************************************/

class ProviderContainer {

    constructor(elms) {

        // Code snippets.
        this.snippets = [];

        // Debug variables
        var idx = 0;

        // for (e of elms) not very covenient here due to the closure.
        elms.forEach(function (e) {

            // Init.
            var cm = new CmCoqProvider(e);
            cm.idx = idx++;
            this.snippets.push(cm);

            // Track focus XXX (make generic)
            cm.editor.on('focus', evt => { this.currentFocus = cm; });

            // Track invalidate
            cm.onInvalidate = stm => { this.onInvalidate(stm); };
            cm.onMouseEnter = stm => { this.onMouseEnter(stm); };
            cm.onMouseLeave = stm => { this.onMouseLeave(stm); };

            // XXX: We use a strong assumption for now: the cursor is
            // at the invalid region, so we just do goCursor().

            // However, in the future, onInvalidate should provice the
            // concrete invalid statement.
        },this);
    }

    // Get the next candidate and mark it.
    getNext(prev) {

        var spr, next;

        // If we have no previous element start with the first
        // snippet, else get the current one.
        if (!prev) {
            spr  = this.snippets[0];
            next = spr.getNext(null);
        } else {
            spr  = prev.sp;
            next = spr.getNext(prev);
        }

        // We got a snippet!
        if (next) {
            next.sp = spr;
            return next;
        } else {
            // Try the next snippet.
            var idx = this.snippets.indexOf(spr);
            while (idx < this.snippets.length - 1) {
                spr  = this.snippets[idx+1];
                next = spr.getNext(null);
                if (next) {
                    next.sp = spr;
                    return next;
                } else {
                    idx = this.snippets.indexOf(spr);
                }
            } // while
            // No next snippet :( !
            return null;
        }
    }

    mark(stm, mark) {
        stm.sp.mark(stm, mark);
    }

    // Focus and movement-related operations.

    // Get the point of the current focused element.
    getAtPoint() {
        return this.currentFocus.getAtPoint();
    }

    // Indicates if stm is after the point.
    // XXX: Improve
    afterPoint(stm) {

        var idx_point = this.snippets.indexOf(this.currentFocus);
        var idx_cur   = this.snippets.indexOf(stm.sp);

        return (idx_point < idx_cur);

    }

    cursorToStart(stm) {
        stm.sp.cursorToStart(stm);
    }

    cursorToEnd(stm) {
        stm.sp.cursorToEnd(stm);
    }

    focus() {
        if (this.currentFocus)
            this.currentFocus.focus();
        else
            this.snippets[0].focus();
    }

}

/***********************************************************************/
/* CoqManager coordinates the coq code objects, the panel, and the coq */
/* js object.                                                          */
/*                                                                     */
/***********************************************************************/

var copyOptions = function(obj, target) {
    if (!target) target = {};
    for (var prop in target) {
        if (obj.hasOwnProperty(prop)) {
            target[prop] = obj[prop];
        }
    }
    return target;
}

class CoqManager {

    constructor(elems, options) {

        options = options ? options : {};

        // Default options
        this.options = {
            mock:    false,
            prelude: true,
            debug:   true,
            wrapper_id: 'ide-wrapper',
            base_path:  "./",
            init_pkgs: ['init'],
            all_pkgs:  ['init', 'math-comp',
                        'coq-base', 'coq-arith', 'coq-reals',
                        'coquelicot', 'flocq', 'sf', 'cpdt', 'color' ]
            // Disabled on 8.6
            // 'coquelicot', 'flocq', 'tlc', 'sf', 'cpdt', 'color', 'relalg', 'unimath',
            // 'plugin-utils', 'extlib', 'mirrorcore']
        };

        this.options = copyOptions(options, this.options);

        // Setup the Coq statement provider.
        this.provider = this.setupProvider(elems);

        // Setup the Panel UI.
        this.layout = new CoqLayoutClassic(this.options);

        // Setup the Coq worker.
        this.coq           = new Worker(this.options.base_path + 'coq-js/jscoq_worker.js');
        this.coq.onmessage = evt => this.coq_handler(evt);

        this.coq.sendCommand = msg => {
            if(this.options.debug) {
                console.log("Posting: ", msg);
            }
            this.coq.postMessage(msg);
        };

        // XXX: Only done for the adjustWidth
        // XXX: We should get a reflow-friendly document from Coq.
        this.layout.coq = this.coq;

        // Keybindings setup
        // XXX: This should go in the panel init.
        document.addEventListener('keydown', evt => this.keyHandler(evt));

        // XXX: Depends on layout IDs.
        document.getElementById('hide-panel')
            .addEventListener('click', evt => this.layout.toggle() );

        // Panel setup 2: packages panel.
        // XXX: In the future this may also manage the downloads.
        this.packages = new PackageManager(this.layout.packages, this.options.base_path, this.coq);
        // Info
        this.packages.pkg_info = [];
        // Packages to load
        this.packages.pkg_init = [];

        // Pre-init packages
        this.pre_packages = [];

        // Display packages panel:
        var pkg_panel = document.getElementById('packages-panel').parentNode;
        pkg_panel.classList.remove('collapsed');
        this.layout.show();

        // Get Coq version, etc...
        this.coq.sendCommand(["GetInfo"]);

        // This is a sid-based index of processed statements.
        this.doc = {
            number_adds:        0,
            sentences:         [],
            stm_id:            [],
            goals:             [],
            pending_sentences: []
        };

        // XXX: Initial sentence == hack
        this.dummyProvider = { mark : function() {}, getNext: function() { return null; } };
        this.doc.stm_id[1] = { text: "dummy sentence", coq_sid: 1, sp: this.dummyProvider };
        this.doc.sentences = [this.doc.stm_id[1]];

        // XXX: Hack
        this.waitForPkgs = [];

        // The fun starts: Load the set of packages.
        let bp = this.options.base_path + "../coq-pkgs/";
        this.coq.sendCommand(["InfoPkg", bp, this.options.all_pkgs]);
    }

    // Provider setup
    setupProvider(elems) {

        var provider = new ProviderContainer(elems);

        provider.onInvalidate = stm => {

            // Clear the last error, XXX it is a bit of a hack.
            if (this.error && this.error == stm) {
                provider.mark(this.error, "clear");
                this.error = null;
                return;
            } else if (this.error) {
                // Not the one invalidated, clear and go.
                provider.mark(this.error, "clear");
                this.error = null;
            }

            this.goCursor();
        };

        provider.onMouseEnter = stm => {
            if (stm.coq_sid) {
                if (this.options.debug)
                    console.log("Requested goals info:", this.doc.goals[stm.coq_sid]);
            } else {
                console.log("Critical: stm not added (without coq_sid)", stm);
            }
        };

        provider.onMouseLeave = stm => {
            if (this.options.debug)
                console.log("leave");
        };

        return provider;
    }

    // Feedback Processing
    feedProcessingIn(sid) {
    }

    feedFileDependency(sid) {
    }

    feedFileLoaded(sid, file, mod) {
        this.layout.log(file + ' loading.', 'Info');
    }

    // The first state is ready.
    feedProcessed(nsid) {

        this.layout.proof.textContent +=
            "\ncoq worker is ready with sid!! " + nsid.toString() +
            "\nPlease, wait for library loading";

        this.feedProcessed = this.feedProcessedReady;

        this.enable();
        this.layout.adjustWidth();

    }

    feedProcessedReady(nsid) {

        if(this.options.debug)
            console.log('State processed', nsid);

        // The semantics of the stm here are a bit inconvenient: it
        // will send `ProcessedReady` feedback message before we the
        // `Stm.add` call has returned, thus we are not ready to
        // handle as we don't know of their existance yet. The typical
        // example is when `Stm.add` forces an observe due to
        // side-effects.
        //
        // We ignore such feedback for now, observe will make it be
        // resent. However it is possible we would have to queue the
        // feedback in the future.
        if (! this.doc.stm_id[nsid] ) {
            console.log('ready but not added?', nsid);
            return;
        }

        var stm = this.doc.stm_id[nsid];

        this.provider.mark(stm, "clear");
        this.provider.mark(stm, "ok");

        // Get goals
        this.coq.sendCommand(["Goals"]);

        // if(update_focus)
        //     this.provider.cursorToEnd(next);

    }

    // Simplifier to the "rich" format coq uses.
    richpp2HTML(msg) {

        // Elements are ...
        if (msg.constructor !== Array) {
            return msg;
        }

        var ret;
        var tag, ct, id, att, m;
        [tag, ct] = msg;

        switch (tag) {

        // Element(tag_of_element, att (single string), list of xml)
        case "Element":
            [id, att, m] = ct;
            let imm = m.map(this.richpp2HTML, this);
            ret = "".concat(...imm);
            ret = `<span class="${id}">` + ret + `</span>`;
            break;

        // PCData contains a string
        case "PCData":
            ret = ct;
            break;

        default:
            ret = msg;
        }
        return ret;
    }

    // compatibility with 8.5
    feedErrorMsg(sid, loc, msg) {

        this.feedMessage(sid, ['Error'], [loc], msg);

    }

    // Error handler.
    handleError(sid) {

        let err_stm;

        // Error on add or in added stm ?
        if (sid < 0) {
            err_stm = this.doc.pending_sentences[0];
            // XXX; hack
            this.error = err_stm;
        } else {
            err_stm = this.doc.stm_id[sid];
            this.error = err_stm;
            this.coqCancelled([sid]);
        }
        // this.provider.mark(err_stm, "clear");
        this.provider.mark(err_stm, "error");

        this.doc.pending_sentences = [];
    }

    feedMessage(sid, lvl, loc, msg) {

        var fmsg = this.richpp2HTML(msg);

        // Coq lvl
        var lvl  = lvl[0];

        if(this.options.debug)
            console.log('Msg', sid, lvl, loc, fmsg);

        this.layout.log(fmsg, lvl);

        // XXX: highlight location.

        if (lvl === 'Error') {
            handleError(sid, loc, msg);
        }
    }

    // Coq Message processing.
    coqFeedback(fb) {

        var feed_tag = fb.contents[0];

        if( this['feed'+feed_tag] ) {
            fb.contents[0] = fb.id[1];
            this['feed'+feed_tag].apply(this, fb.contents);
        } else {
            console.log('Feedback type', feed_tag, 'not handled');
        }

    }

    process_pending(sid) {

        var stm      = this.doc.pending_sentences[0];
        this.coq.sendCommand(["Add", sid, -1, stm.text]);

    }

    add_pending(stm) {
        if(this.doc.pending_sentences.length) {
            this.doc.pending_sentences.push(stm);
        } else {
            var stm_last = this.doc.sentences.last();

            this.doc.pending_sentences.push(stm);
            this.coq.sendCommand(["Add", stm_last.coq_sid, -1, stm.text]);
        }
    }

    coqAdded(nsid) {

        if(this.options.debug)
            console.log('adding: ', nsid);

        // Added by Coq !!
        let cur_stm = this.doc.pending_sentences.shift();
        this.doc.sentences.push(cur_stm);
        this.doc.stm_id[nsid] = cur_stm;
        cur_stm.coq_sid   = nsid;

        // Avoid stack overflows by doing a commit every 2^5 sentences.
        let so_threshold = 48;
        if( !(this.doc.number_adds++ % so_threshold) )
            this.coq.sendCommand(['Observe', nsid]);

        if(this.doc.pending_sentences.length) {

            this.process_pending(nsid);
            return;

        } else if (this.goTarget) {
            // [Modulo the same old bugs, we need a position comparison op]
            // We have reached the destination...
            if (this.provider.getAtPoint() || this.provider.afterPoint(cur_stm) ) {
                this.coq.sendCommand(['Observe', nsid]);
            } else {
                this.goNext(false);
            }
        } else {
            this.coq.sendCommand(['Observe', nsid]);
        }
    }

    // Gets a list of cancelled sids.
    coqCancelled(sids) {

        if(this.options.debug)
            console.log('cancelling', sids);

        sids.forEach(function (sid) {

            let stm_to_cancel = this.doc.stm_id[sid];
            let stm_idx       = this.doc.sentences.indexOf(stm_to_cancel);

            this.doc.stm_id[sid] = null;
            this.doc.sentences.splice(stm_idx, 1);

            this.provider.mark(stm_to_cancel, "clear");

        }, this);

        // Update goalsa

        this.layout.update_goals(this.doc.goals[this.doc.sentences.last().coq_sid]);
    }

    coqGoalInfo(sid, goals) {

        var hgoals = this.richpp2HTML(goals);
        this.doc.goals[sid] = hgoals;

        // XXX this doesn't work propertly
        if (!this.doc.pending_sentences.length)
            this.layout.update_goals(hgoals);
    }

    coqLog(level, msg) {

        if (this.options.debug) console.log(msg, level[0]);

        this.layout.log(msg, level[0]);
    }

    coqLibInfo(bname, bi) {

        this.packages.addBundleInfo(bname, bi);
        this.packages.pkg_info[bname] = bi;

        // Check if we want to load this package.
        var rem_pkg = this.options.init_pkgs;
        var idx = rem_pkg.indexOf(bname);

        // Worker path is coq-js.
        let bp = this.options.base_path + "../coq-pkgs/";

        if(idx > -1) {
            this.coq.sendCommand(['LoadPkg', bp, bname]);
        }
    }

    coqLibProgress(evt) {
        this.packages.onPkgProgress(evt);
    }

    coqLibLoaded(bname) {

        this.packages.onBundleLoad(bname);

        var rem_pkg = this.options.init_pkgs;
        var idx = rem_pkg.indexOf(bname);

        if(idx > -1) {

            this.packages.pkg_init.push(bname);
            rem_pkg.splice(idx, 1);

            // All the packages have been loaded.
            if (rem_pkg.length === 0)
                this.coqInit();
        }
    }

    coqCoqExn(msg) {
        // this.layout.log(msg, "Error");
        console.log('coqExn', msg);
    }

    coqJsonExn(msg) {
        // this.layout.log(msg, "Error");
        console.log('jsonExn', msg);
    };

    coqCoqInfo(info) {

        this.layout.proof.textContent =
               info
            + "\nPlease wait for the libraries to load, thanks!"
            + "\nIf you have trouble try cleaning your browser's cache.\n";
    }

    // Coq Init: At this point, the required libraries are loaded
    // and Coq is ready to be used.
    coqInit() {

        // Hide the packages panel.
        var pkg_panel = document.getElementById('packages-panel').parentNode;
        pkg_panel.classList.add('collapsed');

        // Enable the IDE.
        this.layout.proof.textContent +=
            "\n===> JsCoq filesystem initalized with success!\n" +
            "===> Loaded packages [" + this.options.init_pkgs.join(', ') + "] \n";

        // XXXXXX: Critical point
        var load_lib = [];
        var init_lib = this.options.init_pkgs;

        if (this.options.prelude) {
            load_lib.push(["Coq", "Init", "Prelude"]);
        }

        let bp = this.options.base_path + "../";

        let load_paths = this.packages.pkg_init.map(
            bundle => this.packages.pkg_info[bundle].pkgs
        ).flatten().map( pkg => pkg.pkg_id );

        this.coq.sendCommand(["Init", load_lib, load_paths]);
        // Done!
    };

    coq_handler(evt) {

        var msg     = evt.data;
        var msg_tag = msg[0];

        if(this.options.debug)
            console.log("coq_evt", msg);

        // We call the corresponding coq$msg_tag(msg[1]..msg[n])

        if( this['coq'+msg_tag] ) {
            msg.shift();
            this['coq'+msg_tag].apply(this, msg);
        } else {
            console.log('Message type', msg_tag, 'not handled');
        }

    };

    goPrev(inPlace) {

        if (this.doc.pending_sentences.length)
            return;

        // If we didn't load the prelude, prevent unloading it to
        // workaround a bug in Coq.
        if (!this.options.prelude && this.doc.sentences.length <= 1) return;

        if (this.error) {
            this.provider.mark(this.error, "clear");
            this.error = null;
        }

        var stm      = this.doc.sentences.last();

        if(!inPlace)
            this.provider.cursorToStart(stm);

        // Tell coq to go back to the old state.
        let sid_old              = stm.coq_sid;
        stm.coq_sid              = null;
        this.doc.goals[sid_old]  = null;

        this.coq.sendCommand(['Cancel', sid_old]);
    }

    // Return if we had success.
    goNext(update_focus) {

        var cur_stm;

        if (this.doc.pending_sentences.length > 0)
            cur_stm = this.doc.pending_sentences.last();
        else
            cur_stm = this.doc.sentences.last();

        var next = this.provider.getNext(cur_stm);

        // We are the the end
        if(!next) { return false; }

        // Hack....
        if(next.is_comment) {
            this.provider.mark(next, "ok");
            return true;
        } else {
            this.provider.mark(next, "processing");
        }

        // We focus the new snippet.
        if(update_focus) {
            this.currentFocus = next.sp;
            this.currentFocus.focus();
        }

        // process special jscoq commands, for now:
        // Comment "pkg: list" will load packages.
        this.process_special(next.text);

        this.add_pending(next);

        return false;
    }

    goCursor() {

        var cur = this.provider.getAtPoint();

        if (cur) {

            if(!cur.coq_sid) {
                console.log("critical error, stm not registered");
            } else {
                this.coq.sendCommand(['Cancel', cur.coq_sid]);
            }
        } else {

            this.goTarget = true;
            this.goNext(false);
        }
    }

    // Drops all the state!
    reset() {

        // Not yet initialized.
        if(!this.sid) return;

        var initial_sid;

        if (this.options.prelude) {
            initial_sid = this.sid[1];
            this.sid    = [this.sid[0], this.sid[1]];
        } else {
            initial_sid = this.sid[0];
            this.sid    = [this.sid[0]];
        }

        // Reset Coq.
        this.coq.edit(initial_sid);

        // Reset out sentences
        this.doc.sentences.forEach(function(stm) {
            this.provider.mark(stm, "clear");
        }, this);

        this.doc.sentences = [];

    }

    // Keyboard handling
    keyHandler(e) {

        // All our keybindings are prefixed by alt.
        if (e.keyCode === 119) // F8
            this.layout.toggle();

        if (!e.altKey && !e.metaKey) return true;
        var btn_name;
        switch (e.keyCode) {
            case 13: // ENTER
                btn_name = 'to-cursor';
                break;
            case 78: // N
            case 40: // flèche en bas
                btn_name = 'down';
                break;
            case 80: // P
            case 38: // flèche en haut
                btn_name = 'up';
                break;
        }

        if(btn_name) {
            this.provider.focus();
            this.raiseButton(btn_name);
            e.preventDefault();
        }
    }

    // Enable the IDE.
    enable() {

        // Set Printing Width
        window.addEventListener('resize', evt => { this.layout.adjustWidth(); } );

        // XXX: Should be the task of the layout.
        this.btnEventHandler = this.toolbarClickHandler.bind(this);
        this.layout.buttons.addEventListener('click', this.btnEventHandler);
        this.layout.buttons.style.display = 'inline-block';
        this.layout.buttons.style.opacity = 1;
        this.provider.focus();
    }

    // Disable the IDE.
    disable() {
        // Disable the buttons.
        this.layout.buttons.removeEventListener('click', this.btnEventHandler);
        this.layout.buttons.style.display = 'none';
        this.layout.buttons.style.opacity = 0;
        this.layout.proof.textContent +=
                "\n===> Waiting for Package load!\n";

    }

    toolbarClickHandler(evt) {

        this.provider.focus();

        switch (evt.target.name) {
            case 'to-cursor' :
                this.goCursor();
                break;

            case 'up' :
                this.goPrev(false);
                break;

            case 'down' :
                this.goNext(true);
                break;
        }
    }

    raiseButton(btn_name) {

        // XXX: EG: Here I disagree with the current code, it
        // should be possible to use the coq manager without a toolbar!

        // This is a bit different from most UI indeed.
        var btns = this.layout.buttons.getElementsByTagName('img');
        var btn  = btns.namedItem(btn_name);

        if (btn) {
            btn.dispatchEvent(new MouseEvent('click',
                                             {'view'       : window,
                                              'bubbles'    : true,
                                              'cancelable' : true
                                             }));
        }
    }

    process_special(text) {

        var special;

        if (special = text.match(/Comments \"(.*): (.+)\"./)) {
            let cmd  = special[1];
            let args = special[2];

            switch (cmd) {

            case 'pkgs':
                let pkgs = args.split(' ');
                console.log('Requested pkgs '); console.log(pkgs);

                let pkg_panel = document.getElementById('packages-panel').parentNode;
                pkg_panel.classList.remove('collapsed');

                this.disable();
                this.waitForPkgs = pkgs;

                pkgs.forEach(this.coq.add_pkg,this);

                return true;

            case 'dump':
                window.dumpCache();
                return true;

            default:
                console.log("Unrecognized jscoq command");
                return false;
            }
        }
        return false;
    }
}

// Local Variables:
// js-indent-level: 4
// End:
