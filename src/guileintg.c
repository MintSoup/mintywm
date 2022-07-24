/*
This file is part of mintywm.

mintywm is free software: you can redistribute it and/or modify it under the
terms of the GNU General Public License as published by the Free Software
Foundation, either version 3 of the License, or (at your option) any later
version.

mintywm is distributed in the hope that it will be useful, but WITHOUT ANY
WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS FOR A
PARTICULAR PURPOSE. See the GNU General Public License for more details.

You should have received a copy of the GNU General Public License along with
mintywm. If not, see <https://www.gnu.org/licenses/>.
*/

#include "dwm.h"
#include "guileintg.h"
#include <libguile.h>
#include <libguile/validate.h>

static SCM scm_settitle(SCM str) {
	int n = scm_to_locale_stringbuf(str, stext, sizeof(stext) - 1);
	if (n < sizeof(stext))
		stext[n] = 0;
	updatestatus();
	return SCM_UNSPECIFIED;
}

static SCM scm_zoom() {
	zoom(&(Arg){0});
	return SCM_UNSPECIFIED;
}
static SCM scm_view(SCM arg) {
	view(&(Arg){.ui = scm_to_uint32(arg)});
	return SCM_UNSPECIFIED;
}
static SCM scm_toggleview(SCM arg) {
	toggleview(&(Arg){.ui = scm_to_uint32(arg)});
	return SCM_UNSPECIFIED;
}
static SCM scm_toggletag(SCM arg) {
	toggletag(&(Arg){.ui = scm_to_uint32(arg)});
	return SCM_UNSPECIFIED;
}
static SCM scm_togglefloating() {
	togglefloating(&(Arg){0});
	return SCM_UNSPECIFIED;
}
static SCM scm_togglebar() {
	togglebar(&(Arg){0});
	return SCM_UNSPECIFIED;
}
static SCM scm_tagmon(SCM arg) {
	tagmon(&(Arg){.i = scm_to_int32(arg)});
	return SCM_UNSPECIFIED;
}
static SCM scm_tag(SCM arg) {
	tag(&(Arg){.ui = scm_to_uint32(arg)});
	return SCM_UNSPECIFIED;
}
static SCM scm_setmfact(SCM arg) {
	setmfact(&(Arg){.f = scm_to_double(arg)});
	return SCM_UNSPECIFIED;
}

static SCM scm_setlayout(SCM arg) {
	int n = scm_to_int(arg);
	if (n < 0 || n >= layouts_count) {
		scm_misc_error("set-layout", "Invalid argument", SCM_EOL);
		return SCM_UNSPECIFIED;
	}
	setlayout(&(Arg){.v = &layouts[n]});
	return SCM_UNSPECIFIED;
}
static SCM scm_resizemouse() {
	resizemouse(&(Arg){0});
	return SCM_UNSPECIFIED;
}
static SCM scm_quit() {
	quit(&(Arg){0});
	return SCM_UNSPECIFIED;
}
static SCM scm_movemouse() {
	movemouse(&(Arg){0});
	return SCM_UNSPECIFIED;
}
static SCM scm_killclient() {
	killclient(&(Arg){0});
	return SCM_UNSPECIFIED;
}
static SCM scm_incnmaster(SCM arg) {
	incnmaster(&(Arg){.i = scm_to_int32(arg)});
	return SCM_UNSPECIFIED;
}
static SCM scm_focusstack(SCM arg) {
	focusstack(&(Arg){.i = scm_to_int32(arg)});
	return SCM_UNSPECIFIED;
}
static SCM scm_focusmon(SCM arg) {
	focusmon(&(Arg){.i = scm_to_int32(arg)});
	return SCM_UNSPECIFIED;
}

static SCM reg_fn(const char *name, int req, int opt, int rst, SCM (*fcn)()) {
	SCM r = scm_c_define_gsubr(name, req, opt, rst, fcn);
	scm_c_export(name);
	return r;
}

static void register_core(void *args) {
	reg_fn("set-title", 1, 0, 0, &scm_settitle);
	reg_fn("view", 1, 0, 0, &scm_view);
	reg_fn("toggle-view", 1, 0, 0, &scm_toggleview);
	reg_fn("toggle-tag", 1, 0, 0, &scm_toggletag);
	reg_fn("tagmon", 1, 0, 0, &scm_tagmon);
	reg_fn("tag", 1, 0, 0, &scm_tag);
	reg_fn("set-mfact", 1, 0, 0, &scm_setmfact);
	reg_fn("set-layout", 1, 0, 0, &scm_setlayout);
	reg_fn("inc-nmaster", 1, 0, 0, &scm_incnmaster);
	reg_fn("focus-stack", 1, 0, 0, &scm_focusstack);
	reg_fn("focus-mon", 1, 0, 0, &scm_focusmon);


	reg_fn("zoom", 0, 0, 0, &scm_zoom);
	reg_fn("toggle-floating", 0, 0, 0, &scm_togglefloating);
	reg_fn("toggle-bar", 0, 0, 0, &scm_togglebar);
	reg_fn("resize-mouse", 0, 0, 0, &scm_resizemouse);
	reg_fn("quit", 0, 0, 0, &scm_quit);
	reg_fn("move-mouse", 0, 0, 0, &scm_movemouse);
	reg_fn("kill-client", 0, 0, 0, &scm_killclient);

	scm_primitive_load(scm_from_locale_string("guile/core.scm"));
}

static void *init(void *data) {
	SCM mod = scm_c_define_module("mintywm core", register_core, NULL);

	scm_c_eval_string_in_module("(load-user-config-file \"init.scm\")", mod);
	return NULL;
}

void *init_guile(void *arg) {
	scm_with_guile(&init, NULL);
	return NULL;
}
