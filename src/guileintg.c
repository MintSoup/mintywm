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
#include <libguile/eval.h>
#include <libguile/modules.h>
#include <libguile/strings.h>
#include <libguile/strports.h>
#include <libguile/threads.h>

static SCM set_title(SCM str) {
#define FUNC_NAME "set-title"
	if (!scm_is_string(str)) {
		SCM_MISC_ERROR("arg must be a string", SCM_EOL);
	}
	int n = scm_to_locale_stringbuf(str, stext, sizeof(stext));
	if (n < sizeof(stext))
		stext[n] = 0;
	updatestatus();
	return SCM_UNSPECIFIED;
}

static void register_primitives(void *args) {
	scm_c_define_gsubr("set-title", 1, 0, 0, &set_title);
	scm_c_export("set-title");
}

static void *init(void *data) {
	scm_c_define_module("mintywm core", register_primitives, NULL);
	scm_c_define("core-modules-path", scm_from_locale_string("guile/"));
	scm_c_eval_string("(add-to-load-path core-modules-path)");

	scm_primitive_load(scm_from_locale_string("guile/usercfg.scm"));

	return NULL;
}

void *init_guile(void *arg) {
	scm_with_guile(&init, NULL);
	return NULL;
}
