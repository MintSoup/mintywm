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

static SCM set_title(SCM str) {
#define FUNC_NAME "set-title"
	if (!scm_is_string(str)) {
		SCM_MISC_ERROR("arg must be a string", SCM_EOL);
	}
	int n = scm_to_locale_stringbuf(str, stext, sizeof(stext) - 1);
	if (n < sizeof(stext))
		stext[n] = 0;
	updatestatus();
	return SCM_UNSPECIFIED;
}

static void register_core(void *args) {
	scm_c_define_gsubr("set-title", 1, 0, 0, &set_title);
	scm_c_export("set-title");

	scm_primitive_load(scm_from_locale_string("guile/core.scm"));
}

static void *init(void *data) {
	SCM mod = scm_c_define_module("mintywm core", register_core, NULL);

	scm_c_eval_string_in_module("(load-user-config-file \"init.scm\")",
								mod);
	return NULL;
}

void *init_guile(void *arg) {
	scm_with_guile(&init, NULL);
	return NULL;
}
