#include <libguile.h>
#include "dwm.h"

static SCM set_title(SCM str) {
	#define FUNC_NAME "set-title"
	if (!scm_is_string(str)) {
		SCM_MISC_ERROR("arg must be a string", SCM_EOL);
	}
	scm_to_locale_stringbuf(str, stext, sizeof(stext));
	updatestatus();
	return SCM_UNSPECIFIED;
}

static void* init(void* data) {
	scm_c_define_gsubr ("set-title", 1, 0, 0, &set_title);
	scm_primitive_load(scm_from_locale_string("guile/init.scm"));
	return NULL;
}

void* init_guile(void* arg) {
	scm_with_guile(&init, NULL);
	return NULL;
}
