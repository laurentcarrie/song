#include "fcgi_stdio.h"
#include <stdlib.h>
#include "caml/mlvalues.h"
#include "caml/fail.h"
#include "caml/callback.h"
#include "caml/alloc.h"
#include "caml/memory.h"
#include "assert.h"

#include <sys/types.h>
#include <unistd.h>
#include <time.h>
#include <sys/time.h>

#define MAX_LENGTH_HEADER 10000


FILE * flog = 0 ;

int pid = 0;

#define fcgi_log0(format) \
  fprintf(flog,format) ;\
  fprintf(flog,"\n") ;\
  fflush(flog) ;

#define fcgi_log1(format,a1) \
  fprintf(flog,format,a1) ;\
  fprintf(flog,"\n") ;\
  fflush(flog) ;

#define fcgi_log2(format,a1,a2) \
  fprintf(flog,format,a1,a2) ;\
  fprintf(flog,"\n") ;\
  fflush(flog) ;

CAMLprim
value fcgi_log_string(value os) {
  CAMLparam1(os) ;
  const char* s = String_val(os) ;
  time_t t = time(0) ;
  struct tm * t2 = localtime(&t) ;
  assert(flog) ;
  fprintf(flog,"%d - %02d:%02d:%02d %02d/%02d/%02d - %s\n",
	  pid,
	  t2->tm_hour,t2->tm_min,t2->tm_sec,
	  t2->tm_mday,(t2->tm_mon+1),(t2->tm_year-100),
	  s
	  ) ;
  fflush(flog) ;
  CAMLreturn(Val_unit) ;
}


CAMLprim
value fcgi_print(value os) {
  CAMLparam1(os) ;
  const char* s = String_val(os) ;
  printf(s) ;
  fcgi_log0(s) ;
  CAMLreturn(Val_unit) ;
}

CAMLprim
value fcgi_init() {
  //  assert(0) ;
  CAMLparam0() ;
  char path[10000] ;
  pid = getpid() ;
  // snprintf(path,9999,"%s/tmp/CIF_fcgi.log",getenv("PWD"))  ;
  // snprintf(path,9999,"/var/log/lighttpd/SONG_fcgi.log")  ;
  // flog = fopen("/usr/p119208/u1/work/work-song/build/tmp/SONG_fcgi.log","w") ;
  flog = fopen("/var/log/lighttpd/SONG_fcgi.log","w") ;
  //  flog = fopen(path,"a") ;
  // assert(0) ;
  assert(flog) ;
  fcgi_log0("init") ;
  CAMLreturn(Val_unit) ;
}

CAMLprim
value fcgi_loop()
{
  CAMLparam0();
  assert(flog) ;
  int count = 0;
  // static value * closure_f = NULL ;
  int needs_init=1 ;

  while(FCGI_Accept() >= 0) {
    if (needs_init) {
      value * closure_init = caml_named_value("web_init") ;
      assert(closure_init) ;
      caml_callback(*closure_init,Val_unit) ;
      needs_init=0 ;
    }
    fcgi_log0("accept") ;
    fcgi_log0(getenv("SCRIPT_NAME")) ;
    value * closure_f = NULL ;
    if (closure_f==NULL) {
      closure_f = caml_named_value("process_request") ;
    }
    assert(closure_f) ;
    caml_callback(*closure_f,Val_unit) ;
  }
  CAMLreturn(Val_unit) ;
}


CAMLprim
value fcgi_input(value olength) {
  CAMLparam1(olength) ;
  char header[MAX_LENGTH_HEADER+1] ;
  int length = Int_val(olength) ;
  if ( length >= MAX_LENGTH_HEADER ) {
    fcgi_log1("ERROR : length too big : %d ",length) ;
    header[0]='\0' ;
    value ret = caml_copy_string(header) ;
    CAMLreturn(ret) ;
  } 
  int total_read = 0 ;
  while (total_read<length) {
    size_t nbread = FCGI_fread((void*)(header+total_read),1,(size_t)(length-total_read),FCGI_stdin) ;
    total_read += nbread ;
    fcgi_log2("input %d bytes, required %d",nbread,length) ;
    fcgi_log1("total read : %d",total_read) ;
    if (total_read<length) { sleep(1) ; }
  }
  // if (nbread < length) {
  // fcgi_log2("not enough read, %d, %d",nbread,length) ;
  // }
  header[length] = '\0' ;
  fcgi_log1("FCFI input : %s",header) ;
  CAMLlocal1(ret) ;
  ret = caml_copy_string(header); 
  CAMLreturn(ret) ;
}


CAMLprim
value fcgi_set_exit_status(value ocode) {
  CAMLparam1(ocode) ;
  int code = Int_val(ocode) ;
  fcgi_log1("set exit status %d",code) ;
  FCGI_SetExitStatus(code) ;
  CAMLreturn(Val_unit) ;
}
