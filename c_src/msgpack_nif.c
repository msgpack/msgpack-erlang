#include "msgpack_nif.h"
#include "msgpack.h"
#include "erl_nif.h"

static ERL_NIF_TERM msgpack_object2erlang_term(ErlNifEnv*,
                                               const msgpack_object*);
static bool msgpack_pack_erl_nif_term(msgpack_packer*,
                                      ErlNifEnv*,
                                      ERL_NIF_TERM);


static inline ERL_NIF_TERM msgpack_error_reason(ErlNifEnv* env,
                                                ERL_NIF_TERM reason)
{
  ERL_NIF_TERM e;
  enif_make_existing_atom(env, "error", &e, ERL_NIF_LATIN1);
  return enif_make_tuple2(env, e, reason);
}
static inline ERL_NIF_TERM msgpack_error_tuple(ErlNifEnv* env, const char* atom)
{
  ERL_NIF_TERM v = enif_make_atom(env, atom);
  return msgpack_error_reason(env, v);
}
static inline ERL_NIF_TERM msgpack_make_badarg(ErlNifEnv* env,
                                               ERL_NIF_TERM badarg)
{
  ERL_NIF_TERM v = enif_make_atom(env, "badarg");
  return msgpack_error_reason(env, enif_make_tuple2(env, v, badarg));
}


static inline ERL_NIF_TERM msgpack_nif_empty_binary(ErlNifEnv* env){
  ErlNifBinary empty;
  enif_alloc_binary(0, &empty);
  return enif_make_binary(env, &empty);
}
static inline ERL_NIF_TERM msgpack_nif_binary(ErlNifEnv* env, size_t s,
                                              const char* bin){
  ErlNifBinary nif_bin;
  enif_alloc_binary(s, &nif_bin);
  memcpy(nif_bin.data, bin, s);
  return enif_make_binary(env, &nif_bin);
}

bool msgpack_pack_erl_nif_term(msgpack_packer* pk,
                               ErlNifEnv* env,
                               ERL_NIF_TERM t)
{
  if(enif_is_atom(env, t)){

    unsigned s;
    enif_get_atom_length(env, t, &s, ERL_NIF_LATIN1);
    // TODO: this may cause stack overflow in recursive call
    char * atom_str = (char*)malloc(sizeof(char)*(s+1));
    int r = enif_get_atom(env, t, atom_str, s+1, ERL_NIF_LATIN1);
    if(r == 0)
      return false;
    if(strncmp(atom_str, "nil", r) == 0){
      msgpack_pack_nil(pk);
    }else if(strncmp(atom_str, "true", r) == 0){
      msgpack_pack_true(pk);
    }else if(strncmp(atom_str, "false", r) == 0){
      msgpack_pack_false(pk);
    }else{
      free(atom_str);
      return false;
    }
    free(atom_str);

  }else if(enif_is_binary(env, t)){
    ErlNifBinary bin;
    enif_inspect_binary(env, t, &bin);
    msgpack_pack_raw(pk, bin.size);
    msgpack_pack_raw_body(pk, bin.data, bin.size);

  }else if(enif_is_number(env, t)){
    
    double d;
    if(enif_get_double(env, t, &d)){
      msgpack_pack_double(pk, d);

    }else{
      ErlNifSInt64 i;
      enif_get_int64(env, t, &i);
      msgpack_pack_int64(pk, i);
    }
    
  }else if(enif_is_empty_list(env, t)){
    msgpack_pack_array(pk, 0);

  }else if(enif_is_list(env, t)){
    unsigned s;
    enif_get_list_length(env, t, &s);
    msgpack_pack_array(pk, s);
    ERL_NIF_TERM cur;
    int i=0;
    for(cur = t; !enif_is_empty_list(env, cur); ){
      ERL_NIF_TERM head, tail;
      enif_get_list_cell(env, cur, &head, &tail);
      bool result = msgpack_pack_erl_nif_term(pk, env, head);
      if(!result){
        
        printf("bad %d %d\n", __LINE__, i);
        return false;
      }
      i++;
      cur = tail;
    }

  }else if(enif_is_tuple(env, t)){
    // msgpack map
    int arity;
    const ERL_NIF_TERM * array;
    enif_get_tuple(env, t, &arity, &array);
    if(arity != 1) return false;
    
    // array[0] is an erlang list
    unsigned s = 0;
    enif_get_list_length(env, array[0], &s);
    msgpack_pack_map(pk, s);
    if(s == 0) return true;

    ERL_NIF_TERM cur;
    for(cur = array[0]; !enif_is_empty_list(env, cur); ){
      ERL_NIF_TERM head, tail;
      enif_get_list_cell(env, cur, &head, &tail);
      const ERL_NIF_TERM * array0;
      int arity0;
      enif_get_tuple(env, head, &arity0, &array0);
      if(arity0 != 2) return false;
      
      bool result = true;
      result &= msgpack_pack_erl_nif_term(pk, env, array0[0]);;
      result &= msgpack_pack_erl_nif_term(pk, env, array0[1]);;
      if(!result){
        return false;
      }
      cur = tail;
    }

  }else if(enif_is_exception(env, t)){
    return false;

  }else if(enif_is_fun(env, t)){
    return false;

  }else if(enif_is_pid(env, t)){
    return false;

  }else if(enif_is_port(env, t)){
    return false;

  }else if(enif_is_ref(env, t)){
    return false;
  }else{
    return false;
  }
  return true;
}

static ERL_NIF_TERM msgpack_nif_pack(ErlNifEnv* env, int argc,
                                     const ERL_NIF_TERM argv[])
{
  msgpack_sbuffer sbuf;
  msgpack_sbuffer_init(&sbuf);
  msgpack_packer pk;
  msgpack_packer_init(&pk, &sbuf, msgpack_sbuffer_write);
  if(msgpack_pack_erl_nif_term(&pk, env, argv[0])){
    ERL_NIF_TERM bin_term = msgpack_nif_binary(env, sbuf.size, sbuf.data);
    msgpack_sbuffer_destroy(&sbuf);
    return bin_term;

  }else{
    msgpack_sbuffer_destroy(&sbuf);
    return msgpack_make_badarg(env, argv[0]);
  }
}

static ERL_NIF_TERM msgpack_unpack_as_binary(ErlNifEnv* env,
                                             const msgpack_object* o)
{
  ErlNifBinary new_bin;
  size_t s = o->via.raw.size;
  enif_alloc_binary(s, &new_bin);
  memcpy(new_bin.data, o->via.raw.ptr, s);
  return enif_make_binary(env, &new_bin);
}

static ERL_NIF_TERM msgpack_unpack_as_array(ErlNifEnv* env,
                                            const msgpack_object* o)
{
  ERL_NIF_TERM ret;
  size_t s = o->via.array.size;
  ret = enif_make_list_from_array(env, NULL, 0);
  size_t i;
  for(i=s; i>0; --i){
    ERL_NIF_TERM t = msgpack_object2erlang_term(env, &(o->via.array.ptr[i-1]));
    ret = enif_make_list_cell(env, t, ret);
  }
  return ret;
}

static ERL_NIF_TERM msgpack_unpack_as_map(ErlNifEnv* env,
                                          const msgpack_object* o){
  ERL_NIF_TERM ret;
  size_t s = o->via.map.size;
  ret = enif_make_list_from_array(env, NULL, 0);
  size_t i;
  for(i=s; i>0; --i){
    ERL_NIF_TERM k = msgpack_object2erlang_term(env, &(o->via.map.ptr[i-1].key));
    ERL_NIF_TERM v = msgpack_object2erlang_term(env, &(o->via.map.ptr[i-1].val));
    ret = enif_make_list_cell(env, enif_make_tuple2(env, k, v), ret);
  }
  return enif_make_tuple1(env, ret);
}

ERL_NIF_TERM msgpack_object2erlang_term(ErlNifEnv* env,
                                        const msgpack_object* o)
{
  //msgpack_object_print(stdout, *o);

  //translate msgpack object to erlang term
  switch(o->type){
  case MSGPACK_OBJECT_NIL:
    return enif_make_atom(env, "nil");

  case MSGPACK_OBJECT_BOOLEAN:
    if(o->via.boolean){
      return enif_make_atom(env, "true");
    }else{
      return enif_make_atom(env, "false");
    }

  case MSGPACK_OBJECT_POSITIVE_INTEGER:
    return enif_make_uint64(env, o->via.u64);

  case MSGPACK_OBJECT_NEGATIVE_INTEGER:
    return enif_make_int64(env, o->via.i64);

  case MSGPACK_OBJECT_DOUBLE:
    return enif_make_double(env, o->via.dec);

  case MSGPACK_OBJECT_RAW:
    return msgpack_unpack_as_binary(env, o);

  case MSGPACK_OBJECT_ARRAY:
    return msgpack_unpack_as_array(env, o);

  case  MSGPACK_OBJECT_MAP:
    return msgpack_unpack_as_map(env, o);
    
  default:
        printf("bad %d\n", __LINE__);
    return enif_make_badarg(env);
  }
}

static ERL_NIF_TERM msgpack_nif_unpack_stream(ErlNifEnv* env, int argc, 
                                              const ERL_NIF_TERM argv[])
{
  if(! enif_is_binary(env, argv[0]) ){
        printf("bad %d\n", __LINE__);
    return msgpack_make_badarg(env, argv[0]);
  }
  ErlNifBinary nif_bin;
  if(! enif_inspect_binary(env, argv[0], &nif_bin)){
        printf("bad %d\n", __LINE__);
    return msgpack_make_badarg(env, argv[0]);
  }
  msgpack_zone mempool;
  msgpack_zone_init(&mempool, 4096);

  msgpack_object obj;
  size_t off = 0;
  ERL_NIF_TERM ret_term;

  switch(msgpack_unpack((const char*)nif_bin.data,
                        nif_bin.size, &off, &mempool, &obj)){

  case MSGPACK_UNPACK_SUCCESS:
    ret_term = msgpack_object2erlang_term(env, &obj);
    msgpack_zone_destroy(&mempool);    
    return enif_make_tuple2(env, ret_term,
                            msgpack_nif_empty_binary(env));

  case MSGPACK_UNPACK_EXTRA_BYTES:
    ret_term = msgpack_object2erlang_term(env, &obj);
    msgpack_zone_destroy(&mempool);
    return enif_make_tuple2(env, ret_term,
                            msgpack_nif_binary(env,
                                               nif_bin.size - off,
                                               (const char*)(nif_bin.data + off)));

  case MSGPACK_UNPACK_CONTINUE:
    msgpack_zone_destroy(&mempool);
    return msgpack_error_tuple(env, "incomplete");

  case MSGPACK_UNPACK_PARSE_ERROR:
    msgpack_zone_destroy(&mempool);
        printf("bad %d\n", __LINE__);
    return msgpack_error_tuple(env, "badarg");

  default:
        printf("bad %d\n", __LINE__);
    return enif_make_badarg(env);
  }
}


static int msgpack_nif_upgrade(ErlNifEnv* env, void** priv_data,
                               void** old_priv_data, ERL_NIF_TERM load_info){
  *priv_data = old_priv_data;
  return 0;
}

static ErlNifFunc msgpack_nif_funcs[] = {
  {"pack", 1, msgpack_nif_pack},
  {"unpack_stream", 1, msgpack_nif_unpack_stream}
};

ERL_NIF_INIT(msgpack_nif,
             msgpack_nif_funcs,
             NULL, //load
             NULL, //reload
             msgpack_nif_upgrade,
             NULL) //unload
