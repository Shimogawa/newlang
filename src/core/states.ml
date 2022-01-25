open Elements

class _state =
  object
    val mutable _last_stmt_ret_val : val_t option = None
    val mutable _last_ret_val : val_t = Null
    val mutable _last_expr_val : val_t = Null
    val mutable _loop_end_sig : bool = true
    method last_stmt_ret_val = _last_stmt_ret_val
    method last_ret_val = _last_ret_val
    method set_last_stmt_ret_val v = _last_stmt_ret_val <- v
    method set_last_ret_val v = _last_ret_val <- v
    method last_expr_val = _last_expr_val
    method set_last_expr_val v = _last_expr_val <- v
    method loop_end_sig = _loop_end_sig
    method set_loop_end_sig v = _loop_end_sig <- v
  end

let newlang_state = new _state
