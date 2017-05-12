def ast_map(root, callback):
    if root["expression_type"] == "literal":
        return callback(root)
    elif root["expression_type"] == "lambda":
        return { "expression_type": "lambda",
                 "bound_var": callback(root["bound_var"]),
                 "expression": ast_map(root["expression"], callback) }
    elif root["expression_type"] == "application":
        return { "expression_type": "application",
                 "left": ast_map(root["left"], callback),
                 "right": ast_map(root["right"], callback) }
    else:
        raise NotImplementedError("That expression type is unsupported")

def rename_variable(root, old_name, new_name):
    replace_name = lambda name: new_name if name == old_name else name
    replace_name_cb = lambda node: { "expression_type": "literal",
                       "var_name": replace_name(node["var_name"]) }
    return ast_map(root, replace_name_cb)

def get_free_vars(node):
    if node["expression_type"] == "literal":
        return set(node["var_name"])
    elif node["expression_type"] == "lambda":
        return get_free_vars(node["expression"]) - get_free_vars(node["bound_var"])
    elif root["expression_type"] == "application":
        return get_free_vars(node["left"]).union(get_free_vars(node["right"]))
    else:
        raise NotImplementedError("That expression type is unsupported")
