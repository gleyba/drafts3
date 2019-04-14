def cargo_binary(name,binary):
    return native.genrule(
        name = name,
        srcs = ['Cargo.toml'] + native.glob(['src/*']),
        outs = [binary],
        tools = ['//scripts:cargo_proxy'],
        executable = True,
        local = True,
        output_to_bindir = True,
        cmd = '$(location //scripts:cargo_proxy) '
            + native.package_name() + ' '
            + binary + ' $@',
    )

#def _impl(ctx):
#    output = ctx.outputs.out
#    ctx.actions.run_shell(
#        inputs = [],
#        outputs = [],
#        progress_message = "",
#        command = "stat -L -c%%s '%s' > '%s'" % (input.path, output.path),
#    )
#
#cargo_binary_r = rule(
#    implementation = _impl,
#    attrs = {
#        "name": attr.label(mandatory = True),
#        "binary": attr.label(mandatory = True),
#    }
#)