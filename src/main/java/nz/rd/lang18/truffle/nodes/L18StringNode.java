package nz.rd.lang18.truffle.nodes;

import com.oracle.truffle.api.frame.VirtualFrame;

public class L18StringNode extends L18ExpressionNode {

    private final String value;

    public L18StringNode(String value) {
        this.value = value;
    }

    @Override
    public Object execute(VirtualFrame frame) {
        return value;
    }
}
