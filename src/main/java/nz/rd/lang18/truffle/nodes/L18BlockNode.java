package nz.rd.lang18.truffle.nodes;

import com.oracle.truffle.api.CompilerAsserts;
import com.oracle.truffle.api.frame.VirtualFrame;

public class L18BlockNode extends L18ExpressionNode {

    @Children private final L18ExpressionNode[] children;

    public L18BlockNode(L18ExpressionNode[] children) {
        this.children = children;
    }

    @Override
    public Object execute(VirtualFrame frame) {
        int last = this.children.length - 1;
        CompilerAsserts.compilationConstant(last);
        for (int i=0; i<last; i++) {
            this.children[i].execute(frame);
        }
        return this.children[last].execute(frame);
    }
}
