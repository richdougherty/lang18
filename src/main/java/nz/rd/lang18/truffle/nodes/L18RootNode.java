package nz.rd.lang18.truffle.nodes;

import com.oracle.truffle.api.frame.FrameDescriptor;
import com.oracle.truffle.api.frame.VirtualFrame;
import com.oracle.truffle.api.nodes.RootNode;
import nz.rd.lang18.truffle.Lang18Language;

public class L18RootNode extends RootNode {

    /** The function body that is executed, and specialized during execution. */
    @Child private L18ExpressionNode bodyNode;

    public L18RootNode(Lang18Language language, FrameDescriptor frameDescriptor, L18ExpressionNode bodyNode) {
        super(language, frameDescriptor);
        this.bodyNode = bodyNode;
    }

    @Override
    public Object execute(VirtualFrame frame) {
        assert getLanguage(Lang18Language.class).getContextReference().get() != null;
        return bodyNode.execute(frame);
    }
}
