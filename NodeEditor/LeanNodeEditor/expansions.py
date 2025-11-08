"""
Subport expansion widget for displaying nested port structures
"""
from PySide6.QtWidgets import QGraphicsRectItem, QGraphicsTextItem
from PySide6.QtCore import Qt, QRectF, QTimer
from PySide6.QtGui import QPen, QBrush, QFont

from .colors import *


class SubportExpansion(QGraphicsRectItem):
    WIDTH = 12
    HOVER_MARGIN = 30
    
    def __init__(self, parent_port, subports, is_input):
        super().__init__()
        self.parent_port = parent_port
        self.subports = subports
        self.is_input = is_input
        self.labels = []
        
        self.setAcceptHoverEvents(True)
        
        # Import here to avoid circular dependency
        from .ports import PortWidget
        
        height = len(subports) * PortWidget.SUBPORT_SPACING
        
        if is_input:
            self.setRect(0, 0, self.WIDTH, height)
        else:
            self.setRect(-self.WIDTH, 0, self.WIDTH, height)
        
        self.setBrush(QBrush(HOUDINI_BG_LIGHT))
        self.setPen(QPen(HOUDINI_NODE_BORDER, 1))
        
        self._update_subport_positions()
    
    def _update_subport_positions(self):
        from .ports import PortWidget
        
        current_y = PortWidget.SUBPORT_SPACING // 2
        
        for i, subport in enumerate(self.subports):
            if subport.parentItem() != self:
                subport.setParentItem(self)
            
            if self.is_input:
                subport.setPos(self.WIDTH, current_y)
            else:
                subport.setPos(0, current_y)
            
            if i < len(self.labels):
                label = self.labels[i]
            else:
                label = QGraphicsTextItem(subport.name, self)
                label.setDefaultTextColor(HOUDINI_TEXT)
                font = QFont("Arial", 9)
                label.setFont(font)
                self.labels.append(label)
            
            if self.is_input:
                label.setPos(self.WIDTH + PortWidget.PORT_RADIUS * 2 + 5, current_y - 8)
            else:
                label_width = label.boundingRect().width()
                label.setPos(-PortWidget.PORT_RADIUS * 2 - label_width - 5, current_y - 8)
            
            if subport.expanded and subport.expansion_widget:
                node_pos = subport.node.mapFromItem(subport, 0, 0)
                if self.is_input:
                    subport.expansion_widget.setPos(node_pos.x() - 2*SubportExpansion.WIDTH,
                                                   node_pos.y() + subport.PORT_RADIUS)
                else:
                    subport.expansion_widget.setPos(node_pos.x() + SubportExpansion.WIDTH,
                                                   node_pos.y() + subport.PORT_RADIUS)
            
            current_y += PortWidget.SUBPORT_SPACING
            if subport.expanded and subport.subport_widgets:
                current_y += self._calculate_subport_expansion_height(subport)
        
        new_height = current_y - PortWidget.SUBPORT_SPACING // 2
        
        if self.is_input:
            self.setRect(self.WIDTH, 0, self.WIDTH, new_height)
        else:
            self.setRect(-self.WIDTH, 0, self.WIDTH, new_height)
    
    def _calculate_subport_expansion_height(self, subport):
        from .ports import PortWidget
        
        if not subport.expanded or not subport.subport_widgets:
            return 0
        
        height = 0
        for nested in subport.subport_widgets:
            height += PortWidget.SUBPORT_SPACING
            if nested.expanded and nested.subport_widgets:
                height += self._calculate_subport_expansion_height(nested)
        return height
    
    def shape(self):
        from PySide6.QtGui import QPainterPath
        
        path = QPainterPath()
        rect = self.rect()
        if self.is_input:
            expanded = QRectF(rect.x() - 5, 
                            rect.y() - 5, 
                            rect.width() + self.HOVER_MARGIN + 100,
                            rect.height() + 10)
        else:
            expanded = QRectF(rect.x() - self.HOVER_MARGIN - 100,
                            rect.y() - 5, 
                            rect.width() + self.HOVER_MARGIN + 100, 
                            rect.height() + 10)
        path.addRect(expanded)
        return path
    
    def hoverLeaveEvent(self, event):
        if not self.isUnderMouse():
            QTimer.singleShot(500, self.try_collapse)
        super().hoverLeaveEvent(event)
    
    def try_collapse(self):
        if self.isUnderMouse():
            return
        if self.parent_port.hover_active:
            return
        if self.parent_port._is_any_expansion_hovered():
            return
        self.parent_port.collapse()
