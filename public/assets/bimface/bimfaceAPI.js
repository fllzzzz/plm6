export {initBimfaceApp}  from './index'

// webAppConfig.renderingMode = 1
// 1代表全量绘制，2代表增量绘制，0代表自动

function getConfig() {
    return new Glodon.Bimface.Application.WebApplication3DConfig()
}

function getApp(config) {
    let app = new Glodon.Bimface.Application.WebApplication3D(config)
    return app
}

function getViewer3DEvent() {
    return Glodon.Bimface.Viewer.Viewer3DEvent
}

function getApp3DEvent() {
    return Glodon.Bimface.Application.WebApplication3DEvent
}

function get2DConfig() {
    return new Glodon.Bimface.Application.WebApplicationDrawingConfig()
}

function get2DApp(config) {
    let app = new Glodon.Bimface.Application.WebApplicationDrawing(config)
    return app
}

function getViewer2DEvent() {
    return Glodon.Bimface.Viewer.ViewerDrawingEvent
}

function getColor(color, opacity) {
    let glodonColor = new Glodon.Web.Graphics.Color(color, opacity);
    return glodonColor
}

function getIsolateOption() {
    return Glodon.Bimface.Viewer.IsolateOption
}

function getDrawableContainerConfig() {
    return new Glodon.Bimface.Plugins.Drawable.DrawableContainerConfig()
}

function getDrawableContainer(drawableConfig) {
    return new Glodon.Bimface.Plugins.Drawable.DrawableContainer(drawableConfig)
}

function getCustomItemConfig() {
   return new Glodon.Bimface.Plugins.Drawable.CustomItemConfig()
}

function getCustomItem(config) {
    return new Glodon.Bimface.Plugins.Drawable.CustomItem(config)
}

function getLeadLabelConfig() {
    return new Glodon.Bimface.Plugins.Drawable.LeadLabelConfig()
}
function getLeadLabel(config) {
    return new Glodon.Bimface.Plugins.Drawable.LeadLabel(config)
}

function getPanelPositions() {
    return Glodon.Bimface.UI.Panel.PanelPositions
}

function getPanelConfig() {
    return new Glodon.Bimface.UI.Panel.PanelConfig()
}

function createPanel(config) {
    console.log(Glodon.Bimface)
    return new Glodon.Bimface.UI.Panel.Panel(config)
}

function getButtonConfig() {
    return new Glodon.Bimface.UI.Button.ButtonConfig()
}

function createButton(config) {
    return new Glodon.Bimface.UI.Button.Button(config)
}

function getToolbarConfig() {
    return new Glodon.Bimface.UI.Toolbar.ToolbarConfig()
}

function createToolbar(config) {
    return new Glodon.Bimface.UI.Toolbar.Toolbar(config)
}

function getTreeConfig() {
    return new Glodon.Bimface.UI.Tree.TreeNodeConfig()
}

function createTree(config) {
    return new Glodon.Bimface.UI.Tree.TreeNode(config)
}

function getTreeApi(el) {
    return new Glodon.Bimface.UI.Tree.Tree(el)
}

export {
    getConfig,
    getApp,
    getViewer3DEvent,
    getApp3DEvent,
    get2DConfig,
    get2DApp,
    getViewer2DEvent,
    getColor,
    getIsolateOption,
    getDrawableContainerConfig,
    getDrawableContainer,
    getCustomItemConfig,
    getCustomItem,
    getLeadLabelConfig,
    getLeadLabel,
    getPanelPositions,
    getPanelConfig,
    createPanel,
    getToolbarConfig,
    createToolbar,
    getTreeConfig,
    createTree,
    getTreeApi,
    getButtonConfig,
    createButton
}