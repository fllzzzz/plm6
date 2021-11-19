const getters = {
  // sidebar状态
  sidebar: state => state.app.sidebar,
  // device 登录设备
  device: state => state.app.device,
  // api基础路径
  baseApi: state => state.api.baseApi,
  // api附件上传路径
  fileUploadApi: state => state.api.fileUploadApi,
  // axios 取消接口tokens存储
  axiosCancelTokens: state => state.interface.axiosCancelTokens,

  // logo
  logo: state => state.settings.logo,
  // sidebarLogo
  sidebarLogo: state => state.settings.sidebarLogo,
  // 项目名称显示方式
  projectNameShowConfig: state => state.settings.projectNameShowConfig,
  // 项目名称是否显示合同编号
  projectNameShowContractNo: state => state.settings.projectNameShowContractNo,
  // 显示用户风格设置窗口
  showSettings: state => state.settings.showSettings,
  // 显示标签页
  tagsView: state => state.settings.tagsView,
  // 吸顶
  fixedHeader: state => state.settings.fixedHeader,
  // 显示侧边连Logo
  showSidebarLogo: state => state.settings.showSidebarLogo,
  // 公司名称/标题(可在浏览器标签中展示)
  title: state => state.settings.title,

  // 表格边框显示
  tableBorder: state => state.settings.tableBorder,
  // 表格斑马线显示
  tableStripe: state => state.settings.tableStripe,
  // 表格分页每页默认数量
  tablePageSize: state => state.settings.tablePageSize,

  // 访问过的页面
  visitedViews: state => state.tagsView.visitedViews,

  clsTree: state => state.config.clsTree,
  // 材料类科目树
  matClsTree: state => state.config.matClsTree,
  // 材料科目规格 key:value
  matClsSpecKV: state => state.config.classifySpec.specKV,
  // 用户部门树
  userDeptTree: state => state.config.userDeptTree,

  // token 访问令牌
  token: state => state.user.token,
  // 用户信息
  user: state => state.user.user,
  // 库存预警权限
  inventoryNotifyPerm: state => state.user.user.inventoryNotifyPerm,
  // 用户权限
  roles: state => state.user.roles,
  // 用户菜单
  menus: state => state.user.menus,
  // 当前主模块
  currentMenu: state => state.user.currentMenu,
  // 是否已经加载菜单
  loadedMenus: state => state.user.loadedMenus,

  // 路由
  routes: state => state.permission.routes,

  // 系统当前项目id（Navbar）
  globalProjectId: state => state.project.id,
  // 系统当前项目类型（Navbar）
  currentProjectType: state => state.project.projectType,
  // 当前页面项目类型
  routeProjectType: state => state.project.routeProjectType,
  // 项目级联列表(当前项目类型的级联列表)
  projectsCascade: state => state.project.projectsCascade,
  // 项目级联列表Map（key:项目类型,val:项目级联列表）
  projectsCascadeMap: state => state.project.projectsCascadeMap
}

export default getters
