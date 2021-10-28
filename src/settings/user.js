import variables from '@/styles/element-variables.scss'

/**
 * @type {boolean} true | false
 * @description 是否在右面板显示设置
 */
export const showSettings = false

/**
 * @type {boolean} true | false
 * @description 是否需要标签查看
 */
export const tagsView = true

/**
 * @type {boolean} true | false
 * @description 是否固定标题
 */
export const fixedHeader = true

/**
 * @type {boolean} true | false
 * @description 是否在边栏中显示图标
 */
export const showSidebarLogo = true

/**
 * @type {string | array} 'production' | ['production', 'development']
 * @description 显示错误日志组件。
 * 默认仅在生产环境中使用
 * 如果还想在其他地方使用它，则可以通过 ['production', 'development']
 */
export const errorLog = 'production'

// 【未使用】记住密码状态下的token在Cookie中存储的天数，默认1天
export const tokenCookieExpires = 3

// token key
export const TokenKey = 'User-Token'

// request url key (原公司地址)
export const RequestUrlKey = 'Request-Url'

// 主题
export const theme = variables.theme

// 表格边框
export const tableBorder = true

// 表格斑马线
export const tableStripe = true

// 表格分页每页默认数量
export const tablePageSize = 20
