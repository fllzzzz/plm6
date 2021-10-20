import { projectTypeEnum as pt, projectNameArrangementModeEnum } from '@/utils/enum/modules/contract'
import assetsLogo from '@/assets/logo/logo-colorful-title.png'
import assetsSidebarLogo from '@/assets/logo/logo-white.png'

// 项目类型：全部
// eslint-disable-next-line no-unused-vars
const allPT = Object.keys(pt).reduce((res, cur) => {
  return res | cur
}, 0)

// 标题
export const title = '初鸣智造'
// 页面logo
export const logo = assetsLogo
// 菜单栏顶部logo
export const sidebarLogo = assetsSidebarLogo
// 项目名称显示方式
export const projectNameShowConfig = {
  // 名称合同编号排列方式（默认，合同编号在前）
  arrangement: projectNameArrangementModeEnum.CONTRACT_NO_START,
  // 显示合同编号
  showContractNo: true
}
/**
   * @description 显示选择项目下拉框的的组件
   * @param {string} component 路由路径
   * @param {number} type 项目类型，projectType。未填写type时视为包含所有类型
   * @param {boolean} required 该页面是否必须选择项目才能正常使用
   */
export const showProjectSearch = [
  { component: '/mes/projects/index' },
  { component: '/mes/overall-plan/monomer/index', type: pt.STEEL.V | pt.ENCLOSURE.V, required: true }
]

