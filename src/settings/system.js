import { projectTypeEnum as pt, projectNameArrangementModeEnum } from '@/utils/enum/modules/contract'
import { allPT } from './config'
import assetsLogo from '@/assets/logo/logo-colorful-title.png'
import assetsSidebarLogo from '@/assets/logo/logo-white.png'

// 标题
export const title = '初鸣智造'
// 页面logo
export const logo = assetsLogo
// 菜单栏顶部logo
export const sidebarLogo = assetsSidebarLogo
// 项目名称显示方式
export const projectNameShowConfig = {
  // 名称合同编号排列方式（默认，合同编号在前）
  arrangement: projectNameArrangementModeEnum.SERIAL_NUMBER_START.V,
  // 显示全称
  showProjectFullName: false,
  // 显示合同编号
  showSerialNumber: true
}
/**
   * @description 显示选择项目下拉框的的组件
   * @param {string} component 路由路径
   * @param {number} type 项目类型，projectType。未填写type时视为包含所有类型
   * @param {boolean} required 该页面是否必须选择项目才能正常使用
   */
export const showProjectSearch = [
  { component: '/mes/projects/index' },
  { component: '/mes/overall-plan/monomer/index', type: pt.STEEL.V | pt.ENCLOSURE.V, required: true },
  { component: '/wms/purchase-order/index', type: allPT, required: false }
]

