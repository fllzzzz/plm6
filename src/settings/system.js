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
  { component: '/mes/changed-manage/artifact/index', type: pt.STEEL.V | pt.ENCLOSURE.V, required: false },
  { component: '/mes/changed-manage/machine-part/index', type: pt.STEEL.V | pt.ENCLOSURE.V, required: false },
  { component: '/mes/changed-manage/assemble/index', type: pt.STEEL.V | pt.ENCLOSURE.V, required: false },
  { component: '/mes/scheduling-manage/scheduling/assemble/index', type: pt.STEEL.V | pt.ENCLOSURE.V, required: true },
  { component: '/mes/scheduling-manage/scheduling/artifact/index', type: pt.STEEL.V | pt.ENCLOSURE.V, required: true },
  { component: '/mes/scheduling-manage/scheduling/machine-part/index', type: pt.STEEL.V | pt.ENCLOSURE.V, required: true },
  { component: '/mes/scheduling-manage/scheduling/enclosure/pressed-plate/index', type: pt.STEEL.V | pt.ENCLOSURE.V, required: true },
  { component: '/mes/scheduling-manage/scheduling/enclosure/floor-plate/index', type: pt.STEEL.V | pt.ENCLOSURE.V, required: true },
  { component: '/mes/scheduling-manage/scheduling/enclosure/truss-floor-plate/index', type: pt.STEEL.V | pt.ENCLOSURE.V, required: true },
  { component: '/mes/scheduling-manage/scheduling/enclosure/sandwich-board/index', type: pt.STEEL.V | pt.ENCLOSURE.V, required: true },
  { component: '/mes/scheduling-manage/scheduling/enclosure/folding-piece/index', type: pt.STEEL.V | pt.ENCLOSURE.V, required: true },
  { component: '/mes/scheduling-manage/task/artifact/index', type: pt.STEEL.V | pt.ENCLOSURE.V, required: false },
  { component: '/mes/scheduling-manage/task/machine-part/index', type: pt.STEEL.V | pt.ENCLOSURE.V, required: false },
  { component: '/mes/scheduling-manage/task/enclosure/index', type: pt.STEEL.V | pt.ENCLOSURE.V, required: false },
  { component: '/mes/production-manage/report/artifact/index', type: pt.STEEL.V | pt.ENCLOSURE.V, required: false },
  { component: '/mes/production-manage/report/enclosure/index', type: pt.STEEL.V | pt.ENCLOSURE.V, required: false },
  { component: '/mes/production-manage/analysis/production-statistics/index', type: pt.STEEL.V | pt.ENCLOSURE.V, required: false },
  { component: '/mes/production-manage/analysis/delay-report/index', type: pt.STEEL.V | pt.ENCLOSURE.V, required: false },
  { component: '/mes/production-manage/dashboard/project-dashboard/index', type: pt.STEEL.V | pt.ENCLOSURE.V, required: true },
  { component: '/mes/production-manage/dashboard/main-material-track/index', type: pt.STEEL.V | pt.ENCLOSURE.V, required: true },
  { component: '/mes/production-manage/dashboard/artifact-dashboard/index', type: pt.STEEL.V | pt.ENCLOSURE.V, required: true },
  { component: '/mes/production-manage/dashboard/enclosure-dashboard/index', type: pt.STEEL.V | pt.ENCLOSURE.V, required: true },
  { component: '/mes/production-manage/dashboard/project-report/index', type: pt.STEEL.V | pt.ENCLOSURE.V, required: true },
  { component: '/mes/production-manage/dashboard/painting/index', type: pt.STEEL.V | pt.ENCLOSURE.V, required: true },
  { component: '/mes/team-report/artifact-team/index', type: pt.STEEL.V | pt.ENCLOSURE.V, required: false },
  { component: '/mes/team-report/enclosure-team/index', type: pt.STEEL.V | pt.ENCLOSURE.V, required: false },
  { component: '/mes/team-report/in-staff/piecework-system/index', type: pt.STEEL.V | pt.ENCLOSURE.V, required: false },
  { component: '/mes/team-report/in-staff/allocation-system/index', type: pt.STEEL.V | pt.ENCLOSURE.V, required: false },
  { component: '/mes/team-report/off-staff/wages-config/index', type: pt.STEEL.V | pt.ENCLOSURE.V, required: true },
  { component: '/mes/team-report/wages-adjust/index', type: pt.STEEL.V | pt.ENCLOSURE.V, required: true },
  { component: '/mes/QHSE-manage/disclosure/index', type: pt.STEEL.V | pt.ENCLOSURE.V, required: false },
  { component: '/mes/label-print/artifact/index', type: pt.STEEL.V | pt.ENCLOSURE.V, required: true },
  { component: '/mes/label-print/enclosure/index', type: pt.STEEL.V | pt.ENCLOSURE.V, required: true },
  { component: '/mes/manufactures-manage/inbound-state/artifact-dashboard/index', type: pt.STEEL.V | pt.ENCLOSURE.V, required: true },
  { component: '/mes/manufactures-manage/inbound-state/enclosure-dashboard/index', type: pt.STEEL.V | pt.ENCLOSURE.V, required: true },
  { component: '/mes/manufactures-manage/outbound-state/artifact-dashboard/index', type: pt.STEEL.V | pt.ENCLOSURE.V, required: true },
  { component: '/mes/manufactures-manage/outbound-state/enclosure-dashboard/index', type: pt.STEEL.V | pt.ENCLOSURE.V, required: true },
  { component: '/mes/manufactures-manage/warehouse-state/artifact/index', type: pt.STEEL.V | pt.ENCLOSURE.V, required: true },
  { component: '/mes/manufactures-manage/warehouse-state/enclosure/index', type: pt.STEEL.V | pt.ENCLOSURE.V, required: true },
  { component: '/mes/manufactures-manage/report/index', type: pt.STEEL.V | pt.ENCLOSURE.V, required: false },
  { component: '/mes/pack-and-ship/manual-pack/index', type: pt.STEEL.V | pt.ENCLOSURE.V, required: true },
  { component: '/mes/pack-and-ship/pack-list/index', type: pt.STEEL.V | pt.ENCLOSURE.V, required: false },
  { component: '/mes/pack-and-ship/ship-list/index', type: pt.STEEL.V | pt.ENCLOSURE.V, required: false },
  { component: '/mes/pack-and-ship/receipt-status/index', type: pt.STEEL.V | pt.ENCLOSURE.V, required: false },
  { component: '/mes/pack-and-ship/logistics-list/index', type: pt.STEEL.V | pt.ENCLOSURE.V, required: false },
  { component: '/mes/pack-and-ship/ship-audit/index', type: pt.STEEL.V | pt.ENCLOSURE.V, required: false },
  { component: '/wms/purchase-order/index', type: allPT, required: false }
]
