import {
  projectTypeEnum as pt,
  projectNameArrangementModeEnum,
  TechnologyTypeAllEnum as ttEnum,
  projectModeEnum,
  businessTypeEnum
} from '@/utils/enum/modules/contract'
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
  // bim
  { component: '/bim/model-show/index', type: pt.STEEL.V, required: true },
  { component: '/bim/integration-model-show/index', type: pt.STEEL.V, required: true },
  // 发运管理
  { component: '/ship-manage/pack-and-ship/manual-pack/index', type: allPT, required: true },
  { component: '/ship-manage/pack-and-ship/ship-list/index', type: allPT, required: true },
  { component: '/ship-manage/pack-and-ship/logistics-list/index', type: allPT, required: true },
  { component: '/ship-manage/pack-and-ship/pack-list/index', type: allPT, required: true },
  { component: '/ship-manage/pack-and-ship/receipt-status/index', type: allPT, required: true },
  { component: '/ship-manage/pack-and-ship/ship-audit/index', type: allPT, required: true },
  { component: '/ship-manage/pack-and-ship/product-send-receive-storage/structure/index', type: allPT, required: true },
  { component: '/ship-manage/pack-and-ship/product-send-receive-storage/enclosure/index', type: allPT, required: true },
  // MES
  { component: '/mes/projects/index', type: allPT, required: true },
  { component: '/mes/overall-plan/monomer/index', type: pt.STEEL.V, required: true },
  { component: '/mes/changed-manage/artifact/index', type: pt.STEEL.V, required: false },
  { component: '/mes/changed-manage/machine-part/index', type: pt.STEEL.V, required: false },
  { component: '/mes/changed-manage/assemble/index', type: pt.STEEL.V, required: false },
  { component: '/mes/changed-manage/common-change/index', type: pt.STEEL.V, required: false },
  { component: '/mes/changed-manage/simple-change-record/index', type: pt.STEEL.V, required: false },
  { component: '/mes/changed-manage/surplus-list/index', type: pt.STEEL.V, required: false },
  { component: '/mes/scheduling-manage/scheduling/assemble/index', type: pt.STEEL.V, required: true },
  { component: '/mes/scheduling-manage/scheduling/artifact/index', type: pt.STEEL.V, required: true },
  { component: '/mes/scheduling-manage/scheduling/machine-part-summary/index', type: pt.STEEL.V, required: true },
  { component: '/mes/scheduling-manage/scheduling/enclosure/pressed-plate/index', type: pt.STEEL.V, required: true },
  { component: '/mes/scheduling-manage/scheduling/enclosure/floor-plate/index', type: pt.STEEL.V, required: true },
  { component: '/mes/scheduling-manage/scheduling/enclosure/truss-floor-plate/index', type: pt.STEEL.V, required: true },
  { component: '/mes/scheduling-manage/scheduling/enclosure/sandwich-board/index', type: pt.STEEL.V, required: true },
  { component: '/mes/scheduling-manage/scheduling/enclosure/folding-piece/index', type: pt.STEEL.V, required: true },
  { component: '/mes/work-order-manage/artifact/index', type: pt.STEEL.V, required: false },
  { component: '/mes/work-order-manage/machine-part/index', type: pt.STEEL.V, required: false },
  { component: '/mes/task-tracking/assistance-operate/productionLine-assistance/index', type: pt.STEEL.V, required: false },
  { component: '/mes/task-tracking/assistance-operate/process-assistance/index', type: pt.STEEL.V, required: false },
  { component: '/mes/scheduling-manage/task/artifact/index', type: pt.STEEL.V, required: false },
  { component: '/mes/scheduling-manage/task/machine-part/index', type: pt.STEEL.V, required: false },
  { component: '/mes/scheduling-manage/task/enclosure/index', type: pt.STEEL.V, required: false },
  { component: '/mes/production-manage/report/machine-part/index', type: pt.STEEL.V, required: false },
  { component: '/mes/production-manage/report/assemble/index', type: pt.STEEL.V, required: false },
  { component: '/mes/production-manage/report/artifact/index', type: pt.STEEL.V, required: false },
  { component: '/mes/production-manage/report/enclosure/index', type: pt.STEEL.V, required: false },
  { component: '/mes/production-manage/analysis/production-statistics/index', type: pt.STEEL.V, required: false },
  { component: '/mes/production-manage/analysis/delay-report/index', type: pt.STEEL.V, required: false },
  { component: '/mes/production-manage/dashboard/project-dashboard/index', type: pt.STEEL.V, required: true },
  { component: '/mes/production-manage/dashboard/main-material-track/index', type: pt.STEEL.V, required: true },
  { component: '/mes/production-manage/dashboard/artifact-dashboard/index', type: pt.STEEL.V, required: true },
  { component: '/mes/production-manage/dashboard/production-tracking/index', type: pt.STEEL.V, required: true },
  { component: '/mes/production-manage/dashboard/enclosure-dashboard/index', type: pt.STEEL.V, required: true },
  { component: '/mes/production-manage/dashboard/project-report/index', type: pt.STEEL.V, required: true },
  { component: '/mes/production-manage/dashboard/assembly-match/index', type: pt.STEEL.V, required: true },
  { component: '/mes/production-manage/dashboard/painting/index', type: pt.STEEL.V, required: true },
  { component: '/mes/team-report/artifact-team/index', type: pt.STEEL.V, required: false },
  { component: '/mes/team-report/enclosure-team/index', type: pt.STEEL.V, required: false },
  { component: '/mes/team-report/artifact-team-wage/index', type: pt.STEEL.V, required: false },
  { component: '/mes/team-report/enclosure-team-wage/index', type: pt.STEEL.V, required: false },
  { component: '/mes/team-report/off-staff/wages-config/index', type: pt.STEEL.V, required: true },
  { component: '/mes/team-report/wages-adjust/index', type: pt.STEEL.V, required: true },
  { component: '/mes/QHSE-manage/disclosure/index', type: pt.STEEL.V, required: false },
  { component: '/mes/QHSE-manage/quality-inspection-report/index', type: pt.STEEL.V, required: false },
  { component: '/mes/label-print/artifact/index', type: pt.STEEL.V, required: true },
  { component: '/mes/label-print/part/index', type: pt.STEEL.V, required: true },
  { component: '/mes/label-print/auxiliary-material/index', type: pt.STEEL.V, required: true },
  { component: '/mes/factory-report/group-report/index', type: pt.STEEL.V, required: true },
  { component: '/mes/manufactures-manage/inbound-state/artifact-dashboard/index', type: pt.STEEL.V, required: true },
  { component: '/mes/manufactures-manage/inbound-state/enclosure-dashboard/index', type: pt.STEEL.V, required: true },
  { component: '/mes/manufactures-manage/outbound-state/artifact-dashboard/index', type: pt.STEEL.V, required: true },
  { component: '/mes/manufactures-manage/outbound-state/enclosure-dashboard/index', type: pt.STEEL.V, required: true },
  { component: '/mes/manufactures-manage/warehouse-state/artifact/index', type: pt.STEEL.V, required: true },
  { component: '/mes/manufactures-manage/warehouse-state/enclosure/index', type: pt.STEEL.V, required: true },
  { component: '/mes/manufactures-manage/report/index', type: pt.STEEL.V, required: false },
  { component: '/mes/pack-and-ship/manual-pack/index', type: pt.STEEL.V, required: true },
  { component: '/mes/pack-and-ship/pack-list/index', type: pt.STEEL.V, required: false },
  { component: '/mes/pack-and-ship/ship-list/index', type: pt.STEEL.V, required: false },
  { component: '/mes/pack-and-ship/receipt-status/index', type: pt.STEEL.V, required: false },
  { component: '/mes/pack-and-ship/logistics-list/index', type: pt.STEEL.V, required: false },
  { component: '/mes/pack-and-ship/ship-audit/index', type: pt.STEEL.V, required: false },
  { component: '/mes/craft-manage/artifact-specification-revise/index', type: pt.STEEL.V, required: true },
  { component: '/mes/production-line-wage-statistics/wage-adjust/index', type: pt.STEEL.V, required: true },
  // 计划管理
  { component: '/plan/technical-data-manage/technical-achievement/model/index', type: allPT, required: true },
  { component: '/plan/technical-data-manage/technical-achievement/drawing/index', type: allPT, required: true },
  { component: '/plan/technical-data-manage/technical-achievement/cnc/index', type: allPT, required: true },
  { component: '/plan/technical-data-manage/technical-achievement/xml/index', type: allPT, required: true },
  { component: '/plan/overall-plan/monomer/index', type: pt.STEEL.V, required: true },
  { component: '/plan/project-list/index', type: allPT, required: true },
  { component: '/plan/overall-plan/area/index', type: pt.STEEL.V, required: true },
  { component: '/plan/overall-plan/plan-make/index', type: pt.STEEL.V, required: true },
  { component: '/plan/overall-plan/plan-summary/index', type: pt.STEEL.V, required: true },
  { component: '/plan/overall-plan/plan-progress/index', type: pt.STEEL.V, required: true },
  { component: '/plan/overall-plan/plan-confirm/index', type: pt.STEEL.V, required: true },
  { component: '/plan/technical-manage/artifact-tree/index', type: pt.STEEL.V, required: true },
  { component: '/plan/technical-manage/artifact/index', type: pt.STEEL.V, required: true },
  { component: '/plan/technical-manage/machine-part/index', type: pt.STEEL.V, required: true },
  { component: '/plan/technical-manage/assembly/index', type: pt.STEEL.V, required: true },
  { component: '/plan/technical-manage/enclosure-list/index', type: pt.STEEL.V, required: true },
  { component: '/plan/technical-data-manage/deepen/index', type: allPT, required: true },
  { component: '/plan/technical-data-manage/blueprint/index', type: allPT, required: true },
  { component: '/plan/technical-data-manage/change-file/index', type: allPT, required: true },
  { component: '/plan/technical-data-manage/model/index', type: allPT, required: true },
  { component: '/plan/technical-data-manage/other-file/index', type: allPT, required: true },
  { component: '/plan/technical-data-manage/technical-achievement/process/index', type: allPT, required: false },
  { component: '/plan/technical-manage/summary-list/index', type: allPT, required: true },
  { component: '/plan/technical-manage/steel-statistical/index', type: pt.STEEL.V, required: true },
  { component: '/plan/technical-manage/auxiliary-material/index', type: pt.STEEL.V, required: true },
  { component: '/plan/technical-manage/standard-part/index', type: pt.STEEL.V, required: true },
  { component: '/plan/technical-manage/auxiliary-material-summary/index', type: pt.STEEL.V, required: true },
  { component: '/plan/material-preparation/project-preparation/index', type: allPT, required: false },
  { component: '/contract/contract-change/index', type: allPT, required: false },
  { component: '/contract/contract-record/index', type: allPT, required: false },
  { component: '/contract/contract-ledger/index', type: allPT, required: false },
  { component: '/contract/collection-ledger/index', type: allPT, required: false },
  { component: '/contract/collection-warn/index', type: allPT, required: false },
  { component: '/contract/sales-manage/shipment-tracking/index', type: allPT, required: true },
  { component: '/contract/sales-manage/price-manage/index', type: allPT, required: true },
  { component: '/project-manage/progress-manage/project-progress/index', type: allPT, required: true, businessType: businessTypeEnum.INSTALLATION.V },
  { component: '/project-manage/subcontract-manage/subcontract-plan/index', type: allPT, required: true, businessType: businessTypeEnum.INSTALLATION.V },
  { component: '/project-manage/subcontract-manage/subcontract-progress/index', type: allPT, required: true, businessType: businessTypeEnum.INSTALLATION.V },
  { component: '/project-manage/delivery-manage/homemade-delivery/index', type: allPT, required: true, businessType: businessTypeEnum.INSTALLATION.V },
  { component: '/project-manage/delivery-manage/outsource-delivery/index', type: allPT, required: true, businessType: businessTypeEnum.INSTALLATION.V },
  { component: '/project-manage/delivery-manage/delivery-report/report-list/index', type: allPT, required: true, businessType: businessTypeEnum.INSTALLATION.V },
  { component: '/project-manage/delivery-manage/delivery-report/report-dashboard/index', type: allPT, required: true, businessType: businessTypeEnum.INSTALLATION.V },
  { component: '/project-manage/delivery-manage/delivery-install-list/index', type: allPT, required: true, businessType: businessTypeEnum.INSTALLATION.V },
  { component: '/project-manage/install-manage/install-audit/index', type: allPT, required: true, businessType: businessTypeEnum.INSTALLATION.V },
  { component: '/project-manage/install-manage/install-report/report-list/index', type: allPT, required: true, businessType: businessTypeEnum.INSTALLATION.V },
  { component: '/project-manage/install-manage/install-report/install-dashboard/index', type: allPT, required: true, businessType: businessTypeEnum.INSTALLATION.V },
  { component: '/project-manage/data-manage/image-progress/index', type: allPT, required: true, businessType: businessTypeEnum.INSTALLATION.V },
  { component: '/project-manage/install-manage/handle-install/index', type: allPT, required: true, businessType: businessTypeEnum.INSTALLATION.V },
  { component: '/project-manage/install-config/index', type: allPT, required: true, businessType: businessTypeEnum.INSTALLATION.V },
  { component: '/project-manage/data-manage/construction-log/index', type: allPT, required: true, businessType: businessTypeEnum.INSTALLATION.V },
  { component: '/project-manage/data-manage/construction-data/index', type: allPT, required: true, businessType: businessTypeEnum.INSTALLATION.V },
  // 桥梁
  { component: '/bridge/bridge-plan/box-list/index', type: pt.BRIDGE.V, required: true },
  { component: '/bridge/bridge-plan/cell-part-list/index', type: pt.BRIDGE.V, required: true },
  { component: '/bridge/bridge-plan/list-summary/box-summary/index', type: pt.BRIDGE.V, required: true },
  { component: '/bridge/bridge-plan/list-summary/cell-summary/index', type: pt.BRIDGE.V, required: true },
  { component: '/bridge/bridge-plan/list-summary/part-summary/index', type: pt.BRIDGE.V, required: true },
  { component: '/bridge/work-order-manage/box/index', type: pt.BRIDGE.V, required: false },
  { component: '/bridge/work-order-manage/machine-part/index', type: pt.BRIDGE.V, required: false },
  { component: '/bridge/task-tracking/assistance-operate/productionLine-assistance/index', type: pt.BRIDGE.V, required: false },
  { component: '/bridge/task-tracking/assistance-operate/process-assistance/index', type: pt.BRIDGE.V, required: false },
  { component: '/bridge/production-manage/dashboard/project-dashboard/index', type: pt.BRIDGE.V, required: true },
  { component: '/bridge/production-manage/dashboard/main-material-track/index', type: pt.BRIDGE.V, required: true },
  { component: '/bridge/production-manage/dashboard/production-dashboard/index', type: pt.BRIDGE.V, required: true },
  { component: '/bridge/production-manage/dashboard/assembly-match/index', type: pt.BRIDGE.V, required: true },
  { component: '/bridge/production-manage/dashboard/painting/index', type: pt.BRIDGE.V, required: true },
  { component: '/bridge/QHSE-manage/disclosure/index', type: pt.BRIDGE.V, required: false },
  { component: '/bridge/QHSE-manage/quality-inspection-report/index', type: pt.BRIDGE.V, required: false },
  { component: '/bridge/label-print/box/index', type: pt.BRIDGE.V, required: true },
  { component: '/bridge/label-print/single-element/index', type: pt.BRIDGE.V, required: true },
  { component: '/bridge/label-print/auxiliary-material/index', type: pt.BRIDGE.V, required: true },
  { component: '/bridge/pack-and-ship/manual-pack/index', type: pt.BRIDGE.V, required: true },
  { component: '/bridge/pack-and-ship/pack-list/index', type: pt.BRIDGE.V, required: false },
  { component: '/bridge/pack-and-ship/ship-list/index', type: pt.BRIDGE.V, required: false },
  { component: '/bridge/pack-and-ship/receipt-status/index', type: pt.BRIDGE.V, required: false },
  { component: '/bridge/pack-and-ship/logistics-list/index', type: pt.BRIDGE.V, required: false },
  { component: '/bridge/pack-and-ship/ship-audit/index', type: pt.BRIDGE.V, required: false },
  // { component: '/supply-chain/purchase-order/index', type: allPT, required: false },
  // { component: '/supply-chain/logistics-order/index', type: allPT, required: false }

  // { component: '/wms/inventory-warning/index', type: allPT, required: false },
  // { component: '/wms/material-reject/raw-material/application/index', type: allPT, required: false },
  // { component: '/wms/material-inbound/raw-material/record/index', type: allPT, required: false },
  // { component: '/wms/material-return/raw-material/record/index', type: allPT, required: false },
  // { component: '/wms/material-reject/raw-material/record/index', type: allPT, required: false },
  // { component: '/wms/material-inbound/raw-material/review/index', type: allPT, required: false },
  // { component: '/wms/material-transfer/raw-material/review/index', type: allPT, required: false },
  // { component: '/wms/material-return/raw-material/review/index', type: allPT, required: false },
  // { component: '/wms/material-reject/raw-material/review/index', type: allPT, required: false },
  // { component: '/wms/material-inventory/steel/index', type: allPT, required: false },
  // { component: '/wms/material-inventory/aux-material/index', type: allPT, required: false },
  // { component: '/wms/material-inventory/gas/index', type: allPT, required: false },
  // { component: '/wms/material-outbound/raw-material/review/index', type: allPT, required: false },
  // { component: '/wms/material-outbound/raw-material/record/index', type: allPT, required: false },
  // { component: '/wms/scrap-manage/steel/index', type: allPT, required: false },
  // { component: '/wms/material-freeze/raw-material/record/index', type: allPT, required: false },
  // { component: '/wms/material-freeze/raw-material/unfreeze-record/index', type: allPT, required: false },
  // { component: '/wms/operate-record/raw-material/return-to-party-a/index', type: allPT, required: false },
  // { component: '/wms/operate-record/raw-material/party-a-buy-in/index', type: allPT, required: false },
  // { component: '/wms/report/raw-material/material-inbound-receipt/index', type: allPT, required: false },
  // { component: '/wms/report/raw-material/material-inbound-details/index', type: allPT, required: false },
  // { component: '/wms/report/raw-material/material-outbound-details/index', type: allPT, required: false },
  // { component: '/wms/report/raw-material/material-return-details/index', type: allPT, required: false },
  // { component: '/wms/report/raw-material/send-and-receive-storage/index', type: allPT, required: false }

  // 围护MES
  { component: '/enclosure/enclosure-list/sandwich-board/index', type: pt.STEEL.V | pt.ENCLOSURE.V, required: true },
  { component: '/enclosure/enclosure-list/profiled-plate/index', type: pt.STEEL.V | pt.ENCLOSURE.V, required: true },
  { component: '/enclosure/enclosure-list/pressure-bearing-plate/index', type: pt.STEEL.V | pt.ENCLOSURE.V, required: true },
  { component: '/enclosure/enclosure-list/truss-floor-plate/index', type: pt.STEEL.V | pt.ENCLOSURE.V, required: true },
  { component: '/enclosure/enclosure-list/bending/index', type: pt.STEEL.V | pt.ENCLOSURE.V, required: true },
  { component: '/enclosure/enclosure-list/standard-part/index', type: pt.STEEL.V | pt.ENCLOSURE.V, required: true },
  { component: '/enclosure/production-report/team-production/index', type: pt.STEEL.V | pt.ENCLOSURE.V, required: false },
  { component: '/enclosure/production-report/type-analysis/index', type: pt.STEEL.V | pt.ENCLOSURE.V, required: false },
  { component: '/enclosure/label-print/enclosure/index', type: pt.STEEL.V | pt.ENCLOSURE.V, required: true }
]

/**
 * @description 设置路由的自定义meta属性
 * @param {string} name 路由名称
 * @param {number} productType 所属产品类型
 * @param {number} mode 项目模式
 */
const ENCLOSURE_ALL_BIT =
  ttEnum.PROFILED_PLATE.V | ttEnum.TRUSS_FLOOR_PLATE.V | ttEnum.PRESSURE_BEARING_PLATE.V | ttEnum.SANDWICH_BOARD.V | ttEnum.BENDING.V

export const routerMetaSetting = [
  { name: 'MesSchedulingEnclosure', productType: ENCLOSURE_ALL_BIT },
  { name: 'MesSchedulingPressedPlate', productType: ttEnum.PROFILED_PLATE.V },
  { name: 'MesSchedulingFloorPlate', productType: ttEnum.PRESSURE_BEARING_PLATE.V },
  { name: 'MesSchedulingTrussFloorPlate', productType: ttEnum.TRUSS_FLOOR_PLATE.V },
  { name: 'MesSchedulingSandwichBoard', productType: ttEnum.SANDWICH_BOARD.V },
  { name: 'MesSchedulingFoldingPiece', productType: ttEnum.BENDING.V },
  {
    name: 'MesTaskArtifact',
    productType: ttEnum.STRUCTURE.V,
    mode: projectModeEnum.STRUCTURE.V | projectModeEnum.STRUCTURE_ASSEMBLE.V
  },
  { name: 'MesTaskMachinePart', productType: ttEnum.STRUCTURE.V, mode: projectModeEnum.STRUCTURE_ASSEMBLE.V },
  { name: 'MesTaskEnclosure', productType: ENCLOSURE_ALL_BIT },
  { name: 'MesProductionReportMachinePart', mode: projectModeEnum.STRUCTURE_ASSEMBLE.V },
  { name: 'MesProductionReportAssemble', mode: projectModeEnum.STRUCTURE_ASSEMBLE.V | projectModeEnum.STRUCTURE_ASSEMBLE.V }
]
