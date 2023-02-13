import request from '@/utils/request'

// 打包与发运
/**
 * 打包清单
 */
export function packingList(id) {
  return request({
    url: `api/mes/building/package/${id}/print`,
    method: 'get'
  })
}

/**
 * 发运汇总
 */
export function shipmentSummary(params) {
  return request({
    url: `api/mes/building/cargo/ship/print`,
    method: 'get',
    params
  })
}

/**
   * 发运详情
   */
export function shipmentDetail(id) {
  return request({
    url: `api/mes/building/cargo/ship/${id}/print`,
    method: 'get'
  })
}

/**
 * 收货状态汇总
 */
export function receiptStatusSummary(params) {
  return request({
    url: `api/mes/building/cargo/receipt/print`,
    method: 'get',
    params
  })
}

/**
 * 发运详情
 */
export function shippingList(id) {
  return request({
    url: `api/mes/building/cargo/receipt/${id}/print`,
    method: 'get'
  })
}

/**
 * 发运审核
 */
export function shipmentAudit(params) {
  return request({
    url: `api/mes/building/cargo/review/print`,
    method: 'get',
    params
  })
}

/**
 * 物流汇总
 */
export function logisticsSummary(params) {
  return request({
    url: `api/mes/building/cargo/logistics/print`,
    method: 'get',
    params
  })
}

// 制成品管理
/**
 * 结构出入库状态
 * @export
 * @param {*} monomerId|required 单体id
 * @param {*} monomerId|required 区域id
 * @param {*} factoryId 工厂id
 * @returns
 */
export function warehouseStateStructure({ monomerId, areaId, factoryId }) {
  return request({
    url: 'api/mes/building/warehouse/artifact/print',
    method: 'get',
    params: { monomerId, areaId, factoryId }
  })
}

/**
 * 围护出入库状态
 * @export
 * @param {*} monomerId|required 单体id
 * @param {*} monomerId|required 区域id
 * @param {*} factoryId 工厂id
 * @returns
 */
export function warehouseStateEnclosure({ monomerId, areaId, factoryId }) {
  return request({
    url: 'api/mes/building/warehouse/enclosure/print',
    method: 'get',
    params: { monomerId, areaId, factoryId }
  })
}

/**
 * 入发存报表
 */
export function warehouseStateReport(params) {
  return request({
    url: `/api/mes/building/warehouse/report/print`,
    method: 'get',
    params
  })
}

// 班组报表
/**
 * 围护生产线报表
 */
export function enclosureProductionLine(params) {
  return request({
    url: `/api/mes/building/team_form/enclosure/details/print`,
    method: 'get',
    params
  })
}

/**
 * 结构生产线报表
 */
export function structureProductionLine(params) {
  return request({
    url: `/api/mes/building/team_form/artifact_assemble/process/print`,
    method: 'get',
    params
  })
}

/**
 * 结构生产线报表
 */
export function structureProcess(params) {
  return request({
    url: `/api/mes/building/team_form/artifact_assemble/details/print`,
    method: 'get',
    params
  })
}

// /**
//  * 计件制报表
//  */
// export function piecework(params) {
//   return request({
//     url: `/api/mes/building/wages/in_staff/reckon/print`,
//     method: 'get',
//     params
//   })
// }

// /**
//  * 计件汇总报表（项目、班组汇总）
//  */
// export function pieceworkSummary(params) {
//   return request({
//     url: `/api/mes/building/wages/in_staff/reckon/details/print`,
//     method: 'get',
//     params
//   })
// }

/**
 * 班组工资
 */
export function teamWage(params) {
  return request({
    url: `/api/mes/building/wages/reckon/print`,
    method: 'get',
    params
  })
}

/**
 * 班组工资详情
 */
export function teamWageDetail(params) {
  return request({
    url: `/api/mes/building/wages/reckon/details/print`,
    method: 'get',
    params
  })
}

/**
 * 生产报表
 */
export function productionReport(params) {
  return request({
    url: `/api/mes/building/production_statements/print`,
    method: 'get',
    params
  })
}

/**
 * 在制品统计
 * @export
 * @param {*} productType|required 产品类型
 * @param {*} dateTime|required 统计日期
 * @param {*} monomerId 单体id
 * @param {*} projectId 项目id
 * @returns
 */
export function productionStatistics(params) {
  return request({
    url: `/api/mes/building/analysis/production_summary/group/print`,
    method: 'get',
    params
  })
}

/**
 * 在制品统计明细
 * @export
 * @param {*} productType|required 产品类型
 * @param {*} dateTime|required 统计日期
 * @param {*} monomerId 单体id
 * @param {*} projectId 项目id
 * @returns
 */
export function productionStatisticsIn(params) {
  return request({
    url: `/api/mes/building/analysis/production_summary/in_production/details/print`,
    method: 'get',
    params
  })
}

/**
 * 在制品统计明细
 * @export
 * @param {*} productType|required 产品类型
 * @param {*} dateTime|required 统计日期
 * @param {*} monomerId 单体id
 * @param {*} projectId 项目id
 * @returns
 */
export function productionStatisticsUn(params) {
  return request({
    url: `/api/mes/building/analysis/production_summary/un_production/details/print`,
    method: 'get',
    params
  })
}

/**
 * 在制品统计明细
 * @export
 * @param {*} productType|required 产品类型
 * @param {*} dateTime|required 统计日期
 * @param {*} monomerId 单体id
 * @param {*} projectId 项目id
 * @returns
 */
export function productionStatisticsComplete(params) {
  return request({
    url: `/api/mes/building/analysis/production_summary/complete/details/print`,
    method: 'get',
    params
  })
}

/**
 * 未完成清单
 */
export function unfinishedList(params) {
  return request({
    url: `/api/mes/building/analysis/hysteresis/details/print`,
    method: 'get',
    params
  })
}

/**
 * 工单详情
 */
export function schedulingDetail(params) {
  return request({
    url: `/api/mes/building/scheduling/print`,
    method: 'get',
    params
  })
}

/**
 * 工单管理：构件生产任务单
 */
export function productionTaskOrder(params) {
  return request({
    url: `/api/mes/building/task/process/product/task/list/print`,
    method: 'get',
    params
  })
}

/**
 * 工单管理：部件生产任务单
 */
export function assembleProductionTaskOrder(params) {
  return request({
    url: `/api/mes/building/task/process/product/task/list/print`,
    method: 'get',
    params
  })
}

/**
 * 工单管理：部件套料清单
 */
export function assembleNestingOrder(params) {
  return request({
    url: `/api/mes/building/task/process/nesting/task/list/print`,
    method: 'get',
    params
  })
}

/**
 * 工单管理：钻孔生产任务单
 */
export function drillProductionTaskOrder(params) {
  return request({
    url: `/api/mes/building/task/order/drilling/print`,
    method: 'get',
    params
  })
}

// /**
//  * 编外工资汇总
//  */
// export function wageSummary(params) {
//   return request({
//     url: `/api/mes/building/wages/out_staff/summary/print`,
//     method: 'get',
//     params
//   })
// }

// /**
//  * 编外工资详情
//  */
// export function wageDetail(params) {
//   return request({
//     url: `/api/mes/building/wages/out_staff/details/print`,
//     method: 'get',
//     params
//   })
// }

/**
 * 零件生产详情
 */
export function machinePartDetail(params) {
  return request({
    url: `/api/mes/building/kanban/assemble_matching/detail/print`,
    method: 'get',
    params
  })
}

/**
 * 零部件生产清单详情
 */
export function machinePartList(params) {
  return request({
    url: `/api/mes/building/kanban/assemble_matching/area/product/print`,
    method: 'get',
    params
  })
}

/**
 * 零件生产详情
 */
export function paintingList(params) {
  return request({
    url: `/api/mes/building/kanban/painting/print`,
    method: 'get',
    params
  })
}

/**
 * 结构项目汇总
 */
export function structureProjectSummary(params) {
  return request({
    url: `/api/mes/building/kanban/form/artifact/print`,
    method: 'get',
    params
  })
}

/**
 * 围护项目汇总
 */
export function enclosureProjectSummary(params) {
  return request({
    url: `/api/mes/building/kanban/form/enclosure/print`,
    method: 'get',
    params
  })
}

/**
 * 生产线质检报表
 */
export function qhseProductionLineReport(params) {
  return request({
    url: `/api/mes/building/report/inspection/summary/review/print`,
    method: 'get',
    params
  })
}

// 构件分类清单明细
export function artifactClassList(params) {
  return request({
    url: `/api/mes/building/scheduling/area/listArtifact/print`,
    method: 'get',
    params
  })
}

// 部件分类清单明细
export function assembleClassList(params) {
  return request({
    url: `/api/mes/building/scheduling/area/listAssemble/print`,
    method: 'get',
    params
  })
}

// 零件分类清单明细
export function machinePartClassList(params) {
  return request({
    url: `/api/mes/building/scheduling/area/listMachinePart/print`,
    method: 'get',
    params
  })
}

// 发运管理-项目发运详情
export function mesProjectShipDetail(params) {
  return request({
    url: `/api/mes/building/cargo/project/cargoList/details/print`,
    method: 'get',
    params
  })
}

// 工厂报表-车间报表详情
export function factoryWorkshopReport(params) {
  return request({
    url: `/api/mes/building/workshop/artifact/summary/print`,
    method: 'get',
    params
  })
}

// 任务跟踪-工单跟踪报表详情
export function workOrderTrackingList(params) {
  return request({
    url: `/api/mes/building/task/process/product/print`,
    method: 'get',
    params
  })
}

// 任务跟踪-月度任务跟踪报表详情
export function monthlyTaskList(params) {
  return request({
    url: `/api/mes/building/task/tracking/month/project/print`,
    method: 'get',
    params
  })
}
// 任务跟踪-产线跟踪报表详情
export function productionLineList(params) {
  return request({
    url: `/api/mes/building/task/tracking/productionLine/detail/print`,
    method: 'get',
    params
  })
}
// 任务跟踪-工序呆滞报表详情
export function processList(params) {
  return request({
    url: `/api/mes/building/task/process/dull/process/detail/print`,
    method: 'get',
    params
  })
}

// 项目制造：项目总览
export function projectOverviewList(params) {
  return request({
    url: `/api/mes/building/kanban/project/process/summary/detail/print`,
    method: 'get',
    params
  })
}

// 制成品入发存
export function productSendReceiveStorage(params) {
  return request({
    url: `/api/mes/building/warehouse/finish-product/print`,
    method: 'get',
    params
  })
}

// 制成品入发存详情
export function productSendReceiveStorageDetail(params) {
  return request({
    url: `/api/mes/building/warehouse/finish-product/detail/print`,
    method: 'get',
    params
  })
}

// 主材跟踪汇总
export function mainMaterialTrackSummary(params) {
  return request({
    module: 'mes',
    url: 'kanban/tracking/print',
    method: 'get',
    params
  })
}

/**
 *
 * 主材跟踪-钢材库领用详情
 */
export function mainMaterialTrackUseRecord(params) {
  return request({
    module: 'mes',
    url: 'kanban/tracking/use/print',
    method: 'get',
    params
  })
}

/**
 *
 * 主材跟踪-钢材库存量详情
 */
export function mainMaterialTrackStock(params) {
  return request({
    module: 'mes',
    url: 'kanban/tracking/inventory/print',
    method: 'get',
    params
  })
}
/**
 *
 * 栓钉套筒统计打印
 */
export function studSleeveStatisticsList(params) {
  return request({
    url: `/api/mes/building/price/stud/sleeve/print`,
    method: 'get',
    params
  })
}

export default {
  // 项目制造
  machinePartDetail, // 零件生产详情
  machinePartList, // 零部件生产清单
  paintingList, // 涂装列表
  structureProjectSummary, // 结构项目汇总
  enclosureProjectSummary, // 围护项目汇总
  projectOverviewList, // 项目总览工序清单

  // 工单管理
  schedulingDetail, // 工单详情
  productionTaskOrder, // 工单管理：构件生产任务单
  assembleNestingOrder, // // 工单管理：部件套料清单
  assembleProductionTaskOrder, // 工单管理：部件生产任务单
  drillProductionTaskOrder, // 工单管理： 钻孔生产任务单
  // 生产报表
  productionReport, // 生产报表
  productionStatistics, // 在制品统计
  productionStatisticsIn, // 在制品统计明细-在制品
  productionStatisticsUn, // 在制品统计明细-未生产
  productionStatisticsComplete, // 在制品统计明细-完成品
  unfinishedList, // 未完成清单

  qhseProductionLineReport, // 生产线质检报表

  // 班组报表
  enclosureProductionLine, // 围护生产线报表
  structureProductionLine, // 结构生产线报表
  structureProcess, // 结构工序报表

  teamWage, // 班组工资
  teamWageDetail, // 班组工资详情

  // 打包与发运
  packingList, // 打包清单
  shipmentSummary, // 发运汇总
  shipmentDetail, // 发运详情
  receiptStatusSummary, // 收货状态汇总
  shippingList, // 发货清单
  shipmentAudit, // 发运审核
  logisticsSummary, // 物流汇总

  // 制成品管理
  warehouseStateStructure, // 结构出入库状态
  warehouseStateEnclosure, // 围护出入库状态
  warehouseStateReport, // 入发存报表

  // 生产订单
  artifactClassList, // 构件分类清单明细
  assembleClassList, // 部件分类清单明细
  machinePartClassList, // 零件分类清单明细

  // 发运管理
  mesProjectShipDetail, // 项目发运详情

  // 工厂报表-车间报表
  factoryWorkshopReport, // 车间报表详情

  // 任务跟踪
  workOrderTrackingList, // 工单跟踪报表详情
  monthlyTaskList, // 月度任务跟踪清单详情
  productionLineList, // 产线跟踪清单详情
  processList, // 工序呆滞清单详情

  // 发运管理
  productSendReceiveStorage, // 制成品入发存
  productSendReceiveStorageDetail, // 制成品入发存详情

  // 项目报表
  mainMaterialTrackSummary, // 主材跟踪汇总
  mainMaterialTrackUseRecord, // 主材跟踪-钢材领用记录
  mainMaterialTrackStock, // 主材跟踪-库存明细

  // 产线工资统计
  studSleeveStatisticsList // 栓钉套筒统计
}

