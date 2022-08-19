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

export default {
  // 项目制造
  machinePartDetail, // 零件生产详情
  paintingList, // 涂装列表
  structureProjectSummary, // 结构项目汇总
  enclosureProjectSummary, // 围护项目汇总

  // 工单管理
  schedulingDetail, // 工单详情

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
  warehouseStateReport // 入发存报表
}

