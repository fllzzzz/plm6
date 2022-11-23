// 建刚mes
// --------------------------- 公共权限 start ------------------------------

export const commonPM = {}

// --------------------------- 公共权限 end --------------------------------

// ########################################################################

// --------------------------- 我的项目 start ------------------------------

// 我的项目
export const myProjectPM = {
  get: ['my_project:get'], // 我的项目
  statistics: ['my_project:statistics'], // 项目统计
  print: ['my_project:print'] // 打印
}

// --------------------------- 我的项目 end --------------------------------

// ########################################################################

// --------------------------- 变更管理 start ------------------------------

// 变更管理/变更列表
export const changeListPM = {
  get: ['mes_change_list:get'], // 变更列表
  save: ['mes_change_list:save'], // 处理变更
  detail: ['mes_change_list:detail'] // 查看详情
}

// 变更管理/多余列表
export const surplusListPM = {
  get: ['mes_surplus_list:get'], // 多余列表
  secondUse: ['mes_surplus_list:secondUse'], // 二次利用
  scrap: ['mes_surplus_list:scrap'] // 报废
}

// --------------------------- 变更管理 end --------------------------------

// ########################################################################

// --------------------------- 生产排产 start ------------------------------

// 生产排产/构件排产
export const artifactSchedulingPM = {
  get: ['mes_scheduling_artifact:get'], // 列表
  save: ['mes_scheduling_artifact:save'], // 构件排产保存
  recordGet: ['mes_scheduling_artifact_record:get'], // 获取构件排产记录
  recordEdit: ['mes_scheduling_artifact_record:edit'], // 构件排产记录编辑
  recordDel: ['mes_scheduling_artifact_record:del'], // 构件排产记录删除
  assembleGet: ['mes_scheduling_assemble:get'], // 获取部件排产信息
  assembleSave: ['mes_scheduling_assemble:save'] // 部件排产保存
}

// 生产排产/零件排产/零件排产
export const machinePartSchedulingPM = {
  get: ['mes_scheduling_machine_part:get'], // 列表
  save: ['mes_scheduling_machine_part:save'] // 零件排产保存
}

// 生产排产/零件排产/预览记录
export const machinePartSchedulingRecordPM = {
  get: ['mes_scheduling_machine_part_record:get'], // 列表
  del: ['mes_scheduling_machine_part_record:del'], // 零件排产记录删除
  detail: ['mes_scheduling_machine_part_record:detail'], // 套料明细
  save: ['mes_scheduling_machine_part_record:save'] // 套料下发
}

// 生产排产/零件排产/套料成果
export const machinePartSchedulingNestingResultPM = {
  get: ['mes_scheduling_machine_part_nesting_result:get'], // 列表
  del: ['mes_scheduling_machine_part_nesting_result:del'], // 删除
  save: ['mes_scheduling_machine_part_nesting_result:save'] // 任务下发
}

// --------------------------- 生产排产 end --------------------------------

// ########################################################################

// --------------------------- 生产管理 start ------------------------------

// 生产管理/生产报表/零件报表
export const machinePartProductionReportPM = {
  get: ['mes_production_machine_part_report:get'], // 零件报表列表
  print: ['mes_production_machine_part_report:print'] // 零件报表打印
}

// 生产管理/生产报表/部件报表
export const assembleProductionReportPM = {
  get: ['mes_production_assemble_report:get'], // 部件报表列表
  print: ['mes_production_assemble_report:print'] // 部件报表打印
}

// 生产管理/生产报表/结构报表
export const artifactProductionReportPM = {
  get: ['mes_production_artifact_report:get'], // 结构报表列表
  print: ['mes_production_artifact_report:print'] // 结构报表打印
}

// 生产管理/生产报表/围护报表
export const enclosureProductionReportPM = {
  get: ['mes_production_enclosure_report:get'], // 围护报表列表
  print: ['mes_production_enclosure_report:print'] // 围护报表打印
}

// 生产管理/生产分析/在制品统计
export const productionStatisticsPM = {
  get: ['mes_production_statistics:get'], // 在制品统计列表
  detail: ['mes_production_statistics:detail'], // 在制品统计详情
  print: ['mes_production_statistics:print'], // 在制品统计打印
  printDetail: ['mes_production_statistics:printDetail'] // 在制品统计详情打印
}

// 生产管理/生产分析/迟滞报表
export const analysisDelayReportPM = {
  get: ['mes_production_analysis_delay_report:get'], // 迟滞报表列表
  detail: ['mes_production_analysis_delay_report:detail'], // 迟滞报表详情
  printDetail: ['mes_production_analysis_delay_report:printDetail'] // 迟滞报表详情打印
}

// --------------------------- 生产管理 end --------------------------------

// ########################################################################

// --------------------------- 项目制造 start ------------------------------

// 项目制造/项目看板
export const projectDashboardPM = {
  get: ['mes_project_dashboard_product:get'], // 项目看板查看
  shipGet: ['mes_project_dashboard_product_ship:get'], // 发运车次
  qcGet: ['mes_project_dashboard_product_qc:get'] // qc事件
}

// 项目制造/主材跟踪
export const mainMaterialTrackPM = {
  get: ['mes_main_material_track:get'] // 主材跟踪
}

// 项目制造/结构看板
export const artifactProductionDashboardPM = {
  get: ['mes_artifact_production_dashboard:get'] // 结构看板列表
}

// 项目制造/围护看板
export const enclosureProductionDashboardPM = {
  get: ['mes_enclosure_production_dashboard:get'] // 围护看板列表
}

// 项目制造/项目报表
export const projectReportDashboardPM = {
  get: ['mes_project_report:get'], // 项目报表列表
  print: ['mes_project_report:print'] // 项目报表打印
}

// 项目制造/零件齐套
export const assemblyMatchDashboardPM = {
  get: ['mes_assembly_match:get'], // 零件齐套列表
  print: ['mes_assembly_match:print'] // 零件齐套详情打印
}

// 项目制造/涂装计算
export const paintingDashboardPM = {
  get: ['mes_painting:get'], // 涂装计算列表
  edit: ['mes_painting:edit'], // 涂装计算编辑
  editArea: ['mes_painting:editArea'], // 涂装计算面积编辑
  print: ['mes_painting:print'] // 涂装计算列表打印
}

// --------------------------- 项目制造 end --------------------------------

// ########################################################################

// --------------------------- 班组报表 start ------------------------------

// 班组报表/结构班组进度
export const artifactTeamReportPM = {
  get: ['mes_team_report_artifact:get'], // 结构班组进度列表
  print: ['mes_team_report_artifact:print'], // 结构班组进度列表打印
  detail: ['mes_team_report_artifact:detail'], // 结构班组进度-全景看板
  processDetail: ['mes_team_report_artifact_process:detail'], // 结构班组进度-工序详情
  printDetail: ['mes_team_report_artifact_process:printDetail'] // 结构班组进度-工序详情打印
}

// 班组报表/围护班组进度
export const enclosureTeamReportPM = {
  get: ['mes_team_report_enclosure:get'], // 围护班组进度列表
  print: ['mes_team_report_enclosure:print'], // 围护班组进度列表打印
  detail: ['mes_team_report_enclosure:detail'] // 围护班组进度-全景看板
}

// 班组报表/结构班组工资
export const artifactTeamWagePM = {
  get: ['mes_team_wage_artifact:get'], // 结构班组工资列表
  print: ['mes_team_wage_artifact:print'], // 结构班组工资列表打印
  detail: ['mes_team_wage_artifact:detail'], // 结构班组工资查看详情
  printDetail: ['mes_team_wage_artifact:printDetail'] // 结构班组工资查看详情打印
}

// 班组报表/围护班组工资
export const enclosureTeamWagePM = {
  get: ['mes_team_wage_enclosure:get'], // 围护班组工资列表
  print: ['mes_team_wage_enclosure:print'], // 围护班组工资列表打印
  detail: ['mes_team_wage_enclosure:detail'], // 围护班组工资查看详情
  printDetail: ['mes_team_wage_enclosure:printDetail'] // 围护班组工资查看详情打印
}

// 班组报表/编内-工价调整
export const inStaffWagesAdjustPM = {
  get: ['mes_in_staff_wages_adjust:get'], // 工价调整列表
  edit: ['mes_in_staff_wages_adjust:edit'], // 工价调整编辑
  audit: ['mes_in_staff_wages_adjust:audit'] // 工价调整审核
}

// 班组报表/编外-工价调整
export const offStaffWagesAdjustPM = {
  get: ['mes_off_staff_wages_adjust:get'], // 工价调整列表
  edit: ['mes_off_staff_wages_adjust:edit'], // 工价调整编辑
  audit: ['mes_off_staff_wages_adjust:audit'] // 工价调整审核
}

// // 班组报表/编内-计件制
// export const inStaffPieceworkSystemPM = {
//   get: ['mes_in_staff_piecework:get'], // 编内-计件制列表
//   detail: ['mes_in_staff_piecework:detail'], // 编内-计件制-详情
//   summaryDetail: ['mes_in_staff_piecework_summary:detail'] // 编内-计件制-汇总详情
// }

// // 班组报表/编外-工价
// export const offStaffWagesConfigPM = {
//   get: ['mes_off_staff_wages:get'], // 编外-工价列表
//   edit: ['mes_off_staff_wages:edit'], // 编外-工价编辑
//   audit: ['mes_off_staff_wages:audit'] // 编外-工价审核
// }

// // 班组报表/编外-工资结算
// export const offStaffSettlementPM = {
//   get: ['mes_off_staff_settlement:get'], // 编外-工资结算
//   detail: ['mes_off_staff_settlement:detail'] // 编外-工资结算
// }

// // 班组报表/工价调整
// export const wagesAdjustPM = {
//   get: ['mes_wages_adjust:get'], // 工价调整列表
//   edit: ['mes_wages_adjust:edit'], // 工价调整编辑
//   audit: ['mes_wages_adjust:audit'] // 工价调整审核
// }
// --------------------------- 班组报表 end --------------------------------

// ########################################################################

// --------------------------- 质安管理 start --------------------------------

// 质安管理/质检报表
export const qualityInspectionReportPM = {
  get: ['mes_quality_inspection_report:get'], // 质检报表
  detail: ['mes_quality_inspection_report:detail'] // 质检报表详情
}

// 质安管理/生产线报表
export const qhseProductionLineReportPM = {
  get: ['mes_production_line_report:get'], // 生产线报表
  detail: ['mes_production_line_report:detail'], // 生产线报表详情
  print: ['mes_production_line_report_detail:print'] // 生产线报表详情
}

// 质安管理/问题曝光
export const qhseDisclosurePM = {
  get: ['mes_qhse_disclosure:get'] // 问题曝光列表
}

// --------------------------- 质安管理 end --------------------------------

// ########################################################################

// --------------------------- 产品标签 start --------------------------------

// 产品标签/构件
export const artifactLabelPM = {
  get: ['mes_label_artifact:get'] // 构件列表
}

// 产品标签/围护
export const enclosureLabelPM = {
  get: ['mes_label_enclosure:get'] // 围护列表
}

// 产品标签/配套件
export const auxiliaryMaterialLabelPM = {
  get: ['mes_label_auxiliary_material:get'] // 围护列表
}

// --------------------------- 产品标签 end --------------------------------

// ########################################################################

// --------------------------- 制成品管理 start --------------------------------

// 制成品管理/入库看板-构件
export const artifactInboundDashboardPM = {
  get: ['mes_state_artifact_inbound_dashboard:get'] // 构件列表
}

// 制成品管理/入库看板-围护
export const enclosureInboundDashboardPM = {
  get: ['mes_state_enclosure_inbound_dashboard:get'] // 围护列表
}

// 制成品管理/出库看板-构件
export const artifactOutboundDashboardPM = {
  get: ['mes_state_artifact_outbound_dashboard:get'] // 构件列表
}

// 制成品管理/出库看板-围护
export const enclosureOutboundDashboardPM = {
  get: ['mes_state_enclosure_outbound_dashboard:get'] // 围护列表
}

// 制成品管理/出入库状态-构件
export const artifactWarehousePM = {
  get: ['mes_state_artifact_warehouse:get'], // 构件列表
  print: ['mes_state_artifact_warehouse:print'] // 打印
}

// 制成品管理/出入库状态-围护
export const enclosureWarehousePM = {
  get: ['mes_state_enclosure_warehouse:get'], // 围护列表
  print: ['mes_state_enclosure_warehouse:print'] // 打印
}

// 制成品管理/入发存报表
export const reportWarehouseStatePM = {
  get: ['mes_state_report_warehouse:get'], // 入发存列表
  print: ['mes_state_report_warehouse:print'], // 入发存列表打印
  detail: ['mes_state_report_warehouse:detail'] // 入发存详情
}

// --------------------------- 制成品管理 end --------------------------------

// ########################################################################

// --------------------------- 工艺管理 start --------------------------------

// 工艺管理/构件规格修正
export const artifactSpecificationRevisePM = {
  get: ['artifact-specification-revise:get'], // 列表
  edit: ['artifact-specification-revise:edit'] // 构件规格修改
}

// --------------------------- 工艺管理 end --------------------------------
// ########################################################################

// --------------------------- 生产订单 start --------------------------------

// 生产订单/生产排期
export const mesProductionOrderPM = {
  get: ['mes_production_order:get'], // 列表
  detail: ['mes_production_order:detail'], // 查看计划详情
  edit: ['mes_production_order:edit'], // 排产计划修改
  detailList: ['mes_production_order:detailList'] // 清单详情
}

// --------------------------- 生产订单 end --------------------------------

// ########################################################################

// --------------------------- 发运管理 start --------------------------------

// 发运管理/发运管理列表
export const mesShipSummaryPM = {
  get: ['mes_ship_summary:get'], // 列表
  print: ['mes_ship_summary:print'] // 发运详情打印
}

// 发运管理/手工打包:手工打包
export const manualPackPM = {
  pack: ['mes_manual_pack:pack'] // 手工打包
}

// 发运管理/手工打包:结构
export const artifactManualPackPM = {
  get: ['mes_manual_pack_artifact:get'], // 结构打包列表
  pack: manualPackPM.pack // 手工打包
}

// 发运管理/手工打包:围护
export const enclosureManualPackPM = {
  get: ['mes_manual_pack_enclosure:get'], // 围护打包列表
  pack: manualPackPM.pack // 手工打包
}

// 发运管理/打包记录
export const mesPackPM = {
  get: ['mes_pack:get'], // 打包列表
  detail: ['mes_pack:detail'], // 查看打包清单
  pack: manualPackPM.pack, // 手工打包
  edit: ['mes_pack:edit'], // 编辑打包清单
  del: ['mes_pack:del'], // 删除打包清单
  print: ['mes_pack:print'], // 查看及打印标签
  printRecords: ['mes_pack:printRecords'], // 查看打印记录
  printPackList: ['mes_pack:printPackList'] // 打印包单清单
}

// 发运管理/发运记录
export const mesShipPM = {
  get: ['mes_ship:get'], // 发运列表
  detail: ['mes_ship:detail'], // 查看车次详情
  print: ['mes_ship:print'], // 打印车次汇总
  detailPrint: ['mes_ship:detailPrint'] // 打印车次详情
}

// 发运管理/收货状态
export const receiptStatusPM = {
  get: ['mes_receipt_status:get'], // 收货列表
  detail: ['mes_receipt_status:detail'], // 查看收货详情
  print: ['mes_receipt_status:print'], // 打印收货汇总
  detailPrint: ['mes_receipt_status:detailPrint'], // 打印收货详情
  cancelDelivery: ['mes_receipt_status:cancelDelivery'], // 取消发运
  confirmDelivery: ['mes_receipt_status:confirmDelivery'] // 确定签收
}

// 发运管理/物流记录
export const logisticsPM = {
  get: ['mes_logistics:get'], // 物流列表
  edit: ['mes_logistics:edit'], // 录入物流信息
  print: ['mes_logistics:print'] // 打印物流汇总
}

// 发运管理/发运审核
export const shipAuditPM = {
  get: ['mes_ship_audit:get'], // 审核列表
  detail: ['mes_ship_audit:detail'], // 装车明细
  print: ['mes_ship_audit:print'], // 打印发运审核
  download: ['mes_ship_audit:download'], // 下载发运详情
  audit: ['mes_ship_audit:audit'] // 发运审核
}

// 发运管理/制成品入发存
export const mesProductSendReceiveStoragePM = {
  get: ['mes_product-send-receive-storage:get'], // 列表
  print: ['mes_product-send-receive-storage:print'], // 制成品入发存打印
  detail: ['mes_product-send-receive-storage:detail'], // 制成品入发存详情
  detailPrint: ['mes_product-send-receive-storage:detailPrint'] // 制成品入发存详情打印
}
// --------------------------- 发运管理 end --------------------------------
