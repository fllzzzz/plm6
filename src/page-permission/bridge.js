// 桥梁bridge
// --------------------------- 公共权限 start ------------------------------

export const commonPM = {}

// --------------------------- 桥梁-我的项目 start ------------------------------

// 我的项目
export const bridgeMyProjectPM = {
  get: ['my_project:get'], // 我的项目
  statistics: ['my_project:statistics'], // 项目统计
  print: ['my_project:print'] // 打印
}

// --------------------------- 桥梁-我的项目 end --------------------------------

// --------------------------- 桥梁-计划管理 start ------------------------------

// 桥梁-计划管理/单体列表
export const bridgeMonomerListPM = {
  get: ['plan_monomer_manage:get'], // 单体列表
  add: ['plan_monomer_manage:add'], // 新增单体
  edit: ['plan_monomer_manage:edit'], // 修改单体
  del: ['plan_monomer_manage:del'], // 删除单体
  productTypeEdit: ['plan_monomer_manage:productTypeEdit'], // 项目内容修改
  productTypeDel: ['plan_monomer_manage:productTypeDel'] // 项目内容删除
}

// 桥梁-计划管理/区域列表
export const bridgeAreaListPM = {
  get: ['plan_area_manage:get'], // 区域列表
  add: ['plan_area_manage:add'], // 新增区域
  edit: ['plan_area_manage:edit'], // 修改区域
  del: ['plan_area_manage:del'], // 删除区域
  planList: ['plan_area_manage:planList'] // 办理计划
  // print: ['plan_area_manage:print'] // 打印下载
}

// 桥梁-计划管理/计划列表
export const bridgePlanMakeListPM = {
  get: ['plan_make_manage:get'], // 计划列表
  edit: ['plan_make_manage:edit'], // 修改计划
  print: ['plan_make_manage:print'] // 打印下载
}

// 桥梁-计划管理/计划跟踪
export const bridePlanProgressListPM = {
  get: ['plan_progress:get'] // 计划跟踪列表
}

// 桥梁-计划管理/计划确认
export const bridgePlanConfirmListPM = {
  get: ['plan_confirm:get'], // 计划确认列表
  edit: ['plan_confirm:edit'] // 计划确认
}

// 计划管理/工单汇总
export const bridgePlanSummaryListPM = {
  get: ['plan_summary:get']// 工单汇总列表
}
// --------------------------- 计划管理 end --------------------------------

// ########################################################################

// --------------------------- 技术管理 start ------------------------------

// 桥梁-技术管理/分段清单
export const boxCellPM = {
  get: ['bridge_box_cell_list:get'], // 分段清单列表
  del: ['bridge_box_cell_list:del'], // 删除分段单元
  download: ['bridge_box_cell_list:download'], // 下载分段单元
  import: ['bridge_box_cell_list:import'], // 导入分段单元
  templateDownLoad: ['bridge_box_cell_list:templateDownLoad'] // 下载清单模板
}

// 桥梁-技术管理/单元列表
export const cellListPM = {
  get: ['bridge_cell_list:get'] // 单元列表
}

// 桥梁-技术管理/清单汇总/分段清单汇总
export const boxSummaryListPM = {
  get: ['box_summary_list:get'] // 分段清单汇总列表
}

// 桥梁-技术管理/清单汇总/单元清单汇总
export const cellSummaryListPM = {
  get: ['cell_summary_list:get'] // 单元清单汇总列表
}

// 桥梁-技术管理/清单汇总/零件清单汇总
export const partSummaryListPM = {
  get: ['part_summary_list:get'] // 零件清单汇总列表
}

// 桥梁-技术管理/技术成果/蓝图列表
export const bridgeBlueprintListPM = {
  get: ['plan_blueprint:get'], // 蓝图列表
  edit: ['plan_blueprint:edit'], // 修改蓝图
  download: ['plan_blueprint:download'], // 下载
  import: ['plan_blueprint:import'] // 导入
}

// 桥梁-技术管理/技术成果/变更文件列表
export const bridgeChangeFileListPM = {
  get: ['plan_change_file:get'], // 变更文件列表
  edit: ['plan_change_file:edit'], // 修改变更文件
  download: ['plan_change_file:download'], // 下载
  import: ['plan_change_file:import'] // 导入
}

// 桥梁-技术成果/技术成果/模型文件管理
export const bridgeModelFileListPM = {
  get: ['plan_model_file:get'], // 列表
  del: ['plan_model_file:del'], // 删除
  detail: ['plan_model_file:detail'], // 详情
  import: ['plan_model_file:import'], // 导入
  integration: ['plan_model_file:integration'], // 模型集成
  integrationDel: ['plan_model_file:integrationDel'] // 删除模型集成
}

// 技术成果/图纸文件管理
export const bridgeDrawingFileListPM = {
  get: ['plan_drawing_file:get'], // 列表
  del: ['plan_drawing_file:del'], // 删除
  detail: ['plan_drawing_file:detail'], // 详情
  import: ['plan_drawing_file:import'] // 导入
}

// 技术成果/数控文件管理
export const bridgeCncFileListPM = {
  get: ['plan_cnc_file:get'], // 列表
  del: ['plan_cnc_file:del'], // 删除
  detail: ['plan_cnc_file:detail'], // 详情
  import: ['plan_cnc_file:import'] // 导入
}

// 技术成果/XML文件管理
export const bridgeXmlFileListPM = {
  get: ['plan_xml_file:get'], // 列表
  del: ['plan_xml_file:del'], // 删除
  detail: ['plan_xml_file:detail'], // 详情
  import: ['plan_xml_file:import'] // 导入
}

// --------------------------- 技术管理 end --------------------------------

// ########################################################################

// --------------------------- 生产订单 start --------------------------------

// 生产订单/生产排期
export const bridgeProductionOrderPM = {
  get: ['mes_bridge_production_order:get'], // 列表
  detail: ['mes_bridge_production_order:detail'], // 查看计划详情
  edit: ['mes_bridge_production_order:edit'], // 排产计划修改
  detailList: ['mes_bridge_production_order:detailList'] // 清单详情
}

// --------------------------- 生产订单 end --------------------------------

// ########################################################################

// --------------------------- 生产排产 start ------------------------------

// 生产排产/分段排产
export const boxSchedulingPM = {
  get: ['mes_bridge_scheduling_box:get'], // 列表
  save: ['mes_bridge_scheduling_box:save'], // 分段排产保存
  recordGet: ['mes_bridge_scheduling_box_record:get'], // 获取分段排产记录
  recordEdit: ['mes_bridge_scheduling_box_record:edit'], // 分段排产记录编辑
  recordDel: ['mes_bridge_scheduling_box_record:del'], // 分段排产记录删除
  elementGet: ['mes_bridge_scheduling_element:get'], // 获取单元件排产信息
  elementSave: ['mes_bridge_scheduling_element:save'] // 单元件排产保存
}

// 生产排产/零件排产/零件排产
export const machinePartSchedulingPM = {
  get: ['mes_bridge_scheduling_machine_part:get'], // 列表
  save: ['mes_bridge_scheduling_machine_part:save'] // 零件排产保存
}

// 生产排产/零件排产/预览记录
export const machinePartSchedulingRecordPM = {
  get: ['mes_bridge_scheduling_machine_part_record:get'], // 列表
  del: ['mes_bridge_scheduling_machine_part_record:del'], // 零件排产记录删除
  detail: ['mes_bridge_scheduling_machine_part_record:detail'], // 套料明细
  save: ['mes_bridge_scheduling_machine_part_record:save'] // 套料下发
}

// 生产排产/零件排产/套料成果
export const machinePartSchedulingNestingResultPM = {
  get: ['mes_bridge_scheduling_machine_part_nesting_result:get'], // 列表
  del: ['mes_bridge_scheduling_machine_part_nesting_result:del'], // 删除
  save: ['mes_bridge_scheduling_machine_part_nesting_result:save'] // 任务下发
}

// --------------------------- 生产排产 end --------------------------------

// ########################################################################

// --------------------------- 工单管理 start ------------------------------

// 工单管理/结构工单
export const artifactWorkOrderPM = {
  get: ['mes_bridge_work_order_artifact:get'], // 列表
  detail: ['mes_bridge_work_order_artifact:detail'], // 查看
  print: ['mes_bridge_work_order_artifact:print'] // 打印
}

// 工单管理/零件工单
export const machinePartWorkOrderPM = {
  get: ['mes_bridge_work_order_machine_part:get'], // 列表
  detail: ['mes_bridge_work_order_machine_part:detail'], // 查看
  print: ['mes_bridge_work_order_machine_part:print'] // 打印
}

// --------------------------- 工单管理 end --------------------------------

// ########################################################################

// --------------------------- 质安管理 start --------------------------------

// 质安管理/质检报表
export const bridgeQualityInspectionReportPM = {
  get: ['mes_bridge_quality_inspection_report:get'], // 质检报表
  detail: ['mes_bridge_quality_inspection_report:detail'] // 质检报表详情
}

// 质安管理/生产线报表
export const bridgeQhseProductionLineReportPM = {
  get: ['mes_bridge_production_line_report:get'], // 生产线报表
  detail: ['mes_bridge_production_line_report:detail'], // 生产线报表详情
  print: ['mes_bridge_production_line_report:print'] // 生产线报表详情
}

// 质安管理/问题曝光
export const bridgeQhseDisclosurePM = {
  get: ['mes_bridge_qhse_disclosure:get'] // 问题曝光列表
}

// --------------------------- 质安管理 end --------------------------------

// ########################################################################

// --------------------------- 产品标签 start --------------------------------

// 产品标签/分段
export const bridgeArtifactLabelPM = {
  get: ['mes_bridge_label_artifact:get'] // 分段列表
}

// 产品标签/围护
export const bridgeEnclosureLabelPM = {
  get: ['mes_bridge_label_enclosure:get'] // 围护列表
}

// 产品标签/配套件
export const bridgeAuxiliaryMaterialLabelPM = {
  get: ['mes_bridge_label_auxiliary_material:get'], // 围护列表
  print: ['mes_bridge_label_auxiliary_material:print'] // 批量打印标签
}

// --------------------------- 产品标签 end --------------------------------

// ########################################################################

// --------------------------- 发运管理 start --------------------------------

// 发运管理/发运管理列表
export const bridgeShipSummaryPM = {
  get: ['mes_bridge_ship_summary:get'], // 列表
  print: ['mes_bridge_ship_summary:print'] // 发运详情打印
}

// 发运管理/手工打包:手工打包
export const bridgeManualPackPM = {
  pack: ['mes_bridge_manual_pack:pack'] // 手工打包
}

// 发运管理/手工打包:分段
export const bridgeBoxManualPackPM = {
  get: ['mes_bridge_manual_pack_box:get'], // 分段打包列表
  pack: bridgeManualPackPM.pack // 手工打包
}

// 发运管理/手工打包:围护
export const bridgeEnclosureManualPackPM = {
  get: ['mes_bridge_manual_pack_enclosure:get'], // 围护打包列表
  pack: bridgeManualPackPM.pack // 手工打包
}

// 发运管理/打包记录
export const bridgePackPM = {
  get: ['mes_bridge_pack:get'], // 打包列表
  detail: ['mes_bridge_pack:detail'], // 查看打包清单
  // pack: bridgeManualPackPM.pack, // 手工打包
  edit: bridgeManualPackPM.pack, // 编辑打包清单
  del: ['mes_bridge_pack:del'], // 删除打包清单
  print: ['mes_bridge_pack:print'], // 查看及打印标签
  printRecords: ['mes_bridge_pack:printRecords'], // 查看打印记录
  printPackList: ['mes_bridge_pack:printPackList'] // 打印包单清单
}

// 发运管理/发运记录
export const bridgeShipPM = {
  get: ['mes_bridge_ship:get'], // 发运列表
  detail: ['mes_bridge_ship:detail'], // 查看车次详情
  print: ['mes_bridge_ship:print'], // 打印车次汇总
  detailPrint: ['mes_bridge_ship:detailPrint'] // 打印车次详情
}

// 发运管理/收货状态
export const bridgeReceiptStatusPM = {
  get: ['mes_bridge_receipt_status:get'], // 收货列表
  detail: ['mes_bridge_receipt_status:detail'], // 查看收货详情
  print: ['mes_bridge_receipt_status:print'], // 打印收货汇总
  detailPrint: ['mes_bridge_receipt_status:detailPrint'], // 打印收货详情
  cancelDelivery: ['mes_bridge_receipt_status:cancelDelivery'], // 取消发运
  confirmDelivery: ['mes_bridge_receipt_status:confirmDelivery'] // 确定签收
}

// 发运管理/物流记录
export const bridgeLogisticsPM = {
  get: ['mes_bridge_logistics:get'], // 物流列表
  edit: ['mes_bridge_logistics:edit'], // 录入物流信息
  print: ['mes_bridge_logistics:print'] // 打印物流汇总
}

// 发运管理/发运审核
export const bridgeShipAuditPM = {
  get: ['mes_bridge_ship_audit:get'], // 审核列表
  detail: ['mes_bridge_ship_audit:detail'], // 装车明细
  print: ['mes_bridge_ship_audit:print'], // 打印发运审核
  download: ['mes_bridge_ship_audit:download'], // 下载发运详情
  audit: ['mes_bridge_ship_audit:audit'] // 发运审核
}

// 发运管理/制成品入发存
export const bridgeProductSendReceiveStoragePM = {
  get: ['mes_bridge_product-send-receive-storage:get'], // 列表
  print: ['mes_bridge_product-send-receive-storage:print'], // 制成品入发存打印
  detail: ['mes_bridge_product-send-receive-storage:detail'], // 制成品入发存详情
  detailPrint: ['mes_bridge_product-send-receive-storage:detailPrint'] // 制成品入发存详情打印
}
// --------------------------- 发运管理 end --------------------------------

// ########################################################################

// --------------------------- 任务跟踪 start --------------------------------

// 任务跟踪/工单跟踪
export const bridgeWorkOrderTrackingPM = {
  get: ['mes_bridge_work_order_tracking:get'], // 工单跟踪列表
  print: ['mes_bridge_work_order_tracking:print'] // 工单跟踪详情打印
}

// 任务跟踪/月度任务跟踪
export const bridgeMonthlyTaskTrackingPM = {
  get: ['mes_bridge_monthly_task_tracking:get'], // 月度任务跟踪列表
  print: ['mes_bridge_monthly_task_tracking:print'] // 月度任务跟踪详情打印
}

// 任务跟踪/产线跟踪
export const bridgeProductionLineTrackingPM = {
  get: ['mes_bridge_production_line_tracking:get'], // 产线跟踪列表
  detail: ['mes_bridge_production_line_tracking:detail'], // 查看产线跟踪详情
  print: ['mes_bridge_production_line_tracking:print'] // 产线跟踪详情打印
}

// 任务跟踪/工序呆滞
export const bridgeProcessSluggishPM = {
  get: ['mes_bridge_process_sluggish:get'], // 工序呆滞列表
  print: ['mes_bridge_process_sluggish:print'] // 工序呆滞详情打印
}

// 任务跟踪/协同操作/产线协同
export const bridgeProductionLineAssistancePM = {
  get: ['mes_bridge_production_line_assistance:get'], // 列表
  save: ['mes_bridge_production_line_assistance:save'], // 协同保存
  record: ['mes_bridge_production_line_assistance:record'] // 协同记录
}

// 任务跟踪/协同操作/工序协同
export const bridgeProcessAssistancePM = {
  get: ['mes_bridge_process_assistance:get'], // 列表
  save: ['mes_bridge_process_assistance:save'], // 协同保存
  del: ['mes_bridge_process_assistance:del'] // 协同删除
}

// --------------------------- 任务跟踪 end --------------------------------

// ########################################################################

// --------------------------- 项目制造 start --------------------------------

// 项目制造/项目总览
export const bridgeProjectOverviewPM = {
  get: ['mes_bridge_project_overview:get'], // 项目总览列表
  detail: ['mes_bridge_project_overview:detail'], // 项目总览查看详情
  print: ['mes_bridge_project_overview:print'] // 项目总览详情打印
}

// 项目制造/结构看板
export const bridgeArtifactProductionDashboardPM = {
  get: ['bridge_artifact_production_dashboard:get'] // 结构看板列表
}

// 项目制造/零件齐套
export const bridgeAssemblyMatchDashboardPM = {
  get: ['mes_bridge_assembly_match:get'], // 零件齐套列表
  print: ['mes_bridge_assembly_match:print'], // 零件齐套详情打印
  printDetail: ['mes_bridge_assembly_match:printDetail'] // 零部件生产清单详情打印
}

// 项目制造/涂装计算
export const bridgePaintingDashboardPM = {
  get: ['mes_bridge_painting:get'], // 涂装计算列表
  edit: ['mes_bridge_painting:edit'], // 涂装计算编辑
  editArea: ['mes_bridge_painting:editArea'], // 涂装计算面积编辑
  print: ['mes_bridge_painting:print'] // 涂装计算列表打印
}

// --------------------------- 项目制造 end --------------------------------

// ########################################################################

// --------------------------- 车间报表 start --------------------------------
export const bridgeFactoryReportPM = {
  get: ['mes_bridge_factory_report:get'], // 车间报表列表
  print: ['mes_bridge_factory_report:print'] // 车间报表详情打印
}
// --------------------------- 车间报表 end --------------------------------
