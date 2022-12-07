// 桥梁mes
// --------------------------- 公共权限 start ------------------------------

export const commonPM = {}

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
  planList: ['plan_area_manage:planList'], // 办理计划
  print: ['plan_area_manage:print'] // 打印下载
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
  get: ['bridge_production_order:get'], // 列表
  detail: ['bridge_production_order:detail'], // 查看计划详情
  edit: ['bridge_production_order:edit'], // 排产计划修改
  detailList: ['bridge_production_order:detailList'] // 清单详情
}

// --------------------------- 生产订单 end --------------------------------

// ########################################################################
