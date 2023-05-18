// 计划管理
// --------------------------- 公共权限 start ------------------------------

export const commonPM = {
}

// --------------------------- 公共权限 end --------------------------------

// ########################################################################

// --------------------------- 我的项目 end --------------------------------

// ########################################################################
// --------------------------- 计划管理 start ------------------------------

// 计划管理/区域列表
export const enclosureAreaListPM = {
  get: ['enclosure_area_manage:get'], // 项目列表
  plan: {
    get: ['enclosure_area_plan_manage:get'], // 计划列表
    add: ['enclosure_area_plan_manage:add'], // 新增计划
    edit: ['enclosure_area_plan_manage:edit'], // 修改计划
    del: ['enclosure_area_plan_manage:del'] // 删除区域
  }
}

// --------------------------- 计划管理 end --------------------------------

// ########################################################################

// --------------------------- 清单管理 start ------------------------------

// 清单管理/各围护列表
export const enclosureListPM = {
  get: ['plan_enclosure_list:get'], // 围护列表
  edit: ['plan_enclosure_list:edit'], // 修改围护
  del: ['plan_enclosure_list:del'], // 删除围护
  download: ['plan_enclosure_list:download'], // 下载围护
  import: ['plan_enclosure_list:import'], // 导入围护
  save: ['plan_enclosure_list:save'], // 添加围护
  draw: ['plan_enclosure_list:draw'], // 画图
  drawDownload: ['plan_enclosure_list:drawDownload'], // 下载图片
  templateDownload: ['plan_enclosure_list:templateDownload'], // 下载围护模板
  techDetail: ['plan_enclosure_list:techDetail'] // 技术交底
}

// 清单管理/项目配套
export const enclosureStandardPartPM = {
  get: ['enclosure_standard_part:get'], // 列表
  add: ['enclosure_standard_part:add'], // 添加
  edit: ['enclosure_standard_part:edit'], // 修改
  del: ['enclosure_standard_part:del'], // 删除
  import: ['enclosure_standard_part:import'], // 导入围护
  templateDownload: ['enclosure_standard_part:templateDownload'] // 下载围护模板
}

// --------------------------- 清单管理 end --------------------------------

// ########################################################################
// --------------------------- 生产管理 start ------------------------------

// 生产管理/排产管理
export const enclosureSchedulingManagePM = {
  get: ['enclosure_scheduling_manage:get'], // 列表
  add: ['enclosure_scheduling_manage:add'] // 下发
}

// 生产管理/排产工单
export const enclosureSchedulingWorkOrderPM = {
  get: ['enclosure_scheduling_work_order:get'], // 列表
  del: ['enclosure_scheduling_work_order:del'], // 删除
  detail: ['enclosure_scheduling_work_order:detail'], // 详情
  print: ['enclosure_scheduling_work_order:print'] // 打印
}

// 生产跟踪/任务跟踪
export const enclosureTaskTrackingPM = {
  get: ['enclosure_task_tracking:get'], // 列表
  detail: ['enclosure_task_tracking:detail'], // 详情
  print: ['enclosure_task_tracking:print'] // 打印
}

// 生产跟踪/项目全貌
export const enclosureProjectOverviewPM = {
  get: ['enclosure_project_overview:get'] // 列表
}

// 生产报表/生产统计
export const enclosureProductionStatisticsPM = {
  get: ['enclosure_production_statistics:get'], // 列表
  print: ['enclosure_production_statistics:print'] // 打印
}

// 生产报表/班组产量统计
export const enclosureTeamProductionPM = {
  get: ['enclosure_team_production:get'], // 列表
  detail: ['enclosure_team_production:detail'], // 详情
  print: ['enclosure_team_production:print'], // 打印
  printDetail: ['enclosure_team_production:printDetail'] // 打印详情
}

// 生产报表/类型分析
export const enclosureTypeAnalysisPM = {
  get: ['enclosure_type_analysis:get'] // 列表
}

// 围护-产品标签
export const enclosureLabelPM = {
  get: ['enclosure_label:get'], // 围护列表
  printSetting: ['enclosure_label:printSetting'], // 打印设置
  batchPrint: ['enclosure_label:batchPrint'], // 批量打印标签
  print: ['enclosure_label:print'], // 标签打印
  preview: ['enclosure_label:preview'], // 标签预览
  record: ['enclosure_label:record'] // 标签打印记录
}

// --------------------------- 生产管理 end --------------------------------
