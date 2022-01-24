// 计划管理
// --------------------------- 公共权限 start ------------------------------

export const commonPM = {}

// --------------------------- 公共权限 end --------------------------------

// ########################################################################

// --------------------------- 计划管理 start ------------------------------

// 计划管理/单体列表
export const monomerListPM = {
  get: ['plan_monomer_manage:get'], // 单体列表
  add: ['plan_monomer_manage:add'], // 新增单体
  edit: ['plan_monomer_manage:edit'], // 修改单体
  del: ['plan_monomer_manage:del'] // 删除单体
}

// 计划管理/区域列表
export const areaListPM = {
  get: ['plan_area_manage:get'], // 区域列表
  add: ['plan_area_manage:add'], // 新增区域
  edit: ['plan_area_manage:edit'], // 修改区域
  del: ['plan_area_manage:del'], // 删除区域
  print: ['plan_area_manage:print'] // 打印下载
}

// 计划管理/计划列表
export const planMakeListPM = {
  get: ['plan_make_manage:get'], // 计划列表
  edit: ['plan_make_manage:edit'], // 修改计划
  print: ['plan_make_manage:print'] // 打印下载
}

// 计划管理/计划跟踪
export const planProgressListPM = {
  get: ['plan_progress:get'] // 计划跟踪列表
}

// 计划管理/计划确认
export const planConfirmListPM = {
  get: ['plan_confirm:get'], // 计划确认列表
  edit: ['plan_confirm:edit'] // 计划确认
}

// 计划管理/工单汇总
export const planSummaryListPM = {
  get: ['plan_summary:get']// 工单汇总列表
}
// --------------------------- 计划管理 end --------------------------------

// ########################################################################

// --------------------------- 技术管理 start ------------------------------

// 技术管理/零构件列表
export const artifactTreePM = {
  get: ['plan_artifact_tree_list:get'], // 零构件列表
  edit: ['plan_artifact_tree_list:edit'], // 修改零构件
  del: ['plan_artifact_tree_list:del'], // 删除零构件
  download: ['plan_artifact_tree_list:download'], // 下载零构件
  import: ['plan_artifact_tree_list:import'] // 导入零构件
}

// 技术管理/零件列表
export const machinePartPM = {
  get: ['plan_machine_part_list:get'] // 零件列表
}

// 技术管理/构件列表
export const artifactPM = {
  get: ['plan_artifact_list:get'] // 构件列表
}

// 技术管理/组立列表
export const assemblyListPM = {
  get: ['plan_assembly_list:get'], // 组立列表
  del: ['plan_assembly_list:del'], // 删除组立
  download: ['plan_assembly_list:download'], // 下载组立
  import: ['plan_assembly_list:import'], // 导入组立
  save: ['plan_assembly_list:save'] // 添加构件
}

// 技术管理/围护列表
export const enclosureListPM = {
  get: ['plan_enclosure_list:get'], // 围护列表
  edit: ['plan_enclosure_list:edit'], // 修改围护
  del: ['plan_enclosure_list:del'], // 删除围护
  download: ['plan_enclosure_list:download'], // 下载围护
  import: ['plan_enclosure_list:import'], // 导入围护
  save: ['plan_enclosure_list:save'] // 添加围护
}

// 技术管理/深化列表
export const deepenListPM = {
  get: ['plan_deepen:get'], // 深化列表
  del: ['plan_deepen:del'], // 删除深化
  download: ['plan_deepen:download'], // 下载
  import: ['plan_deepen:import'] // 导入
}

// 技术管理/蓝图列表
export const blueprintListPM = {
  get: ['plan_blueprint:get'], // 蓝图列表
  edit: ['plan_blueprint:edit'], // 修改蓝图
  del: ['plan_blueprint:del'], // 删除蓝图
  download: ['plan_blueprint:download'], // 下载
  import: ['plan_blueprint:import'] // 导入
}

// 技术管理/变更文件列表
export const changeFileListPM = {
  get: ['plan_change_file:get'], // 变更文件列表
  edit: ['plan_change_file:edit'], // 修改变更文件
  del: ['plan_change_file:del'], // 删除变更文件
  download: ['plan_change_file:download'], // 下载
  import: ['plan_change_file:import'] // 导入
}

// 技术管理/模型列表
export const modelListPM = {
  get: ['plan_model:get'], // 模型列表
  edit: ['plan_model:edit'], // 修改模型
  del: ['plan_model:del'], // 删除模型
  download: ['plan_model:download'], // 下载
  import: ['plan_model:import'] // 导入
}

// 技术管理/其他文件列表
export const otherFileListPM = {
  get: ['plan_other_file:get'], // 其他文件列表
  edit: ['plan_other_file:edit'], // 修改其他文件
  del: ['plan_other_file:del'], // 删除其他文件
  download: ['plan_other_file:download'], // 下载
  import: ['plan_other_file:import'] // 导入
}

// 技术管理/清单合计列表
export const summaryListPM = {
  get: ['summary_list:get'] // 清单合计列表
}

// 技术管理/钢材使用量列表
export const steelStatisticalPM = {
  get: ['steel_statistical:get'] // 钢材使用量列表
}
// --------------------------- 技术管理 end --------------------------------
