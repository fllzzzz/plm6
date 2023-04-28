// 计划管理
// --------------------------- 公共权限 start ------------------------------

export const commonPM = {}

// --------------------------- 公共权限 end --------------------------------

// ########################################################################

// --------------------------- 我的项目 start ------------------------------

// 我的项目
export const planProjectsPM = {
  get: ['plan_projects:get'], // 我的项目
  statistics: ['plan_projects:statistics'], // 项目统计
  print: ['plan_projects:print'] // 打印
}

// --------------------------- 我的项目 end --------------------------------

// ########################################################################
// --------------------------- 计划管理 start ------------------------------

// 计划管理/单体列表
export const monomerListPM = {
  get: ['plan_monomer_manage:get'], // 单体列表
  add: ['plan_monomer_manage:add'], // 新增单体
  edit: ['plan_monomer_manage:edit'], // 修改单体
  del: ['plan_monomer_manage:del'], // 删除单体
  productTypeEdit: ['plan_monomer_manage:productTypeEdit'], // 项目内容修改
  productTypeDel: ['plan_monomer_manage:productTypeDel'] // 项目内容删除
}

// 计划管理/区域列表
export const areaListPM = {
  get: ['plan_area_manage:get'], // 区域列表
  add: ['plan_area_manage:add'], // 新增区域
  edit: ['plan_area_manage:edit'], // 修改区域
  del: ['plan_area_manage:del'], // 删除区域
  planList: ['plan_area_manage:planList'] // 办理计划
}

// 计划管理/计划列表
export const planMakeListPM = {
  get: ['plan_make_manage:get'], // 计划列表
  edit: ['plan_make_manage:edit'] // 修改计划
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
  edit: ['plan_artifact_tree_list:edit'], // 编辑零构件状态
  editNum: ['plan_artifact_tree_list:editNum'], // 修改构件数量
  productionStatus: ['plan_artifact_tree_list:productionStatus'], // 生产状态
  del: ['plan_artifact_tree_list:del'], // 删除零构件
  download: ['plan_artifact_tree_list:download'], // 下载零构件
  import: ['plan_artifact_tree_list:import'], // 导入零构件
  templateDownLoad: ['plan_artifact_tree_list:templateDownLoad'], // 下载零构件模板
  techDetail: ['plan_artifact_tree_list:techDetail'] // 技术交底
}

// 技术管理/零件列表
export const machinePartPM = {
  get: ['plan_machine_part_list:get'] // 零件列表
}

// 技术管理/构件列表
export const artifactPM = {
  get: ['plan_artifact_list:get'], // 构件列表
  download: ['plan_artifact_list:download'] // 下载构件
}

// 技术管理/部件列表
export const assemblyListPM = {
  get: ['plan_assembly_list:get'], // 部件列表
  download: ['plan_assembly_list:download'] // 下载部件
}

// 技术管理/配套件汇总
export const auxiliaryMaterialSummaryPM = {
  get: ['auxiliary-material-summary:get'], // 配套件汇总列表
  print: ['auxiliary-material-summary:print'] // 配套件下载
}

// 技术管理/围护列表
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

// 技术管理/蓝图列表
export const blueprintListPM = {
  get: ['plan_blueprint:get'], // 蓝图列表
  edit: ['plan_blueprint:edit'], // 替换蓝图
  del: ['plan_blueprint:del'], // 删除
  download: ['plan_blueprint:download'], // 下载
  import: ['plan_blueprint:import'] // 导入
}

// 技术管理/变更文件列表
export const changeFileListPM = {
  get: ['plan_change_file:get'], // 变更文件列表
  edit: ['plan_change_file:edit'], // 替换变更文件
  del: ['plan_change_file:del'], // 删除
  download: ['plan_change_file:download'], // 下载
  import: ['plan_change_file:import'] // 导入
}

// 技术成果/模型文件管理
export const modelFileListPM = {
  get: ['plan_model_file:get'], // 列表
  del: ['plan_model_file:del'], // 删除
  detail: ['plan_model_file:detail'], // 详情
  import: ['plan_model_file:import'], // 导入
  integration: ['plan_model_file:integration'], // 模型集成
  integrationDel: ['plan_model_file:integrationDel'] // 删除模型集成
}

// 技术成果/图纸文件管理
export const drawingFileListPM = {
  get: ['plan_drawing_file:get'], // 列表
  del: ['plan_drawing_file:del'], // 删除
  detail: ['plan_drawing_file:detail'], // 详情
  import: ['plan_drawing_file:import'] // 导入
}

// 技术成果/数控文件管理
export const cncFileListPM = {
  get: ['plan_cnc_file:get'], // 列表
  del: ['plan_cnc_file:del'], // 删除
  detail: ['plan_cnc_file:detail'], // 详情
  import: ['plan_cnc_file:import'] // 导入
}

// 技术成果/XML文件管理
export const xmlFileListPM = {
  get: ['plan_xml_file:get'], // 列表
  del: ['plan_xml_file:del'], // 删除
  detail: ['plan_xml_file:detail'], // 详情
  import: ['plan_xml_file:import'] // 导入
}

// 技术管理/清单合计列表
export const summaryListPM = {
  get: ['summary_list:get'], // 清单合计列表
  detail: ['summary_list:detail']
}

// 技术管理/钢材使用量列表
export const steelStatisticalPM = {
  get: ['steel_statistical:get'] // 钢材使用量列表
}

// 技术管理/项目配套件清单
export const planStandardPartListPM = {
  get: ['plan_standard_part:get'], // 列表
  add: ['plan_standard_part:add'], // 添加
  edit: ['plan_standard_part:edit'], // 修改
  del: ['plan_standard_part:del'], // 删除
  import: ['plan_standard_part:import'], // 导入清单
  templateDownload: ['plan_standard_part:templateDownload'] // 下载模板
}

// 技术管理工艺文件
export const planProcessListPM = {
  get: ['plan_process_list:get'], // 列表
  add: ['plan_process_list:add'], // 上传工艺文件
  edit: ['plan_process_list:edit'], // 修改
  detail: ['plan_process_list:detail'], // 详情、修订版本、绑定列表
  unbind: ['plan_process_list:unbind'], // 解绑
  bind: ['plan_process_list:bind'] // 绑定构件
}

// --------------------------- 技术管理 end --------------------------------

// ########################################################################

// --------------------------- 备料管理 start ------------------------------

// 计划管理/备料管理
export const materialProjectPreparationPM = {
  get: ['plan_materialPreparation_projectPreparation:get'],
  edit: ['plan_materialPreparation_projectPreparation:edit'],
  download: ['plan_materialPreparation_projectPreparation:download'],
  config: ['plan_materialPreparation_projectPreparation:config']
}
// --------------------------- 备料管理 end --------------------------------
