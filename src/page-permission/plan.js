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
  planList: ['plan_area_manage:planList'], // 办理计划
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
  editNum: ['plan_artifact_tree_list:editNum'], // 修改构件数量
  editInfo: ['plan_artifact_tree_list:editInfo'], // 修改零构件信息
  editSerialNum: ['plan_artifact_tree_list:editSerialNum'], // 修改构件编号
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
  get: ['plan_artifact_list:get'] // 构件列表
}

// 技术管理/组立列表
export const assemblyListPM = {
  get: ['plan_assembly_list:get'], // 组立列表
  del: ['plan_assembly_list:del'], // 删除组立
  download: ['plan_assembly_list:download'], // 下载组立
  import: ['plan_assembly_list:import'], // 导入组立
  artifactAdd: ['plan_assembly_list:artifactAdd'], // 添加构件
  artifactDel: ['plan_assembly_list:artifactDel'], // 删除构件
  templateDownLoad: ['plan_assembly_list:templateDownLoad'] // 下载组立模板
}

// 技术管理/配套件
export const auxiliaryMaterialPM = {
  get: ['auxiliary-material:get'], // 配套件列表
  add: ['auxiliary-material:add'], // 新增配套件
  del: ['auxiliary-material:del'], // 删除配套件
  edit: ['auxiliary-material:edit'] // 修改配套件
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

// 技术管理/深化列表
export const deepenListPM = {
  model: {
    import: ['plan_deepen_model:get'], // 模型导入
    get: ['plan_deepen_model:get'], // 模型导入
    del: ['plan_deepen_model:del'], // 模型删除
    integration: ['plan_deepen_model_integration:edit'], // 模型集成
    integrationDel: ['plan_deepen_model_integration:del'] // 删除模型集成
  },
  deepen: {
    get: ['plan_deepen:get'], // 深化图纸列表
    import: ['plan_deepen:import'], // 导入深化图纸
    del: ['plan_deepen:del'], // 删除深化图纸
    edit: ['plan_deepen:edit'], // 修改深化图纸
    download: ['plan_deepen:download'] // 下载深化图纸
  },
  machinePart: {
    get: ['plan_deepen_machine_part:get'], // 深化图纸列表
    import: ['plan_deepen_machine_part:import'], // 导入深化图纸
    del: ['plan_deepen_machine_part:del'], // 删除深化图纸
    edit: ['plan_deepen_machine_part:edit'], // 修改深化图纸
    download: ['plan_deepen_machine_part:download'] // 下载深化图纸
  }
}

// 技术管理/蓝图列表
export const blueprintListPM = {
  get: ['plan_blueprint:get'], // 蓝图列表
  edit: ['plan_blueprint:edit'], // 修改蓝图
  download: ['plan_blueprint:download'], // 下载
  import: ['plan_blueprint:import'] // 导入
}

// 技术管理/变更文件列表
export const changeFileListPM = {
  get: ['plan_change_file:get'], // 变更文件列表
  edit: ['plan_change_file:edit'], // 修改变更文件
  download: ['plan_change_file:download'], // 下载
  import: ['plan_change_file:import'] // 导入
}

// // 技术管理/模型列表
// export const modelListPM = {
//   get: ['plan_model:get'], // 模型列表
//   edit: ['plan_model:edit'], // 修改模型
//   del: ['plan_model:del'], // 删除模型
//   download: ['plan_model:download'], // 下载
//   import: ['plan_model:import'] // 导入
// }

// // 技术管理/其他文件列表
// export const otherFileListPM = {
//   get: ['plan_other_file:get'], // 其他文件列表
//   edit: ['plan_other_file:edit'], // 修改其他文件
//   del: ['plan_other_file:del'], // 删除其他文件
//   download: ['plan_other_file:download'], // 下载
//   import: ['plan_other_file:import'] // 导入
// }

// 技术管理/清单合计列表
export const summaryListPM = {
  get: ['summary_list:get'] // 清单合计列表
}

// 技术管理/钢材使用量列表
export const steelStatisticalPM = {
  get: ['steel_statistical:get'] // 钢材使用量列表
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
