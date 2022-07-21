// 配置管理
// --------------------------- 公共权限 start ------------------------------

export const commonPM = {}

// --------------------------- 公共权限 end --------------------------------

// ########################################################################

// --------------------------- 基础配置 start ------------------------------

// 基础配置/单位配置
export const configUnitPM = {
  get: ['config_unitConfig:get'], // 单位配置列表
  add: ['config_unitConfig:add'], // 添加单位配置
  edit: ['config_unitConfig:edit'], // 编辑单位配置
  del: ['config_unitConfig:del'] // 删除单位配置
}

// 基础配置/工厂管理
export const configFactoryPM = {
  get: ['factory:get'], // 工厂列表
  add: ['factory:add'], // 添加工厂
  edit: ['factory:edit'], // 编辑工厂
  del: ['factory:del'], // 删除工厂
  editStatus: ['factory:editStatus'] // 更改工厂状态
}

// 基础配置/工厂管理：车间
export const configWorkshopPM = {
  get: ['workshop:get'], // 车间列表
  add: ['workshop:add'], // 添加车间
  edit: ['workshop:edit'], // 编辑车间
  del: ['workshop:del'], // 删除车间
  editStatus: ['workshop:editStatus'] // 更改车间状态
}

// 基础配置/系统配置
export const systemConfigPM = {
  company: {
    get: ['configCompany:get'], // 查看公司信息
    edit: ['configCompany:edit'] // 编辑公司信息
  },
  logo: {
    get: ['configLogo:get'], // 查看公司logo
    edit: ['configLogo:edit'] // 编辑公司logo
  },
  project: {
    get: ['configProject:get'], // 查看项目信息
    edit: ['configProject:edit'] // 编辑项目信息
  }
}

// 基础配置/编号配置
export const numberConfigPM = {
  get: ['config_numberConfig:get'], // 编号配置列表
  edit: ['config_numberConfig:edit'] // 编辑编号配置
}

// 基础配置/常用税率
export const configCommonTaxRatePM = {
  get: ['config_commonTaxRate:get'],
  edit: ['config_commonTaxRate:edit']
}

// 基础配置/费用归类
export const expenseManagementPM = {
  get: ['expense_management:get'], // 费用归类列表
  add: ['expense_management:add'], // 新增费用归类
  edit: ['expense_management:edit'], // 修改费用归类
  del: ['expense_management:del'] // 删除费用归类
}

// 基础配置/分支机构
export const branchCompanyPM = {
  get: ['branch_company:get'], // 分支机构列表
  add: ['branch_company:add'], // 新增分支机构
  edit: ['branch_company:edit'], // 修改分支机构
  del: ['branch_company:del'] // 删除分支机构
}

// 基础配置/项目模式
export const projectModePM = {
  get: ['project_mode:get'], // 项目模式列表
  edit: ['project_mode:edit'] // 修改项目模式
}

// 基础配置/表格模板
export const tablePrintTemplatePM = {
  get: ['table_print_template:get'], // 表格模板列表
  add: ['table_print_template:add'], // 新增表格模板
  detail: ['table_print_template:detail'], // 查看表格模板详情
  edit: ['table_print_template:edit'], // 编辑表格模板
  del: ['table_print_template:del'] // 删除表格模板
}

// ---------------------------- 基础配置 end -------------------------------

// ########################################################################

// --------------------------- 科目管理 start ------------------------------

// 科目管理/科目配置
export const classConfigPM = {
  get: ['config_classConfig:get'], // 科目配置列表
  add: ['config_classConfig:add'], // 添加科目配置
  del: ['config_classConfig:del'] // 删除科目配置
}

// 科目管理/规格配置
export const specConfigPM = {
  get: ['config_specConfig:get'], // 规格配置列表
  add: ['config_specConfig:add'], // 添加规格配置
  edit: ['config_specConfig:edit'], // 编辑规格配置
  del: ['config_specConfig:del'], // 删除规格配置
  weightedAverage: ['config_specConfig:weightedAverage'] // 加权平均价
}

// 科目管理/计量配置
export const measureConfigPM = {
  get: ['config_class_measureConfig:get'], // 计量配置列表
  edit: ['config_class_measureConfig:edit'] // 编辑计量配置
}

// 科目管理/型材库
export const sectionSteelLibraryPM = {
  get: ['config_class_sectionSteelLibrary:get'], // 列表
  add: ['config_class_sectionSteelLibrary:add'], // 添加
  edit: ['config_class_sectionSteelLibrary:edit'], // 编辑
  del: ['config_class_sectionSteelLibrary:del'] // 删除
}

// ---------------------------- 基础配置 end -------------------------------

// ########################################################################

// --------------------------- MES-公共配置 start --------------------------

// MES-公共配置/基础配置
export const configMesBasePM = {
  overweightSMSRecipientGet: ['overweight_sms_recipient:get'], // 查看过磅短信接收人
  overweightSMSRecipientEdit: ['overweight_sms_recipient:edit'], // 编辑过磅短信接收人
  safeAmountFactorGet: ['safe_amount_factor:get'], // 查看安全余额系数
  safeAmountFactorEdit: ['safe_amount_factor:edit'], // 编辑安全余额系数
  driverFillConfigGet: ['driver_fill_config:get'], // 查看物流信息填写配置
  driverFillConfigEdit: ['driver_fill_config:edit'], // 编辑物流信息填写配置
  drawingSNConfigGet: ['drawing_sn_config:get'], // 查看图纸识别规则配置
  drawingSNConfigEdit: ['drawing_sn_config:edit'], // 编辑图纸识别规则配置
  appPrintConfigGet: ['app_print_config:get'], // 查看移动端打印配置
  appPrintConfigEdit: ['app_print_config:edit'], // 编辑移动端打印配置
  carModelConfigGet: ['car_model_config:get'], // 查看车型配置
  carModelConfigEdit: ['car_model_config:edit'], // 编辑车型配置
  foldingPriceMethodGet: ['folding_price_method:get'], // 查看围护折边件计价方式
  foldingPriceMethodEdit: ['folding_price_method:edit'] // 编辑围护折边件计价方式
}

// MES-公共配置/变更原因
export const changeReasonPM = {
  get: ['change_reason:get'], // 变更原因列表
  add: ['change_reason:add'], // 新增变更原因
  edit: ['change_reason:edit'], // 修改变更原因
  del: ['change_reason:del'] // 删除变更原因
}
// ---------------------------- 基础配置 end -------------------------------

// ########################################################################

// --------------------------- 建钢-生产配置 start -------------------------

// MES-公共配置/构件特征定义
export const artifactConfigPM = {
  get: ['artifact_config:get'], // 构件特征定义列表
  add: ['artifact_config:add'], // 添加构件特征定义
  edit: ['artifact_config:edit'], // 修改构件特征定义
  del: ['artifact_config:del'] // 删除构件特征定义
}

// MES-公共配置/部件特征定义
export const machinePartConfigPM = {
  get: ['machine_part_config:get'], // 部件特征定义列表
  add: ['machine_part_config:add'], // 添加部件特征定义
  edit: ['machine_part_config:edit'], // 修改部件特征定义
  del: ['machine_part_config:del'] // 删除部件特征定义
}

// MES-公共配置/零件特征定义
export const steelClassicPM = {
  get: ['steel_classic:get'], // 零件特征定义列表
  add: ['steel_classic:add'], // 新增零件特征定义
  edit: ['steel_classic:edit'], // 修改零件特征定义
  del: ['steel_classic:del'] // 删除零件特征定义
}

// 建钢-生产配置/生产线管理
export const configProductionLinePM = {
  get: ['productionLine:get'], // 生产线列表
  add: ['productionLine:add'], // 添加生产线
  edit: ['productionLine:edit'], // 编辑生产线
  del: ['productionLine:del'], // 删除生产线
  editStatus: ['productionLine:editStatus'] // 更改生产线状态
}

// 建钢-生产配置/生产线管理:班组
export const configProductionLineTeamPM = {
  get: ['productionLine_team:get'], // 班组列表
  add: ['productionLine_team:add'], // 添加班组
  edit: ['productionLine_team:edit'], // 编辑班组
  del: ['productionLine_team:del'] // 删除班组
}

// 建钢-生产配置/生产线管理:质检
export const configProductionLineInspectPM = {
  get: ['productionLine_inspect:get'], // 质检列表
  add: ['productionLine_inspect:add'], // 添加质检
  edit: ['productionLine_inspect:edit'], // 编辑质检
  del: ['productionLine_inspect:del'] // 删除质检
}

// 建钢-生产配置/工序配置
export const configProcessPM = {
  get: ['process:get'], // 工序列表
  // add: ['process:add'], // 添加工序
  edit: ['process:edit'] // 编辑工序
  // del: ['process:del'] // 删除工序
}

// // 建钢-生产配置/工序管理
// export const configProductProcessPM = {
//   get: ['product_process:get'], // 产品工序列表
//   add: ['product_process:add'], // 添加产品工序
//   edit: ['product_process:edit'], // 编辑产品工序
//   del: ['product_process:del'], // 删除产品工序
//   editStatus: ['product_process:editStatus'] // 更改产品工序状态
// }

// 建钢-生产配置/构件工序定义
export const configProductProcessArtifactPM = {
  get: ['product_process_artifact:get'], // 构件工序列表
  edit: ['product_process_artifact:edit'] // 编辑构件工序
}

// 建钢-生产配置/部件工序定义
export const configProductProcessAssemblePM = {
  get: ['product_process_assemble:get'], // 部件工序列表
  edit: ['product_process_assemble:edit'] // 编辑部件工序
}

// 建钢-生产配置/零件工序定义
export const configProductProcessMachinePartPM = {
  get: ['product_process_machine_part:get'], // 零件工序列表
  edit: ['product_process_machine_part:edit'] // 编辑零件工序
}

// 建钢-生产配置/报检方式
export const configInspectionModePM = {
  get: ['inspection_mode:get'], // 报检方式列表
  edit: ['inspection_mode:edit'] // 编辑报检方式
}

// 建钢-生产配置/工价定额
export const configWageQuotaPM = {
  get: ['wage_quota:get'], // 工价定额列表
  edit: ['wage_quota:edit'] // 编辑工价定额
}

// ---------------------------- 基础配置 end -------------------------------

// ########################################################################

// --------------------------- WMS-配置管理 start --------------------------

// WMS-配置管理/基础配置
export const configWmsBasicForPM = {
  basicInboundGet: ['config_wms_basicInbound:get'], // 查看 基础入库配置信息
  basicInboundEdit: ['config_wms_basicInbound:edit'], // 编辑 基础入库配置信息
  steelInboundGet: ['config_wms_steelInbound:get'], // 查看 钢材入库配置信息
  steelInboundEdit: ['config_wms_steelInbound:edit'], // 编辑 钢材入库配置信息
  basicOutboundGet: ['config_wms_basicOutbound:get'], // 查看 基础出库配置信息
  basicOutboundEdit: ['config_wms_basicOutbound:edit'], // 编辑 基础出库配置信息
  partyABorrowReturnGet: ['config_wms_partyABorrowReturn:get'], // 查看 甲供借用归还配置信息
  partyABorrowReturnEdit: ['config_wms_partyABorrowReturn:edit'] // 编辑 甲供借用归还配置信息
  // basicRejectGet: ['config_wms_basicReject:get'], // 查看 基础退货信息
  // basicRejectEdit: ['config_wms_basicReject:edit'] // 编辑 基础退货信息
}

// WMS-配置管理/仓库设置
export const configWmsFactoryWarehousePM = {
  get: ['config_wms_factory_warehouse:get'], // 查看 工厂仓库设置
  add: ['config_wms_factory_warehouse:add'], // 添加 工厂仓库设置
  edit: ['config_wms_factory_warehouse:edit'], // 编辑 工厂仓库设置
  del: ['config_wms_factory_warehouse:del'] // 删除 工厂仓库设置
}

// WMS-配置管理/废料定义
export const configWmsScrapDefinitionPM = {
  steelScrapDefinitionGet: ['config_wms_steelScrapDefinition:get'],
  steelScrapDefinitionEdit: ['config_wms_steelScrapDefinition:edit']
}

// --------------------------- WMS-配置管理 end ----------------------------

// ########################################################################

// --------------------------- 项目配置 start --------------------------

// 项目配置/围护信息配置
export const enclosureInfoConfigPM = {
  get: ['enclosure_info_config:get'], // 围护列表
  add: ['enclosure_info_config:add'], // 新增桁架楼承板
  editStatus: ['enclosure_info_config:editStatus'], // 修改桁架楼承板状态
  del: ['enclosure_info_config:del'], // 删除桁架楼承板
  detailInfo: {
    get: ['enclosure_info_detail:get'], // 围护详情列表
    add: ['enclosure_info_detail:add'], // 新增围护详情
    edit: ['enclosure_info_detail:editStatus'], // 修改围护详情
    del: ['enclosure_info_detail:del'] // 删除围护详情
  }
}

// 项目配置/项目成员模板配置
export const memberConfigPM = {
  get: ['member-config:get'], // 项目成员模板列表
  add: ['member-config:add'], // 新增项目成员模板
  edit: ['member-config:edit'], // 修改项目成员模板
  del: ['member-config:del'] // 删除项目成员模板
}

// --------------------------- 项目配置 end ----------------------------

// ########################################################################
