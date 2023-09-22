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

// 基础配置/公司配置
export const systemConfigPM = {
  company: {
    get: ['config_company:get'], // 查看公司信息
    edit: ['config_company:edit'] // 编辑公司信息
  },
  logo: {
    get: ['config_logo:get'], // 查看公司logo
    edit: ['config_logo:edit'] // 编辑公司logo
  },
  project: {
    get: ['config_project:get'], // 查看项目信息
    edit: ['config_project:edit'] // 编辑项目信息
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

// 基础配置/费用归类
export const expenseSubjectPM = {
  get: ['expense_subject:get'], // 查看
  add: ['expense_subject:add'], // 新增
  edit: ['expense_subject:edit'], // 编辑
  del: ['expense_subject:del'] // 删除
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

// 基础配置/模块小数精度配置
export const moduleDecimalPrecisionPM = {
  get: ['module_decimal_precision:get'], // 列表
  add: ['module_decimal_precision:add'], // 添加
  del: ['module_decimal_precision:del'] // 删除
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
  steelBindConfigGet: ['steel_bind_config:get'], // 查看钢板绑定配置
  steelBindConfigEdit: ['steel_bind_config:edit'], // 编辑钢板绑定配置
  // driverFillConfigGet: ['driver_fill_config:get'], // 查看物流信息填写配置
  // driverFillConfigEdit: ['driver_fill_config:edit'], // 编辑物流信息填写配置
  drawingSNConfigGet: ['drawing_sn_config:get'], // 查看图纸识别规则配置
  drawingSNConfigEdit: ['drawing_sn_config:edit'], // 编辑图纸识别规则配置
  appPrintConfigGet: ['app_print_config:get'], // 查看移动端打印配置
  appPrintConfigEdit: ['app_print_config:edit'], // 编辑移动端打印配置
  carModelConfigGet: ['car_model_config:get'], // 查看车型配置
  carModelConfigEdit: ['car_model_config:edit'], // 编辑车型配置
  foldingPriceMethodGet: ['folding_price_method:get'], // 查看围护折边件计价方式
  foldingPriceMethodEdit: ['folding_price_method:edit'], // 编辑围护折边件计价方式
  appTaskReportGet: ['app_task_report:get'], // 查看app任务上报重量显示
  appTaskReportEdit: ['app_task_report:edit'], // 编辑app任务上报重量显示
  machinePartSchedulingGet: ['machine_part_scheduling:get'], // 查看零件排产车间、产线、生产组显示
  machinePartSchedulingEdit: ['machine_part_scheduling:edit'], // 编辑零件排产车间、产线、生产组显示
  machinePartStructureGet: ['machine_part_structure:get'], // 查看构件部件特征定义审核配置
  machinePartStructureEdit: ['machine_part_structure:edit'], // 编辑查看构件部件特征定义审核配置
  contractPriceConfigGet: ['contract_price_config:get'], // 查看价格录入办理方式
  contractPriceConfigEdit: ['contract_price_config:edit'] // 编辑价格录入办理方式
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

// 建钢-生产配置/车间
export const configWorkshopPM = {
  get: ['workshop:get'], // 车间列表
  add: ['workshop:add'], // 添加车间
  edit: ['workshop:edit'], // 编辑车间
  del: ['workshop:del'], // 删除车间
  editStatus: ['workshop:editStatus'] // 更改车间状态
}

// 建钢-生产配置/构件特征定义
export const artifactConfigPM = {
  get: ['artifact_config:get'], // 构件特征定义列表
  add: ['artifact_config:add'], // 添加构件特征定义
  edit: ['artifact_config:edit'], // 修改构件特征定义
  del: ['artifact_config:del'], // 删除构件特征定义
  audit: ['artifact_config:audit']
}

// 建钢-生产配置/部件特征定义
export const machinePartConfigPM = {
  get: ['machine_part_config:get'], // 部件特征定义列表
  add: ['machine_part_config:add'], // 添加部件特征定义
  edit: ['machine_part_config:edit'], // 修改部件特征定义
  del: ['machine_part_config:del'], // 删除部件特征定义
  audit: ['machine_part_config:audit']
}

// 建钢-生产配置/零件特征定义
export const steelClassicPM = {
  get: ['steel_classic:get'], // 零件特征定义列表
  add: ['steel_classic:add'], // 新增零件特征定义
  edit: ['steel_classic:edit'], // 修改零件特征定义
  del: ['steel_classic:del'] // 删除零件特征定义
}

// 建钢-生产配置/配套件特征定义
export const auxiliaryMaterialConfigPM = {
  get: ['auxiliary-material-config:get'], // 配套件特征定义列表
  add: ['auxiliary-material-config:add'], // 新增配套件特征定义
  edit: ['auxiliary-material-config:edit'], // 修改配套件特征定义
  del: ['auxiliary-material-config:del'] // 删除配套件特征定义
}

// 建钢-生产配置/生产线管理
export const configProductionLinePM = {
  get: ['productionLine:get'], // 生产线列表
  add: ['productionLine:add'], // 添加生产线
  edit: ['productionLine:edit'], // 编辑生产线
  del: ['productionLine:del'], // 删除生产线
  editStatus: ['productionLine:editStatus'] // 更改生产线状态
}

// 建钢-生产配置/生产线管理:生产组
export const configProductionLineGroupPM = {
  get: ['productionLine_group:get'], // 班组列表
  add: ['productionLine_group:add'], // 添加班组
  edit: ['productionLine_group:edit'], // 编辑班组
  del: ['productionLine_group:del'] // 删除班组
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

// 建钢-生产配置/统计配置/构件-种类配置
export const configArtifactTypeConfigPM = {
  get: ['statistical_artifact_type_config:get'], // 构件-种类配置列表
  add: ['statistical_artifact_type_config:add'], // 新增构件-种类配置
  edit: ['statistical_artifact_type_config:edit'], // 编辑构件-种类配置
  del: ['statistical_artifact_type_config:del'] // 删除构件-种类配置
}

// 建钢-生产配置/统计配置/构件-组铆焊价格配置
export const configArtifactRivetWeldConfigPM = {
  get: ['statistical_artifact_rivet_weld_config:get'], // 构件-组铆焊配置列表
  add: ['statistical_artifact_rivet_weld_config:add'], // 新增构件-组铆焊价格配置
  edit: ['statistical_artifact_rivet_weld_config:edit'], // 编辑构件-组铆焊价格配置
  del: ['statistical_artifact_rivet_weld_config:del'] // 删除构件-组铆焊价格配置
}

// 建钢-生产配置/统计配置/构件-涂装配置
export const configStatisticalCoatingPM = {
  get: ['statistical_coating_config:get'], // 构件-涂装配置列表
  edit: ['statistical_coating_config:edit'] // 编辑构件-涂装配置
}

// 建钢-生产配置/统计配置/部件-组铆焊价格配置
export const configAssembleRivetWeldConfigPM = {
  get: ['statistical_assemble_rivet_weld_config:get'], // 部件-组铆焊配置列表
  add: ['statistical_assemble_rivet_weld_config:add'], // 新增部件-组铆焊价格配置
  edit: ['statistical_assemble_rivet_weld_config:edit'], // 编辑部件-组铆焊价格配置
  del: ['statistical_assemble_rivet_weld_config:del'] // 删除部件-组铆焊价格配置
}

// 建钢-生产配置/统计配置/零件-栓钉套筒配置
export const configStatisticalFabricatedPM = {
  get: ['statistical_fabricated_config:get'], // 零件-栓钉套筒配置列表
  del: ['statistical_fabricated_config:del'], // 删除零件-栓钉套筒配置
  edit: ['statistical_fabricated_config:edit'] // 编辑零件-栓钉套筒配置
}

// 建钢-生产配置/统计配置/零件-钻孔配置
export const configStatisticalDrillHolePM = {
  get: ['statistical_drill_hole_config:get'], // 零件-钻孔配置列表
  add: ['statistical_drill_hole_config:add'], // 添加零件-钻孔配置
  del: ['statistical_drill_hole_config:del'], // 删除零件-钻孔配置
  edit: ['statistical_drill_hole_config:edit'] // 编辑零件-钻孔配置
}

// 建钢-生产配置/统计配置/零件-下料配置
export const configStatisticalPartsLayingPM = {
  get: ['statistical_parts_laying_config:get'], // 零件-下料配置列表
  add: ['statistical_parts_laying_config:add'], // 添加零件-下料配置
  del: ['statistical_parts_laying_config:del'], // 删除零件-下料配置
  edit: ['statistical_parts_laying_config:edit'] // 编辑零件-下料配置
}

// 建钢-生产配置/零件下料配置
export const configMachinePartLayingPM = {
  get: ['machine_part_laying_config:get'], // 零件下料配置列表
  add: ['machine_part_laying_config:add'], // 添加零件下料配置
  del: ['machine_part_laying_config:del'], // 删除零件下料配置
  edit: ['machine_part_laying_config:edit'] // 编辑零件下料配置
}

// 建钢-生产配置/工序配置
export const configProcessPM = {
  get: ['process:get'], // 工序列表
  edit: ['process:edit'] // 编辑工序
}

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

// 建钢-生产配置/切割配置
export const mesCuttingConfigPM = {
  get: ['mes_cutting_config:get'], // 切割配置列表
  add: ['mes_cutting_config:add'], // 添加切割配置
  edit: ['mes_cutting_config:edit'], // 编辑切割配置
  del: ['mes_cutting_config:del'] // 删除切割配置
}

// 建钢-生产配置/垫块配置
export const mesParallelConfigPM = {
  get: ['mes_parallel_config:get'], // 垫块配置列表
  add: ['mes_parallel_config:add'], // 添加垫块配置
  edit: ['mes_parallel_config:edit'], // 编辑垫块配置
  del: ['mes_parallel_config:del'], // 删除垫块配置
  import: ['mes_parallel_config:import'] // 导入dxf
}

// ---------------------------- 建钢-生产配置 end -------------------------------

// ########################################################################

// --------------------------- 围护-生产配置 start --------------------------

// 围护-生产配置/生产线管理:班组
export const enclosureConfigProductionLineTeamPM = {
  get: ['enclosure_productionLine_team:get'], // 班组列表
  add: ['enclosure_productionLine_team:add'], // 添加班组
  edit: ['enclosure_productionLine_team:edit'], // 编辑班组
  del: ['enclosure_productionLine_team:del'] // 删除班组
}

// 围护-生产配置/生产线管理:质检
export const enclosureConfigProductionLineInspectPM = {
  get: ['enclosure_productionLine_inspect:get'], // 质检列表
  add: ['enclosure_productionLine_inspect:add'], // 添加质检
  edit: ['enclosure_productionLine_inspect:edit'], // 编辑质检
  del: ['enclosure_productionLine_inspect:del'] // 删除质检
}

// 围护-生产配置/报检方式
export const enclosureConfigInspectionModePM = {
  get: ['enclosure_inspection_mode:get'], // 报检方式列表
  edit: ['enclosure_inspection_mode:edit'] // 编辑报检方式
}

// 围护-生产配置/生产线管理
export const enclosureConfigProductionLinePM = {
  get: ['enclosure_productionLine:get'], // 生产线列表
  add: ['enclosure_productionLine:add'], // 添加生产线
  edit: ['enclosure_productionLine:edit'], // 编辑生产线
  del: ['enclosure_productionLine:del'], // 删除生产线
  editStatus: ['enclosure_productionLine:editStatus'] // 更改生产线状态
}

// 围护-生产配置/生产线管理:生产组
export const enclosureConfigProductionLineGroupPM = {
  get: ['enclosure_productionLine_group:get'], // 班组列表
  edit: ['enclosure_productionLine_group:edit'] // 编辑班组
}

// ---------------------------- 围护-生产配置 end -------------------------------

// ---------------------------- 基础配置 end -------------------------------

// ---------------------------- 建钢配置 end -------------------------------

// ########################################################################
// --------------------------- 桥梁-生产配置 start -------------------------

// 桥梁-生产配置/车间
export const bridgeWorkshopPM = {
  get: ['bridge_workshop:get'], // 车间列表
  add: ['bridge_workshop:add'], // 添加车间
  edit: ['bridge_workshop:edit'], // 编辑车间
  del: ['bridge_workshop:del'], // 删除车间
  editStatus: ['bridge_workshop:editStatus'] // 更改车间状态
}

// 桥梁-生产配置/分段特征定义
export const boxConfigPM = {
  get: ['box_config:get'], // 分段特征定义列表
  add: ['box_config:add'], // 添加分段特征定义
  edit: ['box_config:edit'], // 修改分段特征定义
  del: ['box_config:del'] // 删除分段特征定义
}

// 桥梁-生产配置/单元特征定义
export const cellConfigPM = {
  get: ['cell_config:get'], // 单元特征定义列表
  add: ['cell_config:add'], // 添加单元特征定义
  edit: ['cell_config:edit'], // 修改单元特征定义
  del: ['cell_config:del'] // 删除单元特征定义
}

// 桥梁-生产配置/零件特征定义
export const partConfigPM = {
  get: ['part_config:get'], // 零件特征定义列表
  add: ['part_config:add'], // 新增零件特征定义
  edit: ['part_config:edit'], // 修改零件特征定义
  del: ['part_config:del'] // 删除零件特征定义
}

// 桥梁-生产配置/配套件特征定义
export const bridgeAuxiliaryMaterialConfigPM = {
  get: ['bridge-auxiliary-material-config:get'], // 配套件特征定义列表
  add: ['bridge-auxiliary-material-config:add'], // 新增配套件特征定义
  edit: ['bridge-auxiliary-material-config:edit'], // 修改配套件特征定义
  del: ['bridge-auxiliary-material-config:del'] // 删除配套件特征定义
}

// 桥梁-生产配置/生产线管理
export const bridgeConfigProductionLinePM = {
  get: ['bridgeProductionLine:get'], // 生产线列表
  add: ['bridgeProductionLine:add'], // 添加生产线
  edit: ['bridgeProductionLine:edit'], // 编辑生产线
  del: ['bridgeProductionLine:del'], // 删除生产线
  editStatus: ['bridgeProductionLine:editStatus'] // 更改生产线状态
}

// 桥梁-生产配置/生产线管理:生产组
export const bridgeConfigProductionLineGroupPM = {
  get: ['bridge_productionLine_group:get'], // 班组列表
  add: ['bridge_productionLine_group:add'], // 添加班组
  edit: ['bridge_productionLine_group:edit'], // 编辑班组
  del: ['bridge_productionLine_group:del'] // 删除班组
}

// 桥梁-生产配置/生产线管理:班组
export const bridgeConfigProductionLineTeamPM = {
  get: ['bridge_productionLine_team:get'], // 班组列表
  add: ['bridge_productionLine_team:add'], // 添加班组
  edit: ['bridge_productionLine_team:edit'], // 编辑班组
  del: ['bridge_productionLine_team:del'] // 删除班组
}

// 桥梁-生产配置/生产线管理:质检
export const bridgeConfigProductionLineInspectPM = {
  get: ['bridge_productionLine_inspect:get'], // 质检列表
  add: ['bridge_productionLine_inspect:add'], // 添加质检
  edit: ['bridge_productionLine_inspect:edit'], // 编辑质检
  del: ['bridge_productionLine_inspect:del'] // 删除质检
}

// 桥梁-生产配置/统计配置/分段-种类配置
export const configBoxTypeConfigPM = {
  get: ['bridge_statistical_box_type_config:get'], // 分段-种类配置列表
  add: ['bridge_statistical_box_type_config:add'], // 新增分段-种类配置
  edit: ['bridge_statistical_box_type_config:edit'], // 编辑分段-种类配置
  del: ['bridge_statistical_box_type_config:del'] // 删除分段-种类配置
}

// 桥梁-生产配置/统计配置/分段-组铆焊价格配置
export const configBoxRivetWeldConfigPM = {
  get: ['bridge_statistical_artifact_rivet_weld_config:get'], // 分段-组铆焊配置列表
  add: ['bridge_statistical_artifact_rivet_weld_config:add'], // 新增分段-组铆焊价格配置
  edit: ['bridge_statistical_artifact_rivet_weld_config:edit'], // 编辑分段-组铆焊价格配置
  del: ['bridge_statistical_artifact_rivet_weld_config:del'] // 删除分段-组铆焊价格配置
}

// 桥梁-生产配置/统计配置/分段-涂装配置
export const bridgeConfigStatisticalCoatingPM = {
  get: ['bridge_statistical_coating_config:get'], // 分段-涂装配置列表
  edit: ['bridge_statistical_coating_config:edit'] // 编辑分段-涂装配置
}

// 桥梁-生产配置/统计配置/单元-组铆焊价格配置
export const configCellRivetWeldConfigPM = {
  get: ['statistical_cell_rivet_weld_config:get'], // 单元-组铆焊配置列表
  add: ['statistical_cell_rivet_weld_config:add'], // 新增单元-组铆焊价格配置
  edit: ['statistical_cell_rivet_weld_config:edit'], // 编辑单元-组铆焊价格配置
  del: ['statistical_cell_rivet_weld_config:del'] // 删除单元-组铆焊价格配置
}

// 桥梁-生产配置/统计配置/零件-栓钉套筒配置
export const bridgeConfigStatisticalFabricatedPM = {
  get: ['bridge_statistical_fabricated_config:get'], // 零件-栓钉套筒配置列表
  del: ['bridge_statistical_fabricated_config:del'], // 删除零件-栓钉套筒配置
  edit: ['bridge_statistical_fabricated_config:edit'] // 编辑零件-栓钉套筒配置
}

// 桥梁-生产配置/统计配置/零件-钻孔
export const bridgeConfigStatisticalDrillHolePM = {
  get: ['bridge_statistical_drill_hole_config:get'], // 零件-钻孔配置列表
  add: ['bridge_statistical_drill_hole_config:add'], // 添加零件-钻孔配置
  del: ['bridge_statistical_drill_hole_config:del'], // 删除零件-钻孔配置
  edit: ['bridge_statistical_drill_hole_config:edit'] // 编辑零件-钻孔配置
}

// 桥梁-生产配置/统计配置/零件-下料
export const bridgeConfigStatisticalPartsLayingPM = {
  get: ['bridge_statistical_parts_laying_config:get'], // 零件-下料配置列表
  add: ['bridge_statistical_parts_laying_config:add'], // 添加零件-下料配置
  del: ['bridge_statistical_parts_laying_config:del'], // 删除零件-下料配置
  edit: ['bridge_statistical_parts_laying_config:edit'] // 编辑零件-下料配置
}

// 桥梁-生产配置/零件下料配置
export const bridgeConfigMachinePartLayingPM = {
  get: ['bridge_machine_part_laying_config:get'], // 零件下料配置列表
  add: ['bridge_machine_part_laying_config:add'], // 添加零件下料配置
  del: ['bridge_machine_part_laying_config:del'], // 删除零件下料配置
  edit: ['bridge_machine_part_laying_config:edit'] // 编辑零件下料配置
}

// 桥梁-生产配置/工序配置
export const bridgeConfigProcessPM = {
  get: ['bridge_process:get'], // 工序列表
  edit: ['bridge_process:edit'] // 编辑工序
}

// 桥梁-生产配置/分段工序定义
export const configProductProcessBoxPM = {
  get: ['product_process_box:get'], // 分段工序列表
  edit: ['product_process_box:edit'] // 编辑分段工序
}

// 桥梁-生产配置/单元工序定义
export const configProductProcessCellPM = {
  get: ['product_process_cell:get'], // 单元工序列表
  edit: ['product_process_cell:edit'] // 编辑单元工序
}

// 桥梁-生产配置/零件工序定义
export const bridgeConfigProductProcessMachinePartPM = {
  get: ['bridge_product_process_machine_part:get'], // 零件工序列表
  edit: ['bridge_product_process_machine_part:edit'] // 编辑零件工序
}

// 桥梁-生产配置/报检方式
export const bridgeConfigInspectionModePM = {
  get: ['bridge_inspection_mode:get'], // 报检方式列表
  edit: ['bridge_inspection_mode:edit'] // 编辑报检方式
}

// 桥梁-生产配置/工价定额
export const bridgeConfigWageQuotaPM = {
  get: ['bridge_wage_quota:get'], // 工价定额列表
  edit: ['bridge_wage_quota:edit'] // 编辑工价定额
}

// 桥梁-生产配置/切割配置
export const bridgeCuttingConfigPM = {
  get: ['bridge_cutting_config:get'], // 切割配置列表
  add: ['bridge_cutting_config:add'], // 添加切割配置
  edit: ['bridge_cutting_config:edit'], // 编辑切割配置
  del: ['bridge_cutting_config:del'] // 删除切割配置
}

// ---------------------------- 桥梁配置 end -------------------------------

// ########################################################################

// --------------------------- WMS-配置管理 start --------------------------

// WMS-配置管理/基础配置
export const configWmsBasicForPM = {
  // basicInboundGet: ['config_wms_basicInbound:get'], // 查看 基础入库配置信息
  // basicInboundEdit: ['config_wms_basicInbound:edit'], // 编辑 基础入库配置信息
  reportCenterGet: ['config_wms_reportCenter:get'], // 查看 报表中心下载配置信息
  reportCenterEdit: ['config_wms_reportCenter:edit'], // 编辑 报表中心下载配置信息
  steelInboundGet: ['config_wms_steelInbound:get'], // 查看 钢材入库配置信息
  steelInboundEdit: ['config_wms_steelInbound:edit'], // 编辑 钢材入库配置信息
  basicOutboundGet: ['config_wms_basicOutbound:get'], // 查看 基础出库配置信息
  basicOutboundEdit: ['config_wms_basicOutbound:edit'], // 编辑 基础出库配置信息
  partyABorrowReturnGet: ['config_wms_partyABorrowReturn:get'], // 查看 甲供借用归还配置信息
  partyABorrowReturnEdit: ['config_wms_partyABorrowReturn:edit'], // 编辑 甲供借用归还配置信息
  materialWeightingGet: ['config_wms_materialWeighting:get'], // 查看 物料加权配置信息
  materialWeightingEdit: ['config_wms_materialWeighting:edit'], // 编辑 物料加权配置信息
  // basicRejectGet: ['config_wms_basicReject:get'], // 查看 基础退货信息
  // basicRejectEdit: ['config_wms_basicReject:edit'] // 编辑 基础退货信息
  basicReturnGet: ['config_wms_basicReturn:get'], // 查看 退库审核配置
  basicReturnEdit: ['config_wms_basicReturn:edit'] // 编辑 退库审核配置
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

// WMS-配置管理/入库质检物料
export const configWmsInspectionRawMaterialPM = {
  get: ['config_wms_inspectionRawMaterial:get'], // 入库质检物料列表
  add: ['config_wms_inspectionRawMaterial:add'], // 添加入库质检物料
  del: ['config_wms_inspectionRawMaterial:del'] // 删除入库质检物料
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
    edit: ['enclosure_info_detail:edit'], // 修改围护详情
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

// --------------------------- 审批管理 start --------------------------

// 审批管理/公司审批流程
export const companyProcessConfigPM = {
  get: ['company_process_config:get'], // 列表
  add: ['company_process_config:add'], // 添加
  edit: ['company_process_config:edit'] // 修改
}

// --------------------------- 审批管理 end ----------------------------

// ########################################################################

// --------------------------- 供应链配置 start --------------------------

// 供应链配置/分包类别
export const subcontractConfigPM = {
  get: ['subcontract_config:get'], // 列表
  add: ['subcontract_config:add'], // 新增
  edit: ['subcontract_config:edit'], // 修改
  del: ['subcontract_config:del'] // 删除
}

// --------------------------- 供应链配置 end ----------------------------

// ########################################################################

// ########################################################################

// --------------------------- 项目管理配置 start --------------------------

// 项目管理配置/质安问题分类配置
export const projectProblemConfigPM = {
  get: ['project_problem_config:get'], // 列表
  add: ['project_problem_config:add'], // 新增
  edit: ['project_problem_config:edit'], // 修改
  del: ['project_problem_config:del'] // 删除
}

// 项目管理配置/签证原因分类配置
export const projectVisaReasonConfigPM = {
  get: ['project_visa_reason_config:get'], // 列表
  add: ['project_visa_reason_config:add'], // 新增
  edit: ['project_visa_reason_config:edit'], // 修改
  del: ['project_visa_reason_config:del'] // 删除
}

// --------------------------- 项目管理配置 end ----------------------------

// ########################################################################
