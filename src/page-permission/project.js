
// ########################################################################

// --------------------------- 进度管理 start ------------------------------

// 进度状态
export const projectProgressPM = {
  get: ['project_progress:get'] // 列表
}
// --------------------------- 进度管理 end ---------------------------------

// ########################################################################

// --------------------------- 分包管理 start ------------------------------

// 工期制定
export const subcontractPlanPM = {
  get: ['subcontract_plan:get'], // 列表
  edit: ['subcontract_plan:edit'], // 修改
  account: ['subcontract_plan:account'] // 分包登陆账号
}

// 计划跟踪
export const subcontractProgressPM = {
  get: ['subcontract_progress:get'], // 列表
  detail: ['subcontract_progress:detail'] // 详情
}

// 分包订单付款申请
export const subcontractOrderPaymentPM = {
  get: ['subcontract_order_payment:get'], // 列表
  paymentManage: {
    get: ['subcontract_order_payment_manage:get'], // 付款记录
    add: ['subcontract_order_payment_manage:add'], // 新增付款申请
    edit: ['subcontract_order_payment_manage:edit'], // 修改付款申请
    del: ['subcontract_order_payment_manage:del'],
    detail: ['subcontract_order_payment_manage:detail'], // 付款申请详情
    print: ['subcontract_order_payment_manage:print'] // 付款记录打印下载
  },
  invoice: ['subcontract_order_payment:invoice'], // 收票记录
  invoicePrint: ['subcontract_order_payment:invoicePrint'], // 收票记录打印下载
  settle: ['subcontract_order_payment:settle'] // 新增结算申请
}

// 分包订单签证
export const subcontractVisaManagePM = {
  get: ['subcontract_visa_manage:get'], // 列表
  detail: ['subcontract_visa_manage:detail'], // 分包签证详情
  audit: ['subcontract_visa_manage:audit'], // 分包签证审核
  download: ['subcontract_visa_manage:download'] // 下载分包签证
}

// 质安管理
export const qualitySafetyPM = {
  get: ['quality_safety_manage:get'], // 列表
  detail: ['quality_safety_manage:detail'], // 问题整改详情
  audit: ['quality_safety_manage:audit'], // 问题整改审核
  download: ['quality_safety_manage:download'], // 下载问题报告
  close: ['quality_safety_manage:close'] // 关闭问题报告
}

// 签证结算
export const visaSettlePM = {
  get: ['visa_settle:get'], // 列表
  add: ['visa_settle:add'], // 新增签证或结算
  edit: ['visa_settle:edit'], // 编辑签证或结算
  detail: ['visa_settle:detail'], // 签证或结算详情
  audit: ['visa_settle:audit'], // 签证或结算审核
  download: ['visa_settle:download'] // 下载签证或结算
}

// --------------------------- 分包管理 end ---------------------------------

// ########################################################################

// --------------------------- 收货管理 start ------------------------------

// 自制收货
export const homemadeDeliveryPM = {
  get: ['homemade_delivery:get'], // 列表
  detail: ['homemade_delivery:detail'], // 收货详情
  print: ['homemade_delivery:print'], // 自制收货详情打印
  audit: ['homemade_delivery:audit'] // 收货确认
}

// 外购收货
export const outsourceDeliveryPM = {
  get: ['outsource_delivery:get'], // 列表
  edit: ['outsource_delivery:edit'], // 收货确认
  record: ['outsource_delivery:record'] // 收货记录
}

// 收货报表
export const deliveryReportListPM = {
  get: ['delivery_report_list:get'], // 列表
  print: ['delivery_report_list:print'] // 打印下载
}

// 收货看板
export const deliveryReportDashboardPM = {
  get: ['delivery_report_dashboard:get'] // 列表
}

// 收安报表
export const deliveryInstallListPM = {
  get: ['delivery_install_list:get'], // 列表
  print: ['delivery_install_list:print'] // 打印下载
}

// --------------------------- 收货管理 end ---------------------------------

// ########################################################################

// --------------------------- 安装管理 start ------------------------------

// 安装填报
export const handleInstallPM = {
  get: ['handle_install:get'], // 列表
  add: ['handle_install:add'] // 安装填报申请
}

// 安装审核
export const installAuditPM = {
  get: ['install_audit:get'], // 列表
  detail: ['install_audit:detail'],
  audit: ['install_audit:audit'] // 安装审核
}

// 安装报表
export const installReportListPM = {
  get: ['install_report_list:get'], // 列表
  print: ['install_report_list:print'] // 打印下载
}

// 安装看板
export const installReportDashboardPM = {
  get: ['install_report_dashboard:get'] // 列表
}
// --------------------------- 安装管理 end ---------------------------------

// ########################################################################

// --------------------------- 资料管理 start ------------------------------

// 形象进度
export const imageProgressPM = {
  get: ['image_progress:get'], // 列表
  download: ['image_progress:download']
}

// 施工日志
export const constructionLogPM = {
  get: ['construction_log:get'], // 列表
  add: ['construction_log:add'], // 施工日志添加修改
  detail: ['construction_log:detail'], // 施工日志详情
  download: ['construction_log:download'] // 批量下载施工日志
}

// 施工资料
export const constructionDataPM = {
  get: ['construction_data:get'], // 列表
  add: ['construction_data:add'], // 施工资料添加
  del: ['construction_data:del'] // 施工资料删除
}

// --------------------------- 资料管理 end ---------------------------------

// ########################################################################

// --------------------------- 安装设置 start ------------------------------

// 安装设置
export const installConfigPM = {
  auxiliaryMaterial: {
    get: ['install_config_auxiliary_material:get'], // 配套件安装设置列表
    edit: ['install_config_auxiliary_material:edit'] // 配套件安装设置修改
  },
  reportMethod: {
    get: ['install_config_report_method:get'], // 安装填报方式
    edit: ['install_config_report_method:edit'] // 安装填报方式修改
  }
}

// --------------------------- 安装设置 end ---------------------------------
