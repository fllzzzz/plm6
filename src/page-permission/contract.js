// 计划管理
// --------------------------- 公共权限 start ------------------------------

export const commonPM = {}

// --------------------------- 公共权限 end --------------------------------

// ########################################################################

// --------------------------- 合同管理 start ------------------------------

// 合同管理/项目列表
export const projectListPM = {
  get: ['contract_project:get'], // 项目列表
  add: ['contract_project:add'], // 新增项目
  edit: ['contract_project:edit'], // 修改项目
  editStatus: ['contract_project:editStatus'], // 修改状态
  print: ['contract_project:print'], // 打印
  download: ['contract_project:download'], // 下载
  del: ['contract_project:del'], // 删除项目
  detail: ['contract_project:detail'], // 项目详情
  changeAmount: ['contract_project:changeAmount'], // 合同金额修改
  // settle: ['contract_project:settle'], // 结算
  // variationChange: ['contract_project:variationChange'], // 签证变更
  editMember: ['contract_project:editMember'], // 修改成员
  completeList: {
    get: ['contract_project_complete:get'], // 可完工项目列表
    completeConfirm: ['contract_project_complete:completeConfirm']// 完工项目确认完工
  }
}

// 合同管理/合同变更
export const contractChangePM = {
  get: ['contract_change:get'], // 变更列表
  detail: ['contract_change:detail'], // 变更详情
  audit: ['contract_change:audit'] // 变更审核
}

// 合同管理/合同档案
export const contractRecordPM = {
  get: ['contract_record:get'], // 合同档案列表
  download: ['contract_record:download'], // 下载附件
  detail: ['contract_record:detail'] // 合同档案详情
}

// 合同管理/项目台账
export const contractLedgerPM = {
  get: ['contract_ledger:get'], // 项目台账列表
  amountLog: ['contract_ledger:amountLog'], // 合同额记录
  occurLog: ['contract_ledger:occurLog'], // 累计发生额
  print: ['contract_ledger:print'], // 打印下载
  collection: {
    get: ['contract_ledger_collection:get'], // 收款列表
    add: ['contract_ledger_collection:add'], // 收款添加
    edit: ['contract_ledger_collection:edit'], // 收款修改
    del: ['contract_ledger_collection:del'], // 收款删除
    audit: ['contract_ledger_collection:audit'], // 收款审核
    print: ['contract_ledger_collection:print'] // 打印下载
  },
  invoice: {
    get: ['contract_ledger_invoice:get'], // 开票列表
    add: ['contract_ledger_invoice:add'], // 开票添加
    edit: ['contract_ledger_invoice:edit'], // 开票修改
    del: ['contract_ledger_invoice:del'], // 开票删除
    audit: ['contract_ledger_invoice:audit'], // 开票审核
    print: ['contract_ledger_invoice:print'] // 打印下载
  },
  exportTaxRebate: {
    get: ['contract_ledger_tax:get'], // 出口退税列表
    add: ['contract_ledger_tax:add'], // 添加出口退税
    edit: ['contract_ledger_tax:edit'], // 修改出口退税
    del: ['contract_ledger_tax:del'], // 删除出口退税
    audit: ['contract_ledger_tax:audit'], // 审核出口退税
    print: ['contract_ledger_tax:print'] // 打印出口退税
  }
}

// 合同管理/收款台账
export const collectionLedgerPM = {
  collection: {
    get: ['collection_ledger_collection:get'], // 收款列表
    print: ['collection_ledger_collection:print'] // 打印下载
  },
  invoice: {
    get: ['collection_ledger_invoice:get'], // 开票列表
    print: ['collection_ledger_invoice:print'] // 打印下载
  }
}

// 合同管理/收款预警
export const collectionWarnPM = {
  get: ['collection_warn:get'], // 合同收款列表
  print: ['collection_warn:print'] // 打印下载
}
// --------------------------- 合同管理 end --------------------------------

// ########################################################################

// --------------------------- 供应商付款 start ------------------------------

// 供应商付款/采购合同
export const contractSupplierMaterialPM = {
  get: ['contract_supplier_material:get'], // 采购合同列表
  print: ['supplier_material_payment:print'], // 列表/入库/付款/收票打印下载付款
  detail: ['supplier_material_payment:detail'], // 入库/付款/收票记录
  payment: {
    get: ['contract_supplier_material_payment:get'], // 付款列表
    add: ['contract_supplier_material_payment:add'], // 付款添加
    edit: ['contract_supplier_material_payment:edit'], // 付款修改
    del: ['contract_supplier_material_payment:del'], // 付款删除
    detail: ['contract_supplier_material_payment:detail'], // 付款详情
    audit: ['contract_supplier_material_payment:audit'] // 付款审核
  },
  invoice: {
    get: ['contract_supplier_material_invoice:get'], // 收票列表
    add: ['contract_supplier_material_invoice:add'], // 收票添加
    edit: ['contract_supplier_material_invoice:edit'], // 收票修改
    del: ['contract_supplier_material_invoice:del'], // 收票删除
    audit: ['contract_supplier_material_invoice:audit'], // 收票审核
    detail: ['contract_supplier_material_invoice:detail'] // 收票详情
  }
}

// 供应商付款/制成品订单
export const contractSupplierProductPM = {
  get: ['contract_supplier_product:get'], // 制成品订单列表
  print: ['contract_supplier_product:print'], // 打印下载
  settleDetail: ['contract_supplier_product:settleDetail'], // 结算详情
  settleAudit: ['contract_supplier_product:settleAudit'], // 结算审核
  inbound: {
    get: ['contract_supplier_product_inbound:get'], // 入库记录列表
    print: ['contract_supplier_product_inbound:print'] // 入库记录打印下载
  },
  payment: {
    get: ['contract_supplier_product_payment:get'], // 付款列表
    detail: ['contract_supplier_product_payment:detail'], // 付款详情
    audit: ['contract_supplier_product_payment:audit'], // 付款审核
    print: ['contract_supplier_product_payment:print'] // 付款打印下载
  },
  invoice: {
    get: ['contract_supplier_product_invoice:get'], // 收票列表
    add: ['contract_supplier_product_invoice:add'], // 收票添加
    edit: ['contract_supplier_product_invoice:edit'], // 收票修改
    del: ['contract_supplier_product_invoice:del'], // 收票删除
    audit: ['contract_supplier_product_invoice:audit'], // 收票审核
    detail: ['contract_supplier_product_invoice:detail'], // 收票详情
    print: ['contract_supplier_product_invoice:print'] // 收票打印下载
  }
}

// 供应商付款/原材料物流
export const contractSupplierLogisticsPM = {
  get: ['contract_supplier_logistics:get'], // 原材料物流列表
  print: ['supplier_logistics_payment:print'], // 入库单/付款/收票打印下载
  detail: ['supplier_logistics_payment:detail'], // 关联入库/付款/收票记录
  payment: {
    get: ['contract_supplier_logistics_payment:get'], // 付款列表
    add: ['contract_supplier_logistics_payment:add'], // 付款添加
    edit: ['contract_supplier_logistics_payment:edit'], // 付款修改
    del: ['contract_supplier_logistics_payment:del'], // 付款删除
    detail: ['contract_supplier_logistics_payment:detail'], // 付款详情
    audit: ['contract_supplier_logistics_payment:audit'] // 付款审核
  },
  invoice: {
    get: ['contract_supplier_logistics_invoice:get'], // 收票列表
    add: ['contract_supplier_logistics_invoice:add'], // 收票添加
    edit: ['contract_supplier_logistics_invoice:edit'], // 收票修改
    del: ['contract_supplier_logistics_invoice:del'], // 收票删除
    audit: ['contract_supplier_logistics_invoice:audit'], // 收票审核
    detail: ['contract_supplier_logistics_invoice:detail'] // 收票详情
  }
}

// 供应商付款/制成品物流
export const contractSupplierProductLogisticsPM = {
  get: ['contract_supplier_product_logistics:get'], // 制成品物流列表
  print: ['supplier_product_logistics_payment:print'], // 物流记录/付款/收票打印下载
  detail: ['supplier_product_logistics_payment:detail'], // 物流/付款/收票记录
  payment: {
    get: ['contract_supplier_product_logistics_payment:get'], // 付款列表
    add: ['contract_supplier_product_logistics_payment:add'], // 付款添加
    edit: ['contract_supplier_product_logistics_payment:edit'], // 付款修改
    del: ['contract_supplier_product_logistics_payment:del'], // 付款删除
    detail: ['contract_supplier_product_logistics_payment:detail'], // 付款详情
    audit: ['contract_supplier_product_logistics_payment:audit'] // 付款审核
  },
  invoice: {
    get: ['contract_supplier_product_logistics_invoice:get'], // 收票列表
    add: ['contract_supplier_product_logistics_invoice:add'], // 收票添加
    edit: ['contract_supplier_product_logistics_invoice:edit'], // 收票修改
    del: ['contract_supplier_product_logistics_invoice:del'], // 收票删除
    audit: ['contract_supplier_product_logistics_invoice:audit'], // 收票审核
    detail: ['contract_supplier_product_logistics_invoice:detail'] // 收票详情
  }
}
// 供应商付款/分包订单
export const contractSupplierSubcontractPM = {
  get: ['contract_supplier_subcontract:get'], // 分包订单列表
  print: ['subcontract_chain_subcontract_payment:print'], // 付款/收票打印下载付款
  detail: ['subcontract_chain_subcontract_payment:detail'], // 付款/收票记录
  payment: {
    get: ['contract_supplier_subcontract_payment:get'], // 付款列表
    add: ['contract_supplier_subcontract_payment:add'], // 付款添加
    edit: ['contract_supplier_subcontract_payment:edit'], // 付款修改
    del: ['contract_supplier_subcontract_payment:del'], // 付款删除
    detail: ['contract_supplier_subcontract_payment:detail'], // 付款详情
    audit: ['contract_supplier_subcontract_payment:audit'] // 付款审核
  },
  invoice: {
    get: ['contract_supplier_subcontract_invoice:get'], // 收票列表
    add: ['contract_supplier_subcontract_invoice:add'], // 收票添加
    edit: ['contract_supplier_subcontract_invoice:edit'], // 收票修改
    del: ['contract_supplier_subcontract_invoice:del'], // 收票删除
    detail: ['contract_supplier_subcontract_invoice:detail'], // 收票详情
    audit: ['contract_supplier_subcontract_invoice:audit'] // 收票审核
  }
}

// 供应商付款/应付汇总
export const contractSupplierPayablePM = {
  get: ['contract_supplier_payable:get'], // 应付汇总列表
  print: ['contract_supplier_payable:print'] // 打印下载
}

// 供应商付款/付款台账
export const contractSupplierPaymentLedgerPM = {
  payment: {
    get: ['contract_supplier_payment_ledger_payment:get'], // 付款列表
    print: ['contract_supplier_payment_ledger_payment:print'] // 付款打印下载
  },
  invoice: {
    get: ['contract_supplier_payment_ledger_invoice:get'], // 收票列表
    print: ['contract_supplier_payment_ledger_invoice:print'] // 收票打印下载
  }
}
// --------------------------- 销售管理 end --------------------------------

// --------------------------- 销售管理 start ------------------------------

// 销售管理/价格管理
export const priceManagePM = {
  get: ['sale_price_manage:get'], // 列表
  cost: ['sale_price_manage:cost'], // 项目造价
  detail: ['sale_price_manage:detail'], // 详情
  save: ['sale_price_manage:edit'], // 录入价格
  list: ['sale_price_manage:modify'], // 变更列表
  audit: ['sale_price_manage:audit'], // 变更审核
  print: ['sale_price_manage:print'] // 结构围护配套件打印
}

// 销售管理/订单跟踪
export const orderTrackingPM = {
  get: ['order_tracking:get'], // 列表
  detail: ['order_tracking:detail'], // 详情
  print: ['order_tracking:print'] // 打印
}

// 销售管理/发运跟踪
export const shipmentTrackingPM = {
  get: ['shipment_tracking:get'], // 列表
  summary: ['shipment_tracking:summary'], // 汇总
  print: ['shipment_tracking:print'] // 打印
}

// 销售管理/客户交易记录
export const transactionRecordPM = {
  get: ['transaction_record:get'], // 列表
  detail: ['transaction_record:detail'], // 详情
  print: ['transaction_record:print'] // 打印
}

// 销售管理/签证管理
export const visaManagePM = {
  get: ['contract_visa_manage:get'], // 签证列表
  add: ['contract_visa_manage:add'], // 新增签证单
  edit: ['contract_visa_manage:edit'], // 修改签证单
  detail: ['contract_visa_manage:detail'], // 签证单详情
  audit: ['contract_visa_manage:audit'], // 审核签证单
  download: ['contract_visa_manage:download'] // 签证单下载
}

// 销售管理/结算管理
export const settlementManagePM = {
  get: ['contract_settlement_manage:get'], // 结算列表
  add: ['contract_settlement_manage:add'], // 新增结算单
  edit: ['contract_settlement_manage:edit'], // 修改结算单
  detail: ['contract_settlement_manage:detail'], // 结算单详情
  audit: ['contract_settlement_manage:audit'], // 审核结算单
  download: ['contract_settlement_manage:download'] // 结算单下载
}

// 销售管理/发运跟踪
export const deliveryTrackingPM = {
  get: ['delivery_tracking:get'] // 列表
}

// --------------------------- 销售管理 end --------------------------------

// --------------------------- 费用录入 start ------------------------------

// 费用录入/费用填报
export const expenseReportingPM = {
  get: ['expense_reporting:get'], // 列表
  add: ['expense_reporting:add'], // 新增
  edit: ['expense_reporting:edit'], // 修改
  del: ['expense_reporting:del'], // 删除
  print: ['expense_reporting:print'] // 打印
}

// 费用录入/厂房折旧
export const plantDepreciationPM = {
  get: ['plant_depreciation:get'], // 列表
  add: ['plant_depreciation:add'], // 新增
  edit: ['plant_depreciation:edit'], // 修改
  del: ['plant_depreciation:del'], // 删除
  print: ['plant_depreciation:print'], // 打印
  changeStatus: ['plant_depreciation:change_status'] // 更改状态
}

// 费用录入/设备折旧
export const contractDeviceDepreciationPM = {
  get: ['contract_device_depreciation:get'], // 列表
  add: ['contract_device_depreciation:add'], // 新增
  edit: ['contract_device_depreciation:edit'], // 修改
  del: ['contract_device_depreciation:del'], // 删除
  print: ['contract_device_depreciation:print'], // 打印
  import: ['contract_device_depreciation:import'], // 导入
  download: ['contract_device_depreciation:download'], // 下载
  changeStatus: ['contract_device_depreciation:change_status'] // 更改状态
}

// 费用录入/水电费
export const waterElectricityCostPM = {
  get: ['water_electricity_cost:get'], // 列表
  add: ['water_electricity_cost:add'], // 新增
  edit: ['water_electricity_cost:edit'], // 修改
  del: ['water_electricity_cost:del'], // 删除
  print: ['water_electricity_cost:print'] // 打印
}

// 费用录入/气体
export const gasCostPM = {
  get: ['gas_cost:get'], // 列表
  add: ['gas_cost:add'], // 新增
  edit: ['gas_cost:edit'], // 修改
  del: ['gas_cost:del'], // 删除
  print: ['gas_cost:print'] // 打印
}

// 费用录入/摊销管理
export const amortizationManagePM = {
  get: ['amortization_manage:get'], // 列表
  detail: ['amortization_manage:detail'], // 详情
  set: ['amortization_manage:set'], // 摊销设置
  amortization: ['amortization_manage:amortization'] // 摊销
}

// 费用录入/管理费/员工工资
export const salaryCostPM = {
  get: ['salary_cost:get'], // 列表
  add: ['salary_cost:add'], // 新增
  edit: ['salary_cost:edit'], // 修改
  del: ['salary_cost:del'], // 删除
  print: ['salary_cost:print'] // 打印
}

// 费用录入/管理费/物业费
export const propertyCostPM = {
  get: ['property_cost:get'], // 列表
  add: ['property_cost:add'], // 新增
  edit: ['property_cost:edit'], // 修改
  del: ['property_cost:del'], // 删除
  print: ['property_cost:print'] // 打印
}

// 费用录入/检测费
export const expenseTestingCostPM = {
  get: ['expense_testing_cost:get'], // 列表
  add: ['expense_testing_cost:add'], // 新增
  detail: ['expense_testing_cost:detail'] // 详情
}

// --------------------------- 费用录入 end --------------------------------

// --------------------------- 业财报表 start ------------------------------

// 业财报表
export const fortuneReportPM = {
  get: ['fortune_report_cost:get'], // 列表
  print: ['fortune_report_cost:print'], // 打印
  detail: ['fortune_report_cost:detail'], // 详情
  printDetail: ['fortune_report_cost:printDetail'] // 详情打印
}

// --------------------------- 业财报表 end --------------------------------
