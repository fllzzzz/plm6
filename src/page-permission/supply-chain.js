// --------------------------- 公共权限 start ------------------------------

export const commonPM = {
  // 采购订单详情
  purchaseOrderDetail: ['scm_purchaseOrder:detail']
}

// wms
export const wmsCommonPM = {
  // 原材料-入库单详情
  rawMatInboundReceiptDetail: ['wms_rawMat_inbound_review:detail']
}

// --------------------------- 公共权限 end --------------------------------

// ########################################################################

// --------------------------- 采购订单 start ------------------------------

// 采购订单
export const purchaseOrderPM = {
  get: ['scm_purchaseOrder:get'], // 列表
  add: ['scm_purchaseOrder:add'], // 添加
  edit: ['scm_purchaseOrder:edit'], // 编辑
  editPurchaseStatus: ['scm_purchaseOrder:editPurchaseStatus'], // 修改采购状态
  download: ['scm_purchaseOrder:download'], // 下载
  del: ['scm_purchaseOrder:del'] // 删除
}

// --------------------------- 采购订单 end --------------------------------

// ########################################################################

// --------------------------- 采购对账 start --------------------------------

// 采购对账管理/入库记录
export const supplierMaterialInboundPM = {
  get: ['supplier_material_inbound:get'], // 入库记录列表
  detail: ['supplier_material_inbound:detail'], // 入库记录详情
  print: ['supplier_material_inbound:print'] // 打印入库记录详情
}

// 采购对账管理/付款台账
export const supplierMaterialPaymentPM = {
  get: ['supplier_material_payment:get'], // 付款台账列表
  settle: ['supplier_material_payment:settle'], // 订单结算申请
  detail: ['supplier_material_payment:detail'], // 入库/付款/收票记录
  print: ['supplier_material_payment:print'], // 打印下载付款台账
  application: {
    get: ['supplier_material_payment_application:get'], // 付款申请列表
    add: ['supplier_material_payment_application:add'], // 添加付款申请
    edit: ['supplier_material_payment_application:edit'], // 编辑付款申请
    del: ['supplier_material_payment_application:del'] // 删除付款申请
  },
  paymentLog: {
    get: ['supplier_material_paymentLog:get'] // 付款记录
  }
}

// 采购对账管理/验收记录
export const purchaseAcceptanceLogPM = {
  get: ['purchaseAcceptanceLogPM:get'], // 查看
  download: ['purchaseAcceptanceLogPM:download'] // 下载
}

// 采购对账管理/供应商对账
export const supplierReconciliationLogPM = {
  get: ['supplierReconciliationLogPM:get'], // 查看
  download: ['supplierReconciliationLogPM:download'] // 下载
}

// 物流对账管理/物流记录
export const supplierLogisticsLogPM = {
  get: ['supplier_logistics_Log:get'], // 物流记录列表
  detail: ['supplier_logistics_Log:detail'], // 物流记录详情
  print: ['supplier_logistics_Log:print'] // 打印物流记录
}

// 物流对账管理/付款明细
export const supplierLogisticsPaymentPM = {
  get: ['supplier_logistics_payment:get'], // 付款明细列表
  add: ['supplier_logistics_payment:add'], // 新增申请
  detail: ['supplier_logistics_payment:detail'], // 收票记录
  print: ['supplier_logistics_payment:print'], // 打印下载
  application: {
    get: ['supplier_logistics_payment_application:get'], // 申请记录列表
    edit: ['supplier_logistics_payment_application:edit'], // 编辑付款申请
    del: ['supplier_logistics_payment_application:del'] // 删除付款申请
  }
}

// --------------------------- 供应商 end ---------------------------------

// ########################################################################

// --------------------------- 物流订单 start ------------------------------

// 物流订单
export const logisticsOrderPM = {
  get: ['scm_logisticsOrder:get'], // 列表
  add: ['scm_logisticsOrder:add'], // 添加
  edit: ['scm_logisticsOrder:edit'], // 编辑
  del: ['scm_logisticsOrder:del'], // 删除
  inboundReceiptDetail: wmsCommonPM.rawMatInboundReceiptDetail, // 入库详情
  purchaseOrderDetail: commonPM.purchaseOrderDetail // 采购订单
}

// --------------------------- 物流订单 end --------------------------------

// ########################################################################

// --------------------------- 供应商 start --------------------------------

// 供应商
export const supplierPM = {
  get: ['scm_supplier:get'], // 列表
  add: ['scm_supplier:add'], // 添加
  edit: ['scm_supplier:edit'], // 编辑
  del: ['scm_supplier:del'], // 删除
  editStatus: ['scm_supplier:editStatus'], // 修改供应商状态
  downloadAttachments: ['scm_supplier:downloadAttachments'] // 下载供应商附件
}

// --------------------------- 供应商 end ---------------------------------

// ########################################################################

// --------------------------- 其他模块/组件 start -------------------------

// 组件·采购单选择， purchase-sn-select，purchase-detail-button
export const purchaseOrderDetailCPM = commonPM.purchaseOrderDetail

// --------------------------- 其他模块/组件 end ---------------------------

// ########################################################################

// --------------------------- 分包订单 start --------------------------------

// 分包订单
export const subcontractOrderPM = {
  get: ['subcontract_order:get'], // 列表
  add: ['subcontract_order:add'], // 添加
  edit: ['subcontract_order:edit'], // 编辑
  detail: ['subcontract_order:detail'] // 详情
}

// 分包订单付款台账
export const supplyChainSubcontractPaymentPM = {
  get: ['subcontract_chain_subcontract_payment:get'], // 列表
  payment: {
    get: ['subcontract_chain_subcontract_payment_manage:get'], // 付款记录列表
    print: ['subcontract_chain_subcontract_payment_manage:print'] // 付款记录打印下载
  },
  invoice: {
    get: ['subcontract_chain_subcontract_invoice:get'], // 收票记录列表
    print: ['subcontract_chain_subcontract_invoice:print'] // 收票记录打印下载
  }
}
// --------------------------- 分包订单 end ---------------------------------
