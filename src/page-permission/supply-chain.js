// --------------------------- 公共权限 start ------------------------------

export const commonPM = {
  // 采购合同详情
  purchaseOrderDetail: ['scm_purchaseOrder:detail']
}

// wms
export const wmsCommonPM = {
  // 原材料-入库单详情
  rawMatInboundReceiptDetail: ['wms_rawMat_inbound_review:detail']
}

// --------------------------- 公共权限 end --------------------------------

// ########################################################################

// --------------------------- 采购合同 start ------------------------------

// 采购合同
export const purchaseOrderPM = {
  get: ['scm_purchaseOrder:get'], // 列表
  add: ['scm_purchaseOrder:add'], // 添加
  edit: ['scm_purchaseOrder:edit'], // 编辑
  editPurchaseStatus: ['scm_purchaseOrder:editPurchaseStatus'], // 修改采购状态
  // download: ['scm_purchaseOrder:download'], // 下载
  del: ['scm_purchaseOrder:del'] // 删除
}

// --------------------------- 采购合同 end --------------------------------

// ########################################################################

// --------------------------- 申购订单 start ------------------------------

// 申购订单
export const scmRequisitionsPM = {
  get: ['scm_requisitions:get'], // 列表
  add: ['scm_requisitions:add'], // 添加
  detail: ['scm_requisitions:detail'], // 详情
  print: ['scm_requisitions:print'], // 打印
  del: ['scm_requisitions:del'] // 撤销
}

// --------------------------- 申购订单 end --------------------------------

// ########################################################################

// --------------------------- 采购对账 start --------------------------------

// 采购台账管理/入库记录
export const supplierMaterialInboundPM = {
  get: ['supplier_material_inbound:get'], // 入库记录列表
  detail: ['supplier_material_inbound:detail'], // 入库记录详情
  print: ['supplier_material_inbound:print'] // 打印入库记录详情
}

// 采购台账管理/采购台账
export const supplierMaterialPaymentPM = {
  get: ['supplier_material_payment:get'], // 采购台账列表
  detail: ['supplier_material_payment:detail'], // 入库/付款/收票记录
  print: ['supplier_material_payment:print'] // 列表/入库/付款/收票打印下载
}

// 采购台账管理/订单查询
export const supplierOrderLedgerPM = {
  get: ['supplier_order_ledger:get'], // 列表
  detail: ['supplier_order_ledger:detail'] // 入库记录
}

// 采购台账管理/验收记录
export const purchaseAcceptanceLogPM = {
  get: ['purchase_acceptance_log:get'], // 查看
  download: ['purchase_acceptance_log:download'] // 下载
}

// 采购台账管理/供应商对账
export const supplierReconciliationLogPM = {
  get: ['supplier_reconciliation_Log:get'], // 查看
  download: ['supplier_reconciliation_Log:download'] // 下载
}

// 物流台账/物流记录
export const supplierLogisticsLogPM = {
  get: ['supplier_logistics_Log:get'], // 物流记录列表
  detail: ['supplier_logistics_Log:detail'], // 物流记录详情
  print: ['supplier_logistics_Log:print'] // 打印物流记录
}

// 原材料物流
export const supplierLogisticsPaymentPM = {
  get: ['supplier_logistics_payment:get'], // 原材料物流列表
  detail: ['supplier_logistics_payment:detail'], // 关联供方/付款/收票记录
  print: ['supplier_logistics_payment:print'], // 入库单付款收票记录打印
  freightChangeAdd: ['supplier_logistics_payment:freightChangeAdd'], // 物流费用变更新增
  freightChangeAudit: ['supplier_logistics_payment:freightChangeAudit'] // 物流费用审核
}

// 制成品物流
export const supplierProductLogisticsPaymentPM = {
  get: ['supplier_product_logistics_payment:get'], // 制成品物流列表
  detail: ['supplier_product_logistics_payment:detail'], // 物流/付款/收票记录
  print: ['supplier_product_logistics_payment:print'] // 物流/付款/收票记录打印
}

// --------------------------- 供应商 end ---------------------------------

// ########################################################################

// --------------------------- 物流订单 start ------------------------------

// 物流订单
export const logisticsOrderPM = {
  get: ['scm_logisticsOrder:get'], // 列表
  inboundReceiptDetail: wmsCommonPM.rawMatInboundReceiptDetail, // 入库详情
  purchaseOrderDetail: commonPM.purchaseOrderDetail // 采购合同
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

// 组件·采购合同选择， purchase-sn-select，purchase-detail-button
export const purchaseOrderDetailCPM = commonPM.purchaseOrderDetail

// --------------------------- 其他模块/组件 end ---------------------------

// ########################################################################

// --------------------------- 分包订单 start --------------------------------

// 分包订单
export const subcontractOrderPM = {
  get: ['subcontract_order:get'], // 列表
  add: ['subcontract_order:add'], // 添加
  edit: ['subcontract_order:edit'], // 编辑
  del: ['subcontract_order:del'], // 删除
  detail: ['subcontract_order:detail'] // 详情
}

// 分包订单/付款台账
export const supplyChainSubcontractPaymentPM = {
  get: ['subcontract_chain_subcontract_payment:get'], // 列表
  detail: ['subcontract_chain_subcontract_payment:detail'], // 付款开票记录
  print: ['subcontract_chain_subcontract_payment:print'] // 付款开票记录打印
}
// --------------------------- 分包订单 end ---------------------------------
