// --------------------------- 公共权限 start ------------------------------

export const commonPM = {
  // 采购订单详情
  purchaseOrderDetail: ['scm_purchaseOrder:detail']
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

// --------------------------- 物流订单 start ------------------------------

// 物流订单
export const logisticsOrderPM = {
  get: ['scm_logisticsOrder:get'], // 列表
  add: ['scm_logisticsOrder:add'], // 添加
  edit: ['scm_logisticsOrder:edit'], // 编辑
  del: ['scm_logisticsOrder:del'] // 删除
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

// --------------------------- 供应商 end ----------------------------------

