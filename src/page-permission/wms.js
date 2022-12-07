import { commonPM as SCM_COMMON_PM } from './supply-chain'

// --------------------------- 公共权限 start ------------------------------

export const commonPM = {
  // 采购合同详情
  purchaseOrderDetail: SCM_COMMON_PM.purchaseOrderDetail,
  // 原材料-备料详情
  rawMatPreparationReceiptDetail: ['wms_rawMat_preparation:detail'],
  // 原材料-入库单详情
  rawMatInboundReceiptDetail: ['wms_rawMat_inbound_review:detail'],
  // 原材料-出库单详情
  rawMatOutboundReceiptDetail: ['wms_rawMat_outbound_review:detail'],
  // 原材料-退库单详情
  rawMatReturnReceiptDetail: ['wms_rawMat_return_review:detail'],
  // 原材料-红冲单详情
  rawMatSupplementReceiptDetail: ['wms_report_rawMat_supplement_details:detail'],
  // 原材料-退货单详情
  rawMatRejectReceiptDetail: ['wms_rawMat_reject_review:detail'],
  // 原材料-调拨详情
  rawMatTransferReceiptDetail: ['wms_rawMat_transfer_review:detail'],
  // 原材料-查看原材料-物料冻结详情（所有冻结记录）
  rawMatMaterialFreezeDetail: ['wms_rawMat_material_freeze:detail'],
  // 备料单解冻(TODO:暂无备料单解冻)
  // requisitionsUnFreeze: ['wms_rawMat_freeze_list:unfreeze_requisitions'],
  // 出库解冻
  rawMatOutboundUnFreeze: ['wms_rawMat_freeze_list:unfreeze_outbound'],
  // 调拨解冻
  rawMatTransferUnFreeze: ['wms_rawMat_freeze_list:unfreeze_transfer'],
  // 退货解冻
  rawMatRejectUnFreeze: ['wms_rawMat_freeze_list:unfreeze_reject']
}

// --------------------------- 公共权限 end --------------------------------

// ########################################################################

// --------------------------- 入库模块 start ------------------------------

// 入库办理/钢材入库办理
export const steelInboundApplicationPM = ['wms_steelInbound_application:submit']

// 入库办理/辅材入库办理
export const auxMatInboundApplicationPM = ['wms_auxMatInbound_application:submit']

// 入库办理/气体入库办理
export const gasInboundApplicationPM = ['wms_gasInbound_application:submit']

// 入库办理/入库申请
export const rawMaterialInboundRecordPM = {
  get: ['wms_rawMat_inbound_record:get'],
  edit: ['wms_rawMat_inbound_record:edit'],
  del: ['wms_rawMat_inbound_record:del'],
  showAmount: ['wms_rawMat_inbound_record:showAmount'] // 显示金额
}

// 出入库审核/入库审核
export const rawMaterialInboundReviewPM = {
  get: ['wms_rawMat_inbound_review:get'],
  review: ['wms_rawMat_inbound_review:review'],
  showAmount: ['wms_rawMat_inbound_review:showAmount'] // 显示金额
}

// 质检管理/入库质检表
export const inboundInspectionPM = {
  get: ['wms_inbound_inspection:get'],
  detail: ['wms_inbound_inspection:detail'] // 详情
}

// 质检管理/入库质检明细
export const inboundInspectionDetailPM = {
  get: ['wms_inbound_inspection_detail:get']
}

// --------------------------- 入库模块 end --------------------------------

// ########################################################################

// --------------------------- 冻结模块 start ------------------------------

// 冻结管理/冻结列表
export const rawMaterialFreezeListPM = {
  get: ['wms_rawMat_freeze_list:get']
}

// 冻结管理/解冻记录
export const rawMaterialUnFreezeListPM = {
  get: ['wms_rawMat_unfreeze_list:get'],
  transferReceiptDetail: commonPM.rawMatTransferReceiptDetail, // 调拨详情
  outboundReceiptDetail: commonPM.rawMatOutboundReceiptDetail, // 出库详情
  rejectReceiptDetail: commonPM.rawMatRejectReceiptDetail, // 出库详情
  preparationReceiptDetail: commonPM.rawMatPreparationReceiptDetail // 备料详情
}

// --------------------------- 冻结模块 end --------------------------------

// ########################################################################

// --------------------------- 物料仓 start ------------------------------

// 物料仓/标签打印
export const materialLabelPrintPM = {
  get: ['wms_mat_labelPrint:get'] // 查看 标签打印
}

// 物料仓/出库记录
export const rawMaterialOutboundRecordPM = {
  get: ['wms_rawMat_outbound_record:get']
}

// 物料仓/钢材仓库
export const steelMaterialWarehousePM = {
  get: ['wms_matWarehouse_steel:get'], // 查看 钢材仓库
  outbound: ['wms_matWarehouse_steel:outbound'], // 添加钢材出库单
  outboundAudit: ['wms_matWarehouse_steel:outboundAudit'], // 钢材出库审核
  transfer: ['wms_matWarehouse_steel:transfer'], // 钢材调拨
  labelPrint: materialLabelPrintPM.get, // 标签打印
  outboundRecord: rawMaterialOutboundRecordPM.get, // 出库记录
  freezeRecord: rawMaterialFreezeListPM.get // 冻结记录
}

// 物料仓/辅材仓库
export const auxMatMaterialWarehousePM = {
  get: ['wms_matWarehouse_auxMaterial:get'], // 查看 辅材仓库
  outbound: ['wms_matWarehouse_auxMaterial:outbound'], // 添加辅材出库单
  outboundAudit: ['wms_matWarehouse_auxMaterial:outboundAudit'], // 辅材出库审核
  transfer: ['wms_matWarehouse_auxMaterial:transfer'], // 辅材调拨
  outboundRecord: rawMaterialOutboundRecordPM.get, // 出库记录
  freezeRecord: rawMaterialFreezeListPM.get // 冻结记录
}

// 物料仓/气体仓库
export const gasMaterialWarehousePM = {
  get: ['wms_matWarehouse_gas:get'], // 查看 气体仓库
  outbound: ['wms_matWarehouse_gas:outbound'], // 添加气体出库单
  outboundAudit: ['wms_matWarehouse_gas:outboundAudit'], // 气体出库审核
  transfer: ['wms_matWarehouse_gas:transfer'], // 气体调拨
  outboundRecord: rawMaterialOutboundRecordPM.get, // 出库记录
  freezeRecord: rawMaterialFreezeListPM.get // 冻结记录
}

// 物料仓/出库审核
export const rawMaterialOutboundReviewPM = {
  get: ['wms_rawMat_outbound_review:get'],
  review: ['wms_rawMat_outbound_review:review']
}

// --------------------------- 物料仓 end ----------------------------------

// ########################################################################

// --------------------------- 废料模块 start ------------------------------

// 废料管理/钢材列表
export const steelScrapPM = {
  get: ['wms_steel_scrap:get']
}

// --------------------------- 废料模块 end --------------------------------

// ########################################################################

// --------------------------- 调拨模块 start ------------------------------

// 物料仓/甲供材料借出管理
export const rawMaterialPartyABorrowPM = {
  get: ['wms_rawMat_transfer_partyABorrow:get'],
  return: ['wms_rawMat_transfer_partyABorrow:return'],
  transferReceiptDetail: commonPM.rawMatTransferReceiptDetail // 调拨详情
}

// 出入库审核/调拨审核
export const rawMaterialTransferReviewPM = {
  get: ['wms_rawMat_transfer_review:get'],
  review: ['wms_rawMat_transfer_review:review']
}

// --------------------------- 调拨模块 end --------------------------------

// ########################################################################

// --------------------------- 退库模块 start ------------------------------

// 退库办理/钢板退库办理
export const steelPlateReturnApplicationPM = ['wms_steelPlate_return_application:submit']

// 退库办理/钢板退库办理
export const sectionSteelReturnApplicationPM = ['wms_sectionSteel_return_application:submit']

// 退库办理/钢板退库办理
export const steelCoilReturnApplicationPM = ['wms_steelCoil_return_application:submit']

// 退库办理/辅材退库办理
export const auxMatReturnApplicationPM = ['wms_auxMat_return_application:submit']

// 退库办理/气体退库办理
export const gasReturnApplicationPM = ['wms_gas_return_application:submit']

// 组件·退库办理/可退库列表
export const rawMaterialReturnableListPM = {
  get: [
    ...steelPlateReturnApplicationPM,
    ...sectionSteelReturnApplicationPM,
    ...steelCoilReturnApplicationPM,
    ...auxMatReturnApplicationPM,
    ...gasReturnApplicationPM,
    'wms_rawMat_returnable:get'
  ], // 查看
  outboundReceiptDetail: commonPM.rawMatOutboundReceiptDetail // 出库详情
}

// 退库办理/退库申请
export const rawMaterialReturnRecordPM = {
  get: ['wms_rawMat_return_record:get'], // 查看
  edit: ['wms_rawMat_return_record:edit'], // 编辑
  del: ['wms_rawMat_return_record:del'], // 删除
  outboundReceiptDetail: commonPM.rawMatOutboundReceiptDetail // 出库详情
}

// 出入库办理/退库审核
export const rawMaterialReturnReviewPM = {
  get: ['wms_rawMat_return_review:get'], // 查看
  review: ['wms_rawMat_return_review:review'], // 审核
  outboundReceiptDetail: commonPM.rawMatOutboundReceiptDetail // 出库详情
}

// --------------------------- 退库模块 start ------------------------------

// ########################################################################

// --------------------------- 退货模块 start ------------------------------

// 退货办理/退货申请
export const rawMaterialRejectApplicationPM = {
  get: ['wms_rawMat_reject_application:get'], // 查看
  showAmount: ['wms_rawMat_reject_application:showAmount'], // 显示金额
  purchaseOrderDetail: commonPM.purchaseOrderDetail // 采购合同
}

// 退货办理/退货记录
export const rawMaterialRejectRecordPM = {
  get: ['wms_rawMat_reject_record:get'], // 查看
  del: ['wms_rawMat_reject_record:del'], // 删除
  showAmount: ['wms_rawMat_reject_record:showAmount'], // 显示金额
  inboundReceiptDetail: commonPM.rawMatInboundReceiptDetail, // 入库详情
  purchaseOrderDetail: commonPM.purchaseOrderDetail // 采购合同
}

// 出入库审核/退货审核
export const rawMaterialRejectReviewPM = {
  get: ['wms_rawMat_reject_review:get'], // 查看
  review: ['wms_rawMat_reject_review:review'], // 审核
  showAmount: ['wms_rawMat_reject_review:showAmount'], // 显示金额
  inboundReceiptDetail: commonPM.rawMatInboundReceiptDetail, // 入库详情
  purchaseOrderDetail: commonPM.purchaseOrderDetail // 采购合同
}

// --------------------------- 退货模块 end --------------------------------

// ########################################################################

// --------------------------- 报表中心 start ------------------------------

// 报表中心/原材料/归还甲方
export const operateRecordReturnToPartyAPM = {
  get: ['wms_rawMat_partyABorrow_return:get'], // 查看
  transferReceiptDetail: commonPM.rawMatTransferReceiptDetail // 调拨详情
}

// 报表中心/原材料/甲供买入
export const operateRecordPartyABuyInPM = {
  get: ['wms_rawMat_partyABorrow_buyIn:get'], // 查看
  transferReceiptDetail: commonPM.rawMatTransferReceiptDetail // 调拨详情
}

// 报表中心/原材料/入库单表
export const reportRawMaterialInboundReceiptPM = {
  get: ['wms_report_rawMat_inbound_receipt:get'], // 查看
  showAmount: ['wms_report_rawMat_inbound_receipt:showAmount'], // 显示金额
  purchaseOrderDetail: commonPM.purchaseOrderDetail // 采购合同详情
}

// 报表中心/原材料/入库明细
export const reportRawMaterialInboundDetailsPM = {
  get: ['wms_report_rawMat_inbound_details:get'], // 查看
  showAmount: ['wms_report_rawMat_inbound_details:showAmount'], // 显示金额
  purchaseOrderDetail: commonPM.purchaseOrderDetail, // 采购合同详情
  inboundReceiptDetail: commonPM.rawMatInboundReceiptDetail // 入库详情
}

// 报表中心/原材料/出库明细
export const reportRawMaterialOutboundDetailsPM = {
  get: ['wms_report_rawMat_outbound_details:get'], // 查看
  showAmount: ['wms_report_rawMat_outbound_details:showAmount'], // 显示金额
  outboundReceiptDetail: commonPM.rawMatOutboundReceiptDetail // 出库详情
}

// 报表中心/原材料/退库明细
export const reportRawMaterialReturnDetailsPM = {
  get: ['wms_report_rawMat_return_details:get'], // 查看
  showAmount: ['wms_report_rawMat_return_details:showAmount'], // 显示金额
  outboundReceiptDetail: commonPM.rawMatOutboundReceiptDetail, // 出库详情
  returnReceiptDetail: commonPM.rawMatReturnReceiptDetail // 退库详情
}

// 报表中心/原材料/调拨明细
export const reportRawMaterialTransferDetailsPM = {
  get: ['wms_report_rawMat_transfer_details:get'], // 查看
  showAmount: ['wms_report_rawMat_transfer_details:showAmount'], // 显示金额
  transferReceiptDetail: commonPM.rawMatTransferReceiptDetail // 退库详情
}

// 报表中心/原材料/红冲记录
export const reportRawMaterialSupplementDetailsPM = {
  get: ['wms_report_rawMat_supplement_details:get'], // 查看
  print: ['wms_report_rawMat_supplement_details:print'] // 打印红冲记录
}

// 报表中心/原材料/进销存记录
export const reportRawMaterialPsiPM = {
  get: ['wms_report_rawMat_psi:get'], // 查看
  detail: ['wms_report_rawMat_psi:detail'], // 详情
  download: ['wms_report_rawMat_psi:download'] // 下载
}

// 报表中心/原材料/退货单表
export const reportRawMaterialRejectReceiptPM = {
  get: ['wms_report_rawMat_reject_details:get'], // 查看
  showAmount: ['wms_report_rawMat_reject_details:showAmount'], // 显示金额
  purchaseOrderDetail: commonPM.purchaseOrderDetail, // 采购合同详情
  inboundReceiptDetail: commonPM.rawMatInboundReceiptDetail // 入库详情
}

// 报表中心/原材料/切割余料表
export const reportRawMaterialCutSurplusReceiptPM = {
  get: ['wms_report_rawMat_cut_surplus_receipt:get'] // 查看
}

// 报表中心/原材料/库存报表
export const reportRawMaterialInventoryPM = {
  get: ['reportRawMaterialInventoryPM:get'], // 查看
  download: ['reportRawMaterialInventoryPM:download'] // 下载
}

// 报表中心/原材料/收发存报表
export const reportRawMaterialSendAndReceiveStoragePM = {
  get: ['wms_report_rawMat_sendAndReceiveStorage:get'], // 查看
  showAmount: ['wms_report_rawMat_sendAndReceiveStorage:showAmount'] // 显示金额
}

// 报表中心/原材料/存货明细帐
export const reportRawMaterialInventoryDetailPM = {
  get: ['wms_report_rawMat_inventory_detail:get'], // 查看
  showAmount: ['wms_report_rawMat_inventory_detail:showAmount'], // 显示金额
  detail: ['wms_report_rawMat_inventory_detail:detail'], // 详情
  download: ['wms_report_rawMat_inventory_detail:download'] // 下载
}

// 报表中心/原材料/高新研发费
export const reportRawMaterialHighTechRDFeePM = {
  get: ['wms_report_rawMat_highTechRDFee:get'], // 查看
  setRdRate: ['wms_report_rawMat_highTechRDFee:setRdRate'] // 设置研发费占比
}

// ---------------------------- 报表中心 end -------------------------------

// ########################################################################

// ---------------------------- 库存预警 start -----------------------------

export const rawMaterialInventoryWarningPM = {
  get: ['wms_rawMat_inventoryWarning:get'], // 查看
  add: ['wms_rawMat_inventoryWarning:add'], // 添加
  edit: ['wms_rawMat_inventoryWarning:edit'], // 编辑
  del: ['wms_rawMat_inventoryWarning:del'] // 删除
}

// ---------------------------- 库存预警 end -------------------------------

// ########################################################################

// --------------------------- 其他模块/组件 start -------------------------

// 组件·物料基础信息
export const materialBaseInfoCPM = {
  frozenDetail: commonPM.rawMatMaterialFreezeDetail // 查看物料冻结详情
}

// 组件·物料冻结记录
export const materialFreezeRecordCPM = {
  outboundUnFreeze: commonPM.rawMatOutboundUnFreeze, // 出库解冻
  transferUnFreeze: commonPM.rawMatTransferUnFreeze, // 调拨解冻
  rejectUnFreeze: commonPM.rawMatRejectUnFreeze, // 退货解冻

  transferReceiptDetail: commonPM.rawMatTransferReceiptDetail, // 调拨详情
  outboundReceiptDetail: commonPM.rawMatOutboundReceiptDetail, // 出库详情
  preparationReceiptDetail: commonPM.rawMatPreparationReceiptDetail, // 备料详情
  rejectReceiptDetail: commonPM.rawMatRejectReceiptDetail // 退货单详情
}

// 组件·单据详情
export const receiptDetailCPM = {
  preparationReceiptDetail: commonPM.rawMatPreparationReceiptDetail, // 备料详情
  purchaseOrderDetail: commonPM.purchaseOrderDetail, // 订单详情
  inboundReceiptDetail: commonPM.rawMatInboundReceiptDetail, // 入库详情
  outboundReceiptDetail: commonPM.rawMatOutboundReceiptDetail, // 出库详情
  transferReceiptDetail: commonPM.rawMatTransferReceiptDetail, // 调拨详情
  returnReceiptDetail: commonPM.rawMatReturnReceiptDetail, // 退库详情
  supplementReceiptDetail: commonPM.rawMatSupplementReceiptDetail, // 红冲单详情
  rejectReceiptDetail: commonPM.rawMatRejectReceiptDetail // 退货单详情
}

// 组件·高新研发费设置（目前以组件形式展示，后期可能为页面）
export const highTechRDFeeConfCPM = {
  get: ['wms_highTechRDFeeConf:get', ...reportRawMaterialHighTechRDFeePM.get],
  set: ['wms_highTechRDFeeConf:set', ...reportRawMaterialHighTechRDFeePM.setRdRate]
}

// --------------------------- 其他模块/组件 end ---------------------------
