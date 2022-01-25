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
  download: ['contract_project:download'], // 下载
  del: ['contract_project:del'], // 删除项目
  detail: ['contract_project:detail'], // 项目详情
  changeAmount: ['contract_project:changeAmount'], // 合同金额修改
  settle: ['contract_project:settle'], // 结算
  editMember: ['contract_project:editMember'] // 修改成员
}

// 合同管理/合同变更
export const contractChangePM = {
  get: ['contract_change:get'], // 变更列表
  detail: ['contract_project:detail'], // 变更详情
  audit: ['contract_project:audit'] // 变更审核
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
  print: ['contract_ledger:print'], // 打印下载
  collection: {
    get: ['contract_ledger_collection:get'], // 收款列表
    add: ['contract_ledger_collection:add'], // 收款添加
    edit: ['contract_ledger_collection:edit'], // 收款修改
    del: ['contract_ledger_collection:del'], // 收款删除
    audit: ['contract_ledger_collection:audit'] // 收款审核
  },
  invoice: {
    get: ['contract_ledger_invoice:get'], // 开票列表
    add: ['contract_ledger_invoice:add'], // 开票添加
    edit: ['contract_ledger_invoice:edit'], // 开票修改
    del: ['contract_ledger_invoice:del'], // 开票删除
    audit: ['contract_ledger_invoice:audit'] // 开票审核
  }
}

// 合同管理/收款台账
export const collectionLedgerPM = {
  collection: {
    get: ['collection_ledger_collection:get'] // 收款列表
  },
  invoice: {
    get: ['collection_ledger_invoice:get'] // 开票列表
  }
}

// 合同管理/合同预警
export const collectionWarnPM = {
  get: ['collection_warn:get'] // 合同预警列表
}
// --------------------------- 合同管理 end --------------------------------
