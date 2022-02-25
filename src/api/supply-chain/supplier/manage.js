import request from '@/utils/request'

/**
 * 供应商管理列表
 * @param {Number} page 页码
 * @param {Number} size 每页数量
 * @param {Number} enabled 0：显示，1：隐藏
 * @returns
 */
export function get(params) {
  return request({
    module: 'scm',
    url: 'supplier',
    method: 'get',
    params
  })
}

/**
 * 供应商详情
 * @param {Number} id 供应商id
 * @returns
 */
export function detail(id) {
  return request({
    module: 'scm',
    url: `supplier/${id}`,
    method: 'get'
  })
}

/**
 *新建\编辑供应商
 * @export
 * @param {Number} id 供应商id（仅编辑）
 * @param {String} name 供应商名称
 * @param {String} shortName 供应商简称
 * @param {Number} supplierClassification 供应商分类
 * @param {Number} countryId 国家id
 * @param {Number} provinceId 省id
 * @param {Number} cityId 市id
 * @param {Number} regionId 区id
 * @param {String} address 详细地址
 * @param {Array} attachments 附件id集合
 * @param {String} businessTerm 营业期限
 * @param {String} companyEmail 公司邮箱
 * @param {String} companyPhone 单位电话
 * @param {String} enterpriseType 企业类型（对应字典value）
 * @param {String} legalRepresentative 法人代表人名字
 * @param {String} mainBusiness 公司主营业务
 * @param {String} registeredCapital 注册资本
 * @param {String} registrationDate 成立日期
 * @param {String} firstBankAccount 银行账号
 * @param {String} firstBankName 开户行
 * @param {String} firstContact 联系人1
 * @param {String} firstContactEmail 联系人1邮箱
 * @param {String} firstContactPhone 联系人1电话
 * @param {String} secondContact 联系人2
 * @param {String} secondContactEmail 联系人2邮箱
 * @param {String} socialCode 社会统一代码
 * @param {String} website 网址
 * @returns
 */
export function add(data) {
  return request({
    module: 'scm',
    url: 'supplier',
    method: 'post',
    data
  })
}

/**
 * 编辑供应商
 * @export
 * @returns
 */
export function edit(data) {
  return request({
    module: 'scm',
    url: 'supplier',
    method: 'put',
    data
  })
}

/**
 * 批量新增供应商
 * @export
 * @returns
 */
export function batchAdd(data) {
  return request({
    module: 'scm',
    url: 'supplier/batch',
    method: 'post',
    data
  })
}

/**
 * 修改隐藏状态
 * @export
 * @param {*} data
 * @returns
 */
export function editStatus(data) {
  return request({
    module: 'scm',
    headers: { 'Content-Type': 'application/x-www-form-urlencoded' },
    url: `supplier/enabled`,
    cancelKey: false,
    method: 'put',
    data
  })
}

export function del(ids) {
  return request({
    module: 'scm',
    url: 'supplier',
    method: 'delete',
    data: ids
  })
}

export default { get, add, detail, edit, batchAdd, del }
