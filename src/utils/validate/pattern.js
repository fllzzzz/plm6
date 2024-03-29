/**
 * 正则工具类
 * author: duhh
 */

// 用户名 4-16位字母或数字，首位为字母
const validatorUsername = {
  pattern: /^[a-zA-Z][0-9a-zA-Z]{3,16}$/,
  message: '4-16位字母或数字，首位为字母'
}

// 密码正则 6-20位字母数字组合
const validatorPwd = /^(?![0-9]+$)(?![a-zA-Z]+$)[0-9A-Za-z]{6,20}$/

// 身份证号
const validatorIDCard = /^[1-9]\d{7}((0\d)|(1[0-2]))(([0|1|2]\d)|3[0-1])\d{3}$|^[1-9]\d{5}[1-9]\d{3}((0\d)|(1[0-2]))(([0|1|2]\d)|3[0-1])\d{3}([0-9]|X)$/

// 邮箱
// const validatorEmail = /^(([^<>()\[\]\\.,;:\s@"]+(\.[^<>()\[\]\\.,;:\s@"]+)*)|(".+"))@((\[[0-9]{1,3}\.[0-9]{1,3}\.[0-9]{1,3}\.[0-9]{1,3}\])|(([a-zA-Z\-0-9]+\.)+[a-zA-Z]{2,}))$/
const validatorEmail = /^[\w!#$%&'*+/=?^_`{|}~-]+(?:\.[\w!#$%&'*+/=?^_`{|}~-]+)*@(?:[\w](?:[\w-]*[\w])?\.)+[\w](?:[\w-]*[\w])?$/

// 手机号
const validatorPhone = /^1([38]\d|4[014-9]|5[0-35-9]|6[2567]|7[0-8]|9[0-35-9])\d{8}$/

// 手机 + 固话
const validatorTel = /^(0\d{2}-\d{8}(-\d{1,4})?)|(0\d{3}-\d{7,8}(-\d{1,4})?)$|1([38]\d|4[014-9]|5[0-35-9]|6[2567]|7[0-8]|9[0-35-9])\d{8}$/

// 域名验证
const validatorDomain = /^(?=^.{3,255}$)(www\.)?[a-zA-Z0-9][-a-zA-Z0-9]{0,62}(\.[a-zA-Z0-9][-a-zA-Z0-9]{0,62})+(:\d+)*(\/\w+\.\w+)*\/?$/

// 网址验证
const validatorWebsite = /^(?=^.{3,255}$)(http(s)?:\/\/)?(www\.)?[a-zA-Z0-9][-a-zA-Z0-9]{0,62}(\.[a-zA-Z0-9][-a-zA-Z0-9]{0,62})+(:\d+)*(\/\w+\.\w+)*$/

// 发票编号(数字 + 特殊字符)
const validatorInvoiceNo = /^[0-9!#$%&@',.*+-/=?^_`{|}~]+$/

// 车牌号（新能源 + 非新能源）TODO:正则有问题 浙A·88888 输入浙A88888，自动转换成浙A·88888
const patternLicensePlate = /^[京津晋冀蒙辽吉黑沪苏浙皖闽赣鲁豫鄂湘粤桂琼渝川贵云藏陕甘青宁新][ABCDEFGHJKLMNPQRSTUVWXY]·?[A-Z0-9]{5}[挂学警港澳]?$/

// ----------------------------  数字 start  ---------------------------------------

// 小数精度范围
const patternDP = /^([0-9]|1[0-9]|20)$/

// 数值
const patternNumerical = /^-?[0-9]+.?[0-9]*$/

// 正整数(不含0)
const validatorPositiveInt = /^[1-9]\d*$/

// 自然数 / 正整数(含0)
const validatorNatural = /^[+]{0,1}(\d+)$/

// 1-100整数
const validatorOneToHundred = /^(1|([1-9]\d{0,1})|100)$/

// 100-1000
const validatorMoreHundred = /^(100|([1-9][0-9]\d{1})|1000)$/

// 正数
const positiveNumPattern = /^(?!(0[0-9]{0,}$))[0-9]{1,}[.]{0,}[0-9]{0,}$/

// ----------------------------  数字 end  ---------------------------------------

// ----------------------------  字符组合 start  ----------------------------------

// 大小写字母
const validatorEn = {
  pattern: /^[A-Za-z]+$/,
  message: '请填写英文'
}

// 中文
const validatorCN = {
  pattern: /^[\u4e00-\u9fa5]+$/,
  message: '请填写汉字'
}

// 汉字、英文、数字
const validatorCN_EN_NUM = {
  pattern: /^[\u4E00-\u9FA5A-Za-z0-9]+$/,
  message: '请填写汉字、英文或数字'
}

// 英文或数字
const validatorEnOrNum = {
  pattern: /^[A-Za-z0-9]+$/,
  message: '请填写英文或数字'
}

// ----------------------------  字符组合 end -----------------------------------

export {
  patternDP,
  patternNumerical,
  positiveNumPattern,
  validatorEnOrNum,
  validatorUsername,
  validatorCN_EN_NUM,
  validatorCN,
  validatorEmail,
  validatorPhone,
  validatorTel,
  validatorPwd,
  validatorPositiveInt,
  validatorNatural,
  validatorOneToHundred,
  validatorIDCard,
  validatorMoreHundred,
  validatorEn,
  validatorDomain,
  validatorWebsite,
  patternLicensePlate,
  validatorInvoiceNo
}
