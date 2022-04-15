import { isNotBlank } from '@data-type/index'
import { fieldTypeEnum, dataSourceEnum } from '@/utils/print/enum'
import { patternLicensePlate, validatorPhone, validatorTel } from '@/utils/validate/pattern'
import enumAll from '@/utils/enum/all'
import enumOperate from '@/utils/enum'
import Random from './data'
import Mock from 'mockjs'

/**
 * 随机生成模板数据-列表
 * @param {object} fields 字段
 * @param {object} extraFields 额外字段
 * @param {number} number = 1 生成数量
 * @returns
 */
function getList({ fields, extraFields = [], number = 1 }) {
  const data = []
  if (!isNotBlank(fields)) {
    return data
  }
  for (let i = 0; i < number; i++) {
    const _data = {}
    fields.forEach((field) => {
      const key = field.key
      if (isNotBlank(key) && typeof key === 'string' && field.source === dataSourceEnum.SYSTEM.V) {
        setFieldData(_data, field)
      }
    })
    extraFields.forEach((field) => {
      const key = field.key
      if (isNotBlank(key) && typeof key === 'string') {
        setFieldData(_data, field)
      }
    })
    data.push(_data)
  }
  return data
}

/**
 * 随机生成模板数据-单条数据
 * @param {object} fields 字段
 * @param {object} extraFields 额外字段
 * @returns
 */
function getOne({ fields, extraFields = [] }) {
  const data = {}
  if (!isNotBlank(fields)) {
    return data
  }
  fields.forEach((field) => {
    const key = field.key
    if (isNotBlank(key) && typeof key === 'string' && field.source === dataSourceEnum.SYSTEM.V) {
      setFieldData(data, field)
    }
  })
  extraFields.forEach((field) => {
    const key = field.key
    if (isNotBlank(key) && typeof key === 'string') {
      setFieldData(data, field)
    }
  })
  return data
}

function setFieldData(data, field) {
  const keys = field.key.split('.')
  let _d = data
  keys.forEach((k, i) => {
    if (!_d[k]) {
      _d[k] = {}
    }
    if (i === keys.length - 1) _d[k] = getDataByType(field)
    _d = _d[k]
  })
}

/**
 * 根据字段类型生成数据
 * key.key 未做解析（暂不做处理）
 * @param {object} field 字段
 * @returns
 */
function getDataByType(field) {
  let _md
  switch (field.type) {
    case fieldTypeEnum.BLANK.K:
      _md = ''
      break
    case fieldTypeEnum.GUID.K:
      _md = Random.guid()
      break
    case fieldTypeEnum.STRUCTURE_NAME.K:
      _md = Random.structureName()
      break
    case fieldTypeEnum.ENCLOSURE_NAME.K:
      _md = Random.enclosureName()
      break
    // case fieldTypeEnum.SEGMENTS_NAME.K: _md = Random.segmentsName()
    //   break
    // case fieldTypeEnum.SINGLE_ELEMENT_NAME.K: _md = Random.singleElementName()
    //   break
    case fieldTypeEnum.MONOMER_NAME.K:
      _md = Random.monomerName()
      break
    case fieldTypeEnum.AREA_NAME.K:
      _md = Random.areaName()
      break
    case fieldTypeEnum.FACTORY_NAME.K:
      _md = Random.factoryName()
      break
    case fieldTypeEnum.WAREHOUSE_NAME.K:
      _md = Random.warehouseName()
      break
    case fieldTypeEnum.WORKSHOP.K:
      _md = Random.workshop()
      break
    case fieldTypeEnum.PRODUCTION_LINE.K:
      _md = Random.productionLine()
      break
    case fieldTypeEnum.TEAM_NAME.K:
      _md = Random.componentProcess() + '/' + Random.cname()
      break
    case fieldTypeEnum.COMPONENT_PROCESS.K:
      _md = Random.componentProcess()
      break
    // case fieldTypeEnum.STRUCTURE_PROCESS.K: _md = Random.structureProcess()
    //   break
    // case fieldTypeEnum.ENCLOSURE_PROCESS.K: _md = Random.enclosureProcess()
    //   break
    // case fieldTypeEnum.MACHINE_PART_PROCESS.K: _md = Random.machinePartProcess()
    //   break
    // case fieldTypeEnum.SEGMENTS_PROCESS.K: _md = Random.segmentsProcess()
    //   break
    // case fieldTypeEnum.SINGLE_ELEMENT_PROCESS.K: _md = Random.singleElementProcess()
    //   break
    case fieldTypeEnum.USER_NAME.K:
      _md = Random.cname()
      break
    case fieldTypeEnum.COMPANY_NAME.K:
      _md = Random.cpname()
      break
    case fieldTypeEnum.DEPT.K:
      _md = Random.dept()
      break
    case fieldTypeEnum.ORDER_TYPE.K:
      _md = Random.orderType()
      break
    case fieldTypeEnum.STRUCTURE_TYPE.K:
      _md = Random.structureType()
      break
    case fieldTypeEnum.REIMBURSEMENT_TYPE.K:
      _md = Random.reimbursementType()
      break
    case fieldTypeEnum.SUPPLIER_FEE_TYPE.K:
      _md = Random.supplierFeeType()
      break
    case fieldTypeEnum.PAYMENT_REASON.K:
      _md = Random.paymentReason()
      break
    case fieldTypeEnum.MATERIAL.K:
      _md = Random.materialName()
      break
    case fieldTypeEnum.PLATE_TYPE.K:
      _md = Random.plateType()
      break
    case fieldTypeEnum.COLOR.K:
      _md = Random.color()
      break
    case fieldTypeEnum.MATERIAL_CLASS_FULL_NAME.K:
      _md = Random.typeName() + '/' + Random.className() + '/' + Random.materialName()
      break
    case fieldTypeEnum.MATERIAL_CLASS_NAME.K:
      _md = Random.typeName()
      break
    case fieldTypeEnum.UNIT.K:
      _md = Random.unit()
      break
    case fieldTypeEnum.MEASUREMENT_UNIT.K:
      _md = Random.measurementUnit()
      break
    case fieldTypeEnum.ACCOUNTING_UNIT.K:
      _md = Random.accountingUnit()
      break
    case fieldTypeEnum.BRAND.K:
      _md = Random.brand()
      break
    case fieldTypeEnum.ADDRESS.K:
      _md = Random.address()
      break
    case fieldTypeEnum.INVOICE_NO.K:
      _md = Random.natural(1000000, 100000000000)
      break
    case fieldTypeEnum.BANK.K:
      _md = Random.bankName()
      break
    case fieldTypeEnum.BANK_ACCOUNT.K:
      _md = Random.natural()
      break
    case fieldTypeEnum.DATE.K:
      _md = Number(Random.datetime('T'))
      break
    // case fieldTypeEnum.DATES.K: _md = `${Random.datetime('T')},${Random.datetime('T')},${Random.datetime('T')}`
    case fieldTypeEnum.DATES.K:
      _md = [Number(Random.datetime('T')), Number(Random.datetime('T'))]
      break
    case fieldTypeEnum.AMOUNT.K:
      _md = Random.float(6666, 666666, 0, 2)
      break
    case fieldTypeEnum.QUANTITY.K:
      _md = Random.natural(6, 666)
      break
    case fieldTypeEnum.DP.K:
      _md = 1
      break
    case fieldTypeEnum.METE.K:
      _md = Random.float(6666, 666666, 0, 2)
      break
    case fieldTypeEnum.LENGTH.K:
      _md = Random.float(6666, 666666, 0, 2)
      break
    case fieldTypeEnum.WEIGHT.K:
      _md = Random.float(6666, 666666, 0, 2)
      break
    case fieldTypeEnum.THICKNESS.K:
      _md = Random.float(0, 1, 3, 3)
      break
    case fieldTypeEnum.RATE.K:
      _md = Random.float(1, 100, 0, 2)
      break
    case fieldTypeEnum.SPECIFICATION.K:
      _md = Random.natural(1, 666, 0, 1) + '*' + Random.natural(1, 666, 0, 1) + '*' + Random.natural(1, 666, 0, 1) + '*' + 'Q235B'
      break
    case fieldTypeEnum.ENUM.K:
      _md = mockEnum(field.format.enum, field.format.bit)
      break
    case fieldTypeEnum.CONTRACT_NO.K:
      var contractNo = Mock.mock({
        // 合同编号
        data: /^([A-Z]{2}-[A-Z0-9]{3}-[A-Z0-9]{5})$/
      })
      _md = contractNo.data
      break
    case fieldTypeEnum.PROJECT.K:
      _md = Mock.mock({
        contractNo: /^([A-Z]{2}-[A-Z0-9]{3}-[A-Z0-9]{5})$/,
        name: Random.psname() + '项目',
        shortName: Random.psname()
      })
      break
    case fieldTypeEnum.AREA_AXIS.K:
      var areaAxis = Mock.mock({
        // 轴线
        data: /^[0-9]{1,2}-[0-9]{2,3}轴线\/[a-z]-[a-z]$/
      })
      _md = areaAxis.data
      break
    case fieldTypeEnum.PHONE.K:
      var phone = Mock.mock({
        // 移动电话
        data: validatorPhone
      })
      _md = phone.data
      break
    case fieldTypeEnum.TEL.K:
      var tel = Mock.mock({
        // 移动电话
        data: validatorTel
      })
      _md = tel.data
      break
    case fieldTypeEnum.SERIAL_NUMBER.K:
      var serialNumber = Mock.mock({
        // 短编号
        data: /^([A-Z0-9]{1,4}-[A-Z0-9]{1,4}-[A-Z0-9]{1,4})$/
      })
      _md = serialNumber.data
      break
    case fieldTypeEnum.LICENSE_PLATE.K:
      var licensePlate = Mock.mock({
        // 车牌
        data: patternLicensePlate
      })
      _md = licensePlate.data
      break
    default:
      _md = Random.cword(3, 10)
  }
  return _md
}

/**
 * 随机获取枚举类型的值
 * @param {string} enumName 枚举 K
 * @returns
 */
function mockEnum(enumName, bit) {
  const e = enumAll[enumName]
  const arr = enumOperate.toArr(e)
  if (bit) {
    // 随机获取位运算值
    const _arr = JSON.parse(JSON.stringify(arr))
    const num = Math.ceil(Math.random() * _arr.length) // 设置位运算需要的个数
    const bitArr = []
    for (let i = 1; i <= num; i++) {
      const randomIndex = ~~(Math.random() * _arr.length)
      bitArr.push(_arr[randomIndex].V)
      _arr.splice(randomIndex, 1)
    }
    return bitArr.reduce((res, cur) => {
      return res | cur
    }, undefined)
  } else {
    return arr[Math.floor(Math.random() * arr.length)].V
  }
}

export default { getList, getOne }
