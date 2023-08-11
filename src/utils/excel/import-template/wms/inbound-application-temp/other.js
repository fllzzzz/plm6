import store from '@/store/index'
import { deepClone, toPrecision } from '@/utils/data-type'
import { createUniqueString, trimStr } from '@/utils/data-type/string'
import { matClsEnum } from '@/utils/enum/modules/classification'
import { judgeMaterialUnitExist } from '@/utils/convert/unit'
import { dataValidate } from '@/composables/form/use-table-validate'
import { compareArray } from '@/utils/data-type/array'

const sectionSteelSpecTmpl = {
  title: '其它入库清单', // 表格名称
  dlgWidth: '1500px', // 预览窗口宽度
  startRow: 2, // 起始行
  // 解析字段
  fields: [
    { label: '物料种类', field: 'classifyName', excelField: '__EMPTY' },
    { label: '规格', field: 'specification', excelField: '__EMPTY_1' },
    { label: '计量单位', field: 'measureUnit', excelField: '__EMPTY_2' },
    { label: '数量', field: 'quantity', type: 'number', excelField: '__EMPTY_3' },
    { label: '核算单位', field: 'accountingUnit', excelField: '__EMPTY_4' },
    { label: '核算量', field: 'mete', type: 'number', excelField: '__EMPTY_5' },
    { label: '含税单价', field: 'unitPrice', excelField: '__EMPTY_6' },
    { label: '金额', field: 'amount', excelField: '__EMPTY_7' },
    { label: '颜色', field: 'color', excelField: '__EMPTY_8' },
    { label: '品牌', field: 'brand', excelField: '__EMPTY_9' },
    { label: '备注', field: 'remark', excelField: '__EMPTY_10' }
  ],
  // 校验规则
  rules: {
    classifyName: [{ required: true, message: '物料种类不能为空' }]
  },
  // 提交前的数据格式转换
  format: async (tableList) => {
    // 校验
    const validate = dataValidate(tableList, sectionSteelSpecTmpl.rules)
    if (!validate) return
    // 加载科目树
    if (!store.state.config.loaded.matClsTree) {
      await store.dispatch('config/fetchMatClsTree')
    }

    // 获取 钢板科目
    const matList = store.state.config.rawMatClsList.filter((v) => v.basicClass === matClsEnum.OTHER.V)
    const rawMatClsKV = store.state.config.rawMatClsKV
    // 遍历导入表格
    const list = deepClone(tableList)
    // 匹配到的id
    const existClassifyIds = []
    // 不存在的科目名称
    let unexistClassifyName = []
    // 遍历获取科目名称对应的id
    list.forEach((row) => {
      let exist = false
      // 遍历 钢板科目，寻找与导入名称相同的科目
      // 处理 多层级名称的情况 A/B/C
      const nameArr = trimStr(row.classifyName.split('/'))
      for (const mat of matList) {
        // 1.科目全路径数量必须>=导入的名称数量；2.从数组反向截取比较是否相同
        const flag = mat.fullPathName.length >= nameArr.length && compareArray(mat.fullPathName.slice(-nameArr.length), nameArr)
        if (flag) {
          exist = true
          row.classifyId = mat.id // 设置科目id
          existClassifyIds.push(mat.id)
          break
        }
      }
      if (!exist) unexistClassifyName.push(row.classifyName)
    })
    // 去重
    unexistClassifyName = Array.from(new Set(unexistClassifyName))
    // 有不存在的科目时，视为导入失败
    if (unexistClassifyName.length > 0) {
      throw new Error(`${unexistClassifyName.map((v) => `“${v}”`).join('、')}${unexistClassifyName.length > 1 ? '等' : ''}物料种类不存在`)
    }
    // 根据科目id获取对应科目的规格并做匹配
    const stateClassifySpec = store.state.config.classifySpec
    const unloadClassifyIds = existClassifyIds.filter((id) => stateClassifySpec[id] === undefined)
    // 拉取未加载的当前科目规格
    if (unloadClassifyIds.length > 0) {
      unloadClassifyIds.forEach((id) => {
        stateClassifySpec[id] = {}
      })
      await store.dispatch('config/fetchMarClsSpec', unloadClassifyIds)
    }
    // 匹配规格
    for (const row of list) {
      // 必须是末级科目
      if (rawMatClsKV?.[row.classifyId]?.isLeaf === false) {
        throw new Error(`${row.classifyName}不是末级科目`)
      }
      const materialInfo = stateClassifySpec[row.classifyId]
      // 未配置核算单位
      if (materialInfo.hasUnitConfig === false) {
        throw new Error(`${row.classifyName}未配置核算单位`)
      }
      // 当前规格的物料信息
      let materialSpecInfo
      const specification = row.specification ?? '' // 对应入库“规格”列
      if (materialInfo && Array.isArray(materialInfo.specList)) {
        for (const specInfo of materialInfo.specList) {
          if (specification === specInfo.spec) {
            materialSpecInfo = specInfo
            // 判断单位是否一致,若单位未填写则默认使用系统单位，不做判断

            // 计量
            if (row.measureUnit) {
              if (!materialSpecInfo.classify.measureUnit) throw new Error(`${row.classifyName}不存在计量单位`)
              const boolUnitExist = await judgeMaterialUnitExist(materialSpecInfo.classify.measureUnit, row.measureUnit)
              if (!boolUnitExist) {
                throw new Error(`${row.classifyName}填写的计量单位错误，单位应为：“${materialSpecInfo.classify.measureUnit}”`)
              }
            }

            // 核算
            if (row.accountingUnit) {
              const boolUnitExist = await judgeMaterialUnitExist(materialSpecInfo.classify.accountingUnit, row.accountingUnit)
              if (!boolUnitExist) {
                throw new Error(`${row.classifyName}填写的核算单位错误，单位应为：“${materialSpecInfo.classify.accountingUnit}”`)
              }
            }
            break
          }
        }
      }
      // 存在物料规格，则设置物料信息
      if (materialSpecInfo) {
        row.uid = createUniqueString() // 临时唯一编码
        row.sn = materialSpecInfo.sn // 该科目规格唯一编号
        row.specificationLabels = materialSpecInfo.specificationLabels // 规格中文
        row.serialNumber = materialSpecInfo.serialNumber // 科目编号 - 规格
        row.classifyId = materialSpecInfo.classify.id // 科目id
        row.classifyFullPathId = materialSpecInfo.classify.fullPathId // 全路径id
        row.classifyFullName = materialSpecInfo.classify.fullName // 全路径名称
        row.classifyName = materialSpecInfo.classify.name // 当前科目名称
        row.classifyParentFullName = materialSpecInfo.classify.parentFullName // 父级路径名称
        row.basicClass = materialSpecInfo.classify.basicClass // 基础类型
        row.specification = materialSpecInfo.spec // 规格
        row.specificationMap = materialSpecInfo.specKV // 规格KV格式
        row.measureUnit = materialSpecInfo.classify.measureUnit // 计量单位
        row.accountingUnit = materialSpecInfo.classify.accountingUnit // 核算单位
        row.accountingPrecision = materialSpecInfo.classify.accountingPrecision // 核算单位小数精度
        row.measurePrecision = materialSpecInfo.classify.measurePrecision // 计量单位小数精度

        if (row.quantity) {
          row.quantity = toPrecision(row.quantity, row.measurePrecision)
        }
        if (row.mete) {
          row.mete = toPrecision(row.mete, row.accountingPrecision)
        }

        // 没有计量单位，将数量设置为空
        if (!row.measureUnit) {
          row.quantity = undefined
        }
      } else {
        throw new Error(`${row.classifyName}下不存在规格为“${specification}”的材料，请联系管理员/仓库人员添加`)
      }
    }
    return list
  }
}

export default sectionSteelSpecTmpl
