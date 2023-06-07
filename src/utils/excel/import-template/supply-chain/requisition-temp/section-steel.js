import store from '@/store/index'
import { deepClone, isNotBlank, toPrecision } from '@/utils/data-type'
import { createUniqueString, trimStr } from '@/utils/data-type/string'
import { matClsEnum } from '@/utils/enum/modules/classification'
import { calcSectionSteelTotalLength, calcSectionSteelWeight } from '@/utils/wms/measurement-calc'
import { dataValidate } from '@/composables/form/use-table-validate'
import { compareArray } from '@/utils/data-type/array'
import { getBaseUnit } from '@/utils/wms/convert-unit'

const sectionSteelSpecTmpl = {
  title: '型材申购清单', // 表格名称
  dlgWidth: '1500px', // 预览窗口宽度
  startRow: 2, // 起始行
  endIgnoreRow: 1, // 忽略行 如合计
  // 解析字段
  fields: [
    { label: '物料种类', field: 'classifyName', excelField: '__EMPTY' },
    { label: '规格/材质', field: 'specification', excelField: '__EMPTY_1' },
    { label: '定尺长度（mm）', field: 'length', type: 'number', excelField: '__EMPTY_2' },
    { label: '数量（根）', field: 'quantity', type: 'number', excelField: '__EMPTY_3' },
    { label: '总重（kg）', field: 'weighingTotalWeight', type: 'number', precision: 0, excelField: '__EMPTY_4' },
    { label: '品牌', field: 'brand', excelField: '__EMPTY_5' }
  ],
  // 校验规则
  rules: {
    classifyName: [{ required: true, message: '物料种类不能为空' }],
    specification: [{ required: true, message: '型材规格不能为空' }]
  },
  // 提交前的数据格式转换
  format: async (tableList) => {
    // 校验
    const validate = dataValidate(tableList, sectionSteelSpecTmpl.rules)
    if (!validate) return
    // 获取基础单位
    const baseUnit = await getBaseUnit()
    // 加载科目树
    if (!store.state.config.loaded.matClsTree) {
      await store.dispatch('config/fetchMatClsTree')
    }
    // 获取 型材科目
    const matList = store.state.config.rawMatClsList.filter((v) => v.basicClass === matClsEnum.SECTION_STEEL.V)
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
      // 遍历 型材科目，寻找与导入名称相同的科目
      // 处理 多层级名称的情况 A/B/C
      const nameArr = trimStr(row.classifyName.split('/'))
      console.log(matList, 'matList')
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
      const specification = formatSpec(row.specification) // 对应申购“规格”列
      if (materialInfo && Array.isArray(materialInfo.specList)) {
        for (const specInfo of materialInfo.specList) {
          if (specification === specInfo.spec) {
            materialSpecInfo = specInfo
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
        row.unitWeight = materialSpecInfo.unitWeight // 单位重量 kg/m

        // 设置理论重量
        row.theoryWeight = await calcSectionSteelWeight({
          length: row.length, // 长度
          unitWeight: row.unitWeight // 单位重量
        })

        // 设置理论总重
        if (isNotBlank(row.theoryWeight) && row.quantity) {
          row.theoryTotalWeight = toPrecision(row.theoryWeight * row.quantity, baseUnit[matClsEnum.SECTION_STEEL.V].weight.precision)
        } else {
          row.theoryTotalWeight = undefined
        }

        // 设置过磅重量（即总重量）
        row.weighingTotalWeight = row.weighingTotalWeight ?? row.theoryTotalWeight

        // 设置总长度
        calcTotalLength(row)
        row.mete = row.weighingTotalWeight
      } else {
        throw new Error(`${row.classifyName}下不存在规格为“${specification}”的材料，请联系初鸣售后人员添加`)
      }
    }
    return list
  }
}

// 计算总长
function calcTotalLength(row) {
  if (isNotBlank(row.length) && row.quantity) {
    row.totalLength = calcSectionSteelTotalLength({
      length: row.length, // 长度
      quantity: row.quantity // 数量
    })
  } else {
    row.totalLength = undefined
  }
}

// 处理规格
function formatSpec(spec) {
  const _spec = spec.replace(/\s+/g, '')
  const lastIndex = _spec.lastIndexOf('\*')
  return _spec.substring(0, lastIndex) + ' * ' + _spec.substring(lastIndex + 1)
}

export default sectionSteelSpecTmpl
