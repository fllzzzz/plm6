<template>
  <common-table
    v-bind="$attrs"
    :data="form.steelCoilList"
    :cell-class-name="wrongCellMask"
    :show-empty-symbol="false"
    return-source-data
    row-key="uid"
  >
    <el-table-column label="序号" type="index" align="center" width="60" fixed="left" />
    <el-table-column prop="serialNumber" label="编号" align="center" fixed="left" />
    <el-table-column prop="classifyName" label="物料种类" align="center" fixed="left" show-overflow-tooltip>
      <template #default="{ row }">
        <el-tooltip :content="row.classifyParentFullName" :disabled="!row.classifyParentFullName" :show-after="500" placement="top">
          <span v-empty-text="row.classifyName" />
        </el-tooltip>
      </template>
    </el-table-column>
    <el-table-column prop="specification" label="规格" align="center" fixed="left" show-overflow-tooltip>
      <template #default="{ row }">
        <el-tooltip :content="row.specificationLabels" placement="top">
          <span>{{ row.specification }}</span>
        </el-tooltip>
      </template>
    </el-table-column>
    <el-table-column
      key="weighingTotalWeight"
      prop="weighingTotalWeight"
      align="center"
      :label="`总重 (${baseUnit.weight.unit})`"
      min-width="135"
    >
      <template #default="{ row }">
        <common-input-number
          v-model="row.weighingTotalWeight"
          :min="0"
          :max="999999999"
          controls-position="right"
          :controls="false"
          :precision="baseUnit.weight.precision"
          size="mini"
          placeholder="重量"
        />
      </template>
    </el-table-column>
    <el-table-column prop="thickness" align="center" :label="`厚 (${baseUnit.thickness.unit})`" min-width="120">
      <template #default="{ row }">
        <common-input-number
          v-model="row.thickness"
          :min="0"
          :max="999999"
          controls-position="right"
          :controls="false"
          :precision="baseUnit.thickness.precision"
          size="mini"
          placeholder="厚"
        />
      </template>
    </el-table-column>
    <el-table-column prop="width" align="center" :label="`宽 (${baseUnit.width.unit})`" min-width="120">
      <template #default="{ row }">
        <common-input-number
          v-model="row.width"
          :min="0"
          :max="999999"
          controls-position="right"
          :controls="false"
          :precision="baseUnit.width.precision"
          size="mini"
          placeholder="宽"
        />
      </template>
    </el-table-column>
    <el-table-column prop="length" align="center" :label="`长 (m)`" min-width="120">
      <template #default="{ row }">
        <common-input-number
          v-model="row.length"
          :min="0"
          :max="999999999"
          :precision="row.measurePrecision"
          :controls="false"
          size="mini"
          placeholder="长"
        />
      </template>
    </el-table-column>

    <price-set-columns />

    <el-table-column prop="color" label="颜色" align="center">
      <template #default="{ row }">
        <el-input v-model.trim="row.color" maxlength="20" size="mini" placeholder="颜色" />
      </template>
    </el-table-column>
    <el-table-column prop="brand" label="品牌" align="center">
      <template #default="{ row }">
        <el-input v-model.trim="row.brand" maxlength="60" size="mini" placeholder="品牌" />
      </template>
    </el-table-column>
    <el-table-column label="操作" width="70" align="center" fixed="right">
      <template #default="{ row, $index }">
        <common-button icon="el-icon-delete" type="danger" size="mini" @click="delRow(row.sn, $index)" />
      </template>
    </el-table-column>
  </common-table>
</template>

<script setup>
import { defineExpose, inject, reactive, watch } from 'vue'
import { matClsEnum } from '@/utils/enum/modules/classification'
import { isBlank, isNotBlank } from '@/utils/data-type'

import usePriceSet from '@/composables/wms/use-price-set'
import useTableValidate from '@compos/form/use-table-validate'
import useMatBaseUnit from '@/composables/store/use-mat-base-unit'
import { createUniqueString } from '@/utils/data-type/string'
import { calcSteelCoilLength } from '@/utils/wms/measurement-calc'
import { positiveNumPattern } from '@/utils/validate/pattern'
import priceSetColumns from '@/views/wms/material-inbound/raw-material/components/price-set-columns.vue'

// 当前物料基础类型
const basicClass = matClsEnum.STEEL_COIL.V

// 金额校验
const validateAmount = (value, row) => {
  if (isNotBlank(row.weighingTotalWeight) && isNotBlank(row.unitPrice)) {
    return +(row.weighingTotalWeight * row.unitPrice).toFixed(2) === +value
  }
  return false
}

const rules = {
  classifyId: [{ required: true, message: '请选择物料种类', trigger: 'change' }],
  width: [
    { required: true, message: '请填写宽度', trigger: 'blur' },
    { pattern: positiveNumPattern, message: '宽度必须大于0', trigger: 'blur' }
  ],
  thickness: [
    { required: true, message: '请填写厚度', trigger: 'blur' },
    { pattern: positiveNumPattern, message: '厚度必须大于0', trigger: 'blur' }
  ],
  weighingTotalWeight: [
    { required: true, message: '请填写重量', trigger: 'blur' },
    { pattern: positiveNumPattern, message: '重量必须大于0', trigger: 'blur' }
  ],
  length: [
    { required: true, message: '请填写长度', trigger: 'blur' },
    { pattern: positiveNumPattern, message: '长度必须大于0', trigger: 'blur' }
  ],
  unitPrice: [{ required: true, message: '请填写单价', trigger: 'blur' }],
  amount: [
    { required: true, message: '请填写金额', trigger: 'blur' },
    { validator: validateAmount, message: '金额有误，请手动修改', trigger: 'blur' }
  ]
}

const matSpecRef = inject('matSpecRef') // 调用父组件matSpecRef
const form = inject('crud')?.form
const { baseUnit } = useMatBaseUnit(basicClass) // 当前分类基础单位

const { handleMeteChangeCalcPrice } = usePriceSet('weighingTotalWeight')
const { tableValidate, wrongCellMask } = useTableValidate({ rules: rules, errorMsg: '请修正【钢卷清单】中标红的信息' }) // 表格校验

// 行初始化
function rowInit(row) {
  const _row = reactive({
    uid: createUniqueString(),
    sn: row.sn, // 该科目规格唯一编号
    specificationLabels: row.specificationLabels, // 规格中文
    serialNumber: row.serialNumber, // 科目编号 - 规格
    classifyId: row.classify.id, // 科目id
    classifyFullName: row.classify.fullName, // 全路径名称
    classifyName: row.classify.name, // 当前科目名称
    classifyParentFullName: row.classify.parentFullName, // 父级路径名称
    basicClass: row.classify.basicClass, // 基础类型
    specification: row.spec, // 规格
    specificationMap: row.specKV, // 规格KV格式
    // measureUnit: row.classify.measureUnit, // 计量单位
    accountingUnit: row.classify.accountingUnit, // 核算单位
    accountingPrecision: row.classify.accountingPrecision, // 核算单位小数精度
    // measurePrecision: row.classify.measurePrecision, // 计量单位小数精度
    // quantity: undefined, // 数量（毫米，计量单位对应的值）
    measureUnit: '米', // 计量单位
    measurePrecision: 3, // 计量单位小数精度
    quantity: undefined, // 数量（米，计量单位对应的值）
    color: undefined, // 颜色
    brand: undefined, // 品牌
    thickness: undefined, // 厚度
    length: undefined, // 长度
    width: undefined, // 宽度
    theoryLength: undefined, // 理论单件重量
    weighingTotalWeight: undefined // 过磅重量
  })
  rowWatch(_row)
  return _row
}

// 行监听
// 使用watch 监听方法，优点：初始化时表单数据时，可以不立即执行（惰性），可以避免“草稿/修改”状态下重量被自动修改；缺点：初始化时需要指定监听参数
function rowWatch(row) {
  // 计算理论长度
  watch([() => row.weighingTotalWeight, () => row.width, () => row.thickness, baseUnit], () => calcTheoryLength(row))
  // 计算价格
  watch([() => row.weighingTotalWeight], () => handleMeteChangeCalcPrice(row))
  // 计算总长度
  watch([() => row.theoryLength], () => calcTotalLength(row))
}

// 总重计算与单位重量计算分开，避免修改数量时需要重新计算单件重量
// 计算单件重量
async function calcTheoryLength(row) {
  row.theoryLength = await calcSteelCoilLength({
    name: row.classifyFullName,
    weight: row.weighingTotalWeight,
    width: row.width,
    thickness: row.thickness
  })
}

// 计算总长(没有数量概念，可以直接和calcTheoryLength合并)
function calcTotalLength(row) {
  if (isNotBlank(row.theoryLength)) {
    // mm转为m
    row.length = row.theoryLength / 1000
  } else {
    row.length = undefined
  }
}

// 删除行
function delRow(sn, $index) {
  if (matSpecRef.value) {
    matSpecRef.value.delListItem(sn, $index)
  } else {
    form.steelCoilList.splice($index, 1)
  }
}

// 校验
function validate() {
  if (isBlank(form.steelCoilList)) return true
  const { validResult, dealList } = tableValidate(form.steelCoilList)
  form.steelCoilList = dealList
  form.steelCoilList.forEach((row) => {
    row.quantity = row.length
  })
  return validResult
}

defineExpose({
  rowInit,
  rowWatch,
  validate
})
</script>
