<template>
  <common-table
    v-bind="$attrs"
    :data="bcListObj.sectionSteelList"
    :cell-class-name="wrongCellMask"
    :show-empty-symbol="false"
    return-source-data
    row-key="uid"
  >
    <el-table-column label="序号" type="index" align="center" width="60" fixed="left" />
    <el-table-column label="申购单号" prop="purchaseSN" fixed="left" width="140" align="center">
      <template #default="{ row }">
        <table-cell-tag
          :show="row.boolPurchase"
          name="已采购"
          color="#e6a23c"
        />
        <span>{{ row.purchaseSN }}</span>
      </template>
    </el-table-column>
    <el-table-column prop="serialNumber" label="编号" align="center" fixed="left" />
    <el-table-column prop="classifyName" label="物料种类" align="center" fixed="left" show-overflow-tooltip>
      <template #default="{ row }">
        <el-tooltip :content="row.classifyParentFullName" :disabled="!row.classifyParentFullName" :show-after="500" placement="top">
          <span v-empty-text="row.classifyName" />
        </el-tooltip>
      </template>
    </el-table-column>
    <el-table-column prop="specification" label="规格" align="center" fixed="left" show-overflow-tooltip  min-width="140">
      <template #default="{ row }">
        <el-tooltip :content="row.specificationLabels" placement="top">
          <span>{{ row.specification }}</span>
        </el-tooltip>
      </template>
    </el-table-column>
    <el-table-column prop="length" align="center" :label="`定尺长度 (${baseUnit.length.unit})`" min-width="120">
      <template #default="{ row }">
        <common-input-number
          v-if="Boolean(currentCfg?.length & basicClass)"
          v-model="row.length"
          :max="999999"
          :controls="false"
          :min="0"
          :precision="baseUnit.length.precision"
          size="mini"
          placeholder="长"
        />
        <span v-else>{{ row.length }}</span>
      </template>
    </el-table-column>
    <el-table-column prop="quantity" align="center" :label="`数量 (${baseUnit.measure.unit})`" min-width="120">
      <template #default="{ row }">
        <common-input-number
          v-if="Boolean(currentCfg?.quantity & basicClass)"
          v-model="row.quantity"
          :min="1"
          :max="999999999"
          controls-position="right"
          :controls="false"
          :step="5"
          :precision="baseUnit.measure.precision"
          size="mini"
          placeholder="数量"
        />
        <span v-else>{{ row.quantity }}</span>
      </template>
    </el-table-column>
    <el-table-column prop="totalLength" align="center" :label="`总长度 (m)`" />
    <el-table-column key="weighingTotalWeight" prop="weighingTotalWeight" align="center" :label="`总重 (${baseUnit.weight.unit})`">
      <template #default="{ row }">
        <el-tooltip
          class="item"
          effect="dark"
          :content="`单位重量：${row.unitWeight} kg/m， 申购重量：${row.purchaseTotalWeight} kg， ${overDiffTip}`"
          :disabled="!row.hasOver"
          placement="top"
        >
          <common-input-number
            v-model="row.weighingTotalWeight"
            :min="0"
            :max="999999999"
            controls-position="right"
            :controls="false"
            :precision="baseUnit.weight.precision"
            size="mini"
            placeholder="重量"
            :class="{ 'over-weight-tip': row.hasOver }"
          />
        </el-tooltip>
      </template>
    </el-table-column>
    <el-table-column prop="brand" label="品牌" align="center">
      <template #default="{ row }">
        <el-input v-model.trim="row.brand" maxlength="60" size="mini" placeholder="品牌" />
      </template>
    </el-table-column>
    <el-table-column label="操作" width="90" align="center" fixed="right">
      <template #default="{ row, $index }">
        <common-button icon="el-icon-plus" :disabled="isExist(row.id) || row.boolPurchase" type="warning" size="mini" @click="addRow(row, $index)" />
      </template>
    </el-table-column>
  </common-table>
</template>

<script setup>
import { defineExpose, defineEmits, inject, watchEffect, watch } from 'vue'
import { matClsEnum } from '@/utils/enum/modules/classification'
import { isNotBlank } from '@/utils/data-type'

import useTableValidate from '@compos/form/use-table-validate'
import useMatBaseUnit from '@/composables/store/use-mat-base-unit'
import useWeightOverDiff from '@/composables/wms/use-steel-weight-over-diff'
import { calcSectionSteelTotalLength } from '@/utils/wms/measurement-calc'
import { positiveNumPattern } from '@/utils/validate/pattern'

const emit = defineEmits(['add-purchase'])

// 当前物料基础类型
const basicClass = matClsEnum.SECTION_STEEL.V
const bcListObj = inject('bcListObj')
const form = inject('crud')?.form

const { baseUnit } = useMatBaseUnit(basicClass) // 当前分类基础单位

const { overDiffTip, weightOverDiff, diffSubmitValidate, currentCfg } = useWeightOverDiff(
  baseUnit,
  { cfgType: 'purchase', weightField: 'weighingTotalWeight', compareWeightField: 'purchaseTotalWeight', weightTip: '申购重量' }
) // 超出重量处理

// 校验规则
const rules = {
  quantity: [
    { required: true, message: '请填写数量', trigger: 'blur' },
    { pattern: positiveNumPattern, message: '数量必须大于0', trigger: 'blur' }
  ],
  weighingTotalWeight: [
    { required: true, message: '请填写重量', trigger: 'blur' },
    { validator: diffSubmitValidate, message: '超出误差允许范围,不可提交', trigger: 'blur' },
    { pattern: positiveNumPattern, message: '重量必须大于0', trigger: 'blur' }
  ]
}

const { tableValidate, wrongCellMask } = useTableValidate({ rules: rules, errorMsg: '请修正【型材清单】中标红的信息' }) // 表格校验

function isExist(id) {
  return form.sectionSteelList?.findIndex((v) => v.id === id) !== -1
}

// 行监听
// 使用watch 监听方法，优点：初始化时表单数据时，可以不立即执行（惰性），可以避免“草稿/修改”状态下重量被自动修改；缺点：初始化时需要指定监听参数
function rowWatch(row) {
  watchEffect(() => weightOverDiff(row))
  // 计算单件理论重量
  // watch([() => row.length, () => row.unitWeight, baseUnit], () => calcTheoryWeight(row), { immediate: true })
  // 计算总重
  // watch([() => row.theoryWeight, () => row.quantity], () => {
  //   calcTotalWeight(row)
  // })
  // 计算总长度
  watch([() => row.length, () => row.quantity], () => {
    calcTotalLength(row)
  })
}

// 总重计算与单位重量计算分开，避免修改数量时需要重新计算单件重量
// 计算单件重量
// async function calcTheoryWeight(row) {
//   row.theoryWeight = await calcSectionSteelWeight({
//     length: row.length, // 长度
//     unitWeight: row.unitWeight // 单位重量
//   })
// }

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

// 计算总重
// function calcTotalWeight(row) {
//   // if (row.purchaseNetMete && row.quantity) {
//   //   row.purchaseTotalWeight = toPrecision(row.purchaseNetMete * row.quantity, baseUnit.value.weight.precision)
//   // } else {
//   //   row.purchaseTotalWeight = undefined
//   // }
//   if (isNotBlank(row.theoryWeight) && row.quantity) {
//     row.theoryTotalWeight = toPrecision(row.theoryWeight * row.quantity, baseUnit.value.weight.precision)
//     row.weighingTotalWeight = toPrecision(row.theoryWeight * row.quantity, baseUnit.value.weight.precision)
//   } else {
//     row.theoryTotalWeight = undefined
//     row.weighingTotalWeight = undefined
//   }
// }

function addRow(row, index) {
  emit('add-purchase', row, index)
}

// 校验
function validate(list) {
  const { validResult } = tableValidate(list)
  return validResult
}

defineExpose({
  rowWatch,
  validate
})
</script>
