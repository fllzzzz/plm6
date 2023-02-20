<template>
  <common-table
    v-bind="$attrs"
    :data="bcListObj.steelPlateList"
    :cell-class-name="wrongCellMask"
    :show-empty-symbol="false"
    return-source-data
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
    <el-table-column prop="thickness" align="center" :label="`厚 (${baseUnit.thickness.unit})`">
      <template #default="{ row }">
        <span>{{ row.thickness }}</span>
      </template>
    </el-table-column>
    <el-table-column prop="width" align="center" :label="`宽 (${baseUnit.width.unit})`">
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
    <el-table-column prop="length" align="center" :label="`长 (${baseUnit.length.unit})`">
      <template #default="{ row }">
        <common-input-number
          v-model="row.length"
          :max="999999"
          :controls="false"
          :min="0"
          :precision="baseUnit.length.precision"
          size="mini"
          placeholder="长"
        />
      </template>
    </el-table-column>
    <el-table-column prop="quantity" align="center" :label="`数量 (${baseUnit.measure.unit})`">
      <template #default="{ row }">
        <common-input-number
          v-model="row.quantity"
          :min="1"
          :max="999999999"
          controls-position="right"
          :controls="false"
          :step="1"
          :precision="baseUnit.measure.precision"
          size="mini"
          placeholder="数量"
        />
      </template>
    </el-table-column>
    <el-table-column key="weighingTotalWeight" prop="weighingTotalWeight" align="center" :label="`总重 (${baseUnit.weight.unit})`">
      <template #default="{ row }">
        <el-tooltip
          class="item"
          effect="dark"
          :content="`理论重量：${row.theoryTotalWeight} kg， ${overDiffTip}`"
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
        <common-button
          icon="el-icon-plus"
          :disabled="isNotBlank(form.requisitionListKV?.[row.id])"
          type="warning"
          size="mini"
          @click="addRow(row, $index)"
        />
      </template>
    </el-table-column>
  </common-table>
</template>

<script setup>
import { defineExpose, defineEmits, watchEffect, inject, watch } from 'vue'
import { matClsEnum } from '@/utils/enum/modules/classification'
import { isNotBlank, toPrecision } from '@/utils/data-type'

import useTableValidate from '@compos/form/use-table-validate'
import useMatBaseUnit from '@/composables/store/use-mat-base-unit'
import useWeightOverDiff from '@/composables/wms/use-steel-weight-over-diff'
import { calcSteelPlateWeight } from '@/utils/wms/measurement-calc'
import { positiveNumPattern } from '@/utils/validate/pattern'
import { ElMessage } from 'element-plus'

const emit = defineEmits(['add-purchase'])

// 当前物料基础类型
const basicClass = matClsEnum.STEEL_PLATE.V
const bcListObj = inject('bcListObj')
const form = inject('crud')?.form

const { baseUnit } = useMatBaseUnit(basicClass) // 当前分类基础单位

const { overDiffTip, weightOverDiff, diffSubmitValidate } = useWeightOverDiff(baseUnit) // 过磅重量超出理论重量处理

const rules = {
  width: [
    { required: true, message: '请填写宽度', trigger: 'blur' },
    { pattern: positiveNumPattern, message: '宽度必须大于0', trigger: 'blur' }
  ],
  weighingTotalWeight: [
    { required: true, message: '请填写重量', trigger: 'blur' },
    { validator: diffSubmitValidate, message: '超出误差允许范围,不可提交', trigger: 'blur' },
    { pattern: positiveNumPattern, message: '重量必须大于0', trigger: 'blur' }
  ],
  length: [
    { required: false, message: '请填写长度', trigger: 'blur' },
    { pattern: positiveNumPattern, message: '长度必须大于0', trigger: 'blur' }
  ],
  quantity: [
    { required: true, message: '请填写数量', trigger: 'blur' },
    { pattern: positiveNumPattern, message: '数量必须大于0', trigger: 'blur' }
  ]
}

const { tableValidate, wrongCellMask } = useTableValidate({ rules: rules, errorMsg: '请修正【钢板清单】中标红的信息' }) // 表格校验

// 行监听
// 使用watch 监听方法，优点：初始化时表单数据时，可以不立即执行（惰性），可以避免“草稿/修改”状态下重量被自动修改；缺点：初始化时需要指定监听参数
function rowWatch(row) {
  watchEffect(() => weightOverDiff(row))
  // 计算单件理论重量
  watch([() => row.length, () => row.width, () => row.thickness, baseUnit], () => calcTheoryWeight(row), { immediate: true })
  // 计算总重
  watch([() => row.theoryWeight, () => row.quantity], () => calcTotalWeight(row), { immediate: true })
}

// 总重计算与单位重量计算分开，避免修改数量时需要重新计算单件重量
// 计算单件重量
async function calcTheoryWeight(row) {
  row.theoryWeight = await calcSteelPlateWeight({
    name: row.classifyFullName, // 名称，用于判断是否为不锈钢，不锈钢与普通钢板密度不同
    length: row.length,
    width: row.width,
    thickness: row.thickness
  })
}

// 计算总重
function calcTotalWeight(row) {
  if (isNotBlank(row.theoryWeight) && row.quantity) {
    row.theoryTotalWeight = row.theoryWeight * row.quantity
    row.weighingTotalWeight = toPrecision(row.theoryWeight * row.quantity)
  } else {
    row.theoryTotalWeight = undefined
    row.weighingTotalWeight = undefined
  }
}

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
