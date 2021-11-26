<template>
  <common-table :data="form.steelPlateList" :cell-class-name="wrongCellMask" style="width: 100%">
    <el-table-column label="序号" type="index" align="center" width="60" />
    <el-table-column prop="serialNumber" label="编号" align="center" width="90px" />
    <el-table-column prop="classifyFullName" label="物料种类" align="center" min-width="250px" />
    <el-table-column prop="specificationId" label="规格" align="center" min-width="200px" />
    <el-table-column prop="thickness" align="center" min-width="135px" :label="`厚 \n (mm)`">
      <template v-slot="scope">
        <el-input-number
          v-model="scope.row.thickness"
          :max="999999"
          controls-position="right"
          :min="0"
          :precision="baseUnit.thicknessRetain"
          size="mini"
          style="width: 98%"
          placeholder="厚"
          @change="calcTheoryWeight(scope.row, true)"
        />
      </template>
    </el-table-column>
    <el-table-column prop="width" align="center" min-width="135px" :label="`宽 \n mm`">
      <template v-slot="scope">
        <el-input-number
          v-model="scope.row.width"
          :max="999999"
          controls-position="right"
          :min="0"
          :precision="0"
          size="mini"
          style="width: 98%"
          placeholder="宽"
          @change="calcTheoryWeight(scope.row, true)"
        />
      </template>
    </el-table-column>
    <el-table-column prop="length" align="center" min-width="135px" :label="`长 \n (mm)`">
      <template v-slot="scope">
        <el-input-number
          v-model="scope.row.length"
          :max="999999"
          controls="false"
          :min="0"
          :precision="0"
          size="mini"
          style="width: 98%"
          placeholder="长"
          @change="calcTheoryWeight(scope.row, true)"
        />
      </template>
    </el-table-column>
    <el-table-column
      prop="number"
      align="center"
      min-width="135px"
      :label="`数量 \n (${baseUnit.measure.unit})`"
    >
      <template v-slot="scope">
        <el-input-number
          v-model="scope.row.number"
          :max="999999999"
          controls-position="right"
          :min="1"
          :step="5"
          :precision="0"
          size="mini"
          style="width: 98%"
          placeholder="数量"
          @change="calcTheoryWeight(scope.row, true)"
        />
      </template>
    </el-table-column>
    <el-table-column
      key="weight"
      prop="weight"
      align="center"
      :label="`重量 \n (${baseUnit.weight.unit})`"
      min-width="135px"
    >
      <template v-slot="scope">
        <el-tooltip
          class="item"
          effect="dark"
          :content="scope.row.hasOver ? `与理计重量差值过大` : `${scope.row.weight || 0}`"
          placement="top"
        >
          <el-input-number
            v-model="scope.row.weight"
            :max="999999999"
            controls-position="right"
            :min="0"
            :precision="baseUnit.weight.precision"
            size="mini"
            style="width: 98%"
            placeholder="重量"
            :class="{'over-weight-tip' : scope.row.hasOver }"
            @change="weightChange($event, scope.row, scope.$index)"
          />
        </el-tooltip>
      </template>
    </el-table-column>
    <el-table-column prop="brand" label="品牌" align="center" width="infoWidth">
      <template v-slot="scope">
        <el-input v-model.trim="scope.row.brand" maxlength="60" size="mini" placeholder="品牌" style="width: 98%" />
      </template>
    </el-table-column>
    <el-table-column prop="furnaceLotNumber" label="炉批号/卷号" align="center" width="135px">
      <template v-slot="scope">
        <el-input v-model.trim="scope.row.furnaceLotNumber" size="mini" placeholder="炉批号/卷号" style="width: 98%" />
      </template>
    </el-table-column>
    <el-table-column prop="remark" label="备注" align="center" min-width="infoWidth">
      <template v-slot="scope">
        <el-input v-model.trim="scope.row.remark" maxlength="500" size="mini" placeholder="备注" style="width: 98%" />
      </template>
    </el-table-column>
    <!-- width="fillAmountMethod & fillMethodEnum.WAREHOUSING.V?'180px':'145px'" -->
    <el-table-column label="操作" width="70" align="center">
      <template v-slot="scope">
        <common-button icon="el-icon-delete" type="danger" size="mini" @click="delRow(scope.$index)" />
      </template>
    </el-table-column>
  </common-table>
</template>

<script setup>
import { defineExpose, computed } from 'vue'
import { matClsEnum } from '@/utils/enum/modules/classification'
import { numOrPctEnum } from '@/utils/enum/modules/common'

import { regExtra } from '@/composables/form/use-form'
import useWmsConfig from '@/composables/store/use-wms-config'
import useTableValidate from '@compos/form/use-table-validate'
import useTableOperate from '@/composables/form/use-table-operate'
import useMatBaseUnit from '@/composables/store/use-mat-base-unit'

// 当前物料基础类型
const basicClass = matClsEnum.STEEL_PLATE.V

const tableRules = {
  classifyId: [{ required: true, message: '请选择物料种类', trigger: 'change' }],
  width: [{ required: true, message: '请填写宽度', trigger: 'change' }],
  thickness: [{ required: true, message: '请填写厚度', trigger: 'change' }],
  weight: [{ required: true, message: '请填写重量', trigger: 'change' }],
  length: [{ required: true, message: '请填写长度', trigger: 'change' }],
  number: [{ required: true, message: '请填写数量', trigger: 'change' }]
}

const { baseUnit } = useMatBaseUnit(basicClass)
const { inboundSteelCfg } = useWmsConfig()
const { form } = regExtra()

const { tableValidate, wrongCellMask } = useTableValidate({ rules: tableRules })

// 超出提示
const overDiffTip = computed(() => {
  if (inboundSteelCfg.steelDiffType === numOrPctEnum.PERCENTAGE.V) {
    return `与理论中量的误差不可超过理论重量的${inboundSteelCfg.steelDiff}%`
  }
  return `与理论中量的误差不可超过${inboundSteelCfg.steelDiff}g`
})

// 计算重量是否在正常范围内
function weightOverDiff(row) {
  let hasOver = false
  const overNum = row.weightG - row.theoryWeight
  const steelDiff = inboundSteelCfg.value.inboundSteelCfg
  const steelDiffType = inboundSteelCfg.value.steelDiffType
  if (steelDiffType === numOrPctEnum.PERCENTAGE.V) {
    hasOver = Math.abs(row.weightG / row.theoryWeight - 1) * 100 > steelDiff
  }
  if (steelDiffType === numOrPctEnum.NUMBER.V) {
    hasOver = Math.abs(row.weightG - row.theoryWeight) > steelDiff
  }
  row.hasOver = hasOver
  row.overNum = overNum
  return hasOver
}

// 校验
function validate() {
  const { validResult, dealList } = tableValidate(form.list)
  form.list = dealList
  return validResult
}

defineExpose({
  validate
})
</script>
