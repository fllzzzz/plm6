<template>
  <common-table
    v-bind="$attrs"
    :data="form.list"
    :cell-class-name="wrongCellMask"
    :show-empty-symbol="false"
    return-source-data
    row-key="uid"
  >
    <el-table-column label="序号" type="index" align="center" width="60" fixed="left" />
    <el-table-column v-if="form.useRequisitions" label="申购单号" prop="purchaseSN" fixed="left" width="140" align="center" />
    <el-table-column prop="serialNumber" show-overflow-tooltip label="编号" align="center" fixed="left" />
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
          <span v-empty-text="row.specification" />
        </el-tooltip>
      </template>
    </el-table-column>
    <el-table-column prop="measureUnit" label="计量单位" align="center">
      <template #default="{ row }">
        <span v-empty-text>{{ row.measureUnit }}</span>
      </template>
    </el-table-column>
    <el-table-column prop="quantity" label="数量" align="center">
      <template #default="{ row }">
        <common-input-number
          v-if="
            row.measureUnit &&
            (!form.useRequisitions ||
              (form.useRequisitions && Boolean(currentCfg?.quantity & basicClass) && row.outboundUnitType === measureTypeEnum.MEASURE.V))
          "
          v-model="row.quantity"
          :min="0"
          :max="999999999"
          :controls="false"
          :step="1"
          :precision="row.measurePrecision"
          size="mini"
          placeholder="数量"
          @change="handleQuantityChange(row)"
        />
        <span v-else v-empty-text>{{ row.quantity || '-' }}</span>
      </template>
    </el-table-column>
    <el-table-column prop="accountingUnit" label="核算单位" align="center">
      <template #default="{ row }">
        <span v-empty-text>{{ row.accountingUnit }}</span>
      </template>
    </el-table-column>
    <el-table-column prop="mete" label="核算量" align="center">
      <template #default="{ row }">
        <common-input-number
          v-if="
            !form.useRequisitions ||
            (form.useRequisitions && Boolean(currentCfg?.mete & basicClass) && row.outboundUnitType === measureTypeEnum.ACCOUNTING.V)
          "
          v-model="row.mete"
          :min="0.000001"
          :max="999999999"
          :controls="false"
          :step="1"
          :precision="row.accountingPrecision"
          size="mini"
          placeholder="核算量"
          @change="handleMeteChange(row)"
        />
        <span v-else>{{ row.mete || '-' }}</span>
      </template>
    </el-table-column>

    <price-set-columns weight-attribute="mete" />

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
import { defineExpose, inject, reactive, watch, watchEffect } from 'vue'
import { matClsEnum } from '@/utils/enum/modules/classification'
import { measureTypeEnum } from '@/utils/enum/modules/wms'
import { createUniqueString } from '@/utils/data-type/string'
import { positiveNumPattern } from '@/utils/validate/pattern'
import { isNotBlank, toPrecision } from '@/utils/data-type'

import useWmsConfig from '@/composables/store/use-wms-config'
import usePriceSet from '@compos/wms/use-price-set'
import useTableValidate from '@compos/form/use-table-validate'
import priceSetColumns from '@/views/wms/material-inbound/raw-material/components/price-set-columns.vue'

// 当前物料基础类型
const basicClass = matClsEnum.OTHER.V

const { purchaseCfg: currentCfg } = useWmsConfig()
const matSpecRef = inject('matSpecRef') // 调用父组件matSpecRef
const form = inject('crud')?.form

watchEffect(() => {
  // 辅材不进行核算量合计
  // let _mete = 0
  let _amount = 0
  if (isNotBlank(form.list)) {
    form.list.forEach((v) => {
      // _mete += v.mete || 0
      _amount += Number(v.amount) || 0
    })
  }
  form.amount = _amount
  form.mete = undefined
  form.meteUnit = ''
})

// 数量校验方式
const validateQuantity = (value, row) => {
  if (row.measureUnit) return !!value

  return true
}

// 金额校验
const validateAmount = (value, row) => {
  if (isNotBlank(row.mete) && isNotBlank(row.unitPrice)) {
    return +(row.mete * row.unitPrice).toFixed(2) === +value
  }
  return false
}

const rules = {
  classifyId: [{ required: true, message: '请选择物料种类', trigger: 'change' }],
  quantity: [{ validator: validateQuantity, message: '请填写数量', trigger: 'blur' }],
  mete: [
    { required: true, message: '请填写核算量', trigger: 'blur' },
    { pattern: positiveNumPattern, message: '核算量必须大于0', trigger: 'blur' }
  ],
  unitPrice: [{ required: true, message: '请填写单价', trigger: 'blur' }],
  amount: [
    { required: true, message: '请填写金额', trigger: 'blur' },
    { validator: validateAmount, message: '金额有误，请手动修改', trigger: 'blur' }
  ]
}

const { handleMeteChangeCalcPrice } = usePriceSet('mete')
const { tableValidate, wrongCellMask } = useTableValidate({ rules: rules }) // 表格校验

// 行初始化
function rowInit(row) {
  const _row = reactive({
    uid: createUniqueString(),
    sn: row.sn, // 该科目规格唯一编号
    specificationLabels: row.specificationLabels, // 规格中文
    serialNumber: row.serialNumber, // 科目编号 - 规格
    classifyId: row.classify.id, // 科目id
    classifyFullPathId: row.classify.fullPathId, // 全路径id
    classifyFullName: row.classify.fullName, // 全路径名称
    classifyName: row.classify.name, // 当前科目名称
    classifyParentFullName: row.classify.parentFullName, // 父级路径名称
    basicClass: row.classify.basicClass, // 基础类型
    specification: row.spec, // 规格
    specificationMap: row.specKV, // 规格KV格式
    measureUnit: row.classify.measureUnit, // 计量单位
    accountingUnit: row.classify.accountingUnit, // 核算单位
    accountingPrecision: row.classify.accountingPrecision, // 核算单位小数精度
    measurePrecision: row.classify.measurePrecision, // 计量单位小数精度
    mete: undefined, // 核算量
    quantity: undefined // 数量
  })
  rowWatch(_row)
  return _row
}

// 行监听
function rowWatch(row) {
  // 计算价格
  watch([() => row.mete], () => handleMeteChangeCalcPrice(row))
}

// 删除行
function delRow(sn, $index) {
  if (matSpecRef.value) {
    matSpecRef.value.delListItem(sn, $index)
  } else {
    form.list.splice($index, 1)
  }
}

function handleQuantityChange(row) {
  if (form.useRequisitions) {
    row.mete = toPrecision(row.quantity * row.unitNet, row.measurePrecision)
  }
}

function handleMeteChange(row) {
  if (form.useRequisitions) {
    row.quantity = toPrecision(row.mete * row.accountingUnitNet, row.accountingPrecision)
  }
}

// 校验
function validate() {
  const { validResult, dealList } = tableValidate(form.list)
  form.list = dealList
  form.basicClass = form.currentBasicClass
  return validResult
}

defineExpose({
  rowInit,
  validate,
  rowWatch
})
</script>
