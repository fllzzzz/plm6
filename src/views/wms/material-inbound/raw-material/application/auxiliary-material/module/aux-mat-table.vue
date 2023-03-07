<template>
  <common-table
    ref="tableRef"
    v-bind="$attrs"
    :data="form.list"
    :cell-class-name="wrongCellMask"
    :expand-row-keys="expandRowKeys"
    :show-empty-symbol="false"
    return-source-data
    row-key="uid"
    @select="selectTableChange"
    @select-all="selectAllTableChange"
  >
    <el-table-column v-if="!props.boolPartyA" type="selection" width="55" align="center" :selectable="selectable" />
    <el-expand-table-column :data="form.list" v-model:expand-row-keys="expandRowKeys" row-key="uid" fixed="left">
      <template #default="{ row }">
        <div class="mtb-10">
          <el-input
            v-model="row.remark"
            :rows="1"
            :autosize="{ minRows: 1, maxRows: 1 }"
            type="textarea"
            placeholder="备注"
            maxlength="200"
            show-word-limit
            style="width: 400px"
          />
        </div>
      </template>
    </el-expand-table-column>
    <el-table-column label="序号" type="index" align="center" width="60" fixed="left" />
    <el-table-column prop="serialNumber" label="编号" align="center" width="110px" fixed="left" show-overflow-tooltip />
    <el-table-column prop="classifyName" label="物料种类" align="center" fixed="left" min-width="150" show-overflow-tooltip>
      <template #default="{ row }">
        <el-tooltip :content="row.classifyParentFullName" :disabled="!row.classifyParentFullName" :show-after="500" placement="top">
          <span v-empty-text="row.classifyName" />
        </el-tooltip>
      </template>
    </el-table-column>
    <el-table-column prop="specification" label="规格" align="center" min-width="200px" fixed="left" show-overflow-tooltip>
      <template #default="{ row }">
        <el-tooltip :content="row.specificationLabels" placement="top">
          <span v-empty-text="row.specification" />
        </el-tooltip>
      </template>
    </el-table-column>
    <template v-if="props.boolPartyA">
      <el-table-column prop="measureUnit" label="计量单位" align="center" min-width="70px">
        <template #default="{ row }">
          <span v-empty-text>{{ row.measureUnit }}</span>
        </template>
      </el-table-column>
      <el-table-column prop="quantity" label="数量" align="center" min-width="120px">
        <template #default="{ row }">
          <common-input-number
            v-if="row.measureUnit"
            v-model="row.quantity"
            :min="0"
            :max="999999999"
            :controls="false"
            :step="1"
            :precision="row.measurePrecision"
            size="mini"
            placeholder="数量"
          />
          <span v-else v-empty-text />
        </template>
      </el-table-column>
      <el-table-column prop="accountingUnit" label="核算单位" align="center" min-width="70px">
        <template #default="{ row }">
          <span v-empty-text>{{ row.accountingUnit }}</span>
        </template>
      </el-table-column>
      <el-table-column prop="mete" label="核算量" align="center" min-width="120px">
        <template #default="{ row }">
          <common-input-number
            v-model="row.mete"
            :min="0.000001"
            :max="999999999"
            :controls="false"
            :step="1"
            :precision="row.accountingPrecision"
            size="mini"
            placeholder="核算量"
            @change="handleWeightChange($event, row)"
          />
        </template>
      </el-table-column>
    </template>
    <!-- 单位及其数量 -->
    <material-unit-quantity-columns v-else quantityField="purchaseQuantity" meteField="purchaseMete" />

    <!-- 金额设置 -->
    <price-set-columns v-if="!props.boolPartyA && fillableAmount" weight-attribute="mete" />

    <el-table-column prop="color" label="颜色" align="center" min-width="120px">
      <template #default="{ row }">
        <el-input v-model.trim="row.color" maxlength="20" size="mini" placeholder="颜色" />
      </template>
    </el-table-column>
    <el-table-column prop="brand" label="品牌" align="center" min-width="120px">
      <template #default="{ row }">
        <el-input v-model.trim="row.brand" maxlength="60" size="mini" placeholder="品牌" />
      </template>
    </el-table-column>
    <template v-if="!props.boolPartyA">
      <el-table-column prop="quantity" label="本次实收数" align="center" min-width="120px">
        <template #default="{ row }">
          <common-input-number
            v-if="row.measureUnit"
            v-model="row.quantity"
            :min="0"
            :max="999999999"
            :controls="false"
            :step="1"
            :precision="row.measurePrecision"
            size="mini"
            placeholder="本次实收数"
          />
          <span v-else v-empty-text />
        </template>
      </el-table-column>
      <el-table-column prop="mete" label="实收量" align="center" min-width="120px">
        <template #default="{ row }">
          <common-input-number
            v-model="row.mete"
            :min="0.000001"
            :max="999999999"
            :controls="false"
            :step="1"
            :precision="row.accountingPrecision"
            size="mini"
            placeholder="实收量"
            @change="handleWeightChange($event, row)"
          />
        </template>
      </el-table-column>
    </template>
    <el-table-column v-if="props.boolPartyA" label="操作" width="70" align="center" fixed="right">
      <template #default="{ row, $index }">
        <common-button icon="el-icon-delete" type="danger" size="mini" @click="delRow(row.sn, $index)" />
      </template>
    </el-table-column>
  </common-table>
</template>

<script setup>
import { defineExpose, defineProps, watchEffect, computed, ref, inject, reactive } from 'vue'
import { createUniqueString } from '@/utils/data-type/string'
import { positiveNumPattern } from '@/utils/validate/pattern'
import { isNotBlank, toPrecision } from '@/utils/data-type'

import { regExtra } from '@/composables/form/use-form'
import useTableValidate from '@compos/form/use-table-validate'
import elExpandTableColumn from '@comp-common/el-expand-table-column.vue'

import materialUnitQuantityColumns from '@/components-system/wms/table-columns/material-unit-quantity-columns/index.vue'
import priceSetColumns from '@/views/wms/material-inbound/raw-material/components/price-set-columns.vue'

const props = defineProps({
  boolPartyA: {
    type: Boolean,
    default: false
  },
  fillableAmount: {
    type: Boolean,
    default: false
  }
})

const tableRef = ref()
const matSpecRef = inject('matSpecRef') // 调用父组件matSpecRef
const { form } = regExtra() // 表单
const expandRowKeys = ref([]) // 展开行key

// 数量校验方式
const validateQuantity = (value, row) => {
  if (row.measureUnit) return !!value

  return true
}

const rules = {
  classifyId: [{ required: true, message: '请选择物料种类', trigger: 'change' }],
  quantity: [{ validator: validateQuantity, message: '请填写数量', trigger: 'blur' }],
  mete: [
    { required: true, message: '请填写核算量', trigger: 'blur' },
    { pattern: positiveNumPattern, message: '核算量必须大于0', trigger: 'blur' }
  ]
}

// 金额校验
const validateAmount = (value, row) => {
  if (isNotBlank(row.mete) && isNotBlank(row.unitPrice)) {
    return +toPrecision(row.mete * row.unitPrice, 2) === +value
  }
  return false
}

// 甲供不需要填写价格
const amountRules = {
  unitPrice: [{ required: true, message: '请填写单价', trigger: 'blur' }],
  amount: [
    { required: true, message: '请填写金额', trigger: 'blur' },
    { validator: validateAmount, message: '金额有误，请手动修改', trigger: 'blur' }
  ]
}

const tableRules = computed(() => {
  let _rules = Object.assign({}, rules)
  if (!props.boolPartyA && props.fillableAmount) {
    _rules = Object.assign(_rules, amountRules)
  }
  return _rules
})

const { tableValidate, wrongCellMask } = useTableValidate({ rules: tableRules }) // 表格校验

function selectable(row, rowIndex) {
  return !!row.canPurchaseQuantity || true
}

function selectTableChange(select, row) {
  const boolSelect = Boolean(select.findIndex((v) => v.id === row.id) !== -1)
  form.selectObj[row.purchaseOrderDetailId].isSelected = boolSelect
}

function selectAllTableChange(select) {
  const boolSelect = Boolean(select?.length)
  form.list.forEach((v) => {
    form.selectObj[v.purchaseOrderDetailId].isSelected = boolSelect
  })
}

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

  // 非甲供
  if (!props.boolPartyA && props.fillableAmount) {
    _row.unitPrice = undefined // 含税单价
    _row.amount = undefined // 金额
  }
  return _row
}

function rowWatch(row) {
  watchEffect(() => {
    if (!props.boolPartyA && isNotBlank(form.selectObj?.[row.purchaseOrderDetailId])) {
      const _isSelected = form.selectObj[row.purchaseOrderDetailId]?.isSelected
      form.selectObj[row.purchaseOrderDetailId] = {
        ...form.selectObj[row.purchaseOrderDetailId],
        ...row,
        isSelected: _isSelected
      }
    }
  })
}

// 处理重量变化
function handleWeightChange(val, row) {
  if (isNotBlank(row.unitPrice) && isNotBlank(val)) {
    row.amount = toPrecision(val * row.unitPrice, 2)
  }
}

// 删除行
function delRow(sn, $index) {
  if (matSpecRef.value) {
    matSpecRef.value.delListItem(sn, $index)
  } else {
    form.list.splice($index, 1)
  }
}

// 校验
function validate() {
  const _list = form.list.filter((v) => {
    if (props.boolPartyA || form.selectObj[v.purchaseOrderDetailId]?.isSelected) {
      return true
    } else {
      return false
    }
  })
  const { validResult } = tableValidate(_list)
  // form.list = dealList
  return validResult
}

function toggleRowSelection(row, selected) {
  tableRef?.value?.toggleRowSelection(row, selected)
}

defineExpose({
  rowInit,
  toggleRowSelection,
  rowWatch,
  validate
})
</script>
